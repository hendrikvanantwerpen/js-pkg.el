(require 's)
(require 'json)
(require 'semver)

(setq js-pkgs nil)

(defun js-pkg-res-to-files (resource &optional current-resource)
  "Give filename for a given resource (possible relative to another) or nil if package is unknown."
  (setq current-resource (or current-resource (js-pkg-buffer-res)))
  (when current-resource
    (setq resource
          (js-pkg-res-id-absolute resource current-resource)))
  (js-pkg--res-to-files resource))

(defun js-pkg-file-to-res (file &optional current-resource)
  "Give a resource for the given file, possibly relative to a current resource."
  (setq current-resource (or current-resource (js-pkg-buffer-res)))
  (let ((resource (js-pkg--file-to-res file)))
    (if current-resource
        (js-pkg-res-id-relativize resource current-resource)
      resource)))
  
(defun js-pkg-buffer-res ()
  "Return absolute resource for the current buffer or nil."
  (when (buffer-file-name)
    (js-pkg--file-to-res (buffer-file-name))))

(defun js-pkg--file-to-res (file)
  (setq file (expand-file-name file))
  (let ((pkg-info (js-pkg-info-by-file file)))
    (when pkg-info
      (js-pkg-res-id-normalize
       (concat (js-pkg-info-name pkg-info) "/"
               (s-chop-prefix (js-pkg-info-directory pkg-info)
                              file))))))

(defun js-pkg--res-to-files (resource)
  (setq resource (js-pkg-res-id-normalize resource))
  (let ((pkg-infos (js-pkg-infos-by-res resource)))
    (mapcar (lambda (info)
              (expand-file-name
               (concat (js-pkg-info-directory info)
                       (s-chop-prefix (js-pkg-info-name info)
                                      resource))))
            pkg-infos)))

(defun js-pkg-info-create (name version directory)
  (js-pkg--info-create (list (cons 'name name)
                             (cons 'version version))
                       directory
                       (current-time)))

(defun js-pkg--info-create (json directory last-changed)
  (list 'js-pkg-info json directory last-changed))

(defun js-pkg-info-json (pkg-info)
  (nth 1 pkg-info))

(defun js-pkg-info-name (pkg-info)
  (cdr (assoc 'name (js-pkg-info-json pkg-info))))

(defun js-pkg-info-version (pkg-info)
  (semver-parse (cdr (assoc 'version
                            (js-pkg-info-json pkg-info)))))

(defun js-pkg-info-directory (pkg-info)
  (nth 2 pkg-info))

(defun js-pkg-info-file (pkg-info)
  (concat (js-pkg-info-directory pkg-info)
          "package.json"))

(defun js-pkg-info-last-changed (pkg-info)
  (nth 3 pkg-info))

(defun js-pkg-info= (pkg-info with-respect-to)
  "Is this pkg-info about the same package (name,version)?"
  (and (equal (js-pkg-info-name pkg-info)
              (js-pkg-info-name with-respect-to))
       (semver= (js-pkg-info-version pkg-info)
                (js-pkg-info-version with-respect-to))))

(defun js-pkg-info-by-file (file)
  "Returns package info for this file or nil."
  (let ((package-file
         (js-pkg--find-file-up "package.json" file)))
    (when package-file
      (let ((pkg-info (js-pkg--from-pkgs-by-directory
                       (file-name-directory package-file))))
        (if pkg-info
            pkg-info
          (let ((pkg-info (js-pkg-info-read package-file)))
            (if (not pkg-info)
                (error "Invalid package.json found.")
              (js-pkg--safe-add-to-pkgs pkg-info)
              pkg-info)))))))

(defun js-pkg--from-pkgs-by-directory (directory)
  "Find a package in our internal list. The directory has to match exactly."
  (js-pkg--update-pkgs)
  (catch 'found
    (mapc (lambda (pkg-info)
            (when (equal directory
                         (js-pkg-info-directory pkg-info))
              (throw 'found pkg-info)))
          js-pkgs)
    nil ; the nil seems needed, otherwise the last one is returned
    ))

(defun js-pkg--safe-add-to-pkgs (pkg-info)
  "Add a package to the internal list if we don't have it already. This does not update a package with new info."
  (catch 'found
    (mapc (lambda (info)
            (when (js-pkg-info= pkg-info info)
              (unless (equal (js-pkg-info-directory pkg-info)
                             (js-pkg-info-directory info))
                (error "Package with same version already exists in different location."))
              (throw 'found nil)))
          js-pkgs)
    (setq js-pkgs (cons pkg-info js-pkgs))))

(defun js-pkg--update-pkgs ()
  (let ((curr js-pkgs))
    (while curr
      (let* ((pkg-info (car curr))
             (pkg-file (js-pkg-info-file pkg-info)))
        (if (not (file-exists-p pkg-file))
            (setq js-pkgs (delq pkg-info js-pkgs))
          (when (js-pkg--time-compare
                 (js-pkg--file-last-changed pkg-file)
                 (js-pkg-info-last-changed pkg-info))
            (let ((new-info (js-pkg-info-read pkg-file)))
              (setcar curr new-info)))))
      (setq curr (cdr curr)))))

(defun js-pkg--time-compare (time with-respect-to)
  (unless (= (length time)
           (length with-respect-to))
    (error "Time stamps have different length."))
  (let ((diff 0))
    (while (and time
                (= diff 0))
      (setq diff (- (car time)
                    (car with-respect-to)))
      (setq time (cdr time))
      (setq with-respect-to (cdr with-respect-to)))
    diff))
  
(defun js-pkg-infos-by-res (resource)
  "Returns a list of matching packages for this resource."
  (setq resource (js-pkg-res-id-normalize resource))
  (let ((pkg-infos nil)
        (pkg-name ""))
    (mapc (lambda (info)
            (let* ((name (js-pkg-info-name info)))
              (when (s-starts-with? name resource)
                (cond ((string< pkg-name name)
                       (setq pkg-infos (list info))
                       (setq pkg-name name))
                      ((equal pkg-name name)
                       (setq pkg-infos (cons info pkg-infos)))))))
          js-pkgs)
    pkg-infos))

(defun js-pkg--find-file-up (file start)
  (let ((reverse-parts
         (reverse (s-split "/" (file-name-directory
                                (expand-file-name start)) t))))
    (catch 'found
      (while reverse-parts
        (let* ((search-file
                (concat "/" (s-join "/" (reverse reverse-parts))
                        "/" file)))
          (if (file-exists-p search-file)
              (throw 'found search-file)
            (setq reverse-parts (cdr reverse-parts))))))))

(defun js-pkg-info-read (file)
  (let ((json (json-read-from-string
               (js-pkg--read-file file))))
    (js-pkg--info-create
     json
     (file-name-directory file)
     (js-pkg--file-last-changed file))))

(defun js-pkg-info-write (pkg-info)
  (js-pkg--write-file
   (json-encode (js-pkg-info-json pkg-info))
   (js-pkg-info-file pkg-info)))

(defun js-pkg--file-last-changed (file)
  (let* ((previous-buffer (find-buffer-visiting file)))
    (if previous-buffer
        (with-current-buffer previous-buffer
          (visited-file-modtime))
      (nth 5 (file-attributes file)))))

(defun js-pkg--read-file (file)
  (let* ((previous-buffer (find-buffer-visiting file)))
    (with-current-buffer (or previous-buffer
                             (find-file-noselect file))
      (let ((content (buffer-string)))
        (unless previous-buffer
          (kill-buffer))
        content))))

(defun js-pkg--write-file (string file)
  (let* ((previous-buffer (find-buffer-visiting file)))
    (with-current-buffer (or previous-buffer
                             (find-file-noselect file))
      (delete-region (point-min) (point-max))
      (insert string)
      (save-buffer)
      (unless previous-buffer
        (kill-buffer))
      )))

;;; Resource id manipulation

(defun js-pkg-res-id-split (resource)
  "Return array with resource id parts.

A resource id cannot start with a /, if this is there, it is silently stripped.
"
  (let* ((end-slash (s-ends-with? "/" resource))
         (parts (s-split "/" resource t)))
    (when end-slash
      (nconc parts (list "")))
    parts))

(defun js-pkg-res-id-join (parts)
  "Join the parts of js-pkg-resource-split into a resource."
  (s-join "/" parts))

(defun js-pkg-res-id-relative-p (string)
  "Return wether resource is relative or not."
  (string-match "^\\.\\.?/" string))

(defun js-pkg-res-id-directory (resource)
  "Return the folder part of the resource id."
  (let ((parts (js-pkg-res-id-split resource)))
    (when parts
      (setcar (last parts) ""))
    (js-pkg-res-id-join parts)))

(defun js-pkg-res-id-normalize (resource)
  "Normalize a resource id.

The result is that all relative components are at the beginning.
If the resource id was an absolute one, the normalized version will also be absolute or nil.
"
  (let* ((result nil))
    (catch 'failed
      (mapc (lambda (part)
              (cond ((equal "." part)
                     (if (not result)
                         (setq result (cons part result))))
                    ((equal ".." part)
                     (cond ((or (not result)
                                (equal ".." (car result)))
                            (setq result (cons part result)))
                           ((equal "." (car result))
                            (setq result (cons part (cdr result))))
                           ((= 1 (length result))
                            (throw 'failed nil))
                           (t
                            (setq result (cdr result)))))
                    (t (setq result (cons part result)))))
            (js-pkg-res-id-split resource))
      (js-pkg-res-id-join (reverse result)))))

(defun js-pkg-res-id-absolute (resource reference)
  "Return absolute resource with respect to reference.

If reference is relative, the result will be relative.
If reference is absolute and resource lies outside of the reference path, we return nil. "
  (if (js-pkg-res-id-relative-p resource)
      (when reference
        (js-pkg-res-id-normalize
         (concat (js-pkg-res-id-directory reference)
                 resource)))
    resource))

(defun js-pkg-res-id-relativize (resource reference)
  "Returns a relative resource if possible, otherwise keeps unchanged."
  (let ((norm-res (js-pkg-res-id-normalize resource))
        (norm-ref (js-pkg-res-id-normalize reference)))
    (if (or (not norm-res)
            (not norm-ref)
            (js-pkg-res-id-relative-p norm-res)
            (js-pkg-res-id-relative-p norm-ref))
        (or norm-res resource)
      (let ((prefix (js-pkg-res-id-directory
                     (s-shared-start resource reference))))
        (if (s-blank? prefix)
            resource
          (let* ((res-rest (s-chop-prefix prefix resource))
                 (ref-rest (js-pkg-res-id-split
                            (s-chop-prefix prefix reference)))
                 (diff (1- (length ref-rest))))
            (if (zerop diff)
                (s-concat "./" res-rest)
              (s-concat (s-repeat diff "../") res-rest))))))))

(provide 'js-pkg)

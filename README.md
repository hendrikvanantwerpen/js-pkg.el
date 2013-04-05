js-pkg.el
=========

Emacs CommonJS/NPM Package Library. This library provides functions to help with the discovery of CommonJS/NPM style packages.

CommonJS/NPM packages use a package.json file in their root directory. The library is build around the premise that packages live on disk. It therefore automatically discovers packages in parent directories when file functions like 'js-pkg-file-to-res' are used and tracks changes to the package files.

Internally js-pkg maintains a list of discovered packages. This list is used to convert files to resource ids and vice versa. It can handle multple versions of one package but will bark when two versions of the same package are found. Package.json updates on disk are detected and the package info will be updated automatically.

This library also contains a bunch of js-pkg-res-id-* to manipulate resource ids of the form 'package/some/file/within'.

Install
-------

Manually: put js-pkg.el file in your load-path and (require 'js-pkg).

El-get: Evaluate the following snippet and install with el-get-install.

    (setq el-get-sources
     (cons '(:name js-pkg
             :type github
             :pkgname "hendrikvanantwerpen/js-pkg.el"
             :depends (semver json s))
           el-get-sources))

Ideas
-----

 * Change the functions that take a resource to accept optional semver predicates and use the highest package version. If predicates are provided, use the highest package version that matches a predicate.

License
-------

    Copyright 2013 Hendrik van Antwerpen

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

;;; dev-env-meson.el --- Support for Meson use

;; Copyright (C) 2020 Rémi Hérilier

;; Author: Rémi Hérilier <rherilier@yahoo.fr>
;; Created: 2020-03-02
;; Version: 0.1
;; Keywords: Meson
;; Homepage:
;; Package-Requires:

;; This file is not part of GNU Emacs.

;; This file is free software... see <http://www.wtfpl.net/>.

;;; Code:
(defcustom dev-env-meson-user-options
  nil
  "user defined Meson parameter list"
  :group 'dev-env
  :type '(repeat string))

(defcustom dev-env-meson-system-name
  nil
  "user defined Meson system name (for cross compilation)"
  :group 'dev-env
  :risky t
  :type 'string)

(defvar dev-env-meson-build-type
  nil
  "Meson build type")

(defcustom dev-env-meson--build-types
  '("debugoptimized" "release" "debug" "plain", "minsize")
  "list of common Meson build type"
  :group 'dev-env
  :risky nil
  :type 'string)

(defvar-local dev-env-meson--internal-options
  nil
  "user defined Meson parameter list")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dev-env-meson--ask-build-type ()
  "ask for Meson build type"
  (completing-read "Meson build target: "
                   dev-env-meson--build-types nil t
                   (if dev-env-meson-build-type
                       dev-env-meson-build-type
                     (car dev-env-meson--build-types))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dev-env-meson--get-build-dir (build-type)
  "return the Meson build directory for build type BUILD-TYPE relatively to project root"
  (concat (file-name-as-directory "build")
          (file-name-as-directory (downcase build-type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dev-env-meson--get-configure-command (root-dir build-type)
  "return the Meson configure command according to ROOT-DIR and  BUILD-TYPE"
  (mapconcat 'identity
             (append (if dev-env-user-c-compiler
                         (list (format "CC=%s" dev-env-user-c-compiler))
                       nil)
                     (if dev-env-user-cxx-compiler
                         (list (format "CXX=%s" dev-env-user-cxx-compiler))
                       nil)
                     (list "meson" "setup")
                     dev-env-meson--internal-options
                     dev-env-meson-user-options
                     (if build-type
                         (list (format "--buildtype %s" build-type))
                       nil)
                     (list root-dir))
             " "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dev-env-meson)
;;; dev-env-meson.el ends here

;;; dev-env-cmake.el --- Support for CMake use

;; Copyright (C) 2020 Rémi Hérilier

;; Author: Rémi Hérilier <rherilier@yahoo.fr>
;; Created: 2020-03-02
;; Version: 0.1
;; Keywords: CMake
;; Homepage:
;; Package-Requires:

;; This file is not part of GNU Emacs.

;; This file is free software... see <http://www.wtfpl.net/>.

;;; Code:
(defcustom dev-env-cmake-user-options
  nil
  "user defined CMake parameter list"
  :group 'dev-env
  :type '(repeat string))

(defcustom dev-env-cmake-system-name
  nil
  "user defined CMake system name (for cross compilation)"
  :group 'dev-env
  :risky t
  :type 'string)

(defvar dev-env-cmake-build-type
  nil
  "CMake build type")

(defcustom dev-env-cmake--build-types
  '("RelWithDebInfo" "Release" "Debug" "MinSizeRel")
  "list of common CMake build type"
  :group 'dev-env
  :risky nil
  :type 'string)

(defvar-local dev-env-cmake--internal-options
  '("-DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
  "user defined CMake parameter list")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dev-env-cmake--ask-build-type ()
  "ask for CMake build type"
  (completing-read "CMake build target: "
                   dev-env-cmake--build-types nil t
                   (if dev-env-cmake-build-type
                       dev-env-cmake-build-type
                     (car dev-env-cmake--build-types))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dev-env-cmake--get-build-dir (build-type)
  "return the CMake build directory for build type BUILD-TYPE relatively to project root"
  (concat (file-name-as-directory "build")
          (file-name-as-directory (downcase build-type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dev-env-cmake--get-configure-command (root-dir build-type)
  "return the whole CMake configure command to build project from ROOT-DIR for target BUILD-TYPE"
  (mapconcat 'identity
             (append (list "cmake")
                     dev-env-cmake--internal-options
                     dev-env-cmake-user-options
                     (if build-type
                         (list (format "-DCMAKE_BUILD_TYPE=%s" build-type))
                       nil)
                     (if dev-env-user-c-compiler
                         (list (format "-DCMAKE_C_COMPILER=%s" dev-env-user-c-compiler))
                       nil)
                     (if dev-env-user-cxx-compiler
                         (list (format "-DCMAKE_CXX_COMPILER=%s" dev-env-user-cxx-compiler))
                       nil)
                     (if dev-env-cmake-system-name
                         (list (format "-DCMAKE_SYSTEM_NAME=%s" dev-env-cmake-system-name))
                       nil)
                     (list root-dir)
                     )
             " "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dev-env-cmake)
;;; dev-env-cmake.el ends here

;;; cmake-build.el --- Support for CMake use

;; Copyright (C) 2019 Rémi Hérilier

;; Author: Rémi Hérilier <rherilier@yahoo.fr>
;; Created: 2019-03-22
;; Version: 0.0.1
;; Keywords: CMake
;; Homepage: 
;; Package-Requires: (ido)

;; This file is not part of GNU Emacs.

;; This file is free software... see <http://www.wtfpl.net/>.

;; To use it, call cmake-build-project-init to initialize your project build context.
;; You can add whatever you need to cmake-build-init-hook.

;; TODO: add a cmake-build-mode to activate it: -init must be called once to permit to
;;       customize it...

;;; Code:
(require 'ido)

(defcustom cmake-build--types
  '("relwithdebinfo" "release" "debug" "minsizerel")
  "list of common CMake build type")

(defcustom cmake-build-init-hook
  nil
  "List of functions to be called after a project has been initialized"
  :group 'cmake-build
  :type 'hook)

(defcustom cmake-build-user-c-compiler
  nil
  "user defined CMake C compiler"
  :group 'cmake-build
  :risky t
  :type 'string)

(defcustom cmake-build-user-cxx-compiler
  nil
  "user defined CMake C++ compiler"
  :group 'cmake-build
  :risky t
  :type 'string)

(defcustom cmake-build-user-cmake-options
  nil
  "user defined CMake parameter list"
  :group 'cmake-build
  :type '(repeat string))

(defcustom cmake-build-user-build-basename
  "build"
  "base directory name to use for build"
  :group 'cmake-build
  :risky t
  :type 'string)

(defcustom cmake-build-use-build-type
  t
  "must cmake-build-build-dir use the CMake build-type or not"
  :group 'cmake-build
  :type 'boolean)

(defvar cmake-build-project-dir
  nil
  "CMake project root directory")

(defvar cmake-build-build-dir
  nil
  "CMake project build directory")

(defvar-local cmake-build--build-type
  nil
  "CMake build type")


(defvar-local cmake-build--internal-cmake-options
  '("-DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
  "user defined CMake parameter list")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmake-build--ask-build-type ()
  "ask for build type"
  (completing-read "build target: "
                   cmake-build--types nil t (car cmake-build--types)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmake-build--execute (project-dir build-dir build-type)
  "run CMake with its options list"
  (let ((default-directory (file-name-as-directory build-dir))
        (cmd (mapconcat 'identity
                        (append (list "cmake")
                                cmake-build--internal-cmake-options
                                cmake-build-user-cmake-options
                                (cond (cmake-build-user-c-compiler
                                       (list (format "-DCMAKE_C_COMPILER=%s"
                                                     cmake-build-user-c-compiler)))
                                      (cmake-build-user-cxx-compiler
                                       (list (format "-DCMAKE_CXX_COMPILER=%s"
                                                     cmake-build-user-cxx-compiler)))
                                      (t ()))
                                (list (format "-DCMAKE_BUILD_TYPE=%s"
                                              cmake-build--build-type)
                                      (shell-quote-argument project-dir)))
                        " "))
        (cmf (concat (file-name-as-directory project-dir)
                     "CMakeLists.txt")))
    (if (file-exists-p cmf)
        (progn (message "running %s" cmd)
               (async-shell-command cmd))
      (progn (message "no CMakeLists.txt file found")
             (cmake-build--project-kill)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmake-build--find-project-dir ()
  "find or ask for CMake a project directory"
  (let ((pdir (locate-dominating-file default-directory ".git")))
    (unless (and pdir (file-exists-p (concat (file-name-as-directory pdir) "CMakelists.txt")))
      (setq pdir (ido-read-directory-name "project root directory:" default-directory)))
    (setq cmake-build-project-dir (expand-file-name pdir))
    pdir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmake-build--init-paths ()
  "initialize internal variable about the project"
  (if (not (cmake-build--find-project-dir))
      (progn (message "error can not find a project directory")
             (cmake-build--project-kill)
             nil)
    (progn (setq cmake-build--build-type
                 (cmake-build--ask-build-type))
           (setq cmake-build-build-dir
                 (expand-file-name (concat (file-name-as-directory cmake-build-project-dir)
                                           (file-name-as-directory cmake-build-user-build-basename)
                                           (when cmake-build-use-build-type
                                             (file-name-as-directory cmake-build--build-type)))))
           (make-directory cmake-build-build-dir t)
           t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmake-build-project-init ()
  "prepare a CMake based project from the nearest parent project directory"
  (interactive)
  (if (not (cmake-build--init-paths))
      (progn (message "error initializing project: no root directory")
             (cmake-build--project-kill)
             nil)
    (let ((cmf (concat (file-name-as-directory cmake-build-build-dir)
                       "Makefile")))
      (progn (when (not (file-exists-p cmf))
               (cmake-build--execute cmake-build-project-dir
                                   cmake-build-build-dir
                                   cmake-build--build-type))
             (run-hooks 'cmake-build-init-hook)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmake-build--project-kill ()
  "clear a CMake based project from the nearest parent project directory"
  (interactive)
  (setq cmake-build-project-dir nil
        cmake-build-build-dir nil
        cmake-build--build-type nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmake-build-is-active ()
  ""
  (if cmake-build-project-dir
      't
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmake-build-compile ()
  "run compilation in the defined CMake build directory (or fallback to
convential compile call"
  (interactive)
  (let* ((ddir (cond (cmake-build-build-dir
                      cmake-build-build-dir)
                     (buffer-file-name
                      buffer-file-name)
                     (t
                      default-directory)))
         (default-directory (file-name-as-directory ddir)))
    (call-interactively #'compile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode cmake-build-mode
  "helper to configure and build CMake projects"
  :init-value t
  :group 'cmake-build
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'cmake-build)
;;; filename ends here

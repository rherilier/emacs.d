;;; dev-env.el --- My development environnement configuration

;; Copyright (C) 2020 Rémi Hérilier

;; Author: Rémi Hérilier <rherilier@yahoo.fr>
;; Created: 2020-02-27
;; Version: 0.2
;; Keywords: CMake, Make, projectile, window-purpose
;; Homepage:
;; Package-Requires: (projectile)

;; This file is not part of GNU Emacs.

;; This file is free software... see <http://www.wtfpl.net/>.

;; To use it, call dev-env-project-initialize to initialize your project build context.
;; You can add whatever you need to dev-env-reconfigure-hook.

;;; Code:
(require 'projectile)
(require 'dev-env-layout)
(require 'dev-env-cmake)
(require 'dev-env-meson)
(require 'dev-env-qmake)

(require 'dev-env-style)

(defgroup dev-env nil
  "Settings for dev-env."
  :group 'convenience)

(defvar dev-env-build-dir
  nil
  "dev-env build directory")


(defcustom dev-env-init-hook
  nil
  "List of functions to be called after a project has been initialize"
  :group 'dev-env
  :type 'hook)

(defcustom dev-env-reconfigure-hook
  nil
  "List of functions to be called after a project has been (re)configured"
  :group 'dev-env
  :type 'hook)

(defcustom dev-env-user-c-compiler
  nil
  "user defined C compiler"
  :group 'dev-env
  :risky t
  :type 'string)

(defcustom dev-env-user-cxx-compiler
  nil
  "user defined C++ compiler"
  :group 'dev-env
  :risky t
  :type 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dev-env-project-initialize ()
  "initialize project"
  (interactive)
  (progn (projectile-mode +1)
         (when (not (projectile-project-root))
           (if (member default-directory (projectile-relevant-known-projects))
               (setq projectile-project-root default-directory)
             (progn (call-interactively 'projectile-add-known-project)
                    (setq projectile-project-root (projectile-ensure-project nil)))))
         (dev-env-layout-init)
         (dev-env-project-reconfigure)
	 (dev-env-style-detect (projectile-project-root))
         (run-hooks 'dev-env-init-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dev-env--project-configure (root-dir build-type build-dir config-cmd)
  "do all"
  (let ((path (concat (file-name-as-directory root-dir) (file-name-as-directory build-dir))))
    (progn (setq dev-env-build-dir path
		 projectile-project-compilation-dir build-dir
		 projectile-project-configure-cmd config-cmd)
	   (make-directory path t)
           (projectile-configure-project config-cmd)
           (run-hooks 'dev-env-reconfigure-hook))))

(defun dev-env-project-reconfigure ()
  "reconfigure the current project"
  (interactive)
  (let ((root (projectile-project-root))
        (type (projectile-project-type)))
    (if (not root)
        (message "no project found")
      (cond
       ((eq type 'cmake)
        (let* ((dev-env-cmake-build-type (dev-env-cmake--ask-build-type))
               (build-dir (dev-env-cmake--get-build-dir dev-env-cmake-build-type))
               (config-cmd (dev-env-cmake--get-configure-command root dev-env-cmake-build-type)))
          (dev-env--project-configure root dev-env-cmake-build-type build-dir config-cmd)))
       ((eq type 'meson)
        (let* ((dev-env-meson-build-type (dev-env-meson--ask-build-type))
               (build-dir (dev-env-meson--get-build-dir dev-env-meson-build-type))
               (config-cmd (dev-env-meson--get-configure-command root dev-env-meson-build-type)))
          (dev-env--project-configure root dev-env-meson-build-type build-dir config-cmd)))
       ((eq type 'qmake)
        (let* ((build-dir "build")
               (config-cmd (dev-env-qmake--get-configure-command root)))
          (dev-env--project-configure root dev-env-meson-build-type build-dir config-cmd)))
       ((eq type 'make)
	(setq projectile-project-compilation-cmd "bear make"
	      dev-env-build-dir root
	      projectile-project-compilation-dir ""))
       (t (message "project type '%s' is not supported" type))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'style-local nil 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dev-env)
;;; dev-env.el ends here

;;; dev-env-qmake.el --- Support for QMake use

;; Copyright (C) 2020 Rémi Hérilier

;; Author: Rémi Hérilier <rherilier@yahoo.fr>
;; Created: 2020-04-01
;; Version: 0.1
;; Keywords: QMake
;; Homepage:
;; Package-Requires:

;; This file is not part of GNU Emacs.

;; This file is free software... see <http://www.wtfpl.net/>.

;;; Code:
(defcustom dev-env-qmake-user-options
  nil
  "user defined QMake parameter list"
  :group 'dev-env
  :type '(repeat string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dev-env-qmake--project-p ()
  "Check if a project contains *.pro files."
  (projectile-verify-file-wildcard "?*.pro"))

(projectile-register-project-type 'qmake #'dev-env-qmake--project-p
                                  :configure "qmake"
                                  :compile "bear make"
                                  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dev-env-qmake--get-build-dir ()
  "return the QMake build directory relatively to project root"
  (concat (file-name-as-directory "build")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dev-env-qmake--get-configure-command (root-dir)
  "return the QMake configure command according to ROOT-DIR"
  (mapconcat 'identity
             (append (list "qmake")
                     dev-env-qmake-user-options
                     (list root-dir))
             " "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dev-env-qmake)
;;; dev-env-qmake.el ends here

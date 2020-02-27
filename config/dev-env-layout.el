;;; dev-env-layout.el --- Customized layout

;; Copyright (C) 2020 Rémi Hérilier

;; Author: Rémi Hérilier <rherilier@yahoo.fr>
;; Created: 2020-03-02
;; Version: 0.1
;; Keywords: layout
;; Homepage:
;; Package-Requires: (window-purpose)

;; This file is not part of GNU Emacs.

;; This file is free software... see <http://www.wtfpl.net/>.

;;; Code:
(require 'window-purpose)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dev-env--layout-hook ()
  ""
  (purpose-toggle-window-purpose-dedicated))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dev-env--layout-register-modes (purpose &rest list)
  ""
  (mapc (lambda (mode)
          (progn (add-to-list 'purpose-user-mode-purposes `(,mode ,purpose))
                 (add-hook mode 'dev-env--layout-hook)))
        list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dev-env-layout-init ()
  ""
  (interactive)
  (progn
    (setq split-height-threshold 1200)
    (setq split-width-threshold 2000)
    (dev-env--layout-register-modes 'edit 'c-mode 'c++-mode 'python-mode 'shell-mode 'emacs-lisp-mode 'cuda-mode 'glsl-mode 'cmake-mode 'makefile-mode 'xml-mode 'css-mode 'html-mode)
    (dev-env--layout-register-modes 'output 'compilation-mode 'completion-list-mode 'messages-buffer-mode 'lisp-interaction-mode 'help-mode)
    ;; build it
    (purpose-compile-user-configuration)
    ;; resetting the windows
    (delete-other-windows)
    ;; creating output window
    (purpose-set-window-purpose 'output t)
    (purpose-set-window-purpose-dedicated-p (selected-window) t)
    ;; next window
    (split-window-below)
    ;; creating edit window
    (purpose-set-window-purpose 'edit t)
    (purpose-set-window-purpose-dedicated-p (selected-window) t)

    ;; killing fucking pu-dummy-* buffers
    (mapc (lambda (buffer)
            (let ((name (buffer-name buffer)))
              (when (string-match "^\\*pu-dummy-.*$" name)
                (kill-buffer buffer))))
          (buffer-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dev-env-layout)
;;; dev-env-layout.el ends here


(require 'window-purpose)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dev-layout-hook ()
  ""
  (purpose-toggle-window-purpose-dedicated))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dev-layout--register-modes (purpose &rest list)
  ""
  (mapc (lambda (mode)
          (progn (add-to-list 'purpose-user-mode-purposes `(,mode ,purpose))
                 (add-hook mode 'dev-layout-hook)))
        list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dev-layout-init ()
  ""
  (interactive)
  (progn
    (setq split-height-threshold 1200)
    (setq split-width-threshold 2000)
    (dev-layout--register-modes 'edit 'c-mode 'c++-mode 'python-mode 'shell-mode 'emacs-lisp-mode 'cuda-mode 'glsl-mode)
    (dev-layout--register-modes 'output 'compilation-mode 'completion-list-mode 'messages-buffer-mode 'lisp-interaction-mode 'help-mode)
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
          (buffer-list))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dev-layout)

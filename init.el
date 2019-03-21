
;; inspiration from https://gist.github.com/huytd/6b785bdaeb595401d69adc7797e5c22c
;; TODO:
;; * have a less annoying development layout

(add-to-list 'load-path "~/.emacs.d/config")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package stuff

(package-initialize)

(require 'package)
(require 'cl-lib)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ;;("elpa" . "https://elpa.gnu.org/packages/")
                         ))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDO

(use-package ido
  :config
  (setq ido-enable-flex-matching t
        ido-case-fold nil
        )
  (ido-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP

(use-package lsp-mode
  :init
  (setq lsp-enable-snippet t
        lsp-prefer-flymake nil
        lsp-auto-configure nil
        lsp-clients-clangd-executable "clangd-9"
        lsp-clients-clangd-args (list "-header-insertion-decorators=true"
                                      "-all-scopes-completion=false"
                                      "-completion-style=detailed" ;; or "bundled"
                                      "-clang-tidy=false"
                                      "-completion-style=false"
                                      "-function-arg-placeholders=true"
                                      "-header-insertion=false"
                                      )
        ;;lsp-clients-clangd-args (list "-log=verbose" "-pretty")
        )
  :config
  (require 'lsp-clients)
  (add-hook 'prog-major-mode #'lsp-prog-major-mode-enable)
  :hook
  (c++-mode . lsp)
  )

(use-package lsp-ui
  :disabled
  :after
  lsp-mode
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-ignore-duplicate t
        )
  (define-key lsp-ui-mode-map (kbd "C-c l") 'lsp-ui-imenu)
  :hook
  (lsp-mode . lsp-ui-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company

(use-package company
  :init
  (setq company-minimum-prefix-length 2
        company-auto-complete nil
        company-idle-delay 0
        company-require-match 'never
        tab-always-indent 'complete
        )
  (defvar completion-at-point-functions-saved nil)
  :hook
  (after-init . global-company-mode)
  :config
  (progn (delete 'company-clang company-backends)
         (delete 'company-semantic company-backends)
         (delete 'company-xcode company-backends)
         (delete 'company-eclim company-backends)
         )
  (define-key company-active-map (kbd "TAB") 'company-complete-common)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common)
  (defun company-indent-for-tab-command (&optional arg)
    (interactive "P")
    (let ((completion-at-point-functions-saved completion-at-point-functions)
          (completion-at-point-functions '(company-complete-common-wrapper)))
      (indent-for-tab-command arg)))
  (defun company-complete-common-wrapper ()
    (let ((completion-at-point-functions completion-at-point-functions-saved))
      (company-complete-common)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company-lsp

(use-package company-lsp
  :after
  company
  :init
  (push 'company-lsp company-backends)
  :config
  (setq company-lsp-enable-snippet t
        company-lsp-enable-recompletion t
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp... for config editing :-P

(use-package company-elisp
  :after
  company
  :config
  (push 'company-elisp company-backends)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sh

(use-package company-shell
  :after
  company
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env company-fish-shell))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet

(use-package yasnippet
  :config
  (yas-global-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CMake stuff

(use-package company-cmake
  :after
  company
  :config
  (add-to-list 'company-backends 'company-cmake)
  )

(use-package cmake-mode)

(use-package cmake-build
  :after
  (lsp-mode cmake-mode)
  :bind
  ("C-c b i" . cmake-build-project-init)
  :config
  (cmake-build-mode 1)
  :hook
  (cmake-build-init . (lambda ()
                        (if cmake-build-build-dir
                            (progn (setq lsp-clients-clangd-args
                                         (cl-remove-if (lambda (arg)
                                                (s-prefix? "-compile-commands-dir=" arg))
                                              lsp-clients-clangd-args))
                                   (push (format "-compile-commands-dir=%s"
                                                 cmake-build-build-dir)
                                         lsp-clients-clangd-args)
                                   (if (lsp-workspaces)
                                       (lsp-restart-workspace))))))
  )

(defun my:compile ()
  ""
  (interactive)
  (if (and (fboundp 'cmake-build-is-active)
           (cmake-build-is-active))
      (cmake-build-compile)
    (call-interactively #'compile)
    )
  )

(global-set-key [f9] 'my:compile)
(global-set-key [f10] 'recompile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++

(font-lock-add-keywords 'c-mode
                        '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
                          ("\\<\\(BUG\\):" 1 font-lock-warning-face prepend)
                          ("\\<\\(TODO\\):" 1 font-lock-warning-face prepend)
                          ))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(font-lock-add-keywords 'c++-mode
                        '(("\\<\\(alignas\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(constexpr\\)\\>" . font-lock-keyword-face)
			  ("\\<\\(export\\)\\>" . font-lock-keyword-face)
                          ))
(font-lock-add-keywords 'c++-mode
                        '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
                          ("\\<\\(BUG\\):" 1 font-lock-warning-face prepend)
                          ("\\<\\(TODO\\):" 1 font-lock-warning-face prepend)
                          ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLSL/CUDA

(use-package company-glsl
  :after
  company
  :config
  (add-to-list 'company-backends 'company-glsl)
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
  )

(use-package cuda-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Qt ui/rcc

(use-package nxml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ui\\'" . xml-mode))
  (add-to-list 'auto-mode-alist '("\\.qrc\\'" . xml-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Qt qss

(use-package nxml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.qss\\'" . css-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer layout

(use-package dev-layout
  :bind
  ("C-c l i" . dev-layout-init)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config bootstrap

(global-set-key (kbd "s-c s-b")
		'(lambda ()
		   (interactive)
		   (progn (package-refresh-contents)
			  (package-install-selected-packages))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; correct short-cuts...

(cua-mode t)

;;; window focus switch
(global-set-key (kbd "<C-tab>")
		'other-window)

(global-set-key (kbd "C-S-<iso-lefttab>")
		'(lambda ()
		   (interactive)
		   (other-window -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general configuration

(set-fringe-mode '(nil . 0))
(show-paren-mode t)

(setq-default column-number-mode t
              current-language-environment "UTF-8"
              global-font-lock-mode t
              indicate-buffer-boundaries 'left
              indicate-empty-lines t
              inhibit-startup-screen t
              overflow-newline-into-fringe t
              show-trailing-whitespace t
              tool-bar-mode nil
              )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local configuration

(require 'config-local)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (adoc-mode smart-tabs-mode yasnippet company-shell window-purpose cuda-mode glsl-mode company-glsl company-jedi use-package lsp-ui company-lsp blank-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:background "gray80")))))

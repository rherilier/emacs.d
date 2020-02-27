
;; inspiration from https://gist.github.com/huytd/6b785bdaeb595401d69adc7797e5c22c
;; TODO:
;; * have a less annoying development layout
;; * better c style detection (see https://www.emacswiki.org/emacs/ProjectSettings)

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
        ido-case-fold nil)
  (ido-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projectile

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CCLS

(use-package ccls
  :init
  (setq ccls-executable "/usr/bin/ccls")
  :hook
  ((c-mode c++-mode) .
   (lambda () (require 'ccls) (lsp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP

(use-package lsp-mode
  :after
  ccls
  :init
  (setq lsp-auto-configure nil
        lsp-enable-snippet t
        lsp-prefer-flymake nil
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        lsp-prefer-capf t
        )
  (push "[/\\\\]build$" lsp-file-watch-ignored)
  (push "[/\\\\]cross$" lsp-file-watch-ignored)
  (push "[/\\\\]data$" lsp-file-watch-ignored)
  :commands
  lsp
  :config
  (require 'lsp-clients)
  (add-hook 'prog-major-mode #'lsp-prog-major-mode-enable)
  :hook
  (c++-mode c-mode. lsp))

(use-package lsp-ui
  :after
  lsp-mode
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-ignore-duplicate t)
  (define-key lsp-ui-mode-map (kbd "s-d l") 'lsp-ui-imenu)
  :hook
  (lsp-mode . lsp-ui-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company

(use-package company
  :init
  (setq company-minimum-prefix-length 2
        company-auto-complete nil
        company-idle-delay 0
        company-require-match 'never
        tab-always-indent 'complete)
  (defvar completion-at-point-functions-saved nil)
  :hook
  (after-init . global-company-mode)
  :config
  (setq company-backends nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company-lsp

(use-package company-lsp
  :after
  company
  :init
  (push 'company-lsp company-backends)
  :config
  (setq company-lsp-enable-snippet t
        company-lsp-enable-recompletion t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp... for config editing :-P

(use-package company-elisp
  :after
  company
  :config
  (push 'company-elisp company-backends))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sh

(use-package company-shell
  :after
  company
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env company-fish-shell)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet

(use-package yasnippet
  :config
  (yas-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CMake stuff

(use-package cmake-mode)

(use-package company-cmake
  :after
  company
  :config
  (add-to-list 'company-backends 'company-cmake))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; project stuff

(use-package dev-env
  :after
  (projectile ccls)
  :bind
  ("s-d i" . dev-env-project-initialize)
  ("s-d c" . dev-env-project-reconfigure)
  ("<f9>" . projectile-compile-project)
  ("<f10>" . recompile)
  ("<f11>" . next-error)
  ("<f12>" . previous-error)
  :hook
  (dev-env-reconfigure . (lambda ()
                           (if dev-env-build-dir
                               (progn (setq ccls-initialization-options `(:compilationDatabaseDirectory ,dev-env-build-dir)
                                            ;;ccls-args '("--log-file=ccls.log" "-v=1")
                                            )
                                      (if (lsp-workspaces)
                                          (lsp-restart-workspace))
                                      )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++

(font-lock-add-keywords 'c-mode
                        '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
                          ("\\<\\(BUG\\):" 1 font-lock-warning-face prepend)
                          ("\\<\\(TODO\\):" 1 font-lock-warning-face prepend)))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(font-lock-add-keywords 'c++-mode
                        '(("\\<\\(alignas\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(constexpr\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(export\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(Q_OBJECT\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(Q_SIGNALS\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(Q_SLOTS\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(Q_EMIT\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
                          ("\\<\\(BUG\\):" 1 font-lock-warning-face prepend)
                          ("\\<\\(TODO\\):" 1 font-lock-warning-face prepend)))

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
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode)))

(use-package cuda-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Qt ui/rcc

(use-package nxml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ui\\'" . xml-mode))
  (add-to-list 'auto-mode-alist '("\\.qrc\\'" . xml-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Qt qss

(use-package css-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.qss\\'" . css-mode)))

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
              tool-bar-mode nil)

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
    (ccls projectile adoc-mode smart-tabs-mode yasnippet company-shell window-purpose cuda-mode glsl-mode company-glsl company-jedi use-package lsp-ui company-lsp blank-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:background "gray80")))))

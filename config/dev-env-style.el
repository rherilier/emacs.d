;;; dev-env-style.el ---

;; Copyright (C) 2020 Rémi Hérilier

;; Author: Rémi Hérilier <rherilier@yahoo.fr>
;; Created: 2020-03-02
;; Version: 0.2
;; Keywords: c-mode, c++-mode
;; Homepage:
;; Package-Requires:

;; This file is not part of GNU Emacs.

;; This file is free software... see <http://www.wtfpl.net/>.

;;; Code:
(cl-defstruct env--style
  id
  desc
  (matches nil)
  init
  (uninit nil))

(defvar dev-env-style--alist nil
  "list of styles id, order them according to their :matches values (the more precice first).
It contains all of the style configuration that are currently registered.")

(defvar dev-env-style--map (make-hash-table :test 'eql)
  "styles, order them according to their :matches values (the more precice first).
It contains all of the style configuration that are currently registered.")

(defvar dev-env-style--current-id nil
  "current style")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'dev-env-make-style 'make-env--style)

(defun dev-env-style-register (config)
  "Registers dev-env CONFIG"
  (puthash (env--style-id config) config dev-env-style--map)
  (setq dev-env-style--alist (append dev-env-style--alist (list config))))

(defun dev-env-style--matchs? (root-dir matches)
  "test if ROOT-DIR matches one of MATCHES"
  (cl-some (lambda (match)
	     (string-match match root-dir))
	   matches))

(defun dev-en-style--eval (form)
  "evaluate FORM whathever it is an function or a list of functions"
  (cond
   ((atom (car form))
    (eval form))
   ((list (car form))
    (dolist (expr form)
      (eval expr)))))

(defun dev-env-style-detect (root-dir)
  "initialize project style accoring to ROOT-DIR"
  (let ((result nil))
    (progn (dolist (config dev-env-style--alist)
	     (when (dev-env-style--matchs? root-dir (env--style-matches config))
	       (progn (when dev-env-style--current-id
			(dev-en-style--eval (env--style-uninit (gethash dev-env-style--current-id
									dev-env-style--map))))
		      (message "applying style %s" (env--style-id config))
		      (dev-en-style--eval (env--style-init config))
		      (setq dev-env-style--current-id (env--style-id config))
		      (setq result t))))
	   (unless result
	     (message "no style found for '%s', using default" root-dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dev-env-style)
;;; dev-env-style.el ends here

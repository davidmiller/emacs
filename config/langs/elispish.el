;;
;; elispish.el - Configuration for Lisp-like languages
;;

;; Code starts

(setq inferior-lisp-program "/usr/bin/sbcl")
(require-many 'etags
              'paredit
              'slime)
(ignore-errors
  (slime-setup '(slime-fancy)))

;;
;; Syntax highlighting for my macros that function as
;; keyword extensions/replacements
;;
(font-lock-add-keywords 'emacs-lisp-mode
  '(("\\<\\(require-many\\|emacsdir\\|defext\\)\\>" . font-lock-keyword-face)))

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)

(defun lispish-hook nil
  "Activate common lisp functionality across lispish modes"
  (imenu-add-menubar-index)
  (hs-minor-mode t)
  (which-func-mode t)
  (pretty-lambdas)
  (paredit-mode +1)
  (eldoc-mode t)
  (whitespace-mode t)
  (gset-key (
             ([M-left] 'windmove-left)
             ([M-right] 'windmove-right)
             ([M-up] 'windmove-up)
             ([M-down] 'windmove-down))))


(add-hook 'lisp-mode-hook 'lispish-hook)
(add-hook 'emacs-lisp-mode-hook 'lispish-hook)
(add-hook 'lisp-interaction-mode-hook 'lispish-hook)
(add-hook 'scheme-mode-hook 'lispish-hook)

(defext "\\.cl\\'" lisp-mode)

;;
;; Elisp movement
;;
;; Commentary:
;;
;; These jump to defun at point functions were initially borrowed from
;; Helmut Eller:
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2009-09/msg00669.html
;;
(defun elisp-push-point-marker ()
  (cond ((featurep 'xemacs)
         (push-tag-mark))
        (t (ring-insert find-tag-marker-ring (point-marker)))))

(defun elisp-pop-found-function ()
  (interactive)
  (cond ((featurep 'xemacs) (pop-tag-mark nil))
        (t (pop-tag-mark))))

(defun elisp-find-definition (name)
  "Jump to the definition of the function (or variable) at point."
  (interactive (list (thing-at-point 'symbol)))
  (cond (name
         (let ((symbol (intern-soft name))
               (search (lambda (fun sym)
                         (let* ((r (save-excursion (funcall fun sym)))
                                (buffer (car r))
                                (point (cdr r)))
                           (cond ((not point)
                                  (error "Found no definition for %s in %s"
                                         name buffer))
                                 (t
                                  (switch-to-buffer buffer)
                                  (goto-char point)
                                  (recenter 1)))))))
           (cond ((fboundp symbol)
                  (elisp-push-point-marker)
                  (funcall search 'find-function-noselect symbol))
                 ((boundp symbol)
                  (elisp-push-point-marker)
                  (funcall search 'find-variable-noselect symbol))
                 (t
                  (message "Symbol not bound: %S" symbol)))))
  (t (message "No symbol at point"))))

;; Code ends
(provide 'elispish)

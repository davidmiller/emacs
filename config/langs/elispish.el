;;
;; elispish.el - Configuration for Lisp-like languages
;;

;; Code starts

(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime)
(ignore-errors
  (slime-setup '(slime-fancy)))

;;
;; Syntax highlighting for my macros that function as
;; keyword extensions/replacements
;;
(font-lock-add-keywords 'emacs-lisp-mode
  '(("\\<\\(require-many\\|emacsdir\\|defext\\)\\>" . font-lock-keyword-face)))


(add-hook 'lisp-mode-hook '(lambda ()
                             (set-mode-style ide-style)
                             (eldoc-mode t)))

(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                   (set-mode-style ide-style)
                                   (smart-operator-mode nil)
                                   (eldoc-mode t)))
(defext "\\.cl\\'" lisp-mode)

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)

;; Code ends
(provide 'elispish)
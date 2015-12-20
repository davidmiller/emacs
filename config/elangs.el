;;
;; elangs.el
;;
;; Commentary:
;;
;; This file contains programming-related configurations related to particular
;; programming languages as well as defining particular grouped functional
;; settings
;; that can be invoked either as hook functions for Programming modes, or
;; as-required interactively
;;

;; Module level requires
(add-load-dir (emacsdir "config/langs"))
(require-many 'flymake
              'flymake-cursor ; Warnings in minibuffer when point on Error
              'rainbow-mode ; Background hex colour declarations with their values
;; TODO: enable emodes              
;;              'emodes
              'test-case-mode)

;; Individual language requires
(require-many 'elispish
              'elua
              'epython
              'eruby
              'ejavascript)

(global-font-lock-mode t)

;;
;; Minor customisations
;;

;;;;  PHP
(load-library "php-mode")
(defext "\\.tpl\\'" html-mode)

;; Erlang
(load-library "erlang")
(defext "\\.erl\\'" erlang-mode)
(add-to-list 'erlang-mode-hook '(lambda ()
                                  (set-mode-style ide-style)))

;;;
;;; Elixir
;;;

;; TODO: enable elixir-mode

;; (require 'elixir-mode)
;; (defun ex-hook ()
;;   "Initializations for Elixir"
;;   (autopair-mode))

;; (add-hook 'elixir-mode-hook 'ex-hook)

;;
;; c++
;;

; Indentation style I want to use in c++ mode
(c-add-style "my-style"
         '("stroustrup"
           (indent-tabs-mode . nil)        ; use spaces rather than tabs
           (c-basic-offset . 4)            ; indent by four spaces
           (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                   (brace-list-open . 0)
                   (statement-case-open . +)))))

(defun c-compile ()
  "Run make -k without a prompt please"
  (interactive)
  (compile "make -k" t))

(defun my-c++-mode-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)
  (c-toggle-auto-hungry-state 1)
  (set-mode-style ide-style)
  (whitespace-mode nil)
  (local-set-key "\C-c\C-c" 'c-compile))
(kbd "\C-c<down>")
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; Perl
(add-hook 'perl-mode-hook '(lambda ()
                             (set-mode-style ide-style)))

;; TODO: enable thrift mode
;;; Thrift
;; (require 'thrift-mode)
;; (defext "\\.thrift\\'" thrift-mode)
;; (add-hook 'thrift-mode-hook '(lambda ()
;;                                (set-mode-style ide-style)))


;; TODO: enable Java

;;
;; Java
;;
;; Jde
;; (require 'jde)
;; (setq defer-loading-jde t)

;; (if defer-loading-jde
;;     (progn
;;       (autoload 'jde-mode "jde" "JDE mode." t)
;;       (defext "\\.java\\'" jde-mode))
;;   (require 'jde))

;; ;; Sets the basic indentation for Java source files
;; ;; to two spaces.
;; (defun my-jde-mode-hook ()
;;   (setq c-basic-offset 2))

;; (add-hook 'jde-mode-hook 'my-jde-mode-hook)

;;
;; Clojure
;;
;; TODO: Find out why this vvv is here :S
(add-load-dir "~/emacs/elpa/")

;; TODO: enable clojure mode

;; (require 'clojure-mode)
;; (font-lock-add-keywords 'clojure-mode
;;   '(("true" . font-lock-builtin-face)))
;; (defext "\\.cljs\\'" clojure-mode)


;; TODO: enable csharp mode
;;
;; C#
;;
;; (require 'csharp-mode)

;; (defun drm-csharp-hook ()
;;   (autopair-mode)
;;   (hs-minor-mode))

;; (add-hook 'csharp-mode 'drm-csharp-hook)


;; TODO: enable R mode
;;
;; R
;;
;; (load (sitedir "ess/lisp/ess-site.el"))

;; TODO: enable Haml
;;;
;;; Haml
;;;
;; (require 'haml-mode)
;; (defext "\\.haml\\'" haml-mode)

;;
;; Yaml
;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;; Code ends
(provide 'elangs)

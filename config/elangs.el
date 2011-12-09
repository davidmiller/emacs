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
              'emodes
              'test-case-mode)

(defmacro defext (regexp mode)
  "Set the auto mode for files whose extension matches REGEXP to MODE"
  `(add-to-list 'auto-mode-alist '(,regexp . ,mode)))

;; Individual language requires
(require-many 'elispish
              'elua
;              'epython
              'ejavascript)

(global-font-lock-mode t)

(defvar prj-keybindings '(
  ([f12]         eproject-setup-toggle  always)
  ([s-right]    eproject-nextfile)
  ([s-left]     eproject-prevfile)
  ([C-f12]       eproject-dired)
  )
  "Key bindings in eproject"
  )


;;
;; Python with PDEE
;;
(defvar pdee-install-dir (sitedir "pdee/"))
(require-many 'pdee-init
              'pdee-python
              'pdee-completion
              'pdee-editing)
;; (unload-feature 'python t) ; HACK - force PSF python-mode
;; (when (featurep 'python-mode) (unload-feature 'python-mode t))
;; (require 'python-mode (sitedir "pdee/python-modes/python-mode-el/python-mode.el"))
;; (defext "\\.py\\'" python-mode)
(pdee-setup-checker "pyflakes %f")
(pdee-django-snippets)
(pdee-setup-ipython)
(yas/load-directory (emacsdir "snippets"))
(pdee-set-mode)
(setq global-hl-line-mode nil)
(global-linum-mode -1)

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

(defext "\\Rakefile\\'" 'ruby-mode)

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
  (local-set-key "\C-c\C-c" 'c-compile))
(kbd "\C-c<down>")
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; Perl
(add-hook 'perl-mode-hook '(lambda ()
                             (set-mode-style ide-style)))

;;; Thrift
(require 'thrift-mode)
(defext "\\.thrift\\'" thrift-mode)
(add-hook 'thrift-mode-hook '(lambda ()
                               (set-mode-style ide-style)))

;;
;; Java
;;
;; Jde
(require 'jde)
(setq defer-loading-jde t)

(if defer-loading-jde
    (progn
      (autoload 'jde-mode "jde" "JDE mode." t)
      (defext "\\.java\\'" jde-mode))
  (require 'jde))

;; Sets the basic indentation for Java source files
;; to two spaces.
(defun my-jde-mode-hook ()
  (setq c-basic-offset 2))

(add-hook 'jde-mode-hook 'my-jde-mode-hook)

;;
;; Clojure
;;
(add-load-dir "~/emacs/elpa/")

(require 'clojure-mode)
(font-lock-add-keywords 'clojure-mode
  '(("true" . font-lock-builtin-face)))

;;
;; C#
;;
(require 'csharp-mode)

(defun drm-csharp-hook ()
  (autopair-mode)
  (hs-minor-mode))

  ;; (local-set-key (kbd ";") (lambda (command)
  ;;                            (insert ";")
  ;;                            (newline-and-indent))))

(add-hook 'csharp-mode 'drm-csharp-hook)


;;
;; R
;;
(load (sitedir "ess/lisp/ess-site.el"))


;; Code ends
(provide 'elangs)

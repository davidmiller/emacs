;;
;; epython.el
;;
;; Commentary:
;;
;; Python configuration for Emacs
;;
;; Todo:
;;
;; Working help/signature in modeline would be great
;;

(defvar prj-keybindings '(
  ([f12]         eproject-setup-toggle  always)
  ([s-right]    eproject-nextfile)
  ([s-left]     eproject-prevfile)
  ([C-f12]       eproject-dired)
  )
  "Key bindings in eproject"
  )

(add-to-list 'load-path (sitedir "pony-mode/src"))

(require 'jinja2-mode)
(require 'pony-mode)

(defext "\\.jinja2\\'" jinja2-mode)
(defext "\\.wsgi\\'" python-mode)
(defext "\\.py$" python-mode)

(yas/load-directory (emacsdir "snippets"))

;(autoload 'jedi:setup "jedi" nil t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)
;;
;; Python with PDEE
;;
;; (setq pdee-install-dir (sitedir "pdee/"))
;; (load-file (sitedir "pdee/pdee-init.el"))
;; (require-many
;;  'pdee-init
;;  'pdee-completion
;;  'pdee-editing)
;;(unload-feature 'python t) ; HACK - force PSF python-mode
;; (require 'python-mode (sitedir "pdee/python-modes/python-mode-el/python-mode.el"))
;; (defext "\\.py\\'" python-mode)
;;(pdee-set-mode)
;; (pdee-setup-checker "pyflakes %f")
;; (pdee-django-snippets)
;; (pdee-setup-ipython)
;; (pdee-set-mode)

;; (setq global-hl-line-mode nil)
;; (global-linum-mode -1)




(add-hook 'python-mode-hook
          '(lambda ()
             (highlight-indentation nil)
             (set-mode-style ide-style)))


(provide 'epython)
;; Code ends

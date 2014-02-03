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

(add-to-list 'load-path (sitedir "pony-mode/src"))

(require 'jinja2-mode)
(require 'pony-mode)

(defext "\\.jinja2\\'" jinja2-mode)
(defext "\\.wsgi\\'" python-mode)
(defext "\\.py$" python-mode)


(add-hook 'python-mode-hook
          '(lambda ()
             (highlight-indentation nil)
             (set-mode-style ide-style)))


(provide 'epython)
;; Code ends

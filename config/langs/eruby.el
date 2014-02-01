;;
;; eruby.el
;;
;; Commentary:
;;
;; Ruby configuration for Emacs
;;

(defext "\\Rakefile\\'" ruby-mode)
(defext "\\Gemfile\\'" ruby-mode)
(defext "\\.rake\\'" ruby-mode)
(defext "\\.god\\'" ruby-mode)
(defext "\\.erb\\'" rhtml-mode)

(add-hook 'ruby-mode-hook
          (lambda ()
            (set-mode-style ide-style)))

;; (require 'rails-autoload)y

(require 'come-fly)
(require 'rinari)
(require 'rhtml-mode)

(provide 'eruby)

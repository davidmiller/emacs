;;
;; eruby.el
;;
;; Commentary:
;;
;; Ruby configuration for Emacs
;;

(defext "\\Rakefile\\'" ruby-mode)
(defext "\\.god\\'" ruby-mode)

(add-hook 'ruby-mode-hook
          (lambda ()
            (set-mode-style ide-style)))

(require 'come-fly)

(provide 'eruby)
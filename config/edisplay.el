;;
;; edisplay.el - Emacs Appearance configuration
;;
;; Commentary:
;;
;; Contains configuration values determinining what is visible while editing - e.g.
;; Toolbars, modeline defaults
;;
;; Related: ecolours.el - Theme configuration
;;

;; Code starts.

;;
;; Initialization
;;
(defun emacs-reloaded ()
  "Wander about-type stuff for initialization. Neat. Not useful at all.
For anything. Seriously."
  (switch-to-buffer "*scratch*")
  (animate-string (concat ";; Initialization successful, welcome to "
                          (substring (emacs-version) 0 16)
                          ". \n;; Loaded with .emacs enabled")
                  0 0)
  (newline-and-indent)  (newline-and-indent))

(setq inhibit-startup-message t) ;; No more welcome for me
(defconst animate-n-steps 10)
(add-hook 'after-init-hook 'emacs-reloaded)

;;
;; Viewport
;;
(defun toggle-fullscreen (&optional f)
  "Make the current frame fullscreen"
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
       (set-frame-parameter nil 'fullscreen
                            (if (equal 'fullboth current-value)
                                (if (boundp 'old-fullscreen) old-fullscreen nil)
                                (progn (setq old-fullscreen current-value)
                                       'fullboth)))))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; Don't show a scroll bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; Remove icons from gtk menu
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))  ;; Keep the menu.
(column-number-mode 1);; Enable Colum numbering
(require 'bar-cursor)
(bar-cursor-mode t) ;; Put the cursor on a diet
(display-time) ;; show it in the modeline
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;;
;; Modeline customisations
;;
;; TODO: Enable these on osx
;; (require 'diminish)
;; (require 'rainbow-mode)
;; (eval-after-load "light-symbol" '(diminish 'light-symbol-mode))
;; (eval-after-load "flymake" '(diminish 'flymake-mode "F"))
;; (eval-after-load "eldoc" '(diminish 'eldoc-mode "E"))
;; (eval-after-load "yasnippet" '(diminish 'yas/minor-mode "Y"))
;; (eval-after-load "autopair" '(diminish 'autopair-mode))
;; (eval-after-load "smart-operator" '(diminish 'smart-operator-mode))
;; (eval-after-load "whitespace" '(diminish 'global-whitespace-mode))
;; (eval-after-load "whitespace" '(diminish 'rainbow-mode))

;; Code ends
(provide 'edisplay)
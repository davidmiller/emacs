;;
;; ekeys.el - Keybindings for Emacs
;;
;; Commentary:
;;
;; This file contains all the custom keybindings we want,
;; as well as some macros to make doing that at any length
;; somewhat more pleasant.
;;

;; Code begins

(defmacro gset-key (pairs)
  "Globally set `pairs' as (binding symbol)"
  (let ((bindings (mapcar (lambda (b) (cons 'global-set-key b)) pairs)))
  `(progn ,@bindings)
  ))


;; (defmacro invert-keys (pairs)
;;   "For each binding pair in `pairs', swap the function they are bound to."
;;   (let* ((funs (mapcar (lambda (x) (mapcar #'key-binding x)) pairs))
;;          (swapped (map 'list (lambda (p f)
;;                                (list
;;                                 (list 'global-set-key (first p) (quote(inserter (last p)(last f))))
;;                                 (list 'global-set-key (last p) (quote (inserter (last p)(first f))))))
;;                        pairs funs)))
;;     `(progn ,@swapped)))

;;
;; Definitions
;;


;; TODO: Go through these in detail and decide whether we need them or not
(gset-key (
       ;; Buffer management
       ("\C-x\C-b" 'ibuffer)
       ("\C-c\#" 'ido-find-file-other-window)
       ("\C-cb" 'ido-switch-buffer-other-window)
       ((kbd "<f5>") 'revert-buffer)
       ("\C-c\C-w" 'jump-to-register)
       
       ;; Linux has alt + direction, OSx has ESC + direction
       ([M-left] 'windmove-left) ; move to left windnow
       ([M-right] 'windmove-right) ; move to right window
       ([M-up] 'windmove-up) ; move to upper window
       ([M-down] 'windmove-down) ; move to downer window

       ((kbd "ESC <left>") 'windmove-left) ; move to left windnow
       ((kbd "ESC <right>") 'windmove-right) ; move to right window
       ((kbd "ESC <up>") 'windmove-up) ; move to upper window
       ((kbd "<ESC> <down>") 'windmove-down) ; move to downer window
       
       ((kbd "S-C-<left>") 'shrink-window-horizontally)
       ((kbd "S-C-<right>") 'enlarge-window-horizontally)
       ((kbd "S-C-<down>") 'shrink-window)
       ((kbd "S-C-<up>") 'enlarge-window)
       ("\C-cR" 'rename-current-file-or-buffer)
       ("\C-x\C-d" 'ido-find-file) ; Alias for sloppy chords from C-xC-f

       ;; Version Control
       ("\C-c\hs" 'monky-status)
       ("\C-c\gs" 'magit-status)

       ;; Python
       ("\M-p" 'pyflakes-show-help)

       ;; Show-hide
       ((kbd "<backtab>") 'hs-hide-level)
       ((kbd "<S-right>") 'hs-show-block)
       ((kbd "<S-down>") 'hs-show-all)
       ((kbd "<S-left>") 'hs-hide-block)
       ((kbd "<S-up>") 'hs-hide-all)

       ;; Editing

       ((kbd "<insert>") 'replace-string)

       ;; ("\C-w" 'backward-kill-word)
       ;; ("\C-x\C-k" 'kill-region)
       ;; ("\C-c\C-k" 'kill-region)
       ("\C-c\C-y" 'clipboard-yank); clipboard paste
       ("\C-ccx" 'clipboard-kill-region); clipboard paste
       ((kbd "C-M-S-j")
        #'(lambda () (interactive) (previous-line) (move-end-of-line nil) (newline-and-indent)))
       ((kbd "C-M-j")
        #'(lambda () (interactive) (move-end-of-line nil) (newline-and-indent)))
       ((kbd "C-S-c C-S-c") 'mc/edit-lines)

       ;; Save excursion inserts
       ((kbd "C-M-;") 'colonize)
       ((kbd "C-M-,") 'commatize)
       ("\M-#" 'find-tag-other-window)
       ("\C-cfp" 'flyspell-prog-mode)

       ;; Viewport
       ([f11] 'toggle-fullscreen)

       ;; Comms
       ("\C-cef" 'erc-freenode)
       ("\C-cff" 'browse-url)
       ("\C-cj" 'webjump)

       ;; Lisp
       ("\C-c\m" 'pp-macroexpand-last-sexp)

       ;; Dotfiles
       ("\C-c\C-r" 'x-reload-dot-emacs)
       ("\C-c\C-e" '.emacs)

       ;Programming
       ((kbd "<f7>") 'compile)

       ;; Music
       ((kbd  "C-s-SPC") 'mingus-toggle)
       ((kbd  "C-s-n") 'mingus-next)
       ((kbd  "C-s-p") 'mingus-prev)
       ((kbd  "C-s-b") 'mingus-browse)

       ))

(global-set-key [remap goto-line] 'goto-line-with-feedback)
;(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;;
;; Windows specific consistency rebindings
;;
(if win-p
    (gset-key
     ;; Right Menu Key mapped to different keycode in Win7
     (([apps]'execute-extended-command))
     ))

;;; OSX
(if osx-p
    (global-set-key (kbd "M-3") '(lambda()(interactive)(insert-pound))))

;; Code ends.
(provide 'ekeys)

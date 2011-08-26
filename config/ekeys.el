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

(gset-key (
       ;; Buffer management
       ("\C-x\C-b" 'ibuffer)
       ("\C-c\#" 'ido-find-file-other-window)
       ((kbd "<f5>") #'(lambda () (revert-buffer t t)))
       ("\C-c\C-w" 'jump-to-register)
       ([M-left] 'windmove-left) ; move to left windnow
       ([M-right] 'windmove-right) ; move to right window
       ([M-up] 'windmove-up) ; move to upper window
       ([M-down] 'windmove-down) ; move to downer window
       ((kbd "S-C-<left>") 'shrink-window-horizontally)
       ((kbd "S-C-<right>") 'enlarge-window-horizontally)
       ((kbd "S-C-<down>") 'shrink-window)
       ((kbd "S-C-<up>") 'enlarge-window)
       ("\C-cR" 'rename-current-file-or-buffer)

       ;; Version Control
       ("\C-c\h\p" 'xhg-push)
       ("\C-c\g\p" 'xgit-push)
       ("\C-c\h\P" 'xhg-pull)
       ("\C-c\g\P" 'xgit-pull)
       ("\C-chl" 'xhg-log)
       ("\C-chd" 'xhg-log-toggle-diff-for-changeset)

       ;; Python
       ("\M-p" 'pyflakes-show-help)

       ;; Show-hide
       ((kbd "<backtab>") 'hs-hide-level)
       ((kbd "<S-right>") 'hs-show-block)
       ((kbd "<S-down>") 'hs-show-all)
       ((kbd "<S-left>") 'hs-hide-block)
       ((kbd "<S-up>") 'hs-hide-all)

       ;; Editing

       ("\C-w" 'backward-kill-word)
       ("\C-x\C-k" 'kill-region)
       ("\C-c\C-k" 'kill-region)
       ((kbd "C-M-S-j")
        #'(lambda () (interactive) (previous-line) (move-end-of-line nil) (newline-and-indent)))
       ((kbd "C-M-j")
        #'(lambda () (interactive) (move-end-of-line nil) (newline-and-indent)))

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

       ;; Lisp
       ("\C-c\m" 'pp-macroexpand-last-sexp)

       ))

;;
;; Windows specific consistency rebindings
;;
(if win-p
    (gset-key
     ;; Right Menu Key mapped to different keycode in Win7
     (([apps]'execute-extended-command))
     ))

(define-key global-map "\C-c\C-y" 'clipboard-yank); clipboard paste
(define-key global-map "\C-ccx" 'clipboard-kill-region); clipboard paste

;;
;; Extended Keymap Munging
;;

;; Code ends.

(provide 'ekeys)

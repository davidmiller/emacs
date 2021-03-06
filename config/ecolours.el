;; Set the default font here
(ignore-errors
  (set-default-font
   "-unknown-DejaVu Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"))


;; Colours

(require 'color-theme)
(defun color-theme-bluebulator ()
  (interactive)
  (color-theme-install
   '(color-theme-bluebulator
      ((background-color . "#0f0f0f")
      (background-mode . light)
      (border-color . "#191919")
      (cursor-color . "#43667f")
      (foreground-color . "#c9c9c5")
      (mouse-color . "black"))
      (fringe ((t (:background "#191919"))))
      (mode-line ((t (:foreground "#a3a3a3" :background "#163e60"))))
      (region ((t (:background "#29383d"))))
      (font-lock-builtin-face ((t (:foreground "#ccaa61"))))
      (font-lock-constant-face ((t (:foreground "#ddcc61"))))
      (font-lock-comment-face ((t (:foreground "brown"))))
      (font-lock-function-name-face ((t (:foreground "#197db8"))))
      (font-lock-keyword-face ((t (:foreground "#508195"))))
      (font-lock-string-face ((t (:foreground "#6ea07f"))))
      (font-lock-type-face ((t (:foreground"#539355"))))
      (rst-level-2-face ((t (:foreground "black" :background "grey78"))))
      (rst-level-3-face ((t (:foreground "black" :background "grey71"))))
      (flymake-errline ((t (:background"firebrick" :foreground "white"))))
      (font-lock-variable-name-face ((t (:foreground "#7f989f"))))
      (minibuffer-prompt ((t (:foreground "#a2c4d8" :bold t))))
      (font-lock-warning-face ((t (:foreground "Red" :bold t))))
      (whitespace-space ((t (:background "#0f0f0f" :foreground "#454545"))))
      (whitespace-trailing ((t (:background "#a52a2a"))))
      (whitespace-line ((t (:background "gray10"))))
      (whitespace-newline ((t (:foreground "#454545"))))
      (whitespace-indentation ((t (:background "#121212"))))
      (whitespace-space-after-tab ((t (:background "#0f0"))))
      (whitespace-tab ((t (:background "#0f0f0f"))))
     (whitespace-empty ((t (:background "#0f0f0f"))))
     (which-func ((t (:foreground "#a2c4d8"))))
     )))


(provide 'color-theme-bluebulator)

;; My personal font-lock additions

(dolist (mode (list 'lisp-mode-hook 'emacs-lisp-mode-hook))
  (add-hook mode
            (lambda ()
              (font-lock-add-keywords
               nil
               '(("\\(nil\\)" 1 font-lock-builtin-face prepend))))))


;; make whitespace-mode use “¶” for newline and “▷” for tab.
;; together with the rest of its defaults
(setq whitespace-display-mappings
 '(
   (space-mark 32 [183] [46]) ; normal space, ·
   (space-mark 160 [164] [95])
   (space-mark 2208 [2212] [95])
   (space-mark 2336 [2340] [95])
   (space-mark 3616 [3620] [95])
   (space-mark 3872 [3876] [95])

   (tab-mark 9 [9655 9] [92 9]) ; tab, ▷
))
(add-to-list 'whitespace-display-mappings
             (if *nix
                 '(newline-mark 10 [8629 10])
                 '(newline-mark 10 [182 10]))) ; newlne, ¶)

(setq whitespace-line-column 180)

(defun pretty-lambdas ()
    (font-lock-add-keywords
     nil `(("(\\(lambda\\>\\)"
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                      ,(make-char 'greek-iso8859-7 107))
                      nil))))))

(custom-set-faces
 '(rst-level-1-face ((t (:background "black" :foreground "brown3" :weight bold))) t)
 '(rst-level-2-face ((t (:background "black" :foreground "white" :weight bold))) t))

(provide 'ecolours)

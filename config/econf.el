;;
;;
;; This file represents the entrypoint for our emacs configuration.
;;
;; Sections in this file:
;;
;; 1. Initialization
;; 2. El Get (Package Management)
;; 3. Utility Functions
;; 4. Display
;; 5. Interaction with files/buffers
;; 6. Interacting with Version Control;;
;; 7. Interacting with Org Mode
;; 8. Smart Snippeting
;; 10. Custom keybindings
;; 11. Custom language bindings
;; 11.1 Python
;;
;;


;;
;; 1. Initialization
;;
(defun emacs-reloaded ()
  "Wander about-type stuff for initialization. Neat. Not useful at all.
For anything. Seriously."
  (switch-to-buffer "*scratch*")
  (animate-string (concat ";; Initialization successful, welcome to "
                          (substring (emacs-version) 0 16)
                          ". \n;; Loaded with .emacs enabled\n\n;; This is your newer fresher emacs config.")
                  0 0)
  (newline-and-indent)  (newline-and-indent))

(setq inhibit-startup-message t) ;; No more welcome for me
(defconst animate-n-steps 10)
(add-hook 'after-init-hook 'emacs-reloaded)


;;
;; 2. El Get (Package Management)
;;

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get 'sync)

;;
;; 3. Utility Functions
;;
(defun insert-pound ()
  "Inserts a pound into the buffer"
  (insert "#"))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad "
          "minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

;;
;; Dotfile tweaking
;;
(defmacro dotfile (filename &optional path)
  "Define the function `filename' to edit the dotfile in question"
  (let ((filestr (symbol-name filename)))
    `(progn
       (defun ,(intern filestr) ()
         ,(format "Open %s for editing" filestr)
         (interactive)
         (find-file ,(if path path (concat ~ filestr))))))) ;; ~ set in .emacs

(dotfile .emacs)
(dotfile .bashrc)
(dotfile .ssh "~/.ssh/config")
(dotfile .screenrc)
(dotfile econf "~/emacs/config/econf.el")

(defun rename-current-file-or-buffer ()
  (interactive)
  (if (not (buffer-file-name))
      (call-interactively 'rename-buffer)
    (let ((file (buffer-file-name)))
      (with-temp-buffer
        (set-buffer (dired-noselect file))
        (dired-do-rename)
        (kill-buffer nil))))
  nil)


;;
;; 4. Display
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
(display-time) ;; show it in the modeline
(setq frame-title-format '(buffer-file-name "%f" ("%b"))) ; Window should have full file path
(setq-default cursor-type 'bar)  ; Emacs default is a box


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
      (hl-line ((t (:background "#333333"))))
     ;;  (whitespace-space ((t (:background "#0f0f0f" :foreground "#454545"))))
     ;;  (whitespace-trailing ((t (:background "#a52a2a"))))
     ;;  (whitespace-line ((t (:background "gray10"))))
     ;;  (whitespace-newline ((t (:foreground "#454545"))))
     ;;  (whitespace-indentation ((t (:background "#121212"))))
     ;;  (whitespace-space-after-tab ((t (:background "#0f0"))))
     ;;  (whitespace-tab ((t (:background "#0f0f0f"))))
     ;; (whitespace-empty ((t (:background "#0f0f0f"))))
     (which-func ((t (:foreground "#a2c4d8"))))
     )))
(provide 'color-theme-bluebulator)
(color-theme-bluebulator)

(show-paren-mode 1) ;; Highlight parenthesis pairs
(delete-selection-mode t) ;; Delete contents of region when we start typing

;;
;; 5. Interaction with files/buffers
;;
(setq ring-bell-function 'ignore);; disable bell function

(ido-mode t) ; Better find file/buffer
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

(require 'dired-x) ; For ommitting files
(setq-default dired-omit-files-p t) ; this is the buffer-local variable
(setq dired-omit-files
      ;; kill magic . .. as well as .pyc and emacs *~ files
    (concat dired-omit-files "\\|^\\..+$\\|\\.pyc$\\|\\*~$"))

(defalias 'yes-or-no-p 'y-or-n-p) ;; less typing for me
(setq-default indent-tabs-mode nil) ;; Spaces instead of tabs
;; Brief aside via Georg Brandl
;;
;; Thus spake the Lord:
;; Thou shalt indent with four spaces. No more, no less.
;; Four shall be the number of spaces thou shalt indent,
;; and the number of thy indenting shall be four.
;; Eight shalt thou not indent, nor either indent thou two,
;; excepting that thou then proceed to four.
;;
;; Tabs are right out.
(setq tab-width 4)

;; We basically never want trailing whitespace in files.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(electric-pair-mode) ;; >=Emacs 24 replaces autopair. Parens, Quotes etc are double no

(desktop-save-mode t) ; Save the currently open files between sessions
(require 'full-ack)
(setq ack-executable "/usr/local/bin/ack") ; We're not where Ack expects Ack to be

;;
;; 6. Interacting with Version Control
;;
(require 'magit)

;;
;; 7. Interacting with Org Mode
;;
(setq org-todo-keywords
      '((sequence "TODO" "IN PROGRESS" "|" "DONE" "WONT")))

;;
;; 10. Custom keybindings
;;

(defmacro gset-key (pairs)
  "Globally set `pairs' as (binding symbol)"
  (let ((bindings (mapcar (lambda (b) (cons 'global-set-key b)) pairs)))
  `(progn ,@bindings)
  ))

(gset-key (
       ("\C-x\C-b" 'ibuffer) ; Ibuffer is part of emacs & nicer than the default

       ;; Linux has alt + direction, OSx has ESC + direction
       ([M-left] 'windmove-left) ; move to left windnow
       ([M-right] 'windmove-right) ; move to right window
       ([M-up] 'windmove-up) ; move to upper window
       ([M-down] 'windmove-down) ; move to downer window

       ((kbd "ESC <left>") 'windmove-left) ; move to left windnow
       ((kbd "ESC <right>") 'windmove-right) ; move to right window
       ((kbd "ESC <up>") 'windmove-up) ; move to upper window
       ((kbd "<ESC> <down>") 'windmove-down) ; move to downer window
       ))

(if osx-p
    (global-set-key (kbd "M-3") '(lambda()(interactive)(insert-pound))))

;;
;; 8. Smart Snippeting
;;
(require 'yasnippet)
(yas-global-mode 1)
(add-to-list 'yas-snippet-dirs "~/emacs/newsnippets")

;;
;; 11. Custom language bindings
;;

;;
;; 11.1 Python
;;

;; We need to tell python-environment & Jedi where to find virtualenv
(setq-default python-environment-virtualenv
              '("/usr/local/bin/virtualenv" "--system-site-packages" "--quiet"))

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)  ; optional
(setq jedi:setup-keys t)

(add-load-dir "~/emacs/site-packages/pony-mode/src")
(require 'pony-mode) ; For Interacting with Django.


;; code ends
(provide 'econf)

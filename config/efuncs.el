;;
;; efuncs.el - Collection of useful functions
;;

;; Code starts

;;
;; Function-writing macros
;;
(defmacro* macfun (name body &key args &key docstr &key docformats)
  "Expand to define a function called NAME executing BODY.

Let ARGS be the arguments list to the function.

Let DOCSTR be the documentation for that function, with the optional
arguments DOCFORMATS provided to FORMAT in order to render contextual documentation.

Define the function within a lexical context that contains the variables `name' and
`name-str' (A string version of the name.)"
  (let ((name name)
        (name-str (symbol-name name)))
    `(progn
       (defun ,(intern name-str) ,args
         ,@body))))

;; (macfun iotop
;;         ((interactive)
;;          (ansi-term name-str name-str)
;;          (switch-to-buffer (format "*%s*" name-str))))

;;
;; Shell programs in buffers
;;
(defmacro defshell (name)
  "Define the function `name' such that it runs `name' in a shell buffer"
  (let ((namestr (symbol-name name)))
  `(progn
     (defun ,(intern namestr) ()
       (interactive)
       (ansi-term ,namestr ,namestr)
       (switch-to-buffer ,(format "*%s*" namestr))))))

(defshell ipython)
(defshell iotop)

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

(defun drm-dot-emacs()
  (interactive)
  (load-file (concat ~ ".emacs")))

(defun reload-gnus()
  (interactive)
  (load-file "~/.gnus"))
(global-set-key "\C-cGR" 'reload-gnus)

(dotfile .emacs)
(dotfile .bashrc)
(dotfile .hgrc)
(dotfile .conkyrc)
(dotfile .ssh "~/.ssh/config")
(dotfile .screenrc)
(dotfile .thpppt)
(dotfile .xsession)
(dotfile .xbindkeysrc)

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


;; move (shift) a line of text up or down like you would do in Eclipse
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col (current-column))
        start
        end)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (forward-char)
    (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
      (forward-char col))))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "<M-kp-8>") 'move-line-up)
(global-set-key (kbd "<M-kp-2>") 'move-line-down)

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

;; Frequently we want to copy the current line onto the next one
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key "\C-c\C-p" 'duplicate-line)

(defun regexp-revert (regexp)
  "Revert all buffers whose path matches regexp"
  (interactive "sPath Regexp: ")
  (dolist (buffer (buffer-list))
    (if (string-match-p regexp (or (buffer-file-name buffer) ""))
        (progn
          (set-buffer buffer)
          (revert-buffer nil t)
          (message "Reverting %s" (buffer-file-name buffer))))))

(defun run-hooks-for (hooks)
  "Run the hooks stored in `hooks`"
  (interactive "SHook: ")
  (run-hooks hooks))

(defun ff-hooks ()
  "Run find-file-hooks"
  (interactive)
  (run-hooks-for 'find-file-hooks))

(defun make-ret-indenting ()
  "Locally rebind RET to the function `newline-and-indent` - this is useful for setting in
mode hook styles that are likely to be invoked across multiple major-modes."
  (local-set-key (kbd "RET") 'newline-and-indent))

;; (defun close-and-bookmark()
;;   "Close a PDF, bookmarking the page You've currently read until"
;;   (interactive)
;;   (let ((page (doc-view-current-page)))))

(defun eol-insert (insertee)
  "Insert `insertee' at the eol without losing moving point"
  (save-excursion
    (move-end-of-line nil)
    (insert insertee)))

(defun colonize ()
  "For languages that insist on putting a colon at the end of a line,
do that. But stay where you're thinking is at."
  (interactive)
  (eol-insert ";"))

(defun commatize ()
  "Insert me a comma at EOL please. Leave point alone. Thanks."
  (interactive)
  (eol-insert ","))

(defun no-xls (&optional filename)
  "Run xlhtml and w3m -dump on the entire buffer.
Optional FILENAME says what filename to use.
This is only necessary for buffers without
proper `buffer-file-name'.  FILENAME should
be a real filename, not a path."
  (interactive "fExcel File: ")
  (when (and filename
             (not (buffer-file-name)))
    (write-file (make-temp-file filename)))
  (erase-buffer)
  (shell-command
   (format "xlhtml -nc -te %s | w3m -dump -T text/html" (buffer-file-name))
   (current-buffer))
  (setq buffer-file-name nil)
  (set-buffer-modified-p nil))
(add-to-list 'auto-mode-alist '("\\.xls\\'" . no-xls))
(add-to-list 'auto-mode-alist '("\\.xlsx\\'" . no-xls))


(defun ts2dt ()
  "Convert the Timestamp at point to a datetime and echo in the minibuffer"
  (interactive)
  (message (shell-command-to-string (concat "date -ud @"
                                            (word-at-point)))))

(defmacro defext (regexp mode)
  "Set the auto mode for files whose extension matches REGEXP to MODE"
  `(add-to-list 'auto-mode-alist '(,regexp . ,mode)))


(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun copy-file-name-to-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))


;; Allow hash to be entered
(defun insert-pound ()
  "Inserts a pound into the buffer"
  (insert "#"))



;; Code ends
(provide 'efuncs)

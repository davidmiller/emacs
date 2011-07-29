;; Custom

(defmacro dotfile (filename)
  "Define the function `filename' to edit the dotfile in question"
  (let ((filestr (symbol-name filename)))
    `(progn
       (defun ,(intern filestr) ()
         ,(format "Open %s for editing" filestr)
         (interactive)
         (find-file ,(concat "~/" filestr))))))

(dotfile .emacs)
(defun x-reload-dot-emacs()
  (interactive)
  (load-file "~/.emacs"))

(global-set-key "\C-c\C-r" 'x-reload-dot-emacs)
(global-set-key "\C-c\C-e" '.emacs)

(dotfile .bashrc)
(dotfile .hgrc)
(dotfile .conkyrc)

(defun reload-gnus()
  (interactive)
  (load-file "~/.gnus"))
(global-set-key "\C-cGR" 'reload-gnus)

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

(defun ipython ()
  (interactive)
  (ansi-term "ipython" "iPython")
  (switch-to-buffer "*iPython*"))

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

(provide 'efuncs)
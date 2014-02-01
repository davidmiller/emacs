;;
;; ejavascript.el
;;
;; Commentary:
;;
;; Javascript configuration for Emacs
;;
;; TODO:
;;
;; Get test-case-mode backend working
;;

(require 'flymake-jslint)

(setq lintnode-excludes (list 'nomen 'undef 'plusplus 'onevar 'white))
(add-hook 'js-mode-hook
          (lambda ()
            ;; (lintnode-hook)
            (set-mode-style ide-style)
            (local-set-key (kbd "C-M-;"))))

;; Integrating jasmine with test-case mode
(defcustom test-case-jasmine-executable "jasmine-node"
  "The jasmine-node executable"
  :group 'test-case :type 'file)
(defcustom test-case-jasmine-arguments  "."
  "The arguments to jasmine-node"
  :group 'test-case :type 'string)
(defcustom test-case-jasmine-cwd "../../"
  "*The directory from which to run jasmine-node. Should be set per-buffer."
  :group 'test-case :type 'file :safe 'stringp)

(defvar test-case-jasmine-font-lock-keywords
  "Keywords to link failures back to"
  (eval-when-compile
    `((,(concat
         `((,(concat "\\_<\\(?:it\\)\\_>")
       (0 'test-case-assertion append))))))))

(defun test-case-jasmine-failure-pattern ()
  "Regexp to match errors in jasmine tests"
  (eval-when-compile
    `(,(concat "^[^ \t]+([^ \t]+) "
               "\\[\\(\\([^:]+\\):\\([[:digit:]]+\\)\\)\\]:\n"
               "\\(\\(.+\n\\)*\\)\n")
      2 3 nil 1 4)))

(defun test-case-jasmine-backend (command)
  "Javascript Jasmine backend for `test-case-mode`"
  (case command
    ('name "Jasmine")
    ('supported (or (derived-mode-p 'js2-mode)
                    (string-match "spec.js" (buffer-file-name))))

    ('command (concat "cd " test-case-jasmine-cwd "; "
                      test-case-jasmine-executable " " test-case-jasmine-arguments))
    ('save t)
    ('failure-pattern (test-case-jasmine-failure-pattern))
    ('font-lock-keywords test-case-jasmine-font-lock-keywords)))

(add-to-list 'test-case-backends 'test-case-jasmine-backend)

(provide 'ejavascript)

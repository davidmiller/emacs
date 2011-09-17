;;
;; econf.el - Emacs Configuration
;;
;; Commentary:
;;
;; This file is the root configuration package for My Emacs setup.
;; After establishing a few generic libraries that should be available
;; to all configuration modules, it proceeds to require each individual
;; configuration.
;;
;; The process of factoring this out into logical packages is ongoing.
;;

;; Code begins

;;
;; Module level dependencies
;;
(require 'delsel)

;;
;; Sub-configuration packages
;;
(require-many
 'ecomms      ;; Communications with the outside world - IM/IRC/Twitter/Email
 'edisplay    ;; Visual display of the window
 'edocs       ;; Access to documentation files/resources from within Emacs
 'enterwebs   ;; Interacting with a World Wide Web
 )

;; Dizzee package - Managing projects and supbrocesses
(require 'dizzee)

(dz-defservice onzo-backend "~/src/onzo/backend/lib/backend_server" :port 8080)
(dz-defservice onzo-static "~/src/onzo/thinclient/browser/static.py":port 4567)

(dz-defservice onzo-client "~/src/onzo/client/src/client/run_client_sse.sh"
               :cd '.)

;(dz-defservice onzo-sseweb "~/src/onzo/sseweb/onzo_pss/manage.py" (list "runserver") 8000)

(require 'fab)

(setq ring-bell-function 'ignore);; disable bell function
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

(show-paren-mode 1) ;; Highlight parenthesis pairs
(setq transient-mark-mode t) ;; Highlight region whenever the mark is active
(delete-selection-mode t) ;; Delete contents of region when we start typing
(setq indicate-empty-lines t)

;;;;;;;;;;;;;;;;;;;;;;;;;  Files   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Put backup files (ie foo~) in one place. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/home/david/tmp/emacs_backups/"))
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq temporary-file-directory "/tmp/")
;; Let buffer names be unique in a nicer way
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq wdired-allow-to-change-permissions t) ;; Allow perm changing in Dired



;; Editing
(load-library "light-symbol")
(require 'autopair)

(require 'auto-complete-config)

;; (setq-default ac-sources '(ac-source-words-in-same-mode-buffers
;;                            ac-source-yasnippet
;;                            ac-source-filename
;;                            ac-source-files-in-current-dir))
(add-to-list 'ac-dictionary-directories (sitedir "auto-complete/dict"))

(ac-config-default)
(add-to-list 'ac-modes 'erlang-mode)
(add-to-list 'ac-modes 'erlang-shell-mode)
(add-to-list 'ac-modes 'thrift-mode)
(add-to-list 'ac-modes 'csharp-mode)
(setq-default ac-sources (add-to-list 'ac-sources 'ac-dictionary 'ac-source-yasnippet))
(add-hook 'emacs-lisp-mode-hook
          (lambda () (add-to-list 'ac-sources 'ac-source-symbols)))
(add-hook 'auto-complete-mode-hook
          (lambda ()
            ()))
;; (add-hook 'python-mode-hook
;;           (lambda () (add-to-list 'ac-sources 'ac-source-ropemacs)))
(global-auto-complete-mode t)
                                        ;(ac-css-keywords-initialize)
                                        ;(ac-set-trigger-key "C-c C-/")
                                        ;(setq ac-auto-start nil)
(setq ac-auto-start 2)
(setq ac-ignore-case nil)
(setq ac-quick-help-delay 1)
(require 'yasnippet)
(setq yas/snippet-dirs (emacsdir "snippets"))
(yas/initialize)
(yas/load-directory (sitedir "yasnippet/snippets/text-mode"))

;;;;;;;;;;;;;;;;;;;;;;;;  Buffer Management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq split-window-preferred-function 'split-window-sensibly)
(winner-mode 1)
;(setq split-width-threshold 50)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Org" ;; all org-related buffers
                (mode . org-mode))
               ("Profile"    ;; personal config files
                (filename . ".emacs\$"))
               ("Onzo" ;; Work related sectioning
                (or (filename . "onzo") ;; Local Machine
                    (filename . "scp:devs@devmac"))) ;; TRAMPing to the dev Mac
               ("Gnus"
                (name . "*Group*"))
               ("Programming" ;; prog stuff not already in MyProjectX
                (or
                 (mode . python-mode)
                 (mode . perl-mode)
                 (mode . ruby-mode)
                 (mode . php-mode)
                 (mode . emacs-lisp-mode)
                 (filename . ".tpl\$")
                 ))
               ("Mail"
                 (or  ;; mail-related buffers
                  (mode . message-mode)
                  (mode . mail-mode)
                  ;; etc.; all your mail related modes
                  ))
               ("Jabber"
                (or
                 (mode . jabber-chat-mode)
                 (mode . jabber-roster-mode)
                 ))
               ("Snippets"
                (filename . "yasnippet/snippets"))
               ("ERC"   (mode . erc-mode))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))


(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching


;; So I'll be lowercasin'
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Shell
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Version Control
(require-many
 'magit  ;; Git interface
 'monky) ;; Mercurial interface


;; Programming - IDE stuff

(require 'smart-operator)
(setq smart-operator-double-space-docs nil)

;; Use CEDET projects
(condition-case nil
    (load-file (sitedir "cedet/common/cedet.el"))
  (error nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ORG mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook '(lambda ()
                            (set-mode-style textual-style)))

;; Testing out remember-mode
(org-remember-insinuate)
(setq org-directory "~/src/onzo/scratch/")
(setq org-default-notes-file "~/src/onzo/scratch/notes.org")
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map "\C-cr" 'org-remember)

(setq org-remember-templates
      '(("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U"
         "~/notes/organized.org" "Tasks")))

;; Unittests
(require 'fringe-helper)
(autoload 'test-case-mode "test-case-mode" nil t)
(autoload 'enable-test-case-mode-if-test "test-case-mode")
(autoload 'test-case-find-all-tests "test-case-mode" nil t)
(autoload 'test-case-compilation-finish-run-all "test-case-mode")
(add-hook 'find-file-hook 'enable-test-case-mode-if-test)

;; Session Management
(desktop-save-mode t)

;; Function names in the header line
(require 'which-func)
(which-func-mode 1)
 (eval-after-load "which-func"
      '(progn
         (add-to-list 'which-func-modes 'c++-mode)
         (add-to-list 'which-func-modes 'js-mode)
         (add-to-list 'which-func-modes 'lisp-mode)
         (add-to-list 'which-func-modes 'emacs-lisp-mode)
         (add-to-list 'which-func-modes 'python-mode)))
(delete (assoc 'which-func-mode mode-line-format) mode-line-format)
(setq which-func-header-line-format
              '(which-func-mode
                ("" which-func-format
                 )))
(defadvice which-func-ff-hook (after header-line activate)
  (when which-func-mode
    (delete (assoc 'which-func-mode mode-line-format) mode-line-format)
    (setq header-line-format which-func-header-line-format)))
(add-hook 'find-file-hooks 'which-func-ff-hook)

(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)
(setq ack-executable (executable-find "ack-grep"))


(when
    (load
     (expand-file-name (emacsdir "elpa/package.el")))
  (setq package-user-dir (expand-file-name (emacsdir "/elpa")))
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize))
(setq package-user-dir (expand-file-name (emacsdir "elpa")))


;; Edit DNS records with sane highlighting and auto-increment serial
(add-hook 'find-file-hook
          (lambda nil
            (if (string-match "/etc/bind" (buffer-file-name))
                (dns-mode))))


;; Code ends
(provide 'econf)

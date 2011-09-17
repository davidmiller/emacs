;;
;; eget.el - El get config
;;
;; Commentary:
;;
;; El-get is by far the most civilized way to enjoy third party
;; packages for Emacs.
;;

;; Code begins
(require 'el-get)

(setq el-get-dir (emacsdir "site-packages"))
(setq el-get-status-file (path.join emacs-root "site-packages" ".status.el"))

(ifhost rasputin
        ;; For utterly inane reasons the system git on this box is
        ;; too low a version to be compatible with el-get's git submodule args
        (setq el-get-git "/home/david/bin/git"))

(defmacro el-get-hub (&key user &rest sources)
  "Create el-get sources from the symbols passed as `sources'"
  `,@(loop for package in sources
            collect `(:name ,package
                            :type git
                            :url ,(concat
                                    "git@github.com:"
                                    (symbol-name user) "/"
                                    (symbol-name package)
                                    ".git"))))

(defmacro defsources (&key github &rest body)
  "Splice the expanded github repos into our form for
creating an `el-get-sources' variable"
  `(setq el-get-sources
         '(,@(apply #'append (loop for repo in github
                          collect (macroexpand
                                   (cons 'el-get-hub repo))))
           ,@body)))

(defsources
  :github
  ;; Firstly let's get my packages
  ((:user davidmiller
          emodes pony-mode lintnode come-fly thrift-mode dizzee fabmacs)
   ;;
   ;; Now we move on to 3rd party packlages from Github
   ;;
   (:user ananthakumaran
          monky)
   (:user dima-exe
          emacs-rails-reloaded)
   (:user immerrr
          lua-mode)
   (:user m2ym
          auto-complete)
   (:user magit
          magit)
   (:user nex3
          haml-mode)
   (:user technomancy
          clojure-mode
          ;; Get slime from this github mirror until Clojure
          ;; sort out numerous infrastructure issues
          slime)
   )
   ;;
   ;; Having dealt with source-specific packages,
   ;; let's return to the el-get 'classic' method of
   ;; specifying packages
   ;;
  (:name color-theme
         :type bzr
         :url "bzr://bzr.savannah.nongnu.org/color-theme/trunk")
   (:name csharp-mode
         :type svn
         :url "http://csharpmode.googlecode.com/svn/trunk/")
  (:name diminish
         :type http
         :url "http://www.eskimo.com/~seldon/diminish.el")
  (:name ess
         :type svn
         :url "https://svn.r-project.org/ESS/trunk")
  (:name pastebin
         :type emacswiki)
  (:name python-mode
         :type bzr
         :url "lp:python-mode")
  (:name yasnippet
         :type svn
         :url "http://yasnippet.googlecode.com/svn/trunk/")
   )

(setq my-packages
      (append
       '()
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)

;; Code ends

(provide 'eget)


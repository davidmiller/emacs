;;
;; ecomms.el - Communicating with a world outside
;;

;; Code starts

;;
;; Jabber client
;;
(load "jabber-autoloads")
(setq jabber-account-list
      '(("david@deadpansincerity.com"
         (:network-server . "talk.google.com")
         (:connection-type . ssl))))

;;
;; ERC
;;
(add-hook 'erc-mode-hook (lambda () (longlines-mode t)))
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(defmacro erc-connect (command server port nick)
  "Create interactive command `command', for connecting to an IRC server. The
      command uses interactive mode if passed an argument."
  (fset command
        `(lambda (arg)
           (interactive "p")
           (if (not (= 1 arg))
               (call-interactively 'erc)
             (erc :server ,server :port ,port :nick ,nick)))))

(autoload 'erc "erc" "" t)
(erc-connect erc-freenode "irc.freenode.net" 6667 "davidmiller")

(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#pony-mode" "#celery")))
 (erc-spelling-mode 1)

;;
;; Blogging
;;
(require 'weblogger)
;(setq 'weblogger-config-alist '(("deadpansincerity" "http://blog.deadpansincerity.com/xmlrpc.php" "admin" "" "1")))


;; Code ends
(provide 'ecomms)
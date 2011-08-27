;;
;; enterwebs.el - Interacting with a World Wide Web
;;
;; Commentary:
;;
;;

;; Code Begins

;;
;; Module level requires
;;
(require 
 'mule             ;; See NONSENSE
 'yaoddmuse)       ;; Wiki editing

(setq
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "chrome")

;; 
;; w3m
;; 
;; Commentary:
;; 
;; Yet to get this functioning nicely on Windows,
;; so special-case the platform for now
;;
;; TODO:
;; 
;; Sort out installing x-platform
;; 

(if *nix
    (ifhost parmenides
	    nil ;; Packaging nonsense with emacs versions.
	    (progn
	      (require-many
	       'w3m-load        ;; Text-only browser
	       'w3m-e21
	       'browse-apropos  ;; Quick jumps to sites for w3m
	       )
	      (provide 'w3m-e23)
	      (setq w3m-default-display-inline-images nil)
	      (setq w3m-use-cookies t))))

;; Code Ends
(provide 'enterwebs)

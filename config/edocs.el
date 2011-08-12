;;
;; edocs.el - Documentation access within Emacs
;;

;; Info files live in `emacs-root'/info
(add-to-list 'Info-default-directory-list (emacsdir "info"))

;;
;; Dictionary
;;
(load "dictionary-init")


(provide 'edocs)
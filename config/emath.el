;;
;; emath.el
;;
;; Commentary:
;;
;; Provie utility functions for math in Emacs
;;
(require 'thingatpt)

;;
;; Recognising Floats
;;
;; Commentary:
;;
;; Much of this is inspired by the code of Mark Triggs and Aaron S. Hawley
;; who wrote the original integers.el in 04/05
;;
(defun float-float-in-region (start end)
  "Return float in region between START and END.
Region should only contain the float and no other characters.
Format allows any number of decimal digits and optionally
starting with a dash \(\"-\")."
  (interactive "r")
  (let* ((string (buffer-substring start end))
         (n (if (string-match "^-?[0-9]+[.][0-9]+$" string)
                (string-to-number string)
              nil)))
    (if (numberp n)
        (progn (message "%d" n)
               n)
      (progn (error "No float found")
             n))))

(defun float-bounds-of-float-at-point ()
  "Return the start and end points of an float at the current point.
The result is a paired list of character positions for an float
located at the current point in the current buffer.  An float is any
decimal digit 0 through 9 with an optional starting minus symbol
\(\"-\")."
  (if (looking-at "-?[0-9.]+")
      (let ((end (match-end 0))
            (start
             (save-excursion
               (re-search-backward "[^0-9]")
               (if (looking-at "-")
                   (point) ;; Use current point if a "-".
                 (+ 1 (point)))))) ;; Add 1 to correct extra step
        ;; backwards.
        (cons start end))
    nil))

(put 'float
     'bounds-of-thing-at-point
     'float-bounds-of-float-at-point)

(defun float-float-at-point ()
  (let ((i (thing-at-point 'float)))
    (if (numberp i) (string-to-number i)
      nil)))

(defun float-beginning-of-float ()
  (beginning-of-thing 'float))

(defun float-end-of-float ()
  (end-of-thing 'float))

(defun float-at-point ()
  "Return the float at point"
  (thing-at-point 'float))

;;
;; Performing Math
;;
;; Commentary:
;;
;; Often we work with numerical data in buffers and would like a convenient
;; way of operating on them within Emacs. Provide utility functions for
;; performing arbitrary calculations on numbers at point
;;

(defun math-at-point (&optional here)
  "Perform an arbitrary lisp expression on the number at point"
  (interactive "P")
  (let* ((number (float-at-point))
         (initial-text (format "( %s)"  number))
         (initial-cons (cons initial-text 2))
         (sexp (read-minibuffer "Expression: "
                                initial-cons)) ; input
         (result (format " %d" (eval sexp))))
    (message result)
    (if here (save-excursion
               (move-end-of-line nil)
               (insert result)))))

(provide 'emath)
;; Code ends

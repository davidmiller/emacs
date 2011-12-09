;;
;; efourth-dimension.el
;;
;; Commentary:
;;
;; This file contains utility functions for working with date/time
;; in Emacs.
;;
;; Dependencies:
;;
;; These functions will reqire the *nix date program as well as functions defined in
;; my .emacs - emath is also required for float-at-point
;;

;;
;; Conversions
;;
;; Commentary:
;;
;; Functions for converting date/time values between different formats
;;

(defun date-ud (timestamp)
  "Get the date of `timestamp'"
  (chomp (shell-command-to-string (concat "date -ud @" timestamp))))

(defun mat-2-utc (mat)
  "Return the UTC equivalent of a Matlab timestamp"
  (* (- (string-to-number mat) 719529) 86400))

(defun ts2dt (&optional here)
  "Convert the Timestamp at point to a datetime and echo in the minibuffer"
  (interactive "P")
  (let ((msg (concat " " (date-ud (word-at-point)))))
    (message msg)
    (if here (eol-insert msg))))

(defun mat2nix (&optional here)
  "Convert the Matlab time at point to a unix ts and echo in the minibuffer"
  (interactive "P")
  (let ((msg (concat " " (number-to-string (mat-2-utc (float-at-point))))))
    (message msg)
    (if here (eol-insert msg))))

(defun mat2dt (&optional here)
  "Convert the Matlab time at point to a date string and echo in the minibuffer"
  (interactive "P")
  (let ((msg (message (date-ud (number-to-string (mat-2-utc (float-at-point)))))))
    (message msg)
    (if here (eol-insert msg))))

;;
;; Utilities
;;
;; Commentary:
;;
;; These functions relate to specific actions to be performed on files
;; matching a fixed format that I happen to deal with at work.
;;
;; Unlikely to be of general use.
;;

(defun add2t0 (&optional here)
  "Add The offset at point to the previously declared t0"
  (interactive "P")
  (let* ((offset (word-at-point))
         (t0 (save-excursion
               (re-search-backward "t0: \\([0-9]+\\)")
               (match-string 1)))
         (ts-at-point (+ (string-to-number offset)
                         (string-to-number t0)))
         (output (format "%d (%s)" ts-at-point (date-ud (number-to-string ts-at-point)))))
    (message output)
    (if here (save-excursion
               (move-end-of-line nil)
               (insert (concat " " output))))))


;; Code ends
(provide 'efourth-dimension)

;; Begin common lisp tutorial code followon
;; Udemy course April 2024.

(defun hello ()
  "Say hello to USER env variable"
  (format t "hello ~a" (uiop:getenv "USER")))

(hello)

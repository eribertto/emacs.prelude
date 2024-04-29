;; begin Common Lisp coding practice
;; Sun 21 Apr 2024 12:13:36 PM +03
(require :asdf) ;; tip from Udemy course Learn Common Lisp
(defun hello ()
  "Say hello to the env var USER."
  (format t "Hello there ~a!!!" (uiop:getenv "USER")))

(hello)

;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-o and enter text in its buffer.

(ql:quickload "hunchentoot")
(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))

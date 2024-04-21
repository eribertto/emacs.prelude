;; Example of a function using the package uiop

(require :asdf)
(defun hello ()
  "Say hello to the user env USER."
  (format t "Hello how are you ~a???" (uiop:getenv "USER")))

;; call it
(hello)

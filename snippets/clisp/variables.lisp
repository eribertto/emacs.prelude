;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-o and enter text in its buffer.
;; Lisp variables
(setf p '(John Q Public)) ; => (JOHN Q PUBLIC)
(princ p)
(setf x 10) ; => 10 (4 bits, #xA, #o12, #b1010)
(+ x (* x x)) ; => 110 (7 bits, #x6E, #o156, #b1101110)
(+ x (length p)) ; => 13 (4 bits, #xD, #o15, #b1101)

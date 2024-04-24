;; All about functions
;; To create a new function use the keyword defun
;; (require :asdf)
(setf pub '(John Q Public)) ; => (JOHN Q PUBLIC)
                                        ; => (JOHN Q PUBLIC)

(defun last-name (name)
  "Select the last name from the list object argument name."
  (first (last name)))

(last-name pub)





;; Lisp variables
(setf p '(John Q Public)) ; => (JOHN Q PUBLIC)
(princ p); (JOHN Q PUBLIC) => (JOHN Q PUBLIC)
(setf x 10) ; => 10 (4 bits, #xA, #o12, #b1010)
(+ x (* x x)) ; => 110 (7 bits, #x6E, #o156, #b1101110)
(+ x (length p)) ; => 13 (4 bits, #xD, #o15, #b1101)
(cons 'Mr p) ; => (MR JOHN Q PUBLIC)
(cons (first p) (rest p)) ; same as p
;; note cons means 'construct'
(setf town (list 'Anytown 'USA))
;; list function takes any number of elements as args and returns a new list containing those elements in order
;; append is similar to list but instead of atom/symbol elements, its arguments must be in the form of list objects.
(last p) ;; returns a list
(first (last p)) ;; this returns a symbol element

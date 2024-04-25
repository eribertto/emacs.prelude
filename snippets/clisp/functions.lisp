;; All about functions
;; To create a new function use the keyword defun
;; (require :asdf)
(setf pub '(John Q Public)) ; => (JOHN Q PUBLIC)
                                        ; => (JOHN Q PUBLIC)
                                        ; => (JOHN Q PUBLIC)

(defun last-name (name)
  "Select the last name from the list object argument name."
  (first (last name)))

(last-name pub) ; => PUBLIC
(last-name '(Rear Admiral Grace Murray Hopper)) ; => HOPPER
(setf me '(Eriberto L Mendez Jr)) ; => (ERIBERTO L MENDEZ JR)
(last-name me) ; => JR
(last-name '(Spot)) ; obviously not a last name, maybe a name of a dog/cat/animal-of-your-choice
(last-name '(Aristotle)) ; lol long time ago, when names are still addressed to one

(defun first-name (name)
  "Select the first name from the argument of type list."
  (first name))
(first-name me) ; => ERIBERTO
(first-name pub) ; => JOHN
(first-name '(Wilma Flintstone)) ; => WILMA
;; define a list of arbitrary names
(setf names '((John Q Public) (Malcolm X)
              (Admiral Grace Murray Hopper) (Spot)
              (Aristotle) (A A Milne)
              (Z Z Top) (Sir Richard Attenborough) (Miss Scarlett)))
(first-name (first names)) ; => JOHN
(first-name (second names)) ; => MALCOLM
(last-name (last names)) ; => (MISS SCARLETT) ;; why a list? bcoz last returns a list
;; thy this
(last-name (last-name (last names))) ; => SCARLETT
(first-name (first-name (last names))) ; => MISS

;; using mapcar
(mapcar #'last-name names) ; => (PUBLIC X HOPPER SPOT ARISTOTLE MILNE TOP ATTENBOROUGH SCARLETT)
;; mapcar requires two arguments e.g. a function-name and a list object. this returns a list
(mapcar #'- '(1 2 3 4 5)) ; => (-1 -2 -3 -4 -5)
(mapcar #'+ '(1 2 3 4) '(10 20 30 40)) ; => (11 22 33 44) ; adds list 1 element to list 2 where both have the same no. of elements.
;; how about non-equal elements?
(mapcar #'+ (5 6 7 8) '(50 60 70 80 90)) ;; this is compile-time error illegal function call
;; mapcar above requires 3 arguments, the first one is a binary operation followed by list-type objects
;; mapcar applies the first function to the elements of the second and third list-objects
;; the return value is of type list

(mapcar #'first-name names) ; => (JOHN MALCOLM ADMIRAL SPOT ARISTOTLE A Z SIR MISS)

;; introducing defparameter a special variable declaration that does not change over the program life, by spelling their names with asterisks or ear-muffs, this is just a convention among lisp programmers

(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name.")

;; new function definition using defparameter and if-else statement
(defun first-name (name)
  "Select the first name from a name represented as a list."
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))
;; try it out with mapcar
(mapcar #'first-name names) ; => (JOHN MALCOLM GRACE SPOT ARISTOTLE A Z RICHARD SCARLETT)
(first-name '(Madam Major General Paula Abdul)) ; => PAULA

(trace first-name)
(first-name '(John B Wick))

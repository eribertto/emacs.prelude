;; Lisp variables using setf
;; Section 1.2 of tutorial https://norvig.github.io/paip-lisp/#/chapter1

(setf p '(Eriberto L Mendez Jr))
(princ p)
(setf x 10)
(+ x (* x x)) ;; 110
(+ x (length p)) ;; 13

;; note setf is not a function but rather a part of basic syntax of Lisp.
;; They are known as special forms. They serve the same purpose as statements in other programming languages.
;; From the book Common LISPcraft setf is called special function while
;; (setf x 3) is called a special form
;; When there is a risk of confusion we call setf a special form operator (just one)
;; while (setf x 3) a special form expression (more than one)
;; More examples of special form operators: defun, defparameter, setf, let, case, if, function (#'), quote (')

;; Lists. Note the functions to extract the elements from a list
;; In Lisp, indexing begins at 1 not zero.

(princ p) ; describe function princ
(first p) ; get the first element
(rest p) ; get all elements but the first
(second p)
(length p) ; 3
(fourth p) ; nil, nonexistent

;; Nested lists
;; Note the command to eval aka insert the return value in the buffer sly style C-u C-x C-e

(setf n '((1st element) 2 (element 3) ((4)) 5)) ; => ((1ST ELEMENT) 2 (ELEMENT 3) ((4)) 5)
(length n) ; 5
(second (third n)) ; 3
(last (fourth n)) ; returns a list, take note!
(first (first (fourth n))) ;; this returns the symbol 4, an integer
;; todo: readup Sly manual
;; https://joaotavora.github.io/sly/

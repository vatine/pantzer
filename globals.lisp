(in-package :pantzer)

(defconstant +screen-side+ 512)
(defvar *pantzer-package* (find-package :pantzer))

(defvar *camera* nil)
(defvar *moving-objects* nil)
(defvar *all-shapes* nil)

(defvar *score* 0)
(defparameter *score-table* '((tetraeder . 10)
			      (box . 15)
			      (octaeder . 20)
			      (tank . 100)))


(defconstant +check-interval+ 30)
(defconstant +start-lives+ 3)
(defvar *lives* -1)


(defun decrease-if-positive (base n)
  (if (> base 0)
      (- base n)
    base))

(defun decrease-if-not-negative (base n)
  (if (>= base 0)
      (- base n)
    base))

(define-modify-macro decf-if-positive (&optional (delta 1))
  decrease-if-positive)
(define-modify-macro decf-if-not-negative (&optional (delta 1))
  decrease-if-not-negative)

(defmacro make-the (fun type)
  (let ((macro-name (intern (format nil "THE-~A" (symbol-name fun))
			    (symbol-package fun))))
    `(defmacro ,macro-name (obj)
       (list 'the ',type (list ',fun obj)))))

(make-the x double-float)
(make-the y double-float)
(make-the z double-float)
(make-the angle double-float)
(make-the focal double-float)

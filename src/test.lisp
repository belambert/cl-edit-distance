

(defpackage #:edit-distance-tests
  (:use :common-lisp
	:cl-user
        :edit-distance
        :lisp-unit)
  (:export
   #:run))

(in-package :edit-distance-tests)

(define-test test1
    (let ((result (compute-edit-distance '(1 2 3) '(1 2 4))))
      (assert-equal (distance-errors result) 1)))

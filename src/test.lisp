;; To run these tests: (lisp-unit:run-tests :all :edit-distance-tests)

(defpackage #:edit-distance-tests
  (:use :common-lisp
	:cl-user
        :edit-distance
        :lisp-unit)
  (:export
   #:run))

(in-package :edit-distance-tests)

(define-test test-distance-fast
    (let ((result (compute-edit-distance '(1 2 3) '(1 2 4))))
      (assert-equal (distance-errors result) 1)
      (assert-equal (distance-matches result) 2)))


(define-test test-distance-slow
  (multiple-value-bind (path distance)
      (levenshtein-distance '("1" "2" "3") '("1" "2" "4") :return-path t)
    (assert-equal path '((:MATCH "1" "1") (:MATCH "2" "2") (:SUBSTITUTION "3" "4")))
    (assert-equal distance 1)))

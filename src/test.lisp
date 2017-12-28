;; cl-edit-distance is licensed under a
;; Creative Commons Attribution 4.0 International License.

;; You should have received a copy of the license along with this
;; work. If not, see <http://creativecommons.org/licenses/by/4.0/>.


;; To run these tests: (lisp-unit:run-tests :all :edit-distance-tests)

(defpackage :edit-distance-tests
  (:use :common-lisp
	:cl-user
        :edit-distance
        :lisp-unit)
  (:export :run))

(in-package :edit-distance-tests)

(define-test test-distance-fast
    (let ((result (distance '(1 2 3) '(1 2 4))))
      (assert-equal 1 result)))

(define-test test-distance-slow
  (multiple-value-bind (path distance)
      (diff '("1" "2" "3") '("1" "2" "4"))
    (assert-equal path '((:MATCH "1" "1") (:MATCH "2" "2") (:SUBSTITUTION "3" "4")))
    (assert-equal distance 1)))

(define-test test-printing
  (multiple-value-bind (path distance)
      (diff '(0 1 2 3) '(1 2 4 5))
    (assert-equal distance 3)
    (format-diff path)))

(define-test test-arrays
    (let ((result (distance #(1 2 3) #(1 2 4))))
      (assert-equal 1 result)))

(define-test test-strings
    (let ((result (distance "123" "124")))
      (assert-equal 1 result)))

(define-test test-string-printing
  (multiple-value-bind (path distance)
      (diff "0123" "1245")
    (assert-equal distance 3)
    (format-diff path)))

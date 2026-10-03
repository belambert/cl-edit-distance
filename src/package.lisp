;; Copyright (c) 2014 Ben Lambert. Released under the MIT License; see LICENSE.txt.

(defpackage :edit-distance
  (:use :common-lisp)
  (:export :distance
	   :diff
	   :print-diff
	   :format-diff
	   :insertions-and-deletions))


(defpackage :levenshtein-distance
  (:use :common-lisp)
  (:export :compute-edit-distance
	   :compute-alignment
	   :compute-insertions-and-deletions
	   :sequence-diff	   
	   :edit-distance-result
	   :distance-insertions
	   :distance-deletions
	   :distance-substitutions
	   :distance-matches
	   :distance-errors))

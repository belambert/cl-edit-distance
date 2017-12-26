;; cl-edit-distance is licensed under a
;; Creative Commons Attribution 4.0 International License.

;; You should have received a copy of the license along with this
;; work. If not, see <http://creativecommons.org/licenses/by/4.0/>.

(defpackage :edit-distance
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
	   :distance-errors
	   :levenshtein-distance
	   :edit-distance
	   :print-differences))

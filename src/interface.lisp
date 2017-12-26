;; cl-edit-distance is licensed under a
;; Creative Commons Attribution 4.0 International License.

;; You should have received a copy of the license along with this
;; work. If not, see <http://creativecommons.org/licenses/by/4.0/>.

(in-package :edit-distance)

(defstruct (edit-distance-result (:conc-name distance-))
  "Represents the result of performing an edit distance computation.  The actual
   edit distance is stored in 'errors'.  This is comprised of the sum of insertions/deletions/
   substitutions.  Matches can be used to compute recognition rates (as opposed to error rates)."
  insertions
  deletions
  substitutions
  matches
  errors)

(defun compute-edit-distance (s1 s2 &key (test 'equal))
  "Get the edit distance between two sequences, represented in an edit-distance-result object.
   Insertion/deletion/substitution counts are not currently being computed."
  (unless (arrayp s1) (setf s1 (make-array (length s1) :element-type 'string :initial-contents s1)))
  (unless (arrayp s2) (setf s2 (make-array (length s2) :element-type 'string :initial-contents s2)))
  (multiple-value-bind (errors matches)
      (levenshtein-distance-fast s1 s2 :test test)
    (unless errors (setf errors 0))
    (unless matches (setf matches 0))
    (make-edit-distance-result :insertions 0
			       :deletions 0
			       :substitutions 0
			       :matches matches
			       :errors errors)))

(defun compute-alignment (s1 s2 &key (test 'equal))
  (levenshtein-distance s1 s2 :test test :return-path t))

(defun compute-insertions-and-deletions (seq1 seq2 &key (test 'equal))
  "Return a cons that points to two lists.  The first is a list of the insertions;
   The second is a list of deletions."
  (let* ((path (levenshtein-distance seq1 seq2 :return-path t :test test))
	 (non-matches (remove :match path :key 'first)) ;; get rid of all the matches, b/c they weren't missed or erroneous...
	 (deletions (mapcar 'second non-matches))
	 (insertions (mapcar 'third non-matches)))
    (setf deletions (delete nil deletions))
    (setf insertions (delete nil insertions))
    (cons insertions deletions)))

(defun sequence-diff (seq1 seq2 &key (file-stream t) (test 'equal) prefix1 prefix2 suffix1 suffix2)
  "Print a human readable 'diff' of the two sequences to the given stream (standard out by default).
   Optionally prefixes and suffixes of each line can be printed for easier identification and
   analysis."
  (let ((path (levenshtein-distance seq1 seq2 :return-path t :test test)))
    (print-differences path :file-stream file-stream :prefix1 prefix1 :prefix2 prefix2 :suffix1 suffix1 :suffix2 suffix2)))

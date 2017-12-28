;; cl-edit-distance is licensed under a
;; Creative Commons Attribution 4.0 International License.

;; You should have received a copy of the license along with this
;; work. If not, see <http://creativecommons.org/licenses/by/4.0/>.

(in-package :edit-distance)

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

(defun edit-distance (s1 s2 &key (test 'equal) (return-path nil))
  (levenshtein-distance s1 s2 :test test :return-path return-path))

;;;;;;;;;;;; Use these... ;;;;;;;;;;;

(defun distance (s1 s2 &key (test 'equal))
  (multiple-value-bind (path distance)
      (levenshtein-distance s1 s2 :test test :return-path nil)
    (declare (ignore path))
    distance))

(defun diff (s1 s2 &key (test 'equal))
  (levenshtein-distance s1 s2 :test test :return-path t))

(defun print-diff (seq1 seq2 &key
			 (file-stream t)
			 (test 'equal)
			 (prefix1 "seq1")
			 (prefix2 "seq2")
			 (suffix1 "")
			 (suffix2 ""))
  "Print a human readable 'diff' of the two sequences to the given stream (standard out by default).
   Optionally prefixes and suffixes of each line can be printed for easier identification and
   analysis."
  (let ((path (levenshtein-distance seq1 seq2 :return-path t :test test)))
    (print-differences path
		       :file-stream file-stream
		       :prefix1 prefix1
		       :prefix2 prefix2
		       :suffix1 suffix1
		       :suffix2 suffix2)
    (values)))

(defun insertions-and-deletions (seq1 seq2 &key (test 'equal))
  (compute-insertions-and-deletions seq1 seq2 :test test))

(defun format-diff (path &key (file-stream t) prefix1 prefix2 suffix1 suffix2)
  "Print a human readable 'diff' of the two sequences to the given stream (standard out by default).
   Optionally prefixes and suffixes of each line can be printed for easier identification and
   analysis."
  (print-differences path
		     :file-stream file-stream
		     :prefix1 prefix1
		     :prefix2 prefix2
		     :suffix1 suffix1
		     :suffix2 suffix2))

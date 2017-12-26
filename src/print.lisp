;; cl-edit-distance is licensed under a
;; Creative Commons Attribution 4.0 International License.

;; You should have received a copy of the license along with this
;; work. If not, see <http://creativecommons.org/licenses/by/4.0/>.

(in-package :edit-distance)

(defun print-differences (path &key (file-stream t) prefix1 prefix2 suffix1 suffix2)
  "Given a 'path' as produced by the above function LEVENSTEIN-DISTANCE function above,
   print the differences in the format of the 'align' program.
   The prefix and suffix args allow the caller to supply a string to print before, and
   after each."
  (fresh-line file-stream)
  (let ((prefix-length (max (length prefix1) (length prefix2))))
    (format file-stream "~vA: " prefix-length prefix1)
    (dolist (entry path)
      (let* ((type (first entry))
	     (value1 (write-to-string (second entry)))
	     (value2 (write-to-string (third entry)))
	     (length (max (length value1) (length value2))))
	(cond ((eq type :match)
	       (format file-stream "~A " (string-downcase value1)))
	      ((eq type :substitution)
	       (format file-stream "~vA " length (string-upcase value1)))
	      ((eq type :insertion)
	       (format file-stream "~vA " length (make-string length :initial-element #\*)))
	      ((eq type :deletion)
	       (assert value1)
	       (format file-stream "~vA " length (string-upcase value1))))))
    (if suffix1 
	(format file-stream "[~A]~%" suffix1)
	(format file-stream "~%"))
    (format file-stream "~vA: " prefix-length prefix2)
    (dolist (entry path)
      (let* ((type (first entry))
	     (value1 (write-to-string (second entry)))
	     (value2 (write-to-string (third entry)))
	     (length (max (length value1) (length value2))))
	(cond ((eq type :match)
	       (format file-stream "~A " (string-downcase value2)))
	      ((eq type :substitution)
	       (format file-stream "~vA " length (string-upcase value2)))
	      ((eq type :insertion)
	       (assert value2)
	       (format file-stream "~vA " length (string-upcase value2)))
	      ((eq type :deletion)
	       (format file-stream "~vA " length (make-string length :initial-element #\*))))))
    (if suffix2
	(format file-stream "[~A]~%" suffix2)
	(format file-stream "~%"))))

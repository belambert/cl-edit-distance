;;; Author: Ben Lambert
;;; ben@benjaminlambert.com

(in-package :edit-distance)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Interface(?) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Levenshtein distance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun levenshtein-distance-fast (seq1 seq2 &key (test 'equal))
  "Just like the previous version, but also return the number of *matches*.
   This is pretty fast now... It looks like most of the time taken is in applying the test (e.g. string-equal)
   some non-negligible part of it is taken by the MAKE-ARRAY calls and GC'ing those..."
  (declare ((simple-array *) seq1 seq2))
  ;;(declare (simple-array seq1 seq2))
  ;;(declare (optimize (debug 3)))
  (setf test (coerce test 'function))
  (let* ((n (length seq1))
	 (m (length seq2))
	 (column-height (1+ m)))
    ;; Check trivial cases
    (cond ((= 0 n) (return-from levenshtein-distance-fast m))
	  ((= 0 m) (return-from levenshtein-distance-fast n)))
    (let ((error-col (make-array column-height :element-type '(integer 0 100000)))
	  (error-col-previous (make-array column-height :element-type '(integer 0 100000)))
	  (match-col (make-array column-height :element-type '(integer 0 100000) :initial-element 0))
	  (match-col-previous (make-array column-height :element-type '(integer 0 100000) :initial-element 0)))
      (declare ((simple-array (integer 0 100000)) error-col error-col-previous match-col match-col-previous))
      ;; We need to store only two columns---the current one that is being built and the previous one      
      ;; Fill the the first column:
      (dotimes (i column-height)
	(setf (aref error-col-previous i) i))
      ;; Loop across all chars of each string
      (dotimes (i n)
	;; Set the cell at the top of the column (i.e. the first row)
	(setf (aref error-col 0) (1+ i))
	(dotimes (j m)
	  (let* ((j-plus-one (1+ j))
		 (a (1+ (aref error-col j)))  ;; ins?
		 (b (1+ (aref error-col-previous j-plus-one))) ;; del?
		 (match-value (if (funcall test (aref seq1 i) (aref seq2 j))
				  0  ;; match
				  1)) ;; substitution
		 (c (+ (aref error-col-previous j) match-value))
		 (min (min a b c)))
	    ;; Save the minimum
	    (setf (aref error-col j-plus-one) min)
	    ;; If it was a match, save that too.
	    (cond ((and (= c min) (= match-value 0)) ;; it was a match
		   (setf (aref match-col j-plus-one)
			 (1+ (aref match-col-previous j))))
		  ((and (= c min) (= match-value 1)) ;; it was a substitution
		   (setf (aref match-col j-plus-one)
			 (aref match-col-previous j)))
		  ((= a min)
		   (setf (aref match-col j-plus-one)
			 (aref match-col j)))
		  ((= b min)
		   (setf (aref match-col j-plus-one)
			 (aref match-col-previous j-plus-one)))
		  (t (error "!!")))))
	(rotatef error-col error-col-previous)
	(rotatef match-col match-col-previous))
      (values (aref error-col-previous m) (aref match-col-previous m)))))

(defun edit-distance (s1 s2 &key (test 'equal) (return-path nil))
  (levenshtein-distance s1 s2 :test test :return-path return-path))

;; This one is much slower.  But it's also the only way to get the alignment.
(defun levenshtein-distance (s1 s2 &key (test 'equal) (return-path nil))
  "Compute the Levenshtein distance between two sequences.  If return path is t, returns the return path.
   O/w just returns the distance."
  (declare (optimize (speed 1)))
  ;; Everything should be simple arrays
  (unless (typep s1 'simple-array)
    (setf s1 (make-array (length s1) :element-type 'string :initial-contents s1)))
  (unless (typep s2 'simple-array)
    (setf s2 (make-array (length s2) :element-type 'string :initial-contents s2)))
  ;; If the return path is not required, call the faster version instead.
  (unless return-path
    (return-from levenshtein-distance (values '()  (levenshtein-distance-fast s1 s2 :test test))))
  (setf test (coerce test 'function))
  (setf s1 (make-array (length s1) :element-type 'string :initial-contents s1))
  (setf s2 (make-array (length s2) :element-type 'string :initial-contents s2))
  (let* ((width (1+ (length s1)))
	 (height (1+ (length s2)))
	 (d (make-array (list height width) :element-type '(or null fixnum) :initial-element nil))
	 (bp (make-array (list height width) :initial-element nil)))
    (declare ((simple-array (or null fixnum)) d))
    (dotimes (x width)
      (setf (aref d 0 x) x)
      (unless (= x 0)
	(setf (aref bp 0 x) (list :deletion (elt s1 (1- x)) nil ))))
    (dotimes (y height)
      (setf (aref d y 0) y)
      (unless (= y 0)
	(setf (aref bp y 0) (list :insertion nil (elt s2 (1- y))))))
    (dotimes (x (length s1))
      (dotimes (y (length s2))
	(let* ((a (1+ (aref d y (1+ x))))
	       (b (1+ (aref d (1+ y) x)))
	       (match-value (if (funcall test (elt s1 x) (elt s2 y))
				0
				1))
	       (c (+ (aref d y x)
		     match-value))
	       (min (min a b c)))
	  ;; save info to reconstruct the path
	  (cond ((and (= c min) ( = match-value 0))
		 (setf (aref bp (1+ y) (1+ x)) (list :match (elt s1 x) (elt s2 y))))
		((and (= c min) (/= match-value 0))
		 (setf (aref bp (1+ y) (1+ x)) (list :substitution (elt s1 x) (elt s2 y) )))
		((= a min)
		 (setf (aref bp (1+ y) (1+ x)) (list :insertion  nil (elt s2 y) )))
		((= b min)
		 (setf (aref bp (1+ y) (1+ x)) (list :deletion (elt s1 x) nil)))
		(t (error "")))
	;; save the min distance
	  (setf (aref d (1+ y) (1+ x)) min))))
    (values
     (get-path-from-bp-table bp width height) ;; path
     (aref d (1- height) (1- width)))))       ;; distance

(defun get-path-from-bp-table (bp width height)
  "Given a back-pointer table, return a representation of the shortest path."
  (let ((x (1- width))
	(y (1- height))
	(path '()))
    (loop until (and (= x 0) (= y 0)) do
	 (let* ((this-bp (aref bp y x))
		(bp-type (first this-bp)))
	   (push this-bp path)
	   (unless bp-type (return-from get-path-from-bp-table path))
	   (cond ((or (eq bp-type :match) (eq bp-type :substitution))
		  (decf x) (decf y))
		 ((eq bp-type :insertion)
		  (decf y))
		 ((eq bp-type :deletion)
		  (decf x))
		 (t (error "~A is not a valid match/subst/insertion/deletion." this-bp))
		 )))
    path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Printing sequence differences ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	     (value1 (second entry))
	     (value2 (third entry))
	     (length (max (length value1) (length value2))))
	(cond ((eq type :match)
	       (format file-stream "~A " (string-downcase value1)))
	      ((eq type :substitution)
	       (format file-stream "~vA " length (string-upcase value1)))
	      ((eq type :insertion)
	       (format file-stream "~vA " length (make-string length :initial-element #\*)
		       ))
	      ((eq type :deletion)
	       (assert value1)
	       (format file-stream "~vA " length (string-upcase value1)))
	      )))
    (if suffix1 
	(format file-stream "[~A]~%" suffix1)
	(format file-stream "~%"))
    (format file-stream "~vA: " prefix-length prefix2)
    (dolist (entry path)
      (let* ((type (first entry))
	     (value1 (second entry))
	     (value2 (third entry))
	     (length (max (length value1) (length value2))))
	(cond ((eq type :match)
	       (format file-stream "~A " (string-downcase value2)))
	      ((eq type :substitution)
	       (format file-stream "~vA " length (string-upcase value2)))
	      ((eq type :insertion)
	       (assert value2)
	       (format file-stream "~vA " length (string-upcase value2)))
	      ((eq type :deletion)
	       (format file-stream "~vA " length (make-string length :initial-element #\*)))
	      )))
    (if suffix2
	(format file-stream "[~A]~%" suffix2)
	(format file-stream "~%"))))



;; cl-edit-distance is licensed under a
;; Creative Commons Attribution 4.0 International License.

;; You should have received a copy of the license along with this
;; work. If not, see <http://creativecommons.org/licenses/by/4.0/>.

(in-package :edit-distance)

(defun levenshtein-distance-fast (seq1 seq2 &key (test 'equal))
  "Just like the previous version, but also return the number of *matches*.
   This is pretty fast now... It looks like most of the time taken is in applying the test (e.g. string-equal)
   some non-negligible part of it is taken by the MAKE-ARRAY calls and GC'ing those..."
  (declare ((simple-array *) seq1 seq2))
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
		 (t (error "~A is not a valid match/subst/insertion/deletion." this-bp)))))
    path))

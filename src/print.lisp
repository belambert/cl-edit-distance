;; Copyright (c) 2014 Ben Lambert. Released under the MIT License; see LICENSE.txt.

(in-package :edit-distance)

(defun print-differences (path &key (file-stream t) prefix1 prefix2 suffix1 suffix2)
  "Print the two sides of PATH as aligned lines; substitutions are bracketed and gaps are asterisks."
  (let* ((cols (mapcar #'column path))
         (widths (mapcar (lambda (col) (reduce #'max col :key #'length)) cols))
         (plen (max (length prefix1) (length prefix2))))
    (fresh-line file-stream)
    (print-side file-stream plen prefix1 suffix1 (mapcar #'first cols) widths)
    (print-side file-stream plen prefix2 suffix2 (mapcar #'second cols) widths)))

(defun print-side (stream plen prefix suffix cells widths)
  (format stream "~vA: ~{~A ~}~@[[~A]~]~%" plen prefix
          (mapcar (lambda (cell width) (format nil "~vA" width cell)) cells widths)
          suffix))

(defun column (entry)
  "Return the text shown for ENTRY on the first and second line."
  (destructuring-bind (type a b) entry
    (ecase type
      (:match (list (princ-to-string a) (princ-to-string b)))
      (:substitution (list (format nil "[~A]" a) (format nil "[~A]" b)))
      (:insertion (let ((text (princ-to-string b))) (list (gap text) text)))
      (:deletion (let ((text (princ-to-string a))) (list text (gap text)))))))

(defun gap (text)
  "Return asterisks as wide as TEXT, and at least one."
  (make-string (max 1 (length text)) :initial-element #\*))

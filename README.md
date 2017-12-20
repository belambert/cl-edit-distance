[![Build Status](https://travis-ci.org/belambert/cl-editdistance.svg?branch=master)](https://travis-ci.org/belambert/cl-editdistance)

edit-distance
=================

Computes the Levenshtein distance between two sequences.

https://en.wikipedia.org/wiki/Levenshtein_distance

Compute edit distance using this function:
```lisp
(defun levenshtein-distance (s1 s2 &key (test 'equal) (return-path nil)))
```

[![Build Status](https://travis-ci.org/belambert/cl-editdistance.svg?branch=master)](https://travis-ci.org/belambert/cl-editdistance)

edit-distance
=================

Computes the Levenshtein distance between two sequences.

https://en.wikipedia.org/wiki/Levenshtein_distance

Compute edit distance using this function:
```lisp
    (defun levenshtein-distance (s1 s2 &key (test 'equal) (return-path nil)))
```

---
<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/Text" property="dct:title" rel="dct:type">cl-editdistance</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="https://github.com/belambert/cl-editdistance" property="cc:attributionName" rel="cc:attributionURL">Ben Lambert</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.

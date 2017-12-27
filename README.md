[![Build Status](https://travis-ci.org/belambert/cl-edit-distance.svg?branch=master)](https://travis-ci.org/belambert/cl-edit-distance)

edit-distance
=================

Computes the Levenshtein distance between two sequences.

https://en.wikipedia.org/wiki/Levenshtein_distance

Compute edit distance using this function:
```lisp
    (defun levenshtein-distance (s1 s2 &key (test 'equal) (return-path nil)))
```

or

```lisp
    (defun edit-distance (s1 s2 &key (test 'equal) (return-path nil)))
```

Testing
=======
To run tests with sbcl, run:

    sbcl --eval "(asdf:load-system 'edit-distance-test)" --eval "(unless (lisp-unit:run-tests :all :edit-distance-tests) (uiop:quit 1))"

---
<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/Text" property="dct:title" rel="dct:type">cl-edit-distance</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="https://github.com/belambert/cl-edit-distance" property="cc:attributionName" rel="cc:attributionURL">Ben Lambert</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.

edit-distance
=============
[![Build Status](https://travis-ci.org/belambert/cl-edit-distance.svg?branch=main)
](https://travis-ci.org/belambert/cl-edit-distance)
[![Coverage Status](https://coveralls.io/repos/github/belambert/cl-edit-distance/badge.svg?branch=coverage)
](https://coveralls.io/github/belambert/cl-edit-distance?branch=coverage)

Using
-----
Computes the [edit distance](https://en.wikipedia.org/wiki/Levenshtein_distance)
(aka Levenshtein distance) between two sequences. This is a common distance measure.

For example, to compute the distance between two sequences:

    (edit-distance:distance '(1 2 3 4 5) '(2 3 4 5 6))

To compute the sequence of edit operations needed to convert sequence
1 into sequence two, use the `diff` function

    (edit-distance:diff '(1 2 3 4 5) '(2 3 4 5 6))

That will return two values a structure, as follows, and the distance.

    ((:DELETION 1 NIL) (:MATCH 2 2) (:MATCH 3 3) (:MATCH 4 4) (:MATCH 5 5) (:INSERTION NIL 6))
    2

That structure can be printed more readibly with the `FORMAT-DIFF`
function

    (edit-distance:format-diff *path*)

Or, you can compute the diff and print it readably together by calling `PRINT-DIFF`:

    (edit-distance:print-diff '(1 2 3 4 5) '(2 3 4 5 6))

which will print a result like this:

    seq1: 1   2 3 4 5 *** []
    seq2: *** 2 3 4 5 6   []

Several options are available to the `FORMAT-DIFF` and `PRINT-DIFF` to
print a prefix and suffix for each line.  Note that displaying
substitutions relys on captialization and so substitutions are not
visible for non-alphabetic sequence elements.

Additionally, other equality functions can be used, so this evaluates
to a distance of zero:

     (edit-distance:distance '("ONE" "TWO" "THREE") '("one" "two"
     "three") :test 'string-equal)

Any type of sequence may be used, but for speed the input sequences
are copied into new arrays. If speed is a major concern make sure to
provide simple vectors as your input sequences.

Testing
-------
To test with sbcl, run:

    sbcl --eval "(asdf:load-system 'edit-distance-test)" --eval "(unless (lisp-unit:run-tests :all :edit-distance-tests) (uiop:quit 1))"

---
<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/Text" property="dct:title" rel="dct:type">cl-edit-distance</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="https://github.com/belambert/cl-edit-distance" property="cc:attributionName" rel="cc:attributionURL">Ben Lambert</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.

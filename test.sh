#!/bin/bash
set -ev

if [ "${COVERALLS}" = "true" ]; then
    cl -l edit-distance-test -l edit-distance -l cl-coveralls
       -e '(coveralls:with-coveralls (:exclude (list "t"))
             (unless (lisp-unit:run-tests :all :edit-distance-tests)
               (uiop:quit 1)))'
else

    cl -l edit-distance-test -l edit-distance
       -e '(unless (lisp-unit:run-tests :all :edit-distance-tests)
             (uiop:quit 1))'
fi

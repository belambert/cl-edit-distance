#!/bin/bash

if [ "${COVERALLS}" = "true" ]; then
    echo "Running test with coverage..."
    cl -l edit-distance-test -l edit-distance -l cl-coveralls
       -e "(coveralls:with-coveralls (:exclude (list \"t\")) \
             (unless (lisp-unit:run-tests :all :edit-distance-tests) \
               (uiop:quit 1)))"
else
    echo "Running test without coverage..."
    cl -l edit-distance-test -l edit-distance
       -e "(unless (lisp-unit:run-tests :all :edit-distance-tests) \
             (uiop:quit 1))"
fi

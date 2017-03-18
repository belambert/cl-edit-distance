;;-*- Mode: Lisp -*- 

;;; Author: Ben Lambert
;;; ben@benjaminlambert.com

(asdf:defsystem "edit-distance-test"
  :description "Computing edit distance"
  :version "0.1"
  :author "Ben Lambert"
  :serial t
  :components
  ((:module src
    :serial t
    :components
    ((:file "test")))))

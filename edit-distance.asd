;; -*- Mode: Lisp -*- 

(asdf:defsystem "edit-distance"
  :name "edit-distance"
  :description "Compute edit distance between sequences."
  :version "1.0.0"
  :author "Ben Lambert <belambert@mac.com>"
  :license "CC-BY-4.0"
  :serial t
  :in-order-to ((test-op (test-op "edit-distance-test")))
  :components
  ((:module src
    :serial t
    :components
    ((:file "package")
     (:file "distance")
     (:file "interface")
     (:file "print")))))

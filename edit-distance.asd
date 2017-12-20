;;-*- Mode: Lisp -*- 

;;; Author: Ben Lambert
;;; ben@benjaminlambert.com

(asdf:defsystem "edit-distance"
  :name "edit-distance"
  :description "Compute edit distance between sequences."
  :version "0.1.0"
  :author "Ben Lambert <belambert@mac.com>"
  :license "CC-BY-4.0"
  :serial t
  :components
  ((:module src
    :serial t
    :components
    ((:file "package")
     (:file "distance")))))

;;-*- Mode: Lisp -*- 

;;; Copyright Benjamin E. Lambert, 2005-2011
;;; All rights reserved
;;; Please contact author regarding licensing and use:
;;; ben@benjaminlambert.com

(asdf:defsystem "levenshtein-distance"
  :description "Computing edit distance"
  :version "0.1"
  :author "Benjamin E. Lambert"
  :licence "All rights reserved"
  :serial t
  :components
  ((:module src
    :serial t
    :components
    ((:file "package")
     (:file "distance"))))
  ;;:depends-on '()
  )


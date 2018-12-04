;;;; advent-of-code-2018.asd

(asdf:defsystem #:advent-of-code-2018
  :description "Solutions for the Advent of Code 2018"
  :author "Adrian Braemer"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("cl-ppcre")
  :components ((:file "package")
               (:file "advent-of-code-2018")))

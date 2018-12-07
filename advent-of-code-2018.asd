;;;; advent-of-code-2018.asd

(asdf:defsystem #:advent-of-code-2018
  :description "Solutions for the Advent of Code 2018"
  :author "NobodysHero"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("cl-ppcre")
  :components ((:file "package")
               (:file "utilities")
               (:file "day1")
               (:file "day2")
               (:file "day3")
               (:file "day4")
               (:file "day5")
               (:file "day6")
               (:file "day7")))

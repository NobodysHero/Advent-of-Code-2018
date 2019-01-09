;;;; advent-of-code-2018.asd

(asdf:defsystem #:advent-of-code-2018
  :description "Solutions for the Advent of Code 2018"
  :author "NobodysHero"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("cl-ppcre" "drakma" "cl-heap")
  :components ((:file "package")
               (:file "utilities")
               (:file "day1")
               (:file "day2")
               (:file "day3")
               (:file "day4")
               (:file "day5")
               (:file "day6")
               (:file "day7")
               (:file "day8")
               (:file "day9")
               (:file "day10")
               (:file "day11")
               (:file "day12")
               (:file "day13")
               (:file "day14")
               (:file "day16")
               (:file "day17")
               (:file "day18")
               (:file "day19")
               (:file "day20")
               (:file "day21")
               (:file "day22")
               (:file "day23")
               (:file "day25")))


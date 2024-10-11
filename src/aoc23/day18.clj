(ns aoc23.day18
  (:require [clojure.edn :as edn]
            [clojure.string :as s]
            [instaparse.core :as insta]))

(def testf "./data/day18-test.txt")
(def inputf "./data/day18-input.txt")

(def game-parser
  "Parse the input file"
  (insta/parser
   "<file> := instr+
    instr := direction <space> number <space> <'('> rgb <')'> <newline>
    <direction> := #'R|L|U|D'
    number := #'\\d+'
    <rgb> := #'#[a-f0-9]{6}'
    space := #'\\s+'
    newline := '\\n'"))

(defn transform-data
  [data]
  #_{:clj-kondo/ignore [:unresolved-var]}
  (insta/transform
   {:number edn/read-string}
   data))

(defn read-data
  "Read input data"
  [f]
  (->> f
       slurp
       game-parser
       transform-data))

(defn get-edge
  "Get the coordinates of the boundary edge in the list"
  [start direction length]
  (take (inc length)
        (iterate (fn [[r c]]
                   (case direction
                     "R" [r (inc c)]
                     "L" [r (dec c)]
                     "U" [(dec r) c]
                     "D" [(inc r) c]))
                 start)))
                
(defn boundary
  "Generate all coordinates in the boundary of the following enclosed area"
  [instrs]
  (loop [current [0 0]
         instrs' instrs
         boundary []]
    (if-let [[_ direction length _] (first instrs')]
      (let [coords (get-edge current direction length)
            current' (last coords)
            boundary' (into boundary (rest coords))]
        (recur current' (rest instrs') boundary'))
      boundary)))

(defn process-line
  "Find the enclosed area in a line, including the edges"
  [coords]
  (let [ys (sort-by second coords)]
    (if (nil? ys)
      0
      ys)))
      
(defn calculate-area
  "Calculate the enclosed area, including the edge"
  [perimeter-coords]
  (->> perimeter-coords
       (group-by first)
       vals
       (sort-by (comp first first))
       (map process-line)
       #_(apply +)))

(defn plot-coordinates
  "Plot the coordinates as a string"
  [coordinates]
  (let [min-x (apply min (map first coordinates))
        max-x (apply max (map first coordinates))
        min-y (apply min (map second coordinates))
        max-y (apply max (map second coordinates))
        coord-set (set coordinates)]
    (s/join
     "\n"
     (for [y (range max-y (dec min-y) -1)]
       (apply str
              (for [x (range min-x (inc max-x))]
                (if (coord-set [x y]) "#" " ")))))))
      
(defn part1
  [f]
  (->> f
       read-data
       boundary
       calculate-area))
;; The End

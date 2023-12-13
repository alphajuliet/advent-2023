(ns aoc23.day11
  (:require [aoc23.util :as util]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.edn :as edn]))

(def testf "data/day11-test.txt")
(def inputf "data/day11-input.txt")

(defn read-data
  "Read in the map and convert to a numeric matrix"
  [f]
  (->> f
       util/import-data
       (map #(str/replace % #"\." "0"))
       (map #(str/replace % #"#" "1"))
       (map #(str/split % #""))
       (util/mapmap edn/read-string)))

(defn zero-rows
  "Find where the rows are all zeroes"
  [mat]
  (for [r (range (count mat))
        :when (every? zero? (get mat r))]
    r))

(defn find-zero-rows-cols
  "Find rows and columns in the matrix that only contain zeroes"
  [mat]
  (let [rr (zero-rows mat)
        cc (zero-rows (util/T mat))]
   [rr cc]))

(defn find-ones
  "Find the coordinates of all the ones in the matrix"
  [mat]
  (for [r (range (count mat))
        c (range (count (first mat)))
        :when (= 1 (get-in mat [r c]))]
    [r c]))

(defn crossings
  "Determine how many of the points lie inside the range"
  [start end points]
  (if (<= start end)
    (count (filter #(< start % end) points))
    (count (filter #(> start % end) points))))

(defn expanded-manhattan
  "Return the manhattan distance between p1 and p2, plus an increment for every time the shortest path
   crosses one of the zero rows or columns."
  [[r1 c1] [r2 c2] [zrows zcols] incr]
  (let [dist (util/manhattan [r1 c1] [r2 c2])
        dr (* incr (crossings r1 r2 zrows))
        dc (* incr (crossings c1 c2 zcols))]
    (+ dist dr dc)))

(defn distances
  "Find the distances between all the coordinates in the list"
  [insertions incr coords]
;;   (set! *print-length* 5) ; help the debugger
  (for [[p1 p2] (combo/combinations coords 2)]
    (expanded-manhattan p1 p2 insertions incr)))

(defn part1
  [f]
  (let [data (read-data f)
        insertions (find-zero-rows-cols data)]
    (->> data
        find-ones
        (distances insertions 1)
        (apply +))))

(defn part2
  "If incr > 1, then enter incr-1"
  [f incr]
  (let [data (read-data f)
        insertions (find-zero-rows-cols data)]
    (->> data
         find-ones
         (distances insertions incr)
         (apply +))))

;; The End
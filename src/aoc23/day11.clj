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

(defn insert-rows
  "Insert one zero row after every given row number"
  [row-numbers mat]
  (let [cols (count (first mat))]
    (reduce (fn [m r]
              (concat (take (inc r) m)
                      (vector (repeat cols 0))
                      (take-last (- (count m) (inc r)) m)))
            mat
            (reverse row-numbers))))

(defn find-ones
  "Find the coordinates of all the ones in the matrix"
  [mat]
  (for [r (range (count mat))
        c (range (count (first mat)))
        :when (= 1 (get-in mat [r c]))]
    [r c]))

(defn distances
  "Find the distances between all the coordinates in the list"
  [points]
  (for [[p1 p2] (combo/combinations points 2)]
    (util/manhattan p1 p2)))

(defn part1
  [f]
  (let [data (read-data f)
        insertions (find-zero-rows-cols data)]
    (->> data
        (insert-rows (first insertions))
        util/T
        (insert-rows (second insertions))
        util/T
        find-ones
        distances
        (apply +))))

;; The End
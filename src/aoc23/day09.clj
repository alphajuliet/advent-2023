(ns aoc23.day09
  (:require [clojure.string :as str]
            [aoc23.util :as util]
            [clojure.edn :as edn]))

(def testf "data/day09-test.txt")
(def inputf "data/day09-input.txt")

(defn- read-data
  [f]
  (->> f
       util/import-data
       (map #(str/split % #"\s+"))
       (util/mapmap edn/read-string)))

(defn difference
  "Generate the difference vector from the given collection."
  [coll]
  (map - (rest coll) coll))

(defn all-zero?
  "A vector contains all zeroes"
  [coll]
  (every? zero? coll))

(defn fwd-extrapolate
  "Given a collection, extrapolate the next value based on finite differences"
  [coll]
  (->> coll
       (iterate (comp vec difference))
       (take-while (complement all-zero?))
       (map last)
       (apply +)))

(defn bwd-extrapolate
  "Extrapolate the previous value in the sequence"
  [coll]
  (->> coll
       reverse
       (iterate (comp vec difference))
       (take-while (complement all-zero?))
       (map last)
       reverse
       (apply +)))

(defn part1
  [f]
  (->> f
       read-data
       (map fwd-extrapolate)
       (apply +)))

(defn part2
  [f]
  (->> f
       read-data
       (map bwd-extrapolate)
       (apply +)))
      
;; The End
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

(defn diff
  "Generate the difference vector from the given collection."
  [coll]
  (map - (rest coll) coll))

(defn all-zero?
  "A vector contains all zeroes"
  [coll]
  (every? zero? coll))

(defn differences
  "Run difference operations until we have a zero vector, and collect all the intermediate vectors."
  [coll]
  (take-while (complement all-zero?) (iterate (comp vec diff) coll)))

(defn extrapolate
  "Given a collection, extrapolate the next value based on finite differences"
  [coll]
  (->> coll
       (iterate (comp vec diff))
       (take-while (complement all-zero?))
       (map last)
       (apply +)))

(defn part1
  [f]
  (let [data (read-data f)]
    data))

;; The End
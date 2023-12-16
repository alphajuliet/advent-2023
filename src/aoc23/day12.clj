(ns aoc23.day12
  (:require [aoc23.util :as util]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.edn :as edn]))

(def testf "data/day12-test.txt")
(def inputf "data/day12-input.txt")

(defn enumerate
  "Expand a vector of numbers containing a sequence of each number of length equal to
   its value, e.g. (enumerate [3 1 2]) -> ('333' '1' '22')"
  [coll]
  (map #(str/join "" (repeat % %)) coll))

(defn read-line
  "Read and parse a single line"
  [line]
  (let [[a b] (str/split line #"\s")
        nums (->> (str/split b #",")
                  (map edn/read-string)
                  enumerate)]
    (list a nums)))

(defn read-data
  "Read in all the data"
  [f]
  (->> f
       util/import-data
       (map read-line)))

(defn part1
  [f]
  (let [data (read-data f)]
    data))

;; The End
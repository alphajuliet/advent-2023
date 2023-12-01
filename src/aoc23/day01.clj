(ns aoc23.day01
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc23.util :as util]))

(def testf "data/day01-test.txt")
(def inputf "data/day01-input.txt")

(defn get-numbers
  "Extract each digit from the string"
  [s]
  (->> (str/split s #"")
       (map edn/read-string)
       (filter number?)))

(defn part1
  [f]
  (let [data (util/import-data f)]
    (->> data
         (map get-numbers)
         (map #(+ (* 10 (first %)) (last %)))
         (apply +))))
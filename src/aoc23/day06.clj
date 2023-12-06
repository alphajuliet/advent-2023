(ns aoc23.day06
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [aoc23.util :as util]))

(def testf "data/day06-test.txt")
(def inputf "data/day06-input.txt")

(defn read-data
  "Read the input data from the file"
  [f]
  (let [data (util/import-data f)]
    (->> data
         (map #(rest (str/split % #"\s+")))
         (map #(map edn/read-string %))
         (util/T))))

(defn distance
"How far travelled in t time units if the button is pushed for h units."
  [t h]
  (* h (- t h)))

(defn wins
  "Number of ways to beat r, given t"
  [[t r]]
  (->> (range t)
       (map #(distance t %))
       (filter #(> % r))
       count))

(defn part1
  [f]
  (->> f
       read-data
       (map wins)
       (apply *)))

;; The End
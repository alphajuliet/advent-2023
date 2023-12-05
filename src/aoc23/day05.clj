(ns aoc23.day05
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [aoc23.util :as util]))

(def testf "data/day05-test.txt")
(def inputf "data/day05-input.txt")

(defn read-seeds
  [line]
  (->> (str/split line #"\s+")
       rest
       (map edn/read-string)))

(defn read-mappings
  "Extract mappings from an entry"
  [line]
  (let [ranges (str/split-lines line)]
    (->> (rest ranges)
         (map #(str/split % #"\s+"))
         (util/mapmap edn/read-string))))

(defn map-number
  "Map n to a destination based on the given set of maps."
  [n [dest src _ :as m]]
  (if (nil? m)
    n
    (+ n (- dest src))))

(defn apply-maps
  "Find which mapping applies to the given number"
  [n maps]
  (->> maps
       (filter (fn [[_ src rng]] (<= src n (+ src (dec rng)))))
       first
       (map-number n)))

(defn apply-all-mappings
  "Apply all the mappings to the given number"
  [n mappings]
  (reduce apply-maps
          n
          mappings))

(defn part1
  [f]
  (let [lines (str/split (slurp f) #"\n\n")
        seeds (read-seeds (first lines))
        mappings (map read-mappings (rest lines))]
    (->> seeds
         (map #(apply-all-mappings % mappings))
         (apply min))))

;; The End
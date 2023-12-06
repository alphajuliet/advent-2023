(ns aoc23.day05
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [aoc23.util :as util]))

(def testf "data/day05-test.txt")
(def inputf "data/day05-input.txt")
(def input2f "data/day05-input2.txt")

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

(defn in-range?
  [n [_ src rng]] 
  (<= src n (+ src (dec rng))))

(defn apply-maps
  "Find which mapping from the collection applies to the given number"
  [n maps]
  (->> maps
    ;; (some #(when (in-range? n %) %)) ; is slower!
       (filter #(in-range? n %))
       first
       (map-number n)))

(defn apply-all-mappings
  "Apply all the collections of mappings to the given number"
  [n mappings]
  (reduce apply-maps
          n
          mappings))

(defn seed-ranges
  [seeds]
  (->> seeds
       (partition 2)
       (map #(range (first %) (+ (first %) (second %))))
       (apply concat)))

(defn part1
  [f]
  (let [lines (str/split (slurp f) #"\n\n")
        seeds (read-seeds (first lines))
        mappings (map read-mappings (rest lines))]
    (->> seeds
         (map #(apply-all-mappings % mappings))
         (apply min))))

(defn part2
  [f]
  (let [lines (str/split (slurp f) #"\n\n")
        seeds (seed-ranges (read-seeds (first lines)))
        mappings (map read-mappings (rest lines))]
    (->> seeds
         (map #(apply-all-mappings % mappings))
         (apply min))))

;; The End
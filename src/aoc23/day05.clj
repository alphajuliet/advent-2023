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
  ;; map-number : Int -> [[Int]] -> Int
  [n [dest src _ :as m]]
  (if (nil? m)
    n
    (+ n (- dest src))))

(defn in-range?
  "Check if a number lines in the mapping range"
  ;; in-range? : Int -> [Int Int] -> Bool
  [n [_ src rng]]
  (<= src n (+ src (dec rng))))

(defn apply-maps
  "Find which mapping from the collection applies to the given number"
  [n maps]
  (->> maps
       (filter #(in-range? n %))
       first
       (map-number n)))

(defn apply-stage
  "Apply all the collections of mappings to the given number"
  ;; apply-stage : Int -> [[Int]] -> Int
  [n mappings]
  (reduce apply-maps
          n
          mappings))

(defn map-limits
  "Find the limits of the current stage"
  ;; map-limits : [[Int]] -> [Int Int]
  [mappings]
  (let [lower (->> mappings
                   (map second)
                   (apply min))
        upper (->> mappings
                   (mapv #(+ (second %) (dec (util/third %))))
                   flatten
                   (apply max))]
    [lower upper]))

(defn apply-seed-range
  "Put the all seed ranges through each stage mapping"
  ;; apply-seed-range : [Int] -> [[Int]] -> [Int]
  [[start-seed len] mappings]
  (let [end-seed (+ start-seed (dec len))]
    (reduce (fn [[start-s end-s] m]
              (let [[start-map end-map] (map-limits m)]
                (if (or (<= end-s start-map) (<= end-map start-s))
                  (range start-s end-s)
                  (map #(apply-maps % m) (range start-s end-s)))))
            [start-seed end-seed]
            mappings)))

(defn convert-seed-range
  "Push the seed range through the mapping."
  [[seed-start seed-end :as seed-range] [dest map-start map-range]]
  (let [map-end (+ map-start (dec map-range))
        delta (- dest map-start)]
    (if (< seed-start map-start)
      (cond
        (< seed-end map-start) seed-range ;; case 1
        (< seed-end map-end) (map (partial + delta) [map-start seed-end]);; case 2
        :else (map (partial + delta) [map-start map-end])) ;; case 6
      (cond
        (< seed-start map-end) (map (partial + delta) [seed-start map-end]) ;; case 3
        (> seed-start map-end) seed-range ;; case 4
        :else (map (partial + delta) [seed-start seed-end])))))

(defn part1
  [f]
  (let [lines (str/split (slurp f) #"\n\n")
        seeds (read-seeds (first lines))
        mappings (map read-mappings (rest lines))]
    (->> seeds
         (map #(apply-stage % mappings))
         (apply min))))

(defn part2
  [f]
  (let [lines (str/split (slurp f) #"\n\n")
        seeds (partition 2 (read-seeds (first lines)))
        mappings (map read-mappings (rest lines))]
        (for [s seeds
              m mappings]
          (reduce convert-seed-range s m))
    #_(->> seeds
         (map #(apply-seed-range % mappings))
         last
         flatten
         (apply min))))

;; The End
(ns aoc23.day04
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]
            [aoc23.util :as util]))

(def testf "data/day04-test.txt")
(def inputf "data/day04-input.txt")

(defn read-data
  "Parse each line into a map"
  [line]
  (let [fields (str/split line #":|\|")]
    (->> fields
         rest
         (map #(as-> % <>
                 (str/trim <>)
                 (str/split <> #"\s+")
                 (map edn/read-string <>)))
         (zipmap [:winning :chosen]))))

(defn score-card
  "Count the numbers of chosen numbers in the winning list"
  [{:keys [winning chosen]}]
  (count 
   (set/intersection (set winning) (set chosen))))

(defn add-cards
  "Add additional subsequent cards based on the score"
  [nums index scores]
  (let [score (nth scores index)
        n (nth nums index)
        delta (concat (repeat (inc index) 0)
                      (repeat score n)
                      (repeat (- (count scores) (+ score (inc index))) 0))]
    (map + nums delta)))

(defn process-cards
  "Process the cards in order, adding cards according to the scores"
  [scores]
  (let [len (count scores)]
   (reduce (fn [acc i] 
             (add-cards acc i scores))
          (repeat len 1)
          (range len))))

(defn part1
  [f]
  (->> f
       util/import-data
       (map read-data)
       (map score-card)
       (map #(int (Math/pow 2 (dec %))))
       (apply +)))

(defn part2
  [f]
  (->> f
       util/import-data
       (map read-data)
       (map score-card)
       process-cards
       (apply +)))

;; The End
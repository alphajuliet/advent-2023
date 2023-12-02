(ns aoc23.day01
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc23.util :as util]))

(def testf "data/day01-test.txt")
(def test2f "data/day01-test2.txt")
(def inputf "data/day01-input.txt")

(defn get-numbers
  "Extract each digit from the string"
  [s]
  (->> (str/split s #"")
       (map edn/read-string)
       (filter number?)))

(defn get-numbers-words
  "Parse the string into a list of digits, stored as either numbers or words"
  [s]
  (let [m {"one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9}
        pattern #"(?=(one|two|three|four|five|six|seven|eight|nine|\d))"
        numbers (map last (re-seq pattern s))]
    (map #(or (get m %) (edn/read-string %)) numbers)))

(defn make-number
  "Make a 2-digit number from the digits in the collection"
  [coll]
  (+ (* 10 (first coll)) (last coll)))

(defn part1
  [f]
  (let [data (util/import-data f)]
    (->> data
         (map get-numbers)
         (map make-number)
         (apply +))))

(defn part2
  [f]
  (let [data (util/import-data f)]
    (->> data
         (map get-numbers-words)
         (map make-number)
         (apply +))))

(comment 
  (assert (= 142 (part1 testf))))
  (assert (= 281 (part2 test2f)))
(ns aoc23.day15 
  (:require [clojure.string :as str]))

(def testf "data/day15-test.txt")
(def inputf "data/day15-input.txt")

(defn read-data
  [f]
  (-> f
      slurp
      (str/trim-newline)
      (str/split #",")))

(defn my-hash
  [s]
  (let [ascii (map int s)]
    (reduce (fn [h a]
              (rem (* (+ h a) 17) 256))
            0
            ascii)))

(defn part1
  [f]
  (let [data (read-data f)]
    (->> data
         (map my-hash)
         (apply +))))

;; The End
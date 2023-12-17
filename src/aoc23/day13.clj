(ns aoc23.day13 
  (:require [clojure.string :as str]
            [aoc23.util :as util]))

(def testf "data/day13-test.txt")
(def inputf "data/day13-input.txt")

(defn read-data
  [f]
  (-> f
      slurp
      (str/split #"\n\n")))

(defn read-pattern
  [p]
  (-> p
      ;; (str/replace #"#" "1")
      ;; (str/replace #"\." "0")
      (str/split #"\n")))

(defn transpose
  "Transpose a collection of strings"
  [p]
  (->> p
       util/T
       (mapv #(str/join "" %))))

(defn find-candidates
  "Find two consecutive items in the collection that are equal and return the indices"
  [coll]
  (for [i (range 1 (count coll))
        :let [a (get coll i)
              b (get coll (inc i))]
        :when (= a b)]
    [i (inc i)]))

(defn is-reflection?
   "Expand the range to find if we have a reflected set of lines"
   [coll [start end]]
   (let [len (count coll)]
     (loop [i (dec start) 
            j (inc end)]
       (cond
         (< i 0) true
         (>= j len) true
         (not= (get coll i) (get coll j)) false
         :else (recur (dec i) (inc j))))))

(defn score
  [[r1 r2]]
  (cond 
    (not (nil? r1)) (* 100 (inc (first r1)))
    (not (nil? r2)) (inc (first r2))
    :else 0))

(defn find-reflections
  [coll]
  (let [h (find-candidates coll)
        v (find-candidates (transpose coll))]
    (vector (first (filter #(is-reflection? coll %) h))
            (first (filter #(is-reflection? (transpose coll) %) v)))))

(defn part1
  [f]
  (let [data (read-data f)
        patterns (map read-pattern data)]
    (->> patterns
         (map find-reflections)
         (map score)
         #_(apply +))))

;; The End
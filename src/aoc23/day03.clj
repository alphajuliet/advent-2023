(ns aoc23.day03
  (:require [clojure.edn :as edn]
            [aoc23.util :as util]))

(def testf "data/day03-test.txt")
(def inputf "data/day03-input.txt")

(defn find-integers
  "Given a row number and a string, it returns the start and end position of any numeric strings.
   For example: given '467..114..' it returns (['467' [0 0] [0 2]] ['114' [0 5][5 7]])"
  [row s]
  (let [matcher (re-matcher #"\d+" s)
        indices (atom [])]
    (while (.find matcher)
      (swap! indices conj [(.group matcher) [row (.start matcher)] [row (dec (.end matcher))]]))
    @indices))

(defn find-symbols
  "Given a string, it returns the match and the position of any of the non-numeric characters"
  [row s]
  (let [matcher (re-matcher #"[^\d\.]" s)
        indices (atom [])]
    (while (.find matcher)
      (swap! indices conj [(.group matcher) [row (.start matcher)]]))
    @indices))

(defn in-neighbourhood?
  "Is [r2 c2] in the immediate neighbourhood of [r1 c1]? This includes diagonal neighbours."
  [[r1 c1] [r2 c2]]
  (let [nn [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
        result (some #(= [r2 c2] (map + [r1 c1] %)) nn)]
    (if (nil? result) false true)))

(defn find-adjacent
  "Find the numbers that are adjacent to a symbol in the same, next or preceding line."
  [nums syms]
  (for [[_ sym-rc] syms
        [num num-start num-end] nums
        :when (or (in-neighbourhood? sym-rc num-start)
                  (in-neighbourhood? sym-rc num-end))]
    [sym-rc (edn/read-string num)]))

(defn part1
  [f]
  (let [data (util/import-data f)
        nums (->> data
                  (map-indexed find-integers)
                  (apply concat))
        syms (->> data
                  (map-indexed find-symbols)
                  (apply concat))]
    (->> (find-adjacent nums syms)
         (map edn/read-string)
         (apply +))))

(defn part2
  [f]
  (let [data (util/import-data f)
        nums (->> data
                  (map-indexed find-integers)
                  (apply concat))
        syms (->> data
                  (map-indexed find-symbols)
                  (apply concat)
                  (filter #(= "*" (first %))))]
    (->> (find-adjacent nums syms)
         (group-by first)
         vals
         (filter #(= 2 (count %)))
         (map #(map second %))
         (map #(apply * %))
         (apply +))))

;; The End
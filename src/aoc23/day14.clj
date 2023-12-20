(ns aoc23.day14 
  (:require [clojure.string :as str]
            [aoc23.util :as util]))

(def testf "data/day14-test.txt")
(def inputf "data/day14-input.txt")

(defn read-data
  [f]
  (-> f
      slurp
      str/split-lines)) 

(defn split-keep
  "Split a string on a given character but keep each match"
  [s ch]
  (->> s
       (partition-by #(= % ch))
       (mapv #(str/join "" %))))

(defn count-char 
"Count the number of times ch appears in s"
  [s ch]
  (->> s
       (filter #(= ch %))
       count))

(defn shift-right
  "Return a string of the same length but with all the O characters at the right-hand end"
  [s]
  (let [len (count s)
        n (count-char s \O)]
    (if (zero? n)
      s
    ;; else
      (str (str/join (repeat (- len n) \.))
           (str/join (repeat n \O))))))

(defn score-string
  "Score a string based on the position of the 'O' characters"
  [s]
  (let [len (count s)
        n (count-char s \O)
        a (- len (dec n))]
    (int (* (/ n 2)
        (+ (* 2 a) (dec n))))))

(defn tilt-right
  "Tilt the array to the right"
  [data]
  (->> data
       (map reverse)
       (map str/join)
       (mapv #(split-keep % \#))
       (util/mapmap shift-right)))

(defn score-row
  "Score the full row of strings"
  [row]
  (loop [score 0
         strs row
         posn 0]
    (if (empty? strs)
      score
      ;; else
      (let [s (first strs)
            n (count-char s \O)]
        (recur (+ score (* posn n) (score-string s))
               (next strs)
               (+ posn (count s)))))))

(defn part1
  [f]
  (let [data (read-data f)]
    (->> data
         util/T
         tilt-right
         (map score-row)
         (apply +))))

;; The End
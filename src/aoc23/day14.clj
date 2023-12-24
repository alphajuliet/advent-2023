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

(defn shift-right
  "Return a string of the same length but with all the O characters at the right-hand end"
  [s]
  (let [len (count s)
        n (util/count-if #(= \O %) s)]
    (if (zero? n)
      s
    ;; else
      (str (apply str (repeat (- len n) \.))
           (apply str (repeat n \O))))))

(defn tilt-right
  "Tilt the array to the right"
  [data]
  (->> data
       (mapv #(split-keep % \#))
       (util/mapmap shift-right)
       (map str/join)))

(defn score-string
  "Score a string based on the position of the 'O' characters"
  [s]
  (let [len (count s)
        n (util/count-if #(= \O %) s)
        a (- len (dec n))]
    (int (* (/ n 2)
            (+ (* 2 a) (dec n))))))

(defn score-row
  "Score the full row of strings"
  [row]
  (loop [score 0
         strings row
         posn 0]
    (if (empty? strings)
      score
      ;; else
      (let [s (first strings)
            n (util/count-if #(= \O %) s)]
        (recur (+ score (* posn n) (score-string s))
               (next strings)
               (+ posn (count s)))))))

(defn rotate-array
  "Rotate an array by 90º anti-clockwise"
  [arr]
  (->> arr
       util/T
       (map str/join)))

(defn spin-cycle
  [data]
  (reduce (fn [arr _] 
            (->> arr
                 rotate-array  
                 tilt-right))
          data
          (range 4)))

(defn part1
  [f]
  (let [data (read-data f)]
    (->> data
         util/T
         (map reverse)
         (map str/join)
         tilt-right
         (mapv #(split-keep % \#))
         (map score-row)
         (apply +))))

(defn part2
  [f]
  (let [data (read-data f)
        ncycles 100000]
    (->> (reduce (fn [d _] (spin-cycle d)) 
                 data 
                 (range ncycles))
         (mapv #(split-keep % \#))
         (map score-row))))

;; The End
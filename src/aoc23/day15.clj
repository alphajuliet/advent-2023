(ns aoc23.day15 
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [aoc23.util :as util]))

(def testf "data/day15-test.txt")
(def inputf "data/day15-input.txt")

(defn read-data
  [f]
  (-> f
      slurp
      (str/trim-newline)
      (str/split #",")))

(defn hash-alg
  [s]
  (let [ascii (map int s)]
    (reduce (fn [h a]
              (rem (* (+ h a) 17) 256))
            0
            ascii)))

(defn parse-step
  "Parse each step"
  [step]
  (let [[_ label op numstr] (first (re-seq #"^([a-z]+)(-|=)(\d*)" step))
        box (hash-alg label)
        focal-length (edn/read-string numstr)]
    [box op label focal-length]))

(defn remove-lens
"Remove the lens with the given label from the numbered box"
  [boxes box-num label]
  (if (util/coll-contains? (map first (nth boxes box-num)) label)
    (assoc boxes box-num (vec (remove #(= (first %) label) (nth boxes box-num))))
    ;; else do nothing
    boxes))

(defn insert-lens
  "Add the lens with the given label to the back of the box, or replace one that is already there"
  [boxes box-num label focal-length]
  (cond
    ;; If there is already a lens with the same label, replace it
    (util/coll-contains? (map first (nth boxes box-num)) label) 
        (assoc boxes box-num (vec (map #(if (= (first %) label) [label focal-length] %) (nth boxes box-num)))) 
    ;; If not there, then add it to the end
    :else (assoc boxes box-num (conj (nth boxes box-num) [label focal-length]))))

(defn process-step
  [boxes step]
  (let [[box op label focal-length] step]
    (cond
      (= op "-") (remove-lens boxes box label)
      (= op "=") (insert-lens boxes box label focal-length)
      :else boxes)))

(defn focusing-power
  "Calculate the focusing power of a box"
  [box-num lenses]
  (let []
    (apply + (map-indexed (fn [i [_ focal-length]] (* (inc box-num) (inc i) focal-length)) lenses))))

(defn part1
  [f]
  (let [data (read-data f)]
    (->> data
         (map hash-alg)
         (apply +))))

(defn part2
  [f]
  (let [data (map parse-step (read-data f))
        initial-boxes (vec (repeat 256 []))]
    (->> data
         (reduce process-step initial-boxes)
         (map-indexed focusing-power)
         (apply +))))

;; The End
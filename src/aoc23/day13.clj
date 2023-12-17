(ns aoc23.day13 
  (:require [clojure.string :as str]
            [aoc23.util :as util]
            [clojure.set :as set]))

(def testf "data/day13-test.txt")
(def test2f "data/day13-test2.txt")
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

(defn differ-by-one
  "Test if two strings in a collection are only one character different"
  [s1 s2]
  (let [pairs (map vector s1 s2)
        diffs (filter (fn [[ch1 ch2]] (not= ch1 ch2)) pairs)]
    (and (= (count diffs) 1) (= (count s1) (count s2)))))

(defn find-candidates
  "Find two consecutive items in the collection that are equal and return the indices"
  [coll part]
  (for [i (range 0 (count coll))
        :let [a (get coll i)
              b (get coll (inc i))]
        :when (or (= a b) 
                  (and (= :part2 part) 
                       (differ-by-one a b)))]
    [i (inc i)]))

(defn is-reflection?
   "Expand the range to find if we have a reflected set of lines"
   [coll [start end] part]
   (let [len (count coll)]
     (loop [i (dec start) 
            j (inc end)]
       (let [a (get coll i)
             b (get coll j)]
        (cond
         (or (< i 0) (>= j len)) true
         (and (= :part2 part) (differ-by-one a b)) true
         (not= a b) false
         :else (recur (dec i) (inc j)))))))

(defn score
  "Score the reflection result"
  [{:keys [h v]}]
  (let [hscore (reduce #(+ %1 (inc (first %2))) 0 h)
        vscore (reduce #(+ %1 (inc (first %2))) 0 v)]
    (+ (* 100 hscore) vscore)))

(defn find-reflections
  "Find the horizontal and vertical reflections"
  [coll part]
  (let [h (find-candidates coll part)
        v (find-candidates (transpose coll) part)]
    {:h (filterv #(is-reflection? coll % part) h)
     :v (filterv #(is-reflection? (transpose coll) % part) v)}))

(defn part1
  [f]
  (let [data (read-data f)
        patterns (map read-pattern data)]
    (->> patterns
         (map #(find-reflections % :part1))
         (map score)
         (apply +))))

(defn diff-pattern
  "Find the difference in found reflections."
  [p]
  (let [p1 (find-reflections p :part1)
        p2 (find-reflections p :part2)
        dh (vec (set/difference (set (:h p2)) (set (:h p1))))
        dv (if (empty? dh)
             (vec (set/difference (set (:v p2)) (set (:v p1))))
             [])]
    {:h dh :v dv}))

(defn part2
  [f]
  (let [data (read-data f)
        patterns (map read-pattern data)]
    (->> patterns
         (map diff-pattern)
         #_(map score)
         #_(apply +))))

;; part 2: 
;; 36518 is too high
;; 36504 is too high
;; 29988 is wrong

;; The End
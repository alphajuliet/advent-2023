(ns aoc23.day10
  (:require [aoc23.util :as util]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(def testf "data/day10-test.txt")
(def test2f "data/day10-test2.txt")
(def test3f "data/day10-test3.txt")
(def test4f "data/day10-test4.txt")
(def test5f "data/day10-test5.txt")
(def test6f "data/day10-test6.txt")
(def inputf "data/day10-input.txt")

(defn find-start-node
  "Find the coordinates of the starting node"
  [data]
  (for [c (range (count (first data)))
        r (range (count data))
        :when (= \S (get-in data [r c]))]
    [r c]))

(defn neighbors 
  "Return the neighbors of a given coordinate"
  [[x y]]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn in-bounds? 
  "Return true if the coordinate is in bounds"
  [grid [x y]]
  (and (< -1 x (count grid))
       (< -1 y (count (first grid)))))

(defn navigate-loop
  "Navigate the loop and update the position based on the character and the entry direction"
  [grid rc [dr dc]]
  (let [val (get-in grid rc)
        dir' (case val
               \F (if (zero? dr) [1 0] [0 1])
               \7 (if (zero? dr) [1 0] [0 -1])
               \J (if (zero? dr) [-1 0] [0 -1])
               \L (if (zero? dr) [-1 0] [0 1])
               \| [dr dc]
               \- [dr dc]
               \S [dr dc]
               nil)]
    {:rc (mapv + rc dir') :dir dir'}))

(defn find-first-step
  "Work out the initial direction of traversal"
  [[r c :as start]]
  (cond
    (= [2 0] start) {:rc [2 0] :dir [0 1]}
    (= [62 111] start) {:rc [62 111] :dir [-1 0]} ;; taken from the input data
    :else {:rc [r c] :dir [1 0]}))

(defn traverse-loop
  "Traverse the loop from the start position, counting the steps and all the visited positions."
  [grid]
  (let [start (first (find-start-node grid))]
    (set! *print-length* 5) ; help the debugger
    (loop [{:keys [rc dir]} (find-first-step start)
           visited [start]
           length 0]
      (let [state (navigate-loop grid rc dir)]
        (if (= (:rc state) start)
          {:length (inc length), :visited visited}
          ;; else
          (recur state (conj visited (:rc state)) (inc length)))))))

(defn flood-fill 
  "Flood fill the grid starting at the given coordinate"
  [grid loop-set [x y] visited]
  (if (or (not (in-bounds? grid [x y]))
          (contains? visited [x y])
          (contains? loop-set [x y]))
    visited
    (reduce (fn [acc neighbor]
              (flood-fill grid loop-set neighbor acc))
            (conj visited [x y])
            (neighbors [x y]))))

(defn enclosed-spaces 
  "Return the coordinates of all the spaces that are enclosed by the loop"
  [grid loop-set]
  (let [all-coords (for [x (range (count grid))
                         y (range (count (first grid)))]
                     [x y])
        outside (reduce (fn [acc [x y]]
                          (if (or (zero? x) (zero? y)
                                  (= x (dec (count grid)))
                                  (= y (dec (count (first grid)))))
                            (flood-fill grid loop-set [x y] acc)
                            acc))
                        #{}
                        all-coords)]
    (vec (sort (filter #(and (not (contains? outside %))
                             (not (contains? loop-set %)))
                       all-coords)))))

(defn part1
  [f]
  (let [data (util/import-data f)]
    (-> data
        traverse-loop
        (:length)
        (quot 2))))

(defn part2
  [f]
  (let [data (util/import-data f)]
    (->> data
         traverse-loop
         :visited
         (enclosed-spaces data))))

;; The End
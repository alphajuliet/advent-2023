(ns aoc23.day16
  (:require [aoc23.util :as util]
            [clojure.core.matrix :as m]
            [clojure.string :as str]))

(def testf "data/day16-test.txt")
(def inputf "data/day16-input")

(defn read-data
  "Read the data and convert to an ASCII matrix"
  [f]
  (->> f
       util/import-data
       (util/mapmap int)
       #_(map #(str/split % #""))))

(defn init-state
  []
  {:rc [0 0] 
   :dir [0 1]
   :energised []})

(defn- new-dir [v dc dr dir]
  "Determine the new direction"
  (case v
    ;; reverse slash mirror
    92 [dc dr]
    ;; forward slash mirror
    47 [(- dc) (- dr)]
    ;; vertical split
    124 (case dir
          [0 1] [1 0]
          [0 -1] [1 0]
          dir)
    ;; horizontal split
    45 (case dir
         [1 0] [0 1]
         [-1 0] [0 1]
         dir)
           ;; else a dot => no change
    dir))

(defn step
  "Take one step in the grid"
  [{:keys [rc dir] :as state} grid]
  (let [v (apply m/mget grid rc)
        [dr dc] dir
        dir' (new-dir v dc dr dir)]
    ;; Update the state
    (-> state
        (assoc :rc (mapv + rc dir'))
        (assoc :dir dir')
        (update :energised conj rc))))

(defn outside?
  "Check if outside the grid"
  [grid [r c]]
  (let [[maxr maxc] (m/shape grid)]
    (or (>= r maxr) (>= c maxc) (neg-int? r) (neg-int? c))))

(defn traverse
  "Traverse the grid from a given initial state until either it leaves the grid or re-visits
   a previous location."
  [state grid]
  (loop [st state]
    (if (or (outside? grid (:rc st))
            (util/coll-contains? (:energised st) (:rc st)))
      st
      ;; else
      (recur (step st grid)))))

(defn part1
  [f]
  (let [data (m/matrix (read-data f))]
    (traverse (init-state) data)))

;; The End
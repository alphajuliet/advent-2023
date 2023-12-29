(ns aoc23.day16
  (:require [aoc23.util :as util]
            [clojure.core.matrix :as m]
            [clojure.set :as set]))

(def testf "data/day16-test.txt")
(def inputf "data/day16-input.txt")

(defn read-data
  "Read the data and convert to an ASCII matrix"
  [f]
  (->> f
       util/import-data
       (util/mapmap int)
       #_(map #(str/split % #""))))

(defn outside?
  "Check if outside the grid"
  [[r c] grid]
  (let [[maxr maxc] (m/shape grid)]
    (or (>= r maxr) (>= c maxc) (neg-int? r) (neg-int? c))))

(defn init-state
  []
  {:beams [{:rc [0 0] 
            :dir [0 1] 
            :visited #{}
            :i 0
            :energised #{}}] ; Include a set for energised locations per beam
   :all-energised #{}}) ; Keep a set for all energised locations(defn init-state

(defn- new-dir
  "Based on the location, return the new direction(s)"
  [v [dr dc :as dir]]
  (case v
        ;; reverse slash mirror
    92 [[dc dr]]
        ;; forward slash mirror
    47 [[(- dc) (- dr)]]
        ;; vertical splitter
    124 (if (zero? dr)
          [[1 0] [-1 0]]
          [dir])
        ;; horizontal splitter
    45 (if (zero? dc)
         [[0 1] [0 -1]]
         [dir])
        ;; else a dot => no change
    [dir]))

(defn step
  "Take one step for the beam in the grid, updating the shared :energised state"
  [{:keys [rc dir visited i energised] :as beam} grid]
  (let [v (apply m/mget grid rc)
        dirs (new-dir v dir)]
    (map (fn [new-dir]
           (let [new-rc (mapv + rc new-dir)]
             {:beam (assoc beam :rc new-rc
                                :dir new-dir
                                :visited (conj visited [rc dir])
                                :i (inc i)
                                :energised (conj energised rc))})) ; Add to per-beam energised
         dirs)))

(defn traverse
  "Traverse the grid from given initial state until either all beams leave the grid 
   or we re-visit a previous path."
  [init-state grid]
  (loop [{:keys [beams all-energised]} init-state]
    (let [steps (mapcat (fn [beam]
                          (step beam grid))
                        beams)
          new-beams (map :beam steps)
          new-energised (mapcat :energised new-beams)
          active-beams (remove (fn [beam]
                                 (or (outside? (:rc beam) grid)
                                    ;;  (> (:i beam) 600)
                                    ;;  (contains? (:energised beam) (:rc beam))
                                     (contains? (:visited beam) [(:rc beam) (:dir beam)])))
                               new-beams)
          all-energised (set/union all-energised (set new-energised))] ; Collect all new energised locations
      ;; (println active-beams)
      (if (empty? active-beams)
        (sort all-energised) ; Return sorted for easier reading/verification
           ;; else
        (recur {:beams active-beams :all-energised all-energised}))))) ; Pass on updated all energised(defn traverse

(defn part1
  [f]
  (let [data (m/matrix (read-data f))]
    (->> data
         (traverse (init-state))
         count)))

;; The End
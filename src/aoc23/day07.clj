(ns aoc23.day07
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [aoc23.util :as util]))

(def testf "data/day07-test.txt")
(def inputf "data/day07-input.txt")
(def input2f "data/day07-input2.txt")

(defn read-data
  "Read in the hands and the bets"
  [f]
  (->> f
       util/import-data
       (map #(str/split % #"\s+"))
       (map #(vector (first %) (edn/read-string (second %))))))

(defn compare-label
  [a b]
  (let [labels [\A \K \Q \J \T \9 \8 \7 \6 \5 \4 \3 \2]
        i1 (.indexOf labels a)
        i2 (.indexOf labels b)]
    (compare i1 i2)))

(defn compare-type
  [a b]
  (let [types [:five :four :full-house :three :two-pairs :pair :high-card :nothing]
        i1 (.indexOf types a)
        i2 (.indexOf types b)]
    (compare i1 i2)))

(defn sort-cards
  "Sort the hand first by frequency and then by key"
  [hand]
  (->> hand
       (sort-by first compare-label)
       (sort-by second >)))

(defn classify-hand
  "Classify the hand into its type and cards"
  [hand]
  (let [freq (frequencies hand)
        type (case (sort > (vals freq))
               [5] :five
               [4 1] :four
               [3 2] :full-house
               [3 1 1] :three
               [2 2 1] :two-pairs
               [2 1 1 1] :pair
               [1 1 1 1 1] :high-card
               :error)]
    [type (sort-cards (vec freq))]))

(defn compare-hands
  [[[l1a _] [l1b _] [l1c _]] [[l2a _] [l2b _] [l2c _]]]
  (let [cmp (compare-label l1a l2a)]
    (if (zero? cmp)
      (let [cmp2 (compare-label l1b l2b)]
        (if (zero? cmp2)
          (compare-label l1c l2c)
          cmp2))
      cmp)))

(defn compare-hands3
  [hand1 hand2]
  (loop [[card1 & rest1] hand1
         [card2 & rest2] hand2]
    (cond
      (and (nil? card1) (nil? card2)) 0 ;; Both hands are empty, they are equal
      (nil? card1) -1 ;; Hand1 is shorter, it's less
      (nil? card2) 1 ;; Hand2 is shorter, it's less
      :else (let [cmp (compare-label (first card1) (first card2))]
              (if (zero? cmp)
                (recur rest1 rest2)
                cmp)))))

(defn sort-hands
  "Sort all the hands by type then by the cards"
  [hands]
  (->> hands
       (map (juxt (comp classify-hand first) second))
    ;;    (sort-by second <) ;; should not need this
       (sort-by (comp second first) compare-hands3)
       (sort-by (comp first first) compare-type)))

(defn part1
  [f]
  (let [data (read-data f)
        rank (range 1 (inc (count data)))]
    (->> data 
         sort-hands
         (map second)
         reverse
         (map * rank)
         (apply +))))

(comment 
  (assert (= 6440 (part1 testf))))

;; The End
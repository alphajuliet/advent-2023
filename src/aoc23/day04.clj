(ns aoc23.day04
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [instaparse.core :as insta]
            [aoc23.util :as util]
            [clojure.set :as set]))

(def testf "data/day04-test.txt")
(def inputf "data/day04-input.txt")

(def parse-cards
  (insta/parser
   "<cards> := (card <newline>)+
    card := card-id <':'> winning <' |'> chosen
    card-id := <'Card'> <space>+ number
    winning := (<space> number)+
    chosen := (<space> number)+
    number := #'\\d+'
    <space> := #'\\s+'
    newline := '\\n'"))

(defn transform-data
  [data]
  #_{:clj-kondo/ignore [:unresolved-var]}
  (insta/transform
   {:number edn/read-string
    :card #(apply conj %&)
    :card-id #(hash-map :id %)
    :winning #(hash-map :winning %&)
    :chosen #(hash-map :chosen %&)}
   data))

(defn score-card
  [{:keys [winning chosen]}]
  (let [found (set/intersection (set winning) (set chosen))]
    (int (Math/pow 2 (dec (count found))))))

(defn part1
  [f]
  (let [data (slurp f)]
    (->> data
         parse-cards
         transform-data
         (map score-card)
         (apply +))))

;; The End
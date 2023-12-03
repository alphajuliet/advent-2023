(ns aoc23.day03-test
  (:require
   [clojure.test :refer [deftest is]]
   [aoc23.day03 :as src]))

(deftest in-neighbourhood?-test
  (is (true? (src/in-neighbourhood? [1 3] [0 2])))
  (is (false? (src/in-neighbourhood? [1 3] [0 0])))
  )
(ns aoc23.day05-test
  (:require [clojure.test :refer [deftest is]]
            [aoc23.day05 :as src]))

(deftest apply-map-test
  (is (= 79 (src/apply-map [50 98 2] 79)))
  (is (= 100 (src/apply-map [50 98 2] 100)))
  (is (= 51 (src/apply-map [50 98 2] 99)))
  (is (= 81 (src/apply-map [52 50 48] 79)))
  (is (= 14 (src/apply-map [50 98 2] 14)))
  (is (= 14 (src/apply-map [49 53 8] 14)))
  (is (= 3 (src/apply-map [0 11 42] 14)))
  (is (= 45 (src/apply-map [42 0 7] 3)))
  (is (= 45 (src/apply-map [42 0 7] 3)))
)

(deftest apply-all-mappings-test
  (is (= 81 (src/apply-all-mappings [[50 98 2] [52 50 48]] 79)))
  (is (= 57 (src/apply-all-mappings [[50 98 2] [52 50 48]] 55)))
  (is (= 13 (src/apply-all-mappings [[50 98 2] [52 50 48]] 13)))

  (is (= 14 (src/apply-all-mappings [[50 98 2] [52 50 48]] 14)))
  (is (= 53 (src/apply-all-mappings [[0 15 37] [37 52 2] [39 0 15]] 14)))
  (is (= 49 (src/apply-all-mappings [[49 53 8] [0 11 42] [42 0 7] [57 7 4]] 53)))
  )
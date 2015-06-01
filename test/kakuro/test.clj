(ns kakuro.test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ct]
            [kakuro.core :refer :all]))

(def grid1 [
            [e (d 4) (d 22) e (d 16) (d 3)]
            [(a 3) v v (da 16 6) v v ]
            [(a 18) v v v v v]
            [e (da 17 23) v v v (d 14)]
            [(a 9) v v (a 6) v v]
            [(a 15) v v (a 12) v v]
           ])

(def grid2 [[e (d 23) (d 30) e e (d 27) (d 12) (d 16)]
            [(a 16) v v e (da 17 24) v v v]
            [(a 17) v v (da 15 29) v v v v]
            [(a 35) v v v v v (d 12) e]
            [e (a 7) v v (da 7 8) v v (d 7)]
            [e (d 11) (da 10 16) v v v v v]
            [(a 21) v v v v (a 5) v v]
            [(a 6) v v v e (a 3) v v]
            ])

(deftest test-grid1
  (let [result (-> grid1 solver draw-grid)
        expected "     3         9    \n"]
    (is (= expected (.substring result (- (count result) (count expected)))))))

(deftest test-grid2
  (let [result (-> grid2 solver draw-grid)
        expected "     2         1    \n"]
    (is (= expected (.substring result (- (count result) (count expected)))))))

(ct/defspec test-transpose
  100
  (prop/for-all [vv (gen/not-empty (gen/vector (gen/vector
                                                 gen/int (first (gen/sample (gen/choose 0 10) 1))) (first (gen/sample (gen/choose 0 10) 1))))]
    (= vv (-> vv transpose transpose))))


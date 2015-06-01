(ns kakuro.test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ct]
            [kakuro.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(ct/defspec first-element-is-min-after-sorting 
         100 ;; the number of iterations for test.check to test
         (prop/for-all [v (gen/not-empty (gen/vector gen/int))]
           (= (apply min v)
              (first (sort v)))))

(ct/defspec test-transpose
  100
  (prop/for-all [vv (gen/not-empty (gen/vector (gen/vector gen/int 5) 5))]
    (= vv (-> vv transpose transpose))))

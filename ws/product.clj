;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit alt+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns condemned-cove
  (:require [gorilla-plot.core :as plot]
            [kakuro.core :as k]))
;; @@

;; @@
(require '(kakuro [core :as k]) :reload)
;; @@

;; @@
(k/v)
;; @@

;; @@
(cons 1 [2])
;; @@

;; @@
(cons 1 '(2))
;; @@

;; @@
(defn product [colls]
  (condp = (count colls)
    0 []
    1 (map vector (first colls))
    (let [coll1 (first colls)
          _ (println (product (rest colls)))
          tail-prod (product (rest colls))]
      (mapcat (fn [x] (map #(cons x %) tail-prod)) coll1))))

(println (product [[1 2 3] [:a :b :c]]))
(println (product [[1 2 3] [:a] ["w" "x"]]))
(println (product [[1 2 3] [:a :b :c] ["w" "x"]]))
;; @@

;; @@
(k/permute-all [(k/v) (k/v) (k/v)] 6)
;; @@

;; @@
(k/permute-all2 [(k/v) (k/v) (k/v)] 6)
;; @@

;; @@
(require '[clojure.test :refer [run-tests]])
(require 'kakuro.test :reload-all)
(run-tests 'kakuro.test)
;; @@

;; @@
(get '(0 1 2 3 4) 3)
;; @@

;; @@

;; @@

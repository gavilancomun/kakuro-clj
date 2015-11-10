(ns kakuro.logic
  (:refer-clojure :exclude [== >= <= > < =])
  (:require [clojure.core :as core]
            [clojure.core.logic :as cl]
            [clojure.core.logic.fd :as fd]
            [clojure.pprint :as pp]
            [kakuro.core]))

(defn nary-plus
"total is a concrete integer on initial call, then an lvar on recursion."
  [total vars]
  (cond
    (core/= 1 (count vars)) (cl/== (first vars) total)
    (core/= 2 (count vars)) (fd/+ (first vars) (second vars) total)
    :else
    (cl/fresh [acc]
              (fd/+ (last vars) acc total)
              (nary-plus acc (butlast vars)))))

(defn lvar-row [row]
  (mapv #(vector % (when (:values %) (cl/lvar))) row))

(defn lvar-grid [grid]
  (mapv lvar-row grid))

(defn logic-pair [k [descs values]]
  (if (seq values)
    (let [vars (map second values)
          total (k (first (last descs)))]
      (cl/all
        (fd/distinct vars)
        (nary-plus total vars)))
    cl/succeed))

(defn logic-line 
"a line is a vector of pairs: a record followed by nil or an lvar."
  [line k]
  (let [grouped-values (partition-by #(:values (first %)) line)
        descs-values-pairs (partition-all 2 grouped-values)]
    (cl/everyg #(logic-pair k %) descs-values-pairs)))

(defn logic-row [line]
  (logic-line line :across))

(defn logic-column [line]
  (logic-line line :down))

(defn logic-grid [grid]
  (let [lgrid (lvar-grid grid)
        vars (->> lgrid flatten (filter cl/lvar?))
        var-grid (mapv #(vec (map second %)) lgrid)]
    (if (seq vars)
      (cl/run* [q]
               (cl/everyg #(fd/in % (apply fd/domain (range 1 10))) vars)
               (cl/everyg logic-row lgrid)
               (cl/everyg logic-column (kakuro.core/transpose lgrid))
               (cl/== q var-grid)))))


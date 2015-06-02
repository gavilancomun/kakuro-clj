(ns kakuro.logic
  (:refer-clojure :exclude [== >= <= > < =])
  (:require [clojure.core :as core]
            [clojure.core.logic :as cl]
            [clojure.core.logic.fd :as fd]))

;; total is a concrete integer on initial call, then an lvar on recursion.
(defn nary-plus [total vars]
  (cond
    (core/= 1 (count vars)) (cl/== (first vars) total)
    (core/= 2 (count vars)) (fd/+ (first vars) (second vars) total)
    :else
    (cl/fresh [acc]
              (fd/+ (last vars) acc total)
              (nary-plus acc (butlast vars)))))

(defn lvar-row [row]
  (mapv #(vector % (if (:values %) (cl/lvar) nil)) row))

(defn lvar-grid [grid]
  (mapv lvar-row grid))

(defn logic-pair [k [nvs vs]]
  (if (seq vs)
    (let [vars (map second vs)
          total (k (first (last nvs)))]
      (cl/all
        (fd/distinct vars)
        (nary-plus total vars)))
    cl/succeed))

(defn logic-line [line pair-solver]
  (let [pairs (partition-all 2 (partition-by #(:values (first %)) line))]
    (cl/everyg pair-solver pairs)))

(defn logic-row [row]
  (logic-line row #(logic-pair :across %)))

(defn logic-column [column]
  (logic-line column #(logic-pair :down %)))

(defn logic-grid [grid]
  (let [lgrid (lvar-grid grid)
        vars (->> lgrid flatten (filter cl/lvar?))
        var-grid (mapv #(into [] (filter cl/lvar? (mapv second %))) lgrid)]
    (if (seq vars)
      (cl/run* [q]
               (cl/everyg #(fd/in % (apply fd/domain (range 1 10))) vars)
               (cl/everyg logic-row lgrid)
               (cl/everyg logic-column (kakuro.core/transpose lgrid))
               (cl/== q var-grid)))))


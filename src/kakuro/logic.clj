(ns kakuro.logic
  (:refer-clojure :exclude [== >= <= > < =])
  (:require [clojure.core :as core])
  (:require [clojure.core.logic :as cl])
  (:require [clojure.core.logic.fd :as fd]))

;; total is a concrete integer on initial call, then an lvar on recursion.
(defn nary-plus [total vars]
  (cond
    (core/= 1 (count vars)) (cl/== (first vars) total)
    (core/= 2 (count vars)) (fd/+ (first vars) (second vars) total)
    :else
    (cl/fresh [acc]
              (fd/+ (last vars) acc total)
              (nary-plus acc (butlast vars)))))

(defn solve-sum-n [n total]
  (if (core/> n 0)
    (let [vars (repeatedly n cl/lvar)]
      (cl/run* [q]
               (cl/everyg #(fd/in % (apply fd/domain (range 1 10))) vars)
               (nary-plus total vars)
               (fd/distinct vars)      
               (cl/== q vars)))))

(defn solve-cells-vars [cells vars total]
  (let [n (count cells)]
    (if (core/> n 0)
        (cl/run* [q]
                 (cl/everyg #(fd/in (first %) (apply fd/domain (into (sorted-set) (:values (second %))))) (map vector vars cells))
                 (nary-plus total vars)
                 (fd/distinct vars)      
                 (cl/== q vars)))))

(defn solve-cells [cells total]
  (when (core/> (count cells) 0)
    (solve-cells-vars cells (repeatedly (count cells) cl/lvar) total)))

(defn solve-vars [vars total]
  (if (seq vars)
    (cl/run* [q]
             (cl/everyg #(fd/in % (apply fd/domain (range 1 10))) vars)
             (nary-plus total vars)
             (fd/distinct vars)      
             (cl/== q vars))))

(defn lvar-row [row]
  (mapv #(vector % (if (:values %) (cl/lvar) nil)) row))

(defn lvar-grid [grid]
  (mapv lvar-row grid))

(defn logic-pair [k [nvs vs]]
  (if (seq vs)
    (let [vars (map second vs)]
      (cl/all
        (fd/distinct vars)
        (nary-plus (k (first (last nvs))) vars)))
    cl/succeed))

(defn logic-line [line pair-solver]
  (let [pairs (partition-all 2 (partition-by #(core/= (type (first %)) (type kakuro.core/v)) line))]
    (cl/everyg pair-solver pairs)))

(defn logic-row [row]
  (logic-line row #(logic-pair :across %)))

(defn logic-column [column]
  (logic-line column #(logic-pair :down %)))

(defn logic-grid [grid]
  (let [lgrid (lvar-grid grid)
        vars (->> lgrid (mapcat identity) (map second) (filter #(not (nil? %))))]
    (if (seq vars)
      (cl/run* [q]
               (cl/everyg #(fd/in % (apply fd/domain (range 1 10))) vars)
               (cl/everyg logic-row lgrid)
               (cl/everyg logic-column (kakuro.core/transpose lgrid))
               (cl/== q vars)))))


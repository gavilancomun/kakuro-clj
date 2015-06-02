(ns kakuro.logic
  (:refer-clojure :exclude [== >= <= > < =])
  (:require [clojure.core :as core])
  (:require [clojure.core.logic :as cl])
  (:require [clojure.core.logic.fd :as fd]))

;; sum is a concrete integer on initial call, then an lvar on recursion.
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

(defn solve-cells [cells total]
  (let [n (count cells)]
    (if (core/> n 0)
      (let [vars (repeatedly n cl/lvar)]
        (cl/run* [q]
                 (cl/everyg #(fd/in (first %) (apply fd/domain (into (sorted-set) (:values (second %))))) (map vector vars cells))
                 (nary-plus total vars)
                 (fd/distinct vars)      
                 (cl/== q vars))))))


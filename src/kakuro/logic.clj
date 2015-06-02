(ns kakuro.logic
  (:refer-clojure :exclude [== >= <= > < =])
  (:require [clojure.core :as core])
  (:require [clojure.core.logic :as cl])
  (:require [clojure.core.logic.fd :as fd]))

(defn solve-sum [sum]
  (cl/run* [q]
           (cl/fresh [a b]
                     (cl/everyg #(fd/in % (fd/domain 1 2 3 4 5 6 7 8 9)) [a b])
                     (fd/+ a b sum)
                     (fd/distinct [a b])
                     (cl/== q [a b]))))

(defn nary-plus [sum vars]
  (cond
    (core/= 1 (count vars)) (cl/== (first vars) sum)
    (core/= 2 (count vars)) (fd/+ (first vars) (second vars) sum)
    :else
    (cl/fresh [acc]
              (fd/+ (last vars) acc sum)
              (nary-plus acc (butlast vars)))))

(defn solve-sum-n [n sum]
  (if (core/> n 0)
    (let [vars (repeatedly n cl/lvar)]
      (cl/run* [q]
               (cl/everyg #(fd/in % (apply fd/domain (range 1 10))) vars)
               (nary-plus sum vars)
               (fd/distinct vars)      
               (cl/== q vars)))))


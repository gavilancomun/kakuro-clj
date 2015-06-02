(ns kakuro.logic
  (:refer-clojure :exclude [== >= <= > < =])
  (:require [clojure.core.logic.fd :as fd])
  (:require [clojure.core.logic.arithmetic :as cla])
  (:require [clojure.core.logic :as cl]))

(defn solve-sum [sum]
  (cl/run* [q]
    (cl/fresh [a b]
      (cl/everyg #(fd/in % (fd/domain 1 2 3 4 5 6 7 8 9)) [a b])
      (fd/+ a b sum)
      (fd/distinct [a b])
      (cl/== q [a b]))))

(defn nary-plus [vars sum]
  )

(defn solve-sum-n [n sum]
  (let [vars (repeatedly n cl/lvar)
        accs (repeatedly (dec n) cl/lvar)]
    (cl/run* [q]
      (cl/everyg #(fd/in % (apply fd/domain (range 1 (inc sum)))) accs)
      (cl/everyg #(fd/in % (apply fd/domain (range 1 4))) vars)
      (fd/distinct vars)      
      (cl/== q vars))))


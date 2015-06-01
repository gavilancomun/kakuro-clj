(ns kakuro.logic
  (:refer-clojure :exclude [== >= <= > < =])
  (:require [clojure.core.logic.fd :as fd])
  (:use clojure.core.logic 
        clojure.core.logic.arithmetic))

(defn solve-sum [sum]
  (run* [q]
    (fresh [a b]
      (everyg #(fd/in % (fd/domain 1 2 3 4 5 6 7 8 9)) [a b])
      (fd/+ a b sum)
      (== q [a b]))))


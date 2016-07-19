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
(ns gavilan.clojure
  (:require [clojure.pprint :as pp])
  (:require [gorilla-plot.core :as plot]))
;; @@

;; @@
(count {:a 1 :b 2})
;; @@

;; @@
(count #{:a :b})
;; @@

;; @@
(first [])
;; @@

;; @@
(first nil)
;; @@

;; @@
(def next-states {:satisfied #{}
                  :cancelled #{}
                  :booked #{:cancelled :satisfied :required}
                  :required #{:booked :cancelled}})


(defn valid-next? [item next-state]
  (println next-state (next-states next-state))
  (and item
       (-> next-states
           ((:state item))
           next-state)))

(def item {:state :required})

(valid-next? item :booked)


;; @@
;; ->
;;; :booked #{:satisfied :cancelled :required}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-keyword'>:booked</span>","value":":booked"}
;; <=

;; @@
(-> next-states :required)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-set'>#{</span>","close":"<span class='clj-set'>}</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:cancelled</span>","value":":cancelled"},{"type":"html","content":"<span class='clj-keyword'>:booked</span>","value":":booked"}],"value":"#{:cancelled :booked}"}
;; <=

;; @@
(-> (next-states :required) :booked)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-keyword'>:booked</span>","value":":booked"}
;; <=

;; @@
(-> #{:a :b} :a)

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-keyword'>:a</span>","value":":a"}
;; <=

;; @@

;; @@

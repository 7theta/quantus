(ns ^:dev/always quantus.example.core
  (:require [quantus.core :refer [Quantity] :as qc]
            [quantus.angles :as qa]
            [quantus.math :as qm]))

;; npx shadow-cljs watch dev

(def log (.-log js/console))

(defn ^:export init
  []
  (enable-console-print!))

(defn ^:dev/after-load start []
  (let [a (qa/degrees 134)
        b (qa/radians 3.03)]
    #_(log "a" (str (qa/->degrees (qm/+ a b)))
           (str (qa/->degrees (qm/- a b)))
           (str (qm// 3.0 2.0))
           (.abs js/Math -2.0)
           (qm/sin a))
    (let [a (qc/meters 10)
          a2 (qc/meters 10)
          b (qc/feet 10)]
      #_(log "b" a b (qc/unit-type a) (qc/value a) (qm/+ a b)
             (= a a2)
             (instance? Quantity a)
             (= (qc/unit-type a) (qc/unit-type a2)) (= (qc/value a) (qc/value a2)))
      (log "C" a b))))

(ns ^:dev/always quantus.example.core
  (:require [quantus.core :refer [Quantity] :as q]
            [quantus.math :as qm]
            [quantus.angles :as qa]
            [quantus.coordinates :as qc]
            [quantus.transit]
            [cognitect.transit :as transit]))

;; npx shadow-cljs watch dev

(def log (.-log js/console))

(defn ^:export init
  []
  (enable-console-print!))

(defn ^:dev/after-load start []
  (let [a (qa/degrees 134)
        b (q/kelvin 3.03)
        c (qc/xy 2 3)
        d (q/meters (qc/xy 3 4))]
    #_(log "Testing" a b c d)
    #_(log "A" (hash (q/meters 10)) (hash (q/meters 10))
           (hash (q/meters 11)) (hash (q/meters 11))
           (hash [:length 11]) (.-hash [:length 11]))
    (log "B" (hash (qc/xy 10 10)) (hash (qc/xy 10 10))
         (hash [10 10]) (hash [10 10])
         (hash (q/meters (qc/xy 10 10))) (hash (q/meters (qc/xy 10 10))))
    ))

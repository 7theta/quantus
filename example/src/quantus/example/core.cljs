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
        b (qa/radians 3.03)]
    #_(log "a" (str (qa/->degrees (qm/+ a b)))
           (str (qa/->degrees (qm/- a b)))
           (str (qm// 3.0 2.0))
           (.abs js/Math -2.0)
           (qm/sin a))
    (let [a (q/meters 10)
          a2 (q/meters 10)
          b (q/feet 10)
          c (qc/xy 1 2)]
      #_(log "b" a b (q/unit-type a) (q/value a) (qm/+ a b)
             (= a a2)
             (instance? Quantity a)
             (= (q/unit-type a) (q/unit-type a2)) (= (q/value a) (q/value a2)))
      #_(log "C" a b)
      #_(log "DD" ;;c (qc/x c)
             (= (q/meters c) (q/meters c))
             (type 1)
             (qm/* c 2)
             (qc/magnitude c)
             #_(q/feet c)
             #_(= (q/feet c) (q/feet c)))
      (let [write-handlers {:handlers (:write quantus.transit/handlers)}
            read-handlers {:handlers (:read quantus.transit/handlers)}]
        (log "E" #_(qc/into-xy [1 2])
             #_(transit/write (transit/writer :json write-handlers) (q/feet c))
             (transit/read (transit/reader :json read-handlers) (transit/write (transit/writer :json write-handlers) (q/feet c)))))
      #_(log "F" (= (q/feet c) (q/feet c))))))

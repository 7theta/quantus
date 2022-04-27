(ns ^:dev/always quantus.example.core
  (:require [quantus.angles :as qa]
            [quantus.math :as qm]))

;; npx shadow-cljs watch dev

(def log (.-log js/console))

(defn ^:export init
  []
  (enable-console-print!))

(defn ^:dev/after-load start []
  (let [a (qa/degrees 134)
        b (qa/radians 3.03)]
    (log "a" (str (qa/->degrees (qm/+ a b)))
         (str (qa/->degrees (qm/- a b)))
         (str (qm// 3.0 2.0))
         (.abs js/Math -2.0)
         (qm/sin a))))

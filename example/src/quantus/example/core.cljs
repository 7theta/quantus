(ns ^:dev/always quantus.example.core
  ;;(:require [react-dom])
  (:require [quantus.angles :as qa]
            [quantus.math :as qm])
  )

(def log (.-log js/console))

(defn ^:export init
  []
  (enable-console-print!))

(defn ^:dev/after-load start []
  (let [a (qa/degrees 134)
        b (qa/radians 3.03)]
    (log "a" (str (qa/to-degrees (qm/+ a b)))
         (str (qa/to-degrees (qm/- a b)))
         (str (qm// 3.0 2.0))
         (.abs js/Math -2.0)
         (qm/sin a)
         (qa/to-degrees (qa/radians (qm/atan2 30 30))))))

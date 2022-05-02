(ns quantus.transit
  (:require [quantus.core #?@(:cljs [:refer [Quantity]]) :as q]
            [quantus.angles #?@(:cljs [:refer [AngleQuantity]]) :as qa]
            [cognitect.transit :as transit])
  #?(:clj (:import [quantus.core Quantity]
                   [quantus.angles AngleQuantity])))

(def handlers {:read {"quantity/time"        (transit/read-handler quantus.core/seconds)
                      "quantity/length"      (transit/read-handler quantus.core/meters)
                      "quantity/angle"       (transit/read-handler quantus.angles/radians)
                      "quantity/mass"        (transit/read-handler quantus.core/kilograms)
                      "quantity/speed"       (transit/read-handler quantus.core/meters-per-second)
                      "quantity/temperature" (transit/read-handler quantus.core/kelvin)
                      "quantity/unitless"    (transit/read-handler quantus.core/unitless)}
               :write {Quantity (transit/write-handler
                                 (fn [^Quantity q] (str "quantity/" (name (q/get-unit-type q))))
                                 (fn [^Quantity q] (q/get-value q)))
                       AngleQuantity (transit/write-handler
                                      (constantly "quantity/angle")
                                      (fn [^AngleQuantity q] (q/get-value q)))}})

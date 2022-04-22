(ns quantus.transit
  (:require [quantus.core #?@(:cljs [:refer [Quantity]]) :as q]
            [quantus.angles #?@(:cljs [:refer [AngleQuantity]]) :as qa]
            [cognitect.transit :as transit])
  #?(:clj (:import [quantus.core Quantity]
                   [quantus.angles AngleQuantity])))

(def handlers {:read {"quantity/time"        (transit/read-handler quantus.core/parse-time)
                      "quantity/length"      (transit/read-handler quantus.core/parse-length)
                      "quantity/angle"       (transit/read-handler quantus.angles/parse-angle)
                      "quantity/mass"        (transit/read-handler quantus.core/parse-mass)
                      "quantity/speed"       (transit/read-handler quantus.core/parse-speed)
                      "quantity/temperature" (transit/read-handler quantus.core/parse-temperature)
                      "quantity/unitless"    (transit/read-handler quantus.core/parse-unitless)}
               :write {Quantity (transit/write-handler
                                 (fn [q] (str "quantity/" (name (:unit-type q))))
                                 (fn [q] (:value q)))
                       AngleQuantity (transit/write-handler
                                      (constantly "quantity/angle")
                                      (fn [q] (:value q)))}})

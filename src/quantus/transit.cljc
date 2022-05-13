(ns quantus.transit
  (:require [quantus.core #?@(:cljs [:refer [Quantity]]) :as q]
            [quantus.angles #?@(:cljs [:refer [AngleQuantity]]) :as qa]
            [quantus.coordinates #?@(:cljs [:refer [CoordinateXY CoordinateXYZ]]) :as qc]
            [cognitect.transit :as transit])
  #?(:clj (:import [quantus.core Quantity]
                   [quantus.angles AngleQuantity]
                   [quantus.coordinates CoordinateXY CoordinateXYZ])))

(def handlers {:read {"quantity/time"           (transit/read-handler quantus.core/seconds)
                      "quantity/length"         (transit/read-handler quantus.core/meters)
                      "quantity/angle"          (transit/read-handler quantus.angles/radians)
                      "quantity/mass"           (transit/read-handler quantus.core/kilograms)
                      "quantity/speed"          (transit/read-handler quantus.core/meters-per-second)
                      "quantity/temperature"    (transit/read-handler quantus.core/kelvin)
                      "quantity/unitless"       (transit/read-handler quantus.core/unitless)
                      "quantity/coordinate-xy" (transit/read-handler quantus.coordinates/into-xy)
                      "quantity/coordinate-xyz" (transit/read-handler quantus.coordinates/into-xyz)}
               :write {Quantity (transit/write-handler
                                 (fn [^Quantity q] (str "quantity/" (name (q/unit-type q))))
                                 (fn [^Quantity q] (q/value q)))
                       AngleQuantity (transit/write-handler
                                      (constantly "quantity/angle")
                                      (fn [^AngleQuantity q] (q/value q)))
                       CoordinateXY (transit/write-handler
                                     (constantly "quantity/coordinate-xy")
                                     (fn [^CoordinateXY c] [(.getX c) (.getY c)]))
                       CoordinateXYZ (transit/write-handler
                                      (constantly "quantity/coordinate-xyz")
                                      (fn [^CoordinateXYZ c] [(.getX c) (.getY c) (.getZ c)]))}})

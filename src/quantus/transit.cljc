(ns quantus.transit
  (:require [quantus.core #?@(:cljs [:refer [Quantity]]) :as q]
            [quantus.angles #?@(:cljs [:refer [AngleQuantity]]) :as qa]
            [quantus.coordinates #?@(:cljs [:refer [CoordinateXY CoordinateXYZ]]) :as qc]
            [cognitect.transit :as transit])
  #?(:clj (:import [quantus.core Quantity]
                   [quantus.angles AngleQuantity]
                   [quantus.coordinates CoordinateXY CoordinateXYZ])))

(def handlers {:read {"quantity/time"           (transit/read-handler (comp quantus.core/seconds first))
                      "quantity/length"         (transit/read-handler (comp quantus.core/meters first))
                      "quantity/angle"          (transit/read-handler (comp quantus.angles/radians first))
                      "quantity/mass"           (transit/read-handler (comp quantus.core/kilograms first))
                      "quantity/speed"          (transit/read-handler (comp quantus.core/meters-per-second first))
                      "quantity/temperature"    (transit/read-handler (comp quantus.core/kelvin first))
                      "quantity/unitless"       (transit/read-handler (comp quantus.core/unitless first))
                      "quantity/coordinate-xy"  (transit/read-handler quantus.coordinates/into-xy)
                      "quantity/coordinate-xyz" (transit/read-handler quantus.coordinates/into-xyz)}
               :write {Quantity (transit/write-handler
                                 (fn [^Quantity q] (str "quantity/" (name (q/unit-type q))))
                                 (fn [^Quantity q] [(q/value q)]))
                       AngleQuantity (transit/write-handler
                                      (constantly "quantity/angle")
                                      (fn [^AngleQuantity q] [(q/value q)]))
                       CoordinateXY (transit/write-handler
                                     (constantly "quantity/coordinate-xy")
                                     (fn [^CoordinateXY c] [(qc/x c) (qc/y c)]))
                       CoordinateXYZ (transit/write-handler
                                      (constantly "quantity/coordinate-xyz")
                                      (fn [^CoordinateXYZ c] [(qc/x c) (qc/y c) (qc/z c)]))}})

(ns quantus.transit
  (:require [quantus.core #?@(:cljs [:refer [Quantity]]) :as q]
            [quantus.angles #?@(:cljs [:refer [AngleQuantity]]) :as qa]
            [quantus.coordinates #?@(:cljs [:refer [CoordinateXY CoordinateXYZ]]) :as qc]
            [cognitect.transit :as transit])
  #?(:clj (:import [quantus.core Quantity]
                   [quantus.angles AngleQuantity]
                   [quantus.coordinates CoordinateXY CoordinateXYZ])))

(def handlers {:read {"quantity.length/meters"           (transit/read-handler (comp quantus.core/meters first))
                      "quantity.area/meters-squared"     (transit/read-handler (comp quantus.core/meters-squared first))
                      "quantity.time/seconds"            (transit/read-handler (comp quantus.core/seconds first))
                      "quantity.speed/meters-per-second" (transit/read-handler (comp quantus.core/meters-per-second first))
                      "quantity.acceleration/meters-per-second-squared" (transit/read-handler (comp quantus.core/meters-per-second-squared first))
                      "quantity.mass/kilograms"          (transit/read-handler (comp quantus.core/kilograms first))
                      "quantity.temperature/kelvin"      (transit/read-handler (comp quantus.core/kelvin first))
                      "quantity.unitless/unitless"       (transit/read-handler (comp quantus.core/unitless first))
                      "quantity.angle/radians"           (transit/read-handler (comp quantus.angles/radians first))
                      "coordinate/xy"           (transit/read-handler quantus.coordinates/into-xy)
                      "coordinate/xyz"          (transit/read-handler quantus.coordinates/into-xyz)}
               :write {Quantity (transit/write-handler
                                 (fn [^Quantity q] (str "quantity." (name (q/unit-type q)) "/" (name (q/si-units (q/unit-type q)))))
                                 (fn [^Quantity q] [(q/value q)]))
                       AngleQuantity (transit/write-handler
                                      (constantly "quantity.angle/radians")
                                      (fn [^AngleQuantity q] [(q/value q)]))
                       CoordinateXY (transit/write-handler
                                     (constantly "coordinate/xy")
                                     (fn [^CoordinateXY c] [(qc/x c) (qc/y c)]))
                       CoordinateXYZ (transit/write-handler
                                      (constantly "coordinate/xyz")
                                      (fn [^CoordinateXYZ c] [(qc/x c) (qc/y c) (qc/z c)]))}})

;;   Copyright (c) 7theta. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   MIT License (https://opensource.org/licenses/MIT) which can also be
;;   found in the LICENSE file at the root of this distribution.
;;
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any others, from this software.

(ns quantus.angles
  (:require [quantus.core #?@(:cljs [:refer [Quantity]]) :as q]
            [quantus.math :as qm]
            [clojure.core :as core]
            [clojure.pprint])
  (:refer-clojure :exclude [+ -])
  #?(:clj (:import [quantus.core Quantity])))

(def pi #?(:clj java.lang.Math/PI :cljs js/Math.PI))
(def two-pi (* 2 pi))
(def three-pi (* 3 pi))

(def radians-per-degree (/ pi 180))
(defn degrees->radians [degrees] (* degrees radians-per-degree))
(defn radians->degrees [radians] (/ radians radians-per-degree))

(deftype AngleQuantity [value-field]
  Object
  (toString [^AngleQuantity this]
    (str "#quantity/radians " value-field))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [self q]
    (or (identical? self q)
        (and (instance? AngleQuantity q)
             (= value-field (q/value q)))))

  q/QuantityProtocol
  (unit-type [_] :angle)
  (value [_] value-field))

#?(:clj (defmethod print-method AngleQuantity [^AngleQuantity q ^java.io.Writer w]
          (.write w (.toString q))))

#?(:clj (. clojure.pprint/simple-dispatch addMethod AngleQuantity #(print-method % *out*)))

(defn parse-angle
  [value]
  (AngleQuantity. value))

(defn assert-angle-quantity
  [aq]
  (when-not (instance? AngleQuantity aq)
    (throw (ex-info "Angle Quantity expected" {:aq aq}))))

(defn degrees [v] (AngleQuantity. (mod (degrees->radians v) two-pi)))
(defn ->degrees [^AngleQuantity aq] (assert-angle-quantity aq) (radians->degrees (q/value aq)))

(defn radians [v] (AngleQuantity. (mod v two-pi)))
(defn ->radians [^AngleQuantity aq] (assert-angle-quantity aq) (q/value aq))

(defn -
  "Shortest angular distance between `a` and `b`"
  ([a] (core/- a))
  ([a b] (-> (core/- a b)
             (mod two-pi)
             (core/+ three-pi)
             (mod two-pi)
             (core/- pi))))

(defmethod qm/- [AngleQuantity AngleQuantity]
  [^AngleQuantity a ^AngleQuantity b]
  (- a b))

(defn +
  [a b]
  (mod (core/+ a b) two-pi))

(defmethod qm/+ [AngleQuantity AngleQuantity]
  [^AngleQuantity a ^AngleQuantity b]
  (AngleQuantity. (+ (q/value a) (q/value b))))

(defmulti sin type)
#?(:clj (defmethod sin Number [x] (clojure.math/sin x))
   :cljs (defmethod sin js/Number [x] (js/Math.sin x)))

(defmethod sin AngleQuantity
  [^AngleQuantity a]
  (sin (q/value a)))

(defmulti cos type)
#?(:clj (defmethod cos Number [x] (clojure.math/cos x))
   :cljs (defmethod cos js/Number [x] (js/Math.cos x)))

(defmethod cos AngleQuantity
  [^AngleQuantity a]
  (cos (q/value a)))

(defmulti tan type)
#?(:clj (defmethod tan Number [x] (clojure.math/tan x))
   :cljs (defmethod tan js/Number [x] (js/Math.tan x)))

(defmethod tan AngleQuantity
  [^AngleQuantity a]
  (tan (q/value a)))

(defmulti atan2 (fn [y x] [(type y) (type x)]))
#?(:clj (defmethod atan2 [Number Number] [y x] (clojure.math/atan2 y x))
   :cljs (defmethod atan2 [js/Number js/Number] [y x] (js/Math.atan2 y x)))

(defmethod atan2 [Quantity Quantity]
  [^Quantity y ^Quantity x]
  (q/assert-unit-type-match y x)
  (AngleQuantity. (atan2 (q/value y) (q/value x))))

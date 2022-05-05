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

(deftype AngleQuantity [value]
  Object
  (toString [^AngleQuantity this]
    (str "#quantity/radians " value ))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [self q]
    (or (identical? self q)
        (and (instance? AngleQuantity q)
             (= value (q/value q)))))

  q/QuantityProtocol
  (unit-type [_] :angle)
  (value [_] value))

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

(defmethod qm/sin AngleQuantity
  [^AngleQuantity a]
  (qm/sin (:value a)))

(defmethod qm/cos AngleQuantity
  [^AngleQuantity a]
  (qm/cos (:value a)))

(defmethod qm/tan AngleQuantity
  [^AngleQuantity a]
  (qm/tan (:value a)))

(defmethod qm/atan2 [Quantity Quantity]
  [^Quantity y ^Quantity x]
  (q/assert-unit-type-match y x)
  (AngleQuantity. (qm/atan2 (q/value y) (q/value x))))

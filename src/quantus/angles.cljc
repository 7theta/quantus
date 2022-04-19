;;   Copyright (c) 7theta. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   MIT License (https://opensource.org/licenses/MIT) which can also be
;;   found in the LICENSE file at the root of this distribution.
;;
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any others, from this software.

(ns quantus.angles
  (:require [quantus.math :as qm]
            [clojure.core :as core]
            #?(:clj [clojure.math]))
  ;;#?(:clj (:import [quantus.core Quantity]))
  (:refer-clojure :exclude [+ -]))

(def pi #?(:clj java.lang.Math/PI :cljs js/Math.PI))
(def two-pi (* 2 pi))
(def three-pi (* 3 pi))

(def radians-per-degree (/ pi 180))
(defn degrees->radians [degrees] (* degrees radians-per-degree))
(defn radians->degrees [radians] (/ radians radians-per-degree))

(defrecord AngleQuantity [value]
  Object
  (toString [^AngleQuantity this]
    (str "#quantity/angle [" value "]")))

(defn assert-angle-quantity
  [aq]
  (when-not (instance? AngleQuantity aq)
    (throw (ex-info "Angle Quantity expected" {:aq aq}))))

(defn degrees [v] (->AngleQuantity (mod (degrees->radians v) two-pi)))
(defn to-degrees [^AngleQuantity aq] (assert-angle-quantity aq) (radians->degrees (:value aq)))

(defn radians [v] (->AngleQuantity (mod v two-pi)))
(defn to-radians [^AngleQuantity aq] (assert-angle-quantity aq) (:value aq))

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
  (+ a b))

#_(defn add-degrees
    ([degrees other-degrees] (add-degrees degrees other-degrees :clockwise))
    ([degrees other-degrees cw-or-ccw]
     (assert (#{:ccw :counter-clockwise :left :cw :clockwise :right} cw-or-ccw))
     (let [other-degrees (if (#{:ccw :counter-clockwise :left} cw-or-ccw)
                           (- other-degrees)
                           other-degrees)]
       (-> degrees
           (+ other-degrees)
           (mod 360)))))

#_(defn calculate-turn
    [dir-current dir-new left-or-right]
    (assert (#{:left :right} left-or-right))
    (let [turn (if (= :right left-or-right)
                 (- dir-new dir-current)
                 (- dir-current dir-new))]
      (if (neg? turn)
        (+ turn 360)
        turn)))

#_(defn subtract-degrees
    ;;https://aviation.stackexchange.com/questions/47540/how-do-you-find-the-difference-in-degrees-between-two-headings
    ;;LH turn: [origin hdg] - [destination hdg] (if less than 0, add 360)
    ;;RH turn: [destination hdg] - [origin hdg] (if less than 0, add 360)
    [degrees other-degrees]
    (let [left (calculate-turn degrees other-degrees :left)
          right (calculate-turn degrees other-degrees :right)]
      (if (< left right)
        left
        (- right))))

(defmethod qm/+ [AngleQuantity AngleQuantity]
  [^AngleQuantity a ^AngleQuantity b]
  (AngleQuantity. (+ (:value a) (:value b))))

(defmethod qm/- [AngleQuantity AngleQuantity]
  [^AngleQuantity a ^AngleQuantity b]
  (AngleQuantity. (- (:value a) (:value b))))

(defmethod qm/sin AngleQuantity
  [^AngleQuantity a]
  (qm/sin (:value a)))

(defmethod qm/cos AngleQuantity
  [^AngleQuantity a]
  (qm/cos (:value a)))

(defmethod qm/tan AngleQuantity
  [^AngleQuantity a]
  (qm/tan (:value a)))

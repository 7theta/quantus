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
            #_[clojure.math :as clj-math])
  ;;#?(:clj (:import [quantus.core Quantity]))
  (:refer-clojure :exclude [+ -]))

(def pi #?(:clj java.lang.Math/PI :cljs js/Math.PI))
(def two-pi (* 2 pi))
(def three-pi (* 3 pi))

;; (defn ->rad
;;   [^Quantity d]
;;   (cond-> d
;;     (= :deg (:unit ^Unit (:unit d)))
;;     (#(->Quantity :angle (/ (* (:value %) pi) 180) (->Unit :rad 1)))))

;; (defn ->deg
;;   [^Quantity r]
;;   (cond-> r
;;     (= :rad (:unit ^Unit (:unit r)))
;;     (#(->Quantity :angle (/ (* (:value %) 180) pi) (->Unit :deg 1)))))

(defn -
  "Shortest angular distance between `a` and `b`"
  [a b]
  (-> (clojure.core/- a b) (mod two-pi) (clojure.core/+ three-pi) (mod two-pi) (clojure.core/- pi)))

(defn +
  [a b]
  (mod (clojure.core/+ a b) two-pi))

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

(defrecord AngleQuantity [value]
  Object
  (toString [^AngleQuantity this]
    (str "#quantiy/angle [" value "]")))

(defn assert-angle-quantity
  [aq]
  (when-not (instance? AngleQuantity aq)
    (throw (ex-info "Angle Quantity expected" {:aq aq}))))

(defn degrees [v] (->AngleQuantity (clj-math/to-radians v)))
(defn to-degrees [^AngleQuantity aq] (assert (instance? AngleQuantity aq)) (clj-math/to-degrees (:value aq)))

(defn radians [v] (->AngleQuantity v))
(defn to-radians [^AngleQuantity aq] (assert (instance? AngleQuantity aq)) (:value aq))

(defmethod qm/+ [AngleQuantity AngleQuantity]
  [^AngleQuantity a ^AngleQuantity b]
  (assert-angle-quantity a)
  (assert-angle-quantity b)
  (AngleQuantity. (+ (:value a) (:value b))))

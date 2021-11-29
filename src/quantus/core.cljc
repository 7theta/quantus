;;   Copyright (c) 7theta. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   MIT License (https://opensource.org/licenses/MIT) which can also be
;;   found in the LICENSE file at the root of this distribution.
;;
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any others, from this software.

(ns quantus.core
  (:require [quantus.math :as qm]
            [utilis.fn :refer [apply-kw]]
            [clojure.string :as st]))

(defrecord Unit [prefix unit exponent]
  Object
  (toString [^Unit this]
    (if (or (and prefix (> prefix 1)) (and exponent (> exponent 1)))
      (str "[" (st/join " " (remove nil? [prefix unit exponent])) "]")
      (str unit))))

#?(:clj (defmethod print-method Unit [^Unit u ^java.io.Writer w]
          (.write w (.toString u))))

#?(:clj (. clojure.pprint/simple-dispatch addMethod Unit #(print-method % *out*)))

(defn ->Unit
  ([unit] (->Unit unit 1))
  ([unit exponent] (->Unit nil unit exponent))
  ([prefix unit exponent] (Unit. prefix unit exponent)))

(defrecord Quantity [type value ^Unit unit]
  Object
  (toString [^Quantity this]
    (str "#quantity/" (name type) " [" value " " unit "]")))

#?(:clj (defmethod print-method Quantity [^Quantity q ^java.io.Writer w]
          (.write w (.toString q))))

#?(:clj (. clojure.pprint/simple-dispatch addMethod Quantity #(print-method % *out*)))

(defn ->Quantity
  [type value unit]
  (Quantity. type value unit))

(declare parse-quantity)

(defn parse-time [q] (parse-quantity :time q))
(defn parse-length [q] (parse-quantity :length q))
(defn parse-mass [q] (parse-quantity :mass q))
(defn parse-speed [q] (parse-quantity :speed q))
(defn parse-angle [q] (parse-quantity :angle q))
(defn parse-temperature [q] (parse-quantity :temperature q))

(defn unit-match?
  [^Quantity a ^Quantity b & {:keys [match-exponent] :or {match-exponent true}}]
  (let [^Unit a-unit (:unit a)
        ^Unit b-unit (:unit b)]
    (and (= (:type a) (:type b))
         (= (:unit a-unit) (:unit b-unit))
         (if match-exponent (= (:exponent a-unit) (:exponent b-unit)) true))))

(defn assert-unit-match
  [^Quantity a ^Quantity b & opts]
  (when-not (apply-kw unit-match? a b opts)
    (throw (ex-info "Quantities must have compatible type and units" {:a a :b b}))))

(defmethod qm/+ [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-match a b)
  (Quantity. (:type a) (+ (:value a) (:value b)) (:unit a)))

(defmethod qm/- [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-match a b)
  (Quantity. (:type a) (- (:value a) (:value b)) (:unit a)))

(defmethod qm/* [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-match a b :match-exponent false)
  (let [^Unit unit-a (:unit a)
        ^Unit unit-b (:unit b)]
    (Quantity. (:type a) (* (:value a) (:value b))
               (->Unit (:unit unit-a) (+ (:exponent unit-a) (:exponent unit-b))))))

(defmethod qm/* [#?(:clj java.lang.Number :cljs js/Number) Quantity]
  [a ^Quantity b]
  (Quantity. (:type b) (* a (:value b)) (:unit b)))

(defmethod qm/* [Quantity #?(:clj java.lang.Number :cljs js/Number)]
  [^Quantity a b]
  (Quantity. (:type a) (* (:value a) b) (:unit a)))

(defmethod qm// [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-match a b :match-exponent false)
  (let [^Unit unit-a (:unit a)
        ^Unit unit-b (:unit b)]
    (Quantity. (:type a) (/ (:value a) (:value b))
               (->Unit (:unit unit-a) (- (:exponent unit-a) (:exponent unit-b))))))

(defmethod qm// [#?(:clj java.lang.Number :cljs js/Number) Quantity]
  [a ^Quantity b]
  (Quantity. (:type b) (/ a (:value b)) (:unit b)))

(defmethod qm// [Quantity #?(:clj java.lang.Number :cljs js/Number)]
  [^Quantity a b]
  (Quantity. (:type a) (/ (:value a) b) (:unit a)))

(defmethod qm/abs Quantity
  [^Quantity a]
  (Quantity. (:type a) (qm/abs (:value a)) (:unit a)))

(defmethod qm/round Quantity
  [^Quantity a]
  (Quantity. (:type a) (qm/round (:value a)) (:unit a)))

;; ;; (defmethod gm/ceil Quantity
;; ;;   [^Quantity a]
;; ;;   (Quantity. (:type a) (Math/ceil (:value a)) (:unit a)))

;; ;; (defmethod gm/floor Quantity
;; ;;   [^Quantity a]
;; ;;   (Quantity. (:type a) (Math/floor (:value a)) (:unit a)))


;; ;; (defmethod gm/pow [Quantity #?(:clj java.lang.Number :cljs js/Number)]
;; ;;   [^Quantity a n]
;; ;;   (Quantity. (:type a) (Math/pow (:value a) n)
;; ;;              (let [^Unit unit (:unit a)]
;; ;;                (->Unit (:unit unit) (* n (:exponent unit))))))

;; ;; (defmethod gm/sqrt Quantity
;; ;;   [^Quantity a]
;; ;;   (Quantity. (:type a) (Math/sqrt (:value a))
;; ;;              (let [^Unit unit (:unit a)]
;; ;;                (->Unit (:unit unit) (/ (:exponent unit) 2)))))

(defmethod qm/zero? Quantity
  [^Quantity a]
  (zero? (:value a)))

(defmethod qm/pos? Quantity
  [^Quantity a]
  (pos? (:value a)))

(defmethod qm/neg? Quantity
  [^Quantity a]
  (neg? (:value a)))

(defmethod qm/> [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-match a b)
  (qm/> (:value a) (:value b)))

(defmethod qm/< [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-match a b)
  (qm/< (:value a) (:value b)))

(defmethod qm/>= [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-match a b)
  (qm/>= (:value a) (:value b)))

(defmethod qm/<= [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-match a b)
  (qm/<= (:value a) (:value b)))


;;; Private

(def ^:private quantity-prefixes
  {:yotta 1E24
   :zetta 1E21
   :exa   1E18
   :peta  1E15
   :tera  1E12
   :giga  1E9
   :mega  1E6
   :kilo  1E3
   :hecto 1E2
   :deca  1E1
   :deka  1E1

   :deci  1E-1
   :centi 1E-2
   :milli 1E-3
   :micro 1E-6
   :nano  1E-9
   :pico  1E-12
   :femto 1E-15
   :atto  1E-18
   :zepto 1E-21
   :yocto 1E-24

   :kibi 2E10
   :mebi 2E20
   :gibi 2E30
   :tebi 2E40
   :pebi 2E50
   :exbi 2E60})

(defn- prefix?
  [p]
  (boolean (get quantity-prefixes p)))

(defn- parse-unit
  [u]
  (cond
    (keyword? u) (->Unit u)
    (= 2 (count u)) (if (prefix? (first u))
                      (->Unit (first u) (second u) 1)
                      (->Unit (first u) (second u)))
    (= 3 (count u)) (apply ->Unit u)))

(defn- parse-quantity
  [type [value unit]]
  (->Quantity type value (parse-unit unit)))

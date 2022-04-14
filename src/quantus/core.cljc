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
            [quantus.units :as u]
            [utilis.fn :refer [apply-kw]]
            [clojure.string :as st]))

;; (defrecord Unit [prefix unit exponent]
;;   Object
;;   (toString [^Unit this]
;;     (if (or (and prefix (> prefix 1)) (and exponent (> exponent 1)))
;;       (str "[" (st/join " " (remove nil? [prefix unit exponent])) "]")
;;       (str unit))))

;; #?(:clj (defmethod print-method Unit [^Unit u ^java.io.Writer w]
;;           (.write w (.toString u))))

;; #?(:clj (. clojure.pprint/simple-dispatch addMethod Unit #(print-method % *out*)))

;; (defn ->Unit
;;   ([unit] (->Unit unit 1))
;;   ([unit exponent] (->Unit nil unit exponent))
;;   ([prefix unit exponent] (Unit. prefix unit exponent)))

(defrecord Quantity [value ^clojure.lang.Keyword unit-type]
  Object
  (toString [^Quantity this]
    (str "#quantity/" (name unit-type) " [" value "]")))

#?(:clj (defmethod print-method Quantity [^Quantity q ^java.io.Writer w]
          (.write w (.toString q))))

#?(:clj (. clojure.pprint/simple-dispatch addMethod Quantity #(print-method % *out*)))

#_(defn ->Quantity
    [value unit-type]
    (Quantity. value unit-type))

(declare parse-quantity)

(defn parse-time [q] (parse-quantity :time q))
(defn parse-length [q] (parse-quantity :length q))
(defn parse-mass [q] (parse-quantity :mass q))
(defn parse-speed [q] (parse-quantity :speed q))
(defn parse-angle [q] (parse-quantity :angle q))
(defn parse-temperature [q] (parse-quantity :temperature q))

(defn unit-type-match?
  [^Quantity a ^Quantity b]
  (= (:unit-type a) (:unit-type b)))

(defn assert-unit-type-match
  [^Quantity a ^Quantity b]
  (when-not (unit-type-match? a b)
    (throw (ex-info "Quantities must have the same unit type." {:a a :b b}))))

(defn assert-unit-type
  [^Quantity a unit-type]
  (when-not (= (:unit-type a) unit-type)
    (throw (ex-info "The provided quantity is not compatible with the target unit type." {:quantity a :expected-unit-type unit-type}))))

(defn meters [v] (->Quantity v :length))
(defn to-meters [^Quantity q] (assert-unit-type q :length) (:value q))

(defn feet [v] (->Quantity (u/feet->meters v) :length))
(defn to-feet [^Quantity q] (assert-unit-type q :length) (u/meters->feet (:value q)))

(defn mps [v] (->Quantity v :speed))
(defn to-mps [^Quantity q] (assert-unit-type q :speed) (:value q))

(defn kn [v] (->Quantity (u/kn->mps v) :speed))
(defn to-kn [^Quantity q] (assert-unit-type q :speed) (u/mps->kn (:value q)))

(defn fpm [v] (->Quantity (u/fpm->mps v) :speed))
(defn to-fpm [^Quantity q] (assert-unit-type q :speed) (u/mps->fpm (:value q)))

(defn kg [v] (->Quantity v :mass))
(defn to-kg [^Quantity q] (assert-unit-type q :mass) (:value q))

(defn lb [v] (->Quantity (u/lb->kg v) :mass))
(defn to-lb [^Quantity q] (assert-unit-type q :mass) (u/kg->lb (:value q)))

(defn seconds [v] (->Quantity v :time))
(defn to-seconds [^Quantity q] (assert-unit-type q :time) (:value q))

(defn minutes [v] (->Quantity (u/minutes->seconds v) :time))
(defn to-minutes [^Quantity q] (assert-unit-type q :time) (u/seconds->minutes (:value q)))

(defn hours [v] (->Quantity (u/hours->seconds v) :time))
(defn to-hours [^Quantity q] (assert-unit-type q :time) (u/seconds->hours (:value q)))

(defn unitless [v] (->Quantity v :unitless))
(defn to-number [^Quantity q] (assert-unit-type q :unitless) (:value q))

(defmethod qm/+ [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-type-match a b)
  (Quantity. (+ (:value a) (:value b)) (:unit-type a)))

(defmethod qm/- [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-type-match a b)
  (Quantity. (- (:value a) (:value b)) (:unit-type a)))

(defmethod qm/* [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (if-let [new-unit-type (get-in u/unit [:multiplications [(:unit-type a) (:unit-type b)]])]
    (Quantity. (* (:value a) (:value b)) new-unit-type)
    (throw (ex-info "Multiplying two Quantities must result in a known unit-type" {:a a :b b}))))

(defmethod qm/* [#?(:clj java.lang.Number :cljs js/Number) Quantity]
  [a ^Quantity b]
  (Quantity. (* a (:value b)) (:unit-type b)))

(defmethod qm/* [Quantity #?(:clj java.lang.Number :cljs js/Number)]
  [^Quantity a b]
  (Quantity. (* (:value a) b) (:unit-type a)))

(defmethod qm// [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (if-let [new-unit-type (get-in u/unit [:divisions [(:unit-type a) (:unit-type b)]])]
    (Quantity. (/ (:value a) (:value b)) new-unit-type)
    (throw (ex-info "Multiplying two Quantities must result in a known unit-type" {:a a :b b}))))

#_(defmethod qm// [#?(:clj java.lang.Number :cljs js/Number) Quantity]
    [a ^Quantity b]
    (Quantity. (:type b) (/ a (:value b)) (:unit b)))

(defmethod qm// [Quantity #?(:clj java.lang.Number :cljs js/Number)]
  [^Quantity a b]
  (Quantity. (/ (:value a) b) (:unit-type a)))

(defmethod qm/abs Quantity
  [^Quantity a]
  (Quantity. (qm/abs (:value a)) (:unit-type a)))

#_(defmethod qm/round Quantity
    [^Quantity a]
    (Quantity. (qm/round (:value a)) (:unit-type a)))

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
  (assert-unit-type-match a b)
  (qm/> (:value a) (:value b)))

(defmethod qm/< [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-type-match a b)
  (qm/< (:value a) (:value b)))

(defmethod qm/>= [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-type-match a b)
  (qm/>= (:value a) (:value b)))

(defmethod qm/<= [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-type-match a b)
  (qm/<= (:value a) (:value b)))


;;; Private

#_(def ^:private quantity-prefixes
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

#_(defn- prefix?
    [p]
    (boolean (get quantity-prefixes p)))

#_(defn- parse-unit
    [u]
    (cond
      (keyword? u) (->Unit u)
      (= 2 (count u)) (if (prefix? (first u))
                        (->Unit (first u) (second u) 1)
                        (->Unit (first u) (second u)))
      (= 3 (count u)) (apply ->Unit u)))

(defn- parse-quantity
  [unit-type [value]]
  (->Quantity value unit-type))

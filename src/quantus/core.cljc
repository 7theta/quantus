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
            [quantus.angles :as qa]
            [quantus.units :as u]
            [utilis.fn :refer [apply-kw]]
            [clojure.string :as st]
            [clojure.set]))

(declare allowed-operations)

(defrecord Quantity [value ^clojure.lang.Keyword unit-type]
  Object
  (toString [^Quantity this]
    (str "#quantity/" (name unit-type) " " value)))

#?(:clj (defmethod print-method Quantity [^Quantity q ^java.io.Writer w]
          (.write w (.toString q))))

#?(:clj (. clojure.pprint/simple-dispatch addMethod Quantity #(print-method % *out*)))

(defn ->Quantity
  [value unit-type]
  (Quantity. value unit-type))

(defn parse-time [q] (->Quantity q :time))
(defn parse-length [q] (->Quantity q :length))
(defn parse-mass [q] (->Quantity q :mass))
(defn parse-speed [q] (->Quantity q :speed))
(defn parse-temperature [q] (->Quantity q :temperature))
(defn parse-unitless [q] (->Quantity q :unitless))

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

(defn kelvin [v] (->Quantity v :temperature))
(defn to-kelvin [^Quantity q] (assert-unit-type q :temperature) (:value q))

(defn celsius [v] (->Quantity (u/celsius->kelvin v) :temperature))
(defn to-celsius [^Quantity q] (assert-unit-type q :temperature) (u/kelvin->celsius (:value q)))

(defn rankine [v] (->Quantity (u/rankine->kelvin v) :temperature))
(defn to-rankine [^Quantity q] (assert-unit-type q :temperature) (u/kelvin->rankine (:value q)))

(defn fahrenheit [v] (->Quantity (u/fahrenheit->kelvin v) :temperature))
(defn to-fahrenheit [^Quantity q] (assert-unit-type q :temperature) (u/kelvin->fahrenheit (:value q)))

(defn unitless [v] (->Quantity v :unitless))
(defn to-unitless [^Quantity q] (assert-unit-type q :unitless) (:value q))

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
  (if-let [new-unit-type (get-in allowed-operations [:multiplications [(:unit-type a) (:unit-type b)]])]
    (Quantity. (* (:value a) (:value b)) new-unit-type)
    (throw (ex-info "Multiplying two Quantities must result in a known unit-type" {:a a :b b}))))

(defmethod qm/* [#?(:clj java.lang.Number :cljs js/Number) Quantity]
  [a ^Quantity b]
  (Quantity. (* a (:value b)) (:unit-type b)))

(defmethod qm/* [Quantity #?(:clj java.lang.Number :cljs js/Number)]
  [^Quantity a b]
  (Quantity. (* (:value a) b) (:unit-type a)))

(defmethod qm/divide [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (if-let [new-unit-type (get-in allowed-operations [:divisions [(:unit-type a) (:unit-type b)]])]
    (Quantity. (qm/divide (:value a) (:value b)) new-unit-type)
    (throw (ex-info "Dividing two Quantities must result in a known unit-type" {:a a :b b}))))

(defmethod qm/divide [Quantity #?(:clj java.lang.Number :cljs js/Number)]
  [^Quantity a b]
  (Quantity. (qm/divide (:value a) b) (:unit-type a)))

(defmethod qm/abs Quantity
  [^Quantity a]
  (Quantity. (qm/abs (:value a)) (:unit-type a)))

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

(defmethod qm/atan2 [Quantity Quantity]
  [^Quantity y ^Quantity x]
  (assert-unit-type-match y x)
  (qa/->AngleQuantity (qm/atan2 (:value y) (:value x))))

;;; Private
(defn- multiplication
  [combos new-combo unit-1 unit-2]
  (-> combos
      (update :types conj new-combo unit-1 unit-2)
      (update :multiplications assoc [unit-1 unit-2] new-combo)
      (update :multiplications assoc [unit-2 unit-1] new-combo)
      (update :divisions assoc [new-combo unit-1] unit-2)
      (update :divisions assoc [new-combo unit-2] unit-1)))

(defn- division
  [combos new-combo numerator-unit denominator-unit]
  (-> combos
      (update :types conj new-combo numerator-unit denominator-unit)
      (update :divisions assoc [numerator-unit denominator-unit] new-combo)
      (update :divisions assoc [numerator-unit new-combo] denominator-unit)
      (update :multiplications assoc [new-combo denominator-unit] numerator-unit)
      (update :multiplications assoc [denominator-unit new-combo] numerator-unit)))

(def ^:private reserved-types #{:angle})

(def allowed-operations
  (-> {:multiplications {} :divisions {} :types #{}}
      (multiplication :area :length :length)
      (division :speed :length :time)
      (division :acceleration :speed :time)
      ((fn add-unitless-operations
         [{:keys [types] :as u}]
         (reduce (fn [uu t]
                   (multiplication uu t t :unitless))
                 u
                 types)))
      (division :frequency :unitless :time)
      ((fn check-for-reserved-unit-types
         [{:keys [types] :as u}]
         (when-let [used-reserved-types (seq (clojure.set/intersection types reserved-types))]
           (throw (ex-info "A reserved type was used." {:used-reserved-types used-reserved-types})))
         u))))

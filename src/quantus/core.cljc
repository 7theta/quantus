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
            [clojure.string :as st]
            [clojure.set]
            [clojure.pprint]))

(declare allowed-operations si-units)

(defprotocol QuantityProtocol
  (unit-type [this])
  (value [this]))

(deftype Quantity [value-field ^clojure.lang.Keyword unit-type-field]
  Object
  (toString [^Quantity this]
    (str "#quantity." (name unit-type-field) "/" (:name (si-units unit-type-field)) " " (pr-str value-field)))
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (hash [unit-type-field value-field]))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [self q]
    (or (identical? self q)
        (and (instance? Quantity q)
             (= unit-type-field (unit-type q))
             (= value-field (value q)))))

  QuantityProtocol
  (unit-type [_] unit-type-field)
  (value [_] value-field)

  #?@(:clj
      [clojure.lang.ILookup
       (valAt [q i]
              (Quantity. (.valAt ^clojure.lang.ILookup value-field i) unit-type-field))
       (valAt [q i not-found]
              (Quantity. (.valAt ^clojure.lang.ILookup value-field i not-found) unit-type-field))]))

#?(:clj (defmethod print-method Quantity [^Quantity q ^java.io.Writer w]
          (.write w (.toString q)))
   :cljs (extend-protocol IPrintWithWriter
           quantus.core.Quantity
           (-pr-writer [obj writer _]
             (write-all writer "#quantity." (name (unit-type obj)) "/" (:name (si-units (unit-type obj))) " " (pr-str (value obj))))))

(defn unit-type-match?
  [^Quantity a ^Quantity b]
  (= (unit-type a) (unit-type b)))

(defn assert-unit-type-match
  [^Quantity a ^Quantity b]
  (when-not (unit-type-match? a b)
    (throw (ex-info "Quantities must have the same unit type." {:a a :b b}))))

(defn assert-unit-type
  [^Quantity a type]
  (when-not (= (unit-type a) type)
    (throw (ex-info "The provided quantity is not compatible with the target unit type."
                    {:quantity a :expected-unit-type type}))))

#?(:clj
   (defn round-maybe
     [v]
     (if (number? v)
       (let [r (double (clojure.math/round v))]
         (if (or (= (clojure.math/next-down v) r)
                 (= (clojure.math/next-up v) r))
           r
           v))
       v))
   :cljs
   (defn round-maybe
     [v]
     (if (number? v)
       (let [r (Math/round v)]
         (if (< (/ (qm/abs (- r v))
                   (/ (+ (qm/abs r) (qm/abs v)) 2.0))
                1E-10)
           r
           v))
       v)))

(defmethod qm/zero-value Quantity
  [^Quantity quantity]
  (Quantity. 0 (unit-type quantity)))

(defn kilometers [v] (Quantity. (u/kilometers->meters v) :length))
(defn ->kilometers [^Quantity q] (assert-unit-type q :length) (round-maybe (u/meters->kilometers (value q))))

(defn meters [v] (qm/* (Quantity. v :length) 1))
(defn ->meters [^Quantity q] (assert-unit-type q :length) (round-maybe (value q)))

(defn centimeters [v] (Quantity. (u/centimeters->meters v) :length))
(defn ->centimeters [^Quantity q] (assert-unit-type q :length) (round-maybe (u/meters->centimeters (value q))))

(defn feet [v] (Quantity. (u/feet->meters v) :length))
(defn ->feet [^Quantity q] (assert-unit-type q :length) (round-maybe (u/meters->feet (value q))))

(defn inches [v] (Quantity. (-> v u/inches->centimeters u/centimeters->meters) :length))
(defn ->inches [^Quantity q] (assert-unit-type q :length) (round-maybe (-> q value u/meters->centimeters u/centimeters->inches)))

(defn meters-squared [v] (qm/* (Quantity. v :area) 1))
(defn ->meters-squared [^Quantity q] (assert-unit-type q :area) (round-maybe (value q)))

(defn centimeters-squared [v] (Quantity. (u/centimeters-squared->meters-squared v) :area))
(defn ->centimeters-squared [^Quantity q] (assert-unit-type q :area) (round-maybe (u/meters-squared->centimeters-squared (value q))))

(defn inches-squared [v] (Quantity. (u/inches-squared->meters-squared v) :area))
(defn ->inches-squared [^Quantity q] (assert-unit-type q :area) (round-maybe (u/meters-squared->inches-squared (value q))))

(defn seconds [v] (qm/* (Quantity. v :time) 1))
(defn ->seconds [^Quantity q] (assert-unit-type q :time) (round-maybe (value q)))

(defn minutes [v] (Quantity. (u/minutes->seconds v) :time))
(defn ->minutes [^Quantity q] (assert-unit-type q :time) (round-maybe (u/seconds->minutes (value q))))

(defn hours [v] (Quantity. (u/hours->seconds v) :time))
(defn ->hours [^Quantity q] (assert-unit-type q :time) (round-maybe (u/seconds->hours (value q))))

(defn meters-per-second [v] (qm/* (Quantity. v :speed) 1))
(defn ->meters-per-second [^Quantity q] (assert-unit-type q :speed) (round-maybe (value q)))

(defn kilometers-per-hour [v] (Quantity. (u/kilometers-per-hour->meters-per-second v) :speed))
(defn ->kilometers-per-hour [^Quantity q] (assert-unit-type q :speed) (round-maybe (u/meters-per-second->kilometers-per-hour (value q))))

(defn knots [v] (Quantity. (u/knots->meters-per-second v) :speed))
(defn ->knots [^Quantity q] (assert-unit-type q :speed) (round-maybe (u/meters-per-second->knots (value q))))

(defn feet-per-minute [v] (Quantity. (u/feet-per-minute->meters-per-second v) :speed))
(defn ->feet-per-minute [^Quantity q] (assert-unit-type q :speed) (round-maybe (u/meters-per-second->feet-per-minute (value q))))

(defn meters-per-second-squared [v] (qm/* (Quantity. v :acceleration) 1))
(defn ->meters-per-second-squared [^Quantity q] (assert-unit-type q :acceleration) (round-maybe (value q)))

(defn kilograms [v] (qm/* (Quantity. v :mass) 1))
(defn ->kilograms [^Quantity q] (assert-unit-type q :mass) (round-maybe (value q)))

(defn grams [v] (Quantity. (u/grams->kilograms v) :mass))
(defn ->grams [^Quantity q] (assert-unit-type q :mass) (round-maybe (u/kilograms->grams (value q))))

(defn pounds [v] (Quantity. (u/pounds->kilograms v) :mass))
(defn ->pounds [^Quantity q] (assert-unit-type q :mass) (round-maybe (u/kilograms->pounds (value q))))

(defn ounces [v] (Quantity. (u/ounces->kilograms v) :mass))
(defn ->ounces [^Quantity q] (assert-unit-type q :mass) (round-maybe (u/kilograms->ounces (value q))))

(defn grains [v] (Quantity. (u/grains->kilograms v) :mass))
(defn ->grains [^Quantity q] (assert-unit-type q :mass) (round-maybe (u/kilograms->grains (value q))))

(defn kelvin [v] (qm/* (Quantity. v :temperature) 1))
(defn ->kelvin [^Quantity q] (assert-unit-type q :temperature) (round-maybe (value q)))

(defn celsius [v] (Quantity. (u/celsius->kelvin v) :temperature))
(defn ->celsius [^Quantity q] (assert-unit-type q :temperature) (round-maybe (u/kelvin->celsius (value q))))

(defn rankine [v] (Quantity. (u/rankine->kelvin v) :temperature))
(defn ->rankine [^Quantity q] (assert-unit-type q :temperature) (round-maybe (u/kelvin->rankine (value q))))

(defn fahrenheit [v] (Quantity. (u/fahrenheit->kelvin v) :temperature))
(defn ->fahrenheit [^Quantity q] (assert-unit-type q :temperature) (round-maybe (u/kelvin->fahrenheit (value q))))

(defn unitless [v] (qm/* (Quantity. v :unitless) 1))
(defn ->unitless [^Quantity q] (assert-unit-type q :unitless) (round-maybe (value q)))

(defmethod qm/+ [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-type-match a b)
  (Quantity. (qm/+ (value a) (value b))
             (unit-type a)))

(defmethod qm/- [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-type-match a b)
  (Quantity. (qm/- (value a) (value b)) (unit-type a)))

(defmethod qm/* [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (if-let [new-unit-type (get-in allowed-operations [:multiplications [(unit-type a) (unit-type b)]])]
    (Quantity. (qm/* (value a) (value b)) new-unit-type)
    (throw (ex-info "Multiplying two Quantities must result in a known unit-type" {:a a :b b}))))

(defmethod qm/* [:quantus/number Quantity]
  [a ^Quantity b]
  (Quantity. (qm/* a (value b)) (unit-type b)))

(defmethod qm/* [Quantity :quantus/number]
  [^Quantity a b]
  (Quantity. (qm/* (value a) b) (unit-type a)))

(defmethod qm/divide [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (if-let [new-unit-type (get-in allowed-operations [:divisions [(unit-type a) (unit-type b)]])]
    (Quantity. (qm/divide (value a) (value b)) new-unit-type)
    (throw (ex-info "Dividing two Quantities must result in a known unit-type" {:a a :b b}))))

(defmethod qm/divide [Quantity :quantus/number]
  [^Quantity a b]
  (Quantity. (qm/divide (value a) b) (unit-type a)))

(defmethod qm/divide [:quantus/number Quantity]
  [#?@(:clj [^java.lang.Number a] :cljs [^js/Number a]) ^Quantity b]
  (if-let [new-unit-type (get-in allowed-operations [:divisions [:unitless (unit-type b)]])]
    (Quantity. (qm/divide a (value b)) new-unit-type)
    (throw (ex-info "Dividing a number by a Quantity must result in a known unit-type" {:a a :b b}))))

(defmethod qm/abs Quantity
  [^Quantity a]
  (Quantity. (qm/abs (value a)) (unit-type a)))

;; ;; (defmethod gm/pow [Quantity :quantus/number]
;; ;;   [^Quantity a n]
;; ;;   (Quantity. (:type a) (Math/pow (value a) n)
;; ;;              (let [^Unit unit (:unit a)]
;; ;;                (->Unit (:unit unit) (* n (:exponent unit))))))

;; ;; (defmethod gm/sqrt Quantity
;; ;;   [^Quantity a]
;; ;;   (Quantity. (:type a) (Math/sqrt (value a))
;; ;;              (let [^Unit unit (:unit a)]
;; ;;                (->Unit (:unit unit) (/ (:exponent unit) 2)))))

(defmethod qm/zero? Quantity
  [^Quantity a]
  (zero? (value a)))

(defmethod qm/pos? Quantity
  [^Quantity a]
  (pos? (value a)))

(defmethod qm/neg? Quantity
  [^Quantity a]
  (neg? (value a)))

(defmethod qm/> [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-type-match a b)
  (qm/> (value a) (value b)))

(defmethod qm/< [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-type-match a b)
  (qm/< (value a) (value b)))

(defmethod qm/>= [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-type-match a b)
  (qm/>= (value a) (value b)))

(defmethod qm/<= [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-type-match a b)
  (qm/<= (value a) (value b)))


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
  (-> {:multiplications {} :divisions {} :types #{:mass :temperature :unitless}}
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
      (division :inverse-length :unitless :length)
      (multiplication :speed :length :frequency)
      ((fn check-for-reserved-unit-types
         [{:keys [types] :as u}]
         (when-let [used-reserved-types (seq (clojure.set/intersection types reserved-types))]
           (throw (ex-info "A reserved type was used." {:used-reserved-types used-reserved-types})))
         u))))

(def si-units
  {:length {:fn meters :name "meters"}
   :area {:fn meters-squared :name "meters-squared"}
   :time {:fn seconds :name "seconds"}
   :speed {:fn meters-per-second :name "meters-per-second"}
   :acceleration {:fn meters-per-second-squared :name "meters-per-second-squared"}
   :mass {:fn kilograms :name "kilograms"}
   :temperature {:fn kelvin :name "kelvin"}
   :unitless {:fn unitless :name "unitless"}})

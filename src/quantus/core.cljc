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
  (get-unit-type [this])
  (get-value [this]))

(deftype Quantity [value ^clojure.lang.Keyword unit-type]
  Object
  (toString [^Quantity this]
    (str "#quantity/" (si-units unit-type) " " value))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [self q]
    (or (identical? self q)
        (and (instance? Quantity q)
             (= unit-type (get-unit-type q))
             (= value (get-value q)))))

  QuantityProtocol
  (get-unit-type [_] unit-type)
  (get-value [_] value)

  ;; #?(:cljs
  ;;    IPrintWithWriter)
  ;; #?(:cljs (-pr-writer [obj writer _]
  ;;                      (write-all writer "#quantity/" (name (.. obj -unit-type)) " " (.. obj -value))))
  )

#?(:clj (defmethod print-method Quantity [^Quantity q ^java.io.Writer w]
          (.write w (.toString q)))
   ;; :cljs (extend-protocol IPrintWithWriter
   ;;             quantus.core.Quantity
   ;;             (-pr-writer [obj writer _]
   ;;               (write-all writer "#quantity/" (name (get-unit-type obj)) " " (get-value obj))))
   )

#?(:clj (. clojure.pprint/simple-dispatch addMethod Quantity #(print-method % *out*)))

;; (defn parse-time [q] (Quantity. q :time))
;; (defn parse-length [q] (Quantity. q :length))
;; (defn parse-mass [q] (Quantity. q :mass))
;; (defn parse-speed [q] (Quantity. q :speed))
;; (defn parse-temperature [q] (Quantity. q :temperature))
;; (defn parse-unitless [q] (Quantity. q :unitless))

(defn unit-type-match?
  [^Quantity a ^Quantity b]
  (= (get-unit-type a) (get-unit-type b)))

(defn assert-unit-type-match
  [^Quantity a ^Quantity b]
  (when-not (unit-type-match? a b)
    (throw (ex-info "Quantities must have the same unit type." {:a a :b b}))))

(defn assert-unit-type
  [^Quantity a unit-type]
  (when-not (= (get-unit-type a) unit-type)
    (throw (ex-info "The provided quantity is not compatible with the target unit type." {:quantity a :expected-unit-type unit-type}))))

(defn meters [v] (Quantity. v :length))
(defn ->meters [^Quantity q] (assert-unit-type q :length) (get-value q))

(defn feet [v] (Quantity. (u/feet->meters v) :length))
(defn ->feet [^Quantity q] (assert-unit-type q :length) (u/meters->feet (get-value q)))

(defn meters-per-second [v] (Quantity. v :speed))
(defn ->meters-per-second [^Quantity q] (assert-unit-type q :speed) (get-value q))

(defn knots [v] (Quantity. (u/knots->meters-per-second v) :speed))
(defn ->knots [^Quantity q] (assert-unit-type q :speed) (u/meters-per-second->knots (get-value q)))

(defn feet-per-minute [v] (Quantity. (u/feet-per-minute->meters-per-second v) :speed))
(defn ->feet-per-minute [^Quantity q] (assert-unit-type q :speed) (u/meters-per-second->feet-per-minute (get-value q)))

(defn kilograms [v] (Quantity. v :mass))
(defn ->kilograms [^Quantity q] (assert-unit-type q :mass) (get-value q))

(defn pounds [v] (Quantity. (u/pounds->kilograms v) :mass))
(defn ->pounds [^Quantity q] (assert-unit-type q :mass) (u/kilograms->pounds (get-value q)))

(defn seconds [v] (Quantity. v :time))
(defn ->seconds [^Quantity q] (assert-unit-type q :time) (get-value q))

(defn minutes [v] (Quantity. (u/minutes->seconds v) :time))
(defn ->minutes [^Quantity q] (assert-unit-type q :time) (u/seconds->minutes (get-value q)))

(defn hours [v] (Quantity. (u/hours->seconds v) :time))
(defn ->hours [^Quantity q] (assert-unit-type q :time) (u/seconds->hours (get-value q)))

(defn kelvin [v] (Quantity. v :temperature))
(defn ->kelvin [^Quantity q] (assert-unit-type q :temperature) (get-value q))

(defn celsius [v] (Quantity. (u/celsius->kelvin v) :temperature))
(defn ->celsius [^Quantity q] (assert-unit-type q :temperature) (u/kelvin->celsius (get-value q)))

(defn rankine [v] (Quantity. (u/rankine->kelvin v) :temperature))
(defn ->rankine [^Quantity q] (assert-unit-type q :temperature) (u/kelvin->rankine (get-value q)))

(defn fahrenheit [v] (Quantity. (u/fahrenheit->kelvin v) :temperature))
(defn ->fahrenheit [^Quantity q] (assert-unit-type q :temperature) (u/kelvin->fahrenheit (get-value q)))

(defn unitless [v] (Quantity. v :unitless))
(defn ->unitless [^Quantity q] (assert-unit-type q :unitless) (get-value q))

(defmethod qm/+ [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-type-match a b)
  (Quantity. (+ (get-value a) (get-value b))
             (get-unit-type a)))

(defmethod qm/- [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-type-match a b)
  (Quantity. (- (get-value a) (get-value b)) (get-unit-type a)))

(defmethod qm/* [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (if-let [new-unit-type (get-in allowed-operations [:multiplications [(get-unit-type a) (get-unit-type b)]])]
    (Quantity. (* (get-value a) (get-value b)) new-unit-type)
    (throw (ex-info "Multiplying two Quantities must result in a known unit-type" {:a a :b b}))))

(defmethod qm/* [#?(:clj java.lang.Number :cljs js/Number) Quantity]
  [a ^Quantity b]
  (Quantity. (* a (get-value b)) (get-unit-type b)))

(defmethod qm/* [Quantity #?(:clj java.lang.Number :cljs js/Number)]
  [^Quantity a b]
  (Quantity. (* (get-value a) b) (get-unit-type a)))

(defmethod qm/divide [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (if-let [new-unit-type (get-in allowed-operations [:divisions [(get-unit-type a) (get-unit-type b)]])]
    (Quantity. (qm/divide (get-value a) (get-value b)) new-unit-type)
    (throw (ex-info "Dividing two Quantities must result in a known unit-type" {:a a :b b}))))

(defmethod qm/divide [Quantity #?(:clj java.lang.Number :cljs js/Number)]
  [^Quantity a b]
  (Quantity. (qm/divide (get-value a) b) (get-unit-type a)))

(defmethod qm/abs Quantity
  [^Quantity a]
  (Quantity. (qm/abs (get-value a)) (get-unit-type a)))

;; ;; (defmethod gm/pow [Quantity #?(:clj java.lang.Number :cljs js/Number)]
;; ;;   [^Quantity a n]
;; ;;   (Quantity. (:type a) (Math/pow (get-value a) n)
;; ;;              (let [^Unit unit (:unit a)]
;; ;;                (->Unit (:unit unit) (* n (:exponent unit))))))

;; ;; (defmethod gm/sqrt Quantity
;; ;;   [^Quantity a]
;; ;;   (Quantity. (:type a) (Math/sqrt (get-value a))
;; ;;              (let [^Unit unit (:unit a)]
;; ;;                (->Unit (:unit unit) (/ (:exponent unit) 2)))))

(defmethod qm/zero? Quantity
  [^Quantity a]
  (zero? (get-value a)))

(defmethod qm/pos? Quantity
  [^Quantity a]
  (pos? (get-value a)))

(defmethod qm/neg? Quantity
  [^Quantity a]
  (neg? (get-value a)))

(defmethod qm/> [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-type-match a b)
  (qm/> (get-value a) (get-value b)))

(defmethod qm/< [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-type-match a b)
  (qm/< (get-value a) (get-value b)))

(defmethod qm/>= [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-type-match a b)
  (qm/>= (get-value a) (get-value b)))

(defmethod qm/<= [Quantity Quantity]
  [^Quantity a ^Quantity b]
  (assert-unit-type-match a b)
  (qm/<= (get-value a) (get-value b)))

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

(def si-units
  {:length "meters"
   :time "seconds"
   :speed "meters-per-second"
   :acceleration "meters-per-second-squared"
   :area "meters-squared"
   :unitless "unitless"
   :mass "kilograms"
   :temperature "kelvin"})

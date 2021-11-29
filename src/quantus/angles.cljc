;;   Copyright (c) 7theta. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   MIT License (https://opensource.org/licenses/MIT) which can also be
;;   found in the LICENSE file at the root of this distribution.
;;
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any others, from this software.

(ns quantus.angles
  (:require [quantus.core :refer [->Quantity ->Unit]])
  #?(:clj (:import [quantus.core Quantity Unit])))

(def pi #?(:clj java.lang.Math/PI :cljs js/Math.PI))

(defn ->rad
  [^Quantity d]
  (cond-> d
    (= :deg (:unit ^Unit (:unit d)))
    (#(->Quantity :angle (/ (* (:value %) pi) 180) (->Unit :rad 1)))))

(defn ->deg
  [^Quantity r]
  (cond-> r
    (= :rad (:unit ^Unit (:unit r)))
    (#(->Quantity :angle (/ (* (:value %) 180) pi) (->Unit :deg 1)))))

(defn difference
  "Shortest angular distance between `a` and `b`"
  [^Quantity a ^Quantity b]
  (when-not (and (= :deg (:unit ^Unit (:unit a)))
                 (= :deg (:unit ^Unit (:unit b))))
    (throw (ex-info "Units must be :deg" {:a a :b b})))
  (->Quantity :angle (-> (- (:value a) (:value b)) (mod 360) (+ 540) (mod 360) (- 180)) (->Unit :deg)))

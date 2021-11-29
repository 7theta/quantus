;;   Copyright (c) 7theta. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   MIT License (https://opensource.org/licenses/MIT) which can also be
;;   found in the LICENSE file at the root of this distribution.
;;
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any others, from this software.

(ns quantus.mass
  (:require [quantus.core :refer [->Quantity ->Unit]])
  #?(:clj (:import [quantus.core Quantity Unit])))

(defn ->kg
  [^Quantity lb]
  (cond-> lb
    (= :lb (:unit ^Unit (:unit lb)))
    (#(->Quantity :mass (* (:value %) 2.205) (->Unit :kg 1)))))

(defn ->lb
  [^Quantity kg]
  (cond-> kg
    (= :kg (:unit ^Unit (:unit kg)))
    (#(->Quantity :angle (/ (:value %) 2.205) (->Unit :lb 1)))))

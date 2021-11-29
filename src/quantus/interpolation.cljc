;;   Copyright (c) 7theta. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   MIT License (https://opensource.org/licenses/MIT) which can also be
;;   found in the LICENSE file at the root of this distribution.
;;
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any others, from this software.

(ns quantus.interpolation
  (:require [quantus.core :refer [->Quantity ->Unit]]
            [quantus.angles :as angle]))

(declare linear-interpolator)

(defn linear
  "Returns a function that interpolates between `points` using a
  linear interpolation"
  [points]
  (linear-interpolator
   points
   (fn [x l-x l-y r-x r-y c]
     (+ (* (- 1.0 c) l-y) (* c r-y)))))

(defn angular
  "Returns a function that interpolates between `points` using a
  linear interpolation between angles"
  [points]
  (linear-interpolator
   points
   (fn [x l-alt l-dir r-alt r-dir c]
     (let [shortest-angle (angle/difference (->Quantity :angle l-dir (->Unit :deg))
                                            (->Quantity :angle r-dir (->Unit :deg)))
           linear-angle (- l-dir r-dir)]
       (if (< (#?(:clj java.lang.Math/abs :cljs js/Math.abs) (:value shortest-angle))
              (#?(:clj java.lang.Math/abs :cljs js/Math.abs) linear-angle))
         (-> (* (:value shortest-angle) c)
             (->> ((if (< l-dir r-dir) - +) l-dir))
             (mod 360))
         (+ (* l-dir (- 1 c)) (* r-dir c)))))))

;;; Private

(defn- linear-interpolator
  [points blend-fn]
  (let [points (sort-by first points)
        x-values (mapv first points)
        y-values (mapv second points)]
    (fn [x]
      (cond
        (= x (last x-values))
        (last y-values)

        (not (<= (first x-values) x (last x-values)))
        (throw (ex-info "Out of range" {:range [(first x-values) (last x-values)]
                                        :value x}))

        :else
        (let [l-index (reduce-kv (fn [l i v]
                                   (if (<= v x) i (reduced l))) nil x-values)
              r-index (inc l-index)
              l-x (double (get x-values l-index))
              l-y (double (get y-values l-index))
              r-x (double (get x-values r-index))
              r-y (double (get y-values r-index))
              x (double x)
              c (/ (- x l-x) (- r-x l-x))]
          (blend-fn x l-x l-y r-x r-y c))))))

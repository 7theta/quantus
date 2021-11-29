;;   Copyright (c) 7theta. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   MIT License (https://opensource.org/licenses/MIT) which can also be
;;   found in the LICENSE file at the root of this distribution.
;;
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any others, from this software.

(ns quantus.vector
  (:require [quantus.core :refer [->Quantity ->Unit]]
            [quantus.angles :refer [->deg ->rad]]
            [quantus.math :as qm])
  #?(:clj (:import [java.lang Math])))

(defrecord Vector2D [magnitude angle])

(defmethod qm/+ [Vector2D Vector2D]
  [^Vector2D a ^Vector2D b]
  (let [a-magnitude (:magnitude a)
        a-angle (:value (->rad (:angle a)))
        b-magnitude (:magnitude b)
        b-angle (:value (->rad (:angle b)))]
    (->Vector2D (#?(:clj Math/sqrt :cljs js/Math.sqrt)
                 (+ (* a-magnitude a-magnitude) (* b-magnitude b-magnitude)
                    (* 2 a-magnitude b-magnitude
                       (#?(:clj Math/cos :cljs js/Math.cos) (- b-angle a-angle)))))
                (let [angle (-> (+ a-angle (#?(:clj Math/atan2 :cljs js/Math.atan2)
                                            (* b-magnitude (#?(:clj Math/sin :cljs js/Math.sin) (- b-angle a-angle)))
                                            (+ a-magnitude (* b-magnitude (#?(:clj Math/cos :cljs js/Math.cos)
                                                                           (- b-angle a-angle))))))
                                (#(->Quantity :angle % (->Unit :rad)))
                                ->deg)
                      a (:value angle)]
                  (if (>= a 0) angle (assoc angle :value (+ 360 a)))))))

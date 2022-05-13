;;   Copyright (c) 7theta. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   MIT License (https://opensource.org/licenses/MIT) which can also be
;;   found in the LICENSE file at the root of this distribution.
;;
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any others, from this software.

(ns quantus.math
  (:require [clojure.core :as core]
            #?(:clj [clojure.math]))
  (:refer-clojure :exclude [+ - * / = not= < > <= >= zero? pos? neg? min max #?(:clj abs :cljs divide)])
  #?(:import [java.lang Number]))

(defn arity-dispatch
  ([] ::nulary)
  ([x] (type x)) ; unary
  ([x y] [(type x) (type y)]) ; binary
  ([x y & more] ::nary))

;; Addition

(defmulti +
  "Returns the sum of all arguments"
  arity-dispatch)

(defmethod + ::nulary [] 0)

(defmethod + #?(:clj Number :cljs js/Number) [x] x)

(defmethod + [#?(:clj Number :cljs js/Number) #?(:clj Number :cljs js/Number)]
  [x y]
  (clojure.core/+ x y))

(defmethod + ::nary
  [x y & more]
  (reduce + (+ x y) more))

;; Subtraction

(defmulti -
  "Returns the difference of the first argument and the
  sum of all rest do the arguments"
  arity-dispatch)

(defmethod - ::nulary [] 0)

(defmethod - #?(:clj Number :cljs js/Number) [x] (clojure.core/- x))

(defmethod - [#?(:clj Number :cljs js/Number) #?(:clj Number :cljs js/Number)]
  [x y]
  (clojure.core/- x y))

(defmethod - ::nary
  [x y & more]
  (reduce - (- x y) more))

;; Multiplication

(defmulti *
  "Returns the product of all the arguments"
  arity-dispatch)

(defmethod * ::nulary [] 1)

(defmethod * #?(:clj Number :cljs js/Number) [x] x)

(defmethod * [#?(:clj Number :cljs js/Number) #?(:clj Number :cljs js/Number)]
  [x y]
  (clojure.core/* x y))

(defmethod * ::nary
  [x y & more]
  (reduce * (* x y) more))

;; Division

(defmulti divide
  "Return the quotient of the first argument and the
  product of all other arguments"
  arity-dispatch)

(defmethod divide #?(:clj Number :cljs js/Number) [x] (clojure.core// x))

(defmethod divide [#?(:clj Number :cljs js/Number) #?(:clj Number :cljs js/Number)]
  [x y]
  (clojure.core// x y))

(defmethod divide ::nary
  [x y & more]
  (reduce divide (divide x y) more))

;; Workaround for (defmulti / arity-dispatch) not working
(defn /
  "Return the quotient of the first argument and the
  product of all other arguments. "
  ([x] (divide x)) ; unary
  ([x y] (divide x y)) ; binary
  ([x y & more] (apply divide x y more)))

;; zero?

(defmulti zero?
  "Return true if `x` is zero."
  type)

(defmethod zero? #?(:clj Number :cljs js/Number) [x] (clojure.core/zero? x))

;; pos?

(defmulti pos?
  "Return true if `x` is positive."
  type)

(defmethod pos? #?(:clj Number :cljs js/Number) [x] (clojure.core/pos? x))

;; neg?

(defmulti neg?
  "Return true if `x` is negative."
  type)

(defmethod neg? #?(:clj Number :cljs js/Number) [x] (clojure.core/neg? x))

;; Absolute value

(defmulti abs
  "Return the absolute value of `x`. If `x` is a BigDecimal, abs takes an
  optional `math-context` argument."
  (fn [x & more] (type x)))

(defmethod abs :default
  [x]
  (if (neg? x) (- x) x))

#?(:cljs (defmethod abs js/Number [x] (js/Math.abs x)))

#?(:clj (defmethod abs java.lang.Double [^java.lang.Double x] (java.lang.Math/abs x)))
#?(:clj (defmethod abs java.lang.Float [^java.lang.Float x] (java.lang.Math/abs x)))
#?(:clj (defmethod abs java.lang.Integer [^java.lang.Integer x] (java.lang.Math/abs x)))
#?(:clj (defmethod abs java.lang.Long [x] (java.lang.Math/abs (long x))))

#?(:clj (defmethod abs java.math.BigDecimal
          ([^java.math.BigDecimal x] (.abs x))
          ([^java.math.BigDecimal x math-context] (.abs x math-context))))

#?(:clj (defmethod abs java.math.BigInteger [^java.math.BigInteger x] (.abs x)))

#?(:clj (defmethod abs clojure.lang.BigInt
          [^clojure.lang.BigInt x]
          (if (nil? (.bipart x))
            (clojure.lang.BigInt/fromLong (abs (.lpart x)))
            (clojure.lang.BigInt/fromBigInteger (abs (.bipart x))))))

#?(:clj (defmethod abs clojure.lang.Ratio
          [^clojure.lang.Ratio x]
          (/ (abs (numerator x))
             (abs (denominator x)))))

;; Round

(defmulti round
  "Returns a rounded value of `x`. If `x` is a BigDecimal, a `math-context`
  argument is required. If `x` is a Ratio, it is converted to a Double before
  rounding. If a `x` is a Ratio and a `math-context` is provided, it is
  converted to a BigDecimal before rounding."
  (fn [x & more] (type x)))

#?(:cljs (defmethod round js/Number [x] (js/Math.round x)))

#?(:clj (defmethod round java.lang.Float [^java.lang.Float x] (java.lang.Math/round x)))
#?(:clj (defmethod round java.lang.Double [^java.lang.Double x] (java.lang.Math/round x)))

#?(:clj (defmethod round java.lang.Byte [x] x))
#?(:clj (defmethod round java.lang.Short [x] x))
#?(:clj (defmethod round java.lang.Integer [x] x))
#?(:clj (defmethod round java.lang.Long [x] x))
#?(:clj (defmethod round java.math.BigInteger [x] x))
#?(:clj (defmethod round clojure.lang.BigInt [x] x))

#?(:clj (defmethod round java.math.BigDecimal
          [^java.math.BigDecimal x math-context]
          (.round x math-context)))

#?(:clj (defmethod round clojure.lang.Ratio
          ([x] (round (double x)))
          ([x math-context] (round (bigdec x) math-context))))

;; Equality

(defmulti =
  "Return true if all arguments are equal."
  arity-dispatch)

(defmethod = #?(:clj Number :cljs js/Number) [x] true)
(defmethod = [#?(:clj Number :cljs js/Number) #?(:clj Number :cljs js/Number)] [x y] (clojure.core/= x y))

(defmethod = ::nary
  [x y & more]
  (if (= x y)
    (if (next more)
      (recur y (first more) (next more))
      (= y (first more)))
    false))

(defn not=
  "Equivalent to (not (= ...))."
  [& args]
  (not (apply = args)))

;; Greater than

(defmulti >
  "Return true if each argument is larger than the following ones."
  arity-dispatch)

(defmethod > #?(:clj Number :cljs js/Number) [x] true)
(defmethod > [#?(:clj Number :cljs js/Number) #?(:clj Number :cljs js/Number)] [x y] (clojure.core/> x y))

(defmethod > ::nary
  [x y & more]
  (if (> x y)
    (if (next more)
      (recur y (first more) (next more))
      (> y (first more)))
    false))

;; Less than

(defmulti <
  "Return true if each argument is smaller than the following ones."
  arity-dispatch)

(defmethod < #?(:clj Number :cljs js/Number) [x] true)
(defmethod < [#?(:clj Number :cljs js/Number) #?(:clj Number :cljs js/Number)] [x y] (clojure.core/< x y))

(defmethod < ::nary
  [x y & more]
  (if (< x y)
    (if (next more)
      (recur y (first more) (next more))
      (< y (first more)))
    false))

;; Greater than or equal

(defmulti >=
  "Return true if each argument is larger than or equal to the following ones."
  arity-dispatch)

(defmethod >= #?(:clj Number :cljs js/Number) [x] true)
(defmethod >= [#?(:clj Number :cljs js/Number) #?(:clj Number :cljs js/Number)] [x y] (clojure.core/>= x y))

(defmethod >= ::nary
  [x y & more]
  (if (>= x y)
    (if (next more)
      (recur y (first more) (next more))
      (>= y (first more)))
    false))

;; Less than or equal

(defmulti <=
  "Return true if each argument is smaller than or equal to the following ones."
  arity-dispatch)

(defmethod <= #?(:clj Number :cljs js/Number) [x] true)
(defmethod <= [#?(:clj Number :cljs js/Number) #?(:clj Number :cljs js/Number)] [x y] (clojure.core/<= x y))

(defmethod <= ::nary
  [x y & more]
  (if (<= x y)
    (if (next more)
      (recur y (first more) (next more))
      (<= y (first more)))
    false))

;; Approximate =

(defn approx=
  "Return true if the absolute value of the difference between x and y
   is less than eps."
  [x y eps]
  (< (abs (- x y)) eps))

;; Max

(defn max
  "Returns the greatest of its arguments."
  ([x] x)
  ([x y] (if (> x y) x y))
  ([x y & more]
   (reduce max (max x y) more)))

;; Min

(defn min
  "Returns the least of its arguments."
  ([x] x)
  ([x y] (if (< x y) x y))
  ([x y & more]
   (reduce min (min x y) more)))

;; Tools

(defn linspace
  "Outputs a range with `n` equally spaced elements, the first one being
  `lower` and the last one being `upper`."
  [lower upper n]
  (let [d (core// (core/- upper lower) (dec n))]
    (range lower (core/+ upper (core// d 2)) d)))

(defn fn-average
  "Maps `f` across all the elements in `rng` and computes the average of
  the outputs.  Instead of range, upper and lower can be provided,
  with an optional number of steps (default: 100) (see linspace)."
  ([f rng]
   (let [] (->> rng
                (map f)
                (reduce +))
        (/ (count rng))))
  ([f lower upper] (fn-average f lower upper 100))
  ([f lower upper n] (fn-average f (linspace lower upper n))))

(defn interpolate
  [data x-key x-val y-key]
  (cond
    (<= x-val (x-key (first data))) (y-key (first data))
    (>= x-val (x-key (last data))) (y-key (last data))
    :else
    (let [ind (->> data
                   (map-indexed vector)
                   (drop-while #(< (x-key (second %)) x-val))
                   ffirst)
          d0 (nth data (dec ind))
          d1 (nth data ind)
          alpha (/ (- x-val (x-key d0))
                   (- (x-key d1) (x-key d0)))]
      (+ (y-key d0)
         (* alpha (- (y-key d1)
                     (y-key d0)))))))

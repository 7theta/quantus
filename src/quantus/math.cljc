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
  (:refer-clojure :exclude [+ - * / = not= < > <= >= zero? pos? neg? min max abs #?(:cljs divide)])
  #?(:import [java.lang Number]))

(defn resolve-type
  [x]
  (if (number? x) :quantus/number (type x)))

(defn arity-dispatch
  ([] ::nulary)
  ([x] (resolve-type x)) ; unary
  ([x y] [(resolve-type x) (resolve-type y)]) ; binary
  ([x y & more] ::nary))

;; Addition

(defmulti +
  "Returns the sum of all arguments"
  arity-dispatch)

(defmethod + ::nulary [] 0)

(defmethod + :quantus/number [x] x)

(defmethod + [:quantus/number :quantus/number]
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

(defmethod - :quantus/number [x] (clojure.core/- x))

(defmethod - [:quantus/number :quantus/number]
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

(defmethod * :quantus/number [x] x)

(defmethod * [:quantus/number :quantus/number]
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

(defmethod divide :quantus/number [x] (clojure.core// x))

(defmethod divide [:quantus/number :quantus/number]
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

;; Zero-value

(defmulti zero-value
  "Returns the same type or unit-type with a value of zero."
  arity-dispatch)

#?(:cljs (defmethod zero-value js/Number [_] 0))

#?(:clj (defmethod zero-value java.lang.Double [_] 0.0))
#?(:clj (defmethod zero-value java.lang.Float [_] (float 0)))
#?(:clj (defmethod zero-value java.lang.Integer [_] (int 0)))
#?(:clj (defmethod zero-value java.lang.Long [_] 0))

(defmethod zero-value :default [x] (* x 0))

;; zero?

(defmulti zero?
  "Return true if `x` is zero."
  type)

(defmethod zero? :quantus/number [x] (clojure.core/zero? x))

;; pos?

(defmulti pos?
  "Return true if `x` is positive."
  type)

(defmethod pos? :quantus/number [x] (clojure.core/pos? x))

;; neg?

(defmulti neg?
  "Return true if `x` is negative."
  type)

(defmethod neg? :quantus/number [x] (clojure.core/neg? x))

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

;; Square Root

(defmulti sqrt
  "Return the square root of `x`."
  (fn [x & more] (type x)))

#?(:clj (defmethod sqrt Number [x] (clojure.math/sqrt x))
   :cljs (defmethod sqrt js/Number [x] (js/Math.sqrt x)))

;; Power

(defmulti pow
  "Return the power of `x`."
  (fn [x p & more] [(type x) (type p)]))

#?(:clj (defmethod pow [Number Number] [x p] (clojure.math/pow x p))
   :cljs (defmethod pow [js/Number js/Number] [x p] (js/Math.pow x p)))

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

(defmethod = :quantus/number [x] true)
(defmethod = [:quantus/number :quantus/number] [x y] (clojure.core/= x y))

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

(defmethod > :quantus/number [x] true)
(defmethod > [:quantus/number :quantus/number] [x y] (clojure.core/> x y))

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

(defmethod < :quantus/number [x] true)
(defmethod < [:quantus/number :quantus/number] [x y] (clojure.core/< x y))

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

(defmethod >= :quantus/number [x] true)
(defmethod >= [:quantus/number :quantus/number] [x y] (clojure.core/>= x y))

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

(defmethod <= :quantus/number [x] true)
(defmethod <= [:quantus/number :quantus/number] [x y] (clojure.core/<= x y))

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
   (let [sum-f (->> rng
                    (map f)
                    (reduce +))]
     (/ sum-f (count rng))))
  ([f lower upper] (fn-average f lower upper 100))
  ([f lower upper n] (fn-average f (linspace lower upper n))))

(defn interpolate
  "Given `data` that consists of a sequence of collections (typically
  maps or tuples), this function will sort it by `x-key` and return a
  function that given an arbitrary `x-val`, will return the
  interpolated value for `y-key`.

  If `x-key` and `y-key` are not provided, they default to `first` and
  `second`, for working with tuples.

  If `x-val` is outside the range of the x-key values for the data,
  the outermost y value is returned."
  ([data] (interpolate data first second))
  ([data x-key y-key]
   (let [sorted-data (sort-by x-key data)]
     (fn [x-val]
       (cond
         (<= x-val (x-key (first sorted-data))) (y-key (first sorted-data))
         (>= x-val (x-key (last sorted-data))) (y-key (last sorted-data))
         :else
         (let [ind (->> sorted-data
                        (map-indexed vector)
                        (drop-while #(< (x-key (second %)) x-val))
                        ffirst)
               d0 (nth sorted-data (dec ind))
               d1 (nth sorted-data ind)
               alpha (/ (- x-val (x-key d0))
                        (- (x-key d1) (x-key d0)))]
           (+ (y-key d0)
              (* alpha (- (y-key d1)
                          (y-key d0))))))))))

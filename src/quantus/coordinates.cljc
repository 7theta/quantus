(ns quantus.coordinates
  (:require [quantus.core #?@(:cljs [:refer [Quantity]]) :as q]
            [quantus.angles #?@(:cljs [:refer [AngleQuantity]]) :as qa]
            [quantus.math :as qm]
            [clojure.core :as core])
  #?(:clj (:import [quantus.core Quantity]
                   [quantus.angles AngleQuantity]))
  (:refer-clojure :exclude [+ - * /]))

(defn- add-hashcode [hash x]
  (core/+ hash (core/* 37 hash) (Float/floatToIntBits x)))

(definterface iCoordinateXYZ
  (^double getX [])
  (^double getY [])
  (^double getZ []))

(deftype CoordinateXYZ [^double x ^double y ^double z]
  iCoordinateXYZ
  (getX [_] x)
  (getY [_] y)
  (getZ [_] z)

  clojure.lang.Counted
  (count [_] 3)

  clojure.lang.Sequential

  clojure.lang.Seqable
  (seq [_] (list x y z))

  clojure.lang.ILookup
  (valAt [c i]
    (.valAt c i nil))
  (valAt [_ i not-found]
    (case i
      (0 :x) x
      (1 :y) y
      (2 :z) z
      (if (number? i)
        (case (int i) 0 x 1 y 2 z not-found)
        not-found)))

  clojure.lang.Associative
  (equiv [c o] (.equals c o))
  (containsKey [_ k]
    (or (#{0 1 2 :x :y :z} k)
        (when (number? k)
          (#{0 1 2} (int k)))
        false))
  (entryAt [_ k]
    (case k
      (0 :x) (clojure.lang.MapEntry. 0 x)
      (1 :y) (clojure.lang.MapEntry. 1 y)
      (2 :z) (clojure.lang.MapEntry. 2 z)
      (when (number? k)
        (case (int k)
          0 (clojure.lang.MapEntry. 0 x)
          1 (clojure.lang.MapEntry. 1 y)
          2 (clojure.lang.MapEntry. 2 z)
          nil))))
  (assoc [_ i v]
    (case i
      (0 :x) (CoordinateXYZ. v y z)
      (1 :y) (CoordinateXYZ. x v z)
      (2 :z) (CoordinateXYZ. x y v)
      (when (number? i)
        (case (int i)
          0 (CoordinateXYZ. v y z)
          1 (CoordinateXYZ. x v z)
          2 (CoordinateXYZ. x y v)))))

  clojure.lang.IFn
  (invoke [c i]
    (.valAt c i))

  Object
  (toString [_]
    (str "#quantity/coordinate-xyz [" x " " y " " z "]"))
  (hashCode [_]
    (-> 17 (add-hashcode x)
        (add-hashcode y)
        (add-hashcode z)))
  (equals [self c]
    (or (identical? self c)
        (and (instance? CoordinateXYZ c)
             (== x (.getX ^CoordinateXYZ c))
             (== y (.getY ^CoordinateXYZ c))
             (== z (.getZ ^CoordinateXYZ c)))
        (and (counted? x)
             (== (count x) 3)
             (== x (c 0))
             (== y (c 1))
             (== z (c 2))))))

(definterface iCoordinateXY
  (^double getX [])
  (^double getY []))

(deftype CoordinateXY [^double x ^double y]
  iCoordinateXY
  (getX [_] x)
  (getY [_] y)

  clojure.lang.Counted
  (count [_] 2)

  clojure.lang.Sequential

  clojure.lang.Seqable
  (seq [_] (list x y))

  clojure.lang.ILookup
  (valAt [c i]
    (.valAt c i nil))
  (valAt [_ i not-found]
    (case i
      (0 :x) x
      (1 :y) y
      (if (number? i)
        (case (int i) 0 x 1 y not-found)
        not-found)))

  clojure.lang.Associative
  (equiv [v o] (.equals v o))
  (containsKey [_ k]
    (or (#{0 1 :x :y} k)
        (when (number? k)
          (#{0 1} (int k)))
        false))
  (entryAt [_ k]
    (case k
      (0 :x) (clojure.lang.MapEntry. 0 x)
      (1 :y) (clojure.lang.MapEntry. 1 y)
      (when (number? k)
        (case (int k)
          0 (clojure.lang.MapEntry. 0 x)
          1 (clojure.lang.MapEntry. 1 y)
          nil))))
  (assoc [_ i v]
    (case i
      (0 :x) (CoordinateXY. v y)
      (1 :y) (CoordinateXY. x v)
      (when (number? i)
        (case (int i)
          0 (CoordinateXY. v y)
          1 (CoordinateXY. x v)))))

  clojure.lang.IFn
  (invoke [c i]
    (.valAt c i))

  Object
  (toString [_]
    (str "#quantity/coordinate-xy [" x " " y "]"))
  (hashCode [_]
    (-> 17 (add-hashcode x)
        (add-hashcode y)))
  (equals [self c]
    (or (identical? self c)
        (and (instance? CoordinateXY c)
             (== x (.getX ^CoordinateXY c))
             (== y (.getY ^CoordinateXY c)))
        (and (counted? c)
             (== (count c) 2)
             (== x (c 0))
             (== y (c 1))))))

(defn- add-xy [^CoordinateXY c1 ^CoordinateXY c2]
  (CoordinateXY. (core/+ (.getX c1) (.getX c2))
                 (core/+ (.getY c1) (.getY c2))))

(defn- sub-xy [^CoordinateXY c1 ^CoordinateXY c2]
  (CoordinateXY. (core/- (.getX c1) (.getX c2))
                 (core/- (.getY c1) (.getY c2))))

(defn- mult-xy [^CoordinateXY c1 ^CoordinateXY c2]
  (CoordinateXY. (core/* (.getX c1) (.getX c2))
                 (core/* (.getY c1) (.getY c2))))

(defn- div-xy [^CoordinateXY c1 ^CoordinateXY c2]
  (CoordinateXY. (core// (.getX c1) (.getX c2))
                 (core// (.getY c1) (.getY c2))))

(defn- scale-xy [^CoordinateXY c ^double f]
  (CoordinateXY. (core/* (.getX c) f)
                 (core/* (.getY c) f)))

(defn- dot-xy [^CoordinateXY c1 ^CoordinateXY c2]
  (core/+ (core/* (.getX c1) (.getX c2))
          (core/* (.getY c1) (.getY c2))))

(defn- magnitude-xy [^CoordinateXY c]
  (let [x (.getX c)
        y (.getY c)]
    (Math/sqrt (core/+ (core/* x x) (core/* y y)))))

(defn- add-xyz [^CoordinateXYZ c1 ^CoordinateXYZ c2]
  (CoordinateXYZ. (core/+ (.getX c1) (.getX c2))
                  (core/+ (.getY c1) (.getY c2))
                  (core/+ (.getZ c1) (.getZ c2))))

(defn- sub-xyz [^CoordinateXYZ c1 ^CoordinateXYZ c2]
  (CoordinateXYZ. (core/- (.getX c1) (.getX c2))
                  (core/- (.getY c1) (.getY c2))
                  (core/- (.getZ c1) (.getZ c2))))

(defn- mult-xyz [^CoordinateXYZ c1 ^CoordinateXYZ c2]
  (CoordinateXYZ. (core/* (.getX c1) (.getX c2))
                  (core/* (.getY c1) (.getY c2))
                  (core/* (.getZ c1) (.getZ c2))))

(defn- div-xyz [^CoordinateXYZ c1 ^CoordinateXYZ c2]
  (CoordinateXYZ. (core// (.getX c1) (.getX c2))
                  (core// (.getY c1) (.getY c2))
                  (core// (.getZ c1) (.getZ c2))))

(defn- scale-xyz [^CoordinateXYZ c ^double f]
  (CoordinateXYZ. (core/* (.getX c) f)
                  (core/* (.getY c) f)
                  (core/* (.getZ c) f)))

(defn- dot-xyz [^CoordinateXYZ c1 ^CoordinateXYZ c2]
  (core/+ (core/* (.getX c1) (.getX c2))
          (core/* (.getY c1) (.getY c2))
          (core/* (.getZ c1) (.getZ c2))))

(defn- magnitude-xyz [^CoordinateXYZ c]
  (let [x (.getX c)
        y (.getY c)
        z (.getZ c)]
    (Math/sqrt (core/+ (core/* x x) (core/* y y) (core/* z z)))))

(defprotocol Coordinate
  (^:no-doc add* [c1 c2] "Add two coordinates together.")
  (^:no-doc sub* [c1 c2] "Subtract the second coordinate from the first.")
  (^:no-doc mult* [c1 c2] "Multiply one coordinate by another.")
  (^:no-doc div* [c1 c2] "Divide one coordinate by another.")
  (scale [c f] "Scale a coordinate by a factor.")
  (dot [c1 c2] "Find the dot-product of two coordinates.")
  (magnitude [c] "The magnitude (length) of the coordinate."))

(extend-protocol Coordinate
  CoordinateXY
  (add* [c1 c2] (add-xy c1 c2))
  (sub* [c1 c2] (sub-xy c1 c2))
  (mult* [c1 c2] (mult-xy c1 c2))
  (div* [c1 c2] (div-xy c1 c2))
  (scale [c f] (scale-xy c f))
  (dot [c1 c2] (dot-xy c1 c2))
  (magnitude [c] (magnitude-xy c))
  CoordinateXYZ
  (add* [c1 c2] (add-xyz c1 c2))
  (sub* [c1 c2] (sub-xyz c1 c2))
  (mult* [c1 c2] (mult-xyz c1 c2))
  (div* [c1 c2] (div-xyz c1 c2))
  (scale [c f] (scale-xyz c f))
  (dot [c1 c2] (dot-xyz c1 c2))
  (magnitude [c] (magnitude-xyz c))
  ;; Quantity
  ;; (add* [c1 c2] (add-xyz c1 c2))
  ;; (sub* [c1 c2] (sub-xyz c1 c2))
  ;; (mult* [c1 c2] (mult-xyz c1 c2))
  ;; (div* [c1 c2] (div-xyz c1 c2))
  ;; (scale [c f] (scale-xyz c f))
  ;; (dot [c1 c2] (dot-xyz c1 c2))
  ;; (magnitude [c] (magnitude-xyz c))
  )

(defn +
  "Return the sum of one or more coordinates."
  ([c] c)
  ([c1 c2] (add* c1 c2))
  ([c1 c2 & more] (reduce add* c1 (cons c2 more))))

(defn -
  "If only one coordinate is supplied, return the negation of the
  coordinate. Otherwise all subsequent coordinates are subtracted from
  the first."
  ([c] (scale c -1.0))
  ([c1 c2] (sub* c1 c2))
  ([c1 c2 & more] (reduce sub* c1 (cons c2 more))))

(defn *
  "Return the element-wise product of one or more coordinates."
  ([c] c)
  ([c1 c2] (mult* c1 c2))
  ([c1 c2 & more] (reduce mult* c1 (cons c2 more))))

(defn /
  "If only one coordinate is supplied, return the element-wise
  reciprocal.  Otherwise the first coordinate is divided element-wise
  by all other coordinates."
  ([c] (if (instance? CoordinateXYZ c)
         (div* (CoordinateXYZ. 1.0 1.0 1.0) c)
         (div* (CoordinateXY. 1.0 1.0) c)))
  ([c1 c2] (div* c1 c2))
  ([c1 c2 & more] (reduce div* c1 (cons c2 more))))

(defn normalize
  "Normalize a coordinate by dividing by its magnitude."
  [c]
  (scale c (/ 1.0 (magnitude c))))

(defn cross
  "Find the cross-product of two 3D coordinates."
  [^CoordinateXYZ c1 ^CoordinateXYZ c2]
  (CoordinateXYZ. (- (* (.getY c1) (.getZ c2))
                     (* (.getZ c1) (.getY c2)))
                  (- (* (.getZ c1) (.getX c2))
                     (* (.getX c1) (.getZ c2)))
                  (- (* (.getX c1) (.getY c2))
                     (* (.getY c1) (.getX c2)))))

(defn get-x
  "Find the x component of a coordinate."
  [c]
  (if (instance? CoordinateXYZ c)
    (.getX ^CoordinateXYZ c)
    (.getX ^CoordinateXY c)))

(defn get-y
  "Find the y component of a coordinate."
  [c]
  (if (instance? CoordinateXYZ c)
    (.getY ^CoordinateXYZ c)
    (.getY ^CoordinateXY c)))

(defn get-z
  "Find the z component of a coordinate."
  [c]
  (.getZ ^CoordinateXYZ c))

(defn coordinate
  "Create a new XY or XYZ coordinate."
  ([^double x ^double y]
   (CoordinateXY. x y))
  ([^double x ^double y ^double z]
   (CoordinateXYZ. x y z)))

(defn ->xy
  [c]
  (CoordinateXY. (:x c) (:y c)))

(defn ->xyz
  [c]
  (CoordinateXYZ. (:x c) (:y c) (:z c)))

(defn ->map
  [c]
  (cond-> {:x (:x c) :y (:y c)}
    (contains? c :z) (assoc :z (:z c))))

(defn into-coordinate [coll]
  "Turn a collection of numbers into a coordinate."
  (if (satisfies? Coordinate coll)
    coll
    (apply coordinate coll)))

#?(:clj
   (defmethod print-method CoordinateXY [^CoordinateXY c ^java.io.Writer w]
     (.write w (.toString c)))

   (defmethod print-method CoordinateXYZ [^CoordinateXYZ c ^java.io.Writer w]
     (.write w (.toString c)))

   (defmethod print-dup CoordinateXY [^CoordinateXY c ^java.io.Writer w]
     (.write w (.toString c)))

   (defmethod print-dup CoordinateXYZ [^CoordinateXYZ c ^java.io.Writer w]
     (.write w (.toString c))))

;; XY

(defmulti xy
  "Create a coordinateXY"
  qm/arity-dispatch)

(defmethod xy [#?(:clj java.lang.Number :cljs js/Number)
               #?(:clj java.lang.Number :cljs js/Number)]
  [x y]
  (CoordinateXY. x y))

(defmethod xy [Quantity Quantity]
  [^Quantity x ^Quantity y]
  (q/assert-unit-type-match x y)
  (q/->Quantity (CoordinateXY. (q/value x) (q/value y))
                (q/unit-type x)))

(defn into-xy
  [v]
  (xy (nth v 0) (nth v 1)))

(defn polar->xy
  [magnitude angle]
  (xy (qm/* magnitude (qa/sin angle))
      (qm/* magnitude (qa/cos angle))))

(defmethod qm/+ [CoordinateXY CoordinateXY]
  [^CoordinateXY a ^CoordinateXY b]
  (+ a b))

(defmethod qm/- [CoordinateXY CoordinateXY]
  [^CoordinateXY a ^CoordinateXY b]
  (- a b))

(defmethod qm/* [CoordinateXY CoordinateXY]
  [^CoordinateXY a ^CoordinateXY b]
  (* a b))

(defmethod qm/* [CoordinateXY #?(:clj java.lang.Number :cljs js/Number)]
  [^CoordinateXY a b]
  (scale a b))

(defmethod qm/* [#?(:clj java.lang.Number :cljs js/Number) CoordinateXY]
  [a ^CoordinateXY b]
  (scale b a))

(defmethod qm/divide [CoordinateXY CoordinateXY]
  [^CoordinateXY a ^CoordinateXY b]
  (/ a b))

(defmethod qm/divide [CoordinateXY #?(:clj java.lang.Number :cljs js/Number)]
  [^CoordinateXY a b]
  (scale a (core// b)))

(defmulti angle
  "Returns the bearing angle of a coordinateXY."
  type)

(defmethod angle Quantity
  [^Quantity q]
  (qa/->AngleQuantity (angle (q/value q))))

(defmethod angle CoordinateXY
  [^CoordinateXY c]
  (qa/atan2 (.getY c) (.getX c)))

;;; XYZ

(defmulti xyz
  "Create a coordinateXYZ"
  (fn [x y z] [(type x) (type y) (type z)]))

(defmethod xyz [#?(:clj java.lang.Number :cljs js/Number)
                #?(:clj java.lang.Number :cljs js/Number)
                #?(:clj java.lang.Number :cljs js/Number)]
  [x y z]
  (CoordinateXYZ. x y z))

(defmethod xyz [Quantity Quantity Quantity]
  [^Quantity x ^Quantity y ^Quantity z]
  (q/assert-unit-type-match x y)
  (q/assert-unit-type-match x z)
  (q/->Quantity (CoordinateXYZ. (q/value x) (q/value y) (q/value z))
                (q/unit-type x)))

(defn into-xyz
  [v]
  (xyz (nth v 0) (nth v 1) (nth v 2)))

(defn cylindrical->xyz
  [magnitude angle z-component]
  (xyz (qm/* magnitude (qa/sin angle))
       (qm/* magnitude (qa/cos angle))
       #_(-> direction-deg Math/toRadians Math/sin (* magnitude))
       #_(-> direction-deg Math/toRadians Math/cos (* magnitude))
       z-component))

(defmethod qm/+ [CoordinateXYZ CoordinateXYZ]
  [^CoordinateXYZ a ^CoordinateXYZ b]
  (+ a b))

(defmethod qm/- [CoordinateXYZ CoordinateXYZ]
  [^CoordinateXYZ a ^CoordinateXYZ b]
  (- a b))

(defmethod qm/* [CoordinateXYZ CoordinateXYZ]
  [^CoordinateXYZ a ^CoordinateXYZ b]
  (* a b))

(defmethod qm/* [CoordinateXYZ #?(:clj java.lang.Number :cljs js/Number)]
  [^CoordinateXYZ a b]
  (scale a b))

(defmethod qm/* [#?(:clj java.lang.Number :cljs js/Number) CoordinateXYZ]
  [a ^CoordinateXYZ b]
  (scale b a))

(defmethod qm/divide [CoordinateXYZ CoordinateXYZ]
  [^CoordinateXYZ a ^CoordinateXYZ b]
  (/ a b))

(defmethod qm/divide [CoordinateXYZ #?(:clj java.lang.Number :cljs js/Number)]
  [^CoordinateXYZ a b]
  (scale a (core// b)))

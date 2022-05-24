(ns quantus.coordinates
  (:require [quantus.core :as q]
            [quantus.angles :as qa]
            [quantus.math :as qm]
            [clojure.core :as core])
  (:import [quantus.core Quantity]
           [quantus.angles AngleQuantity])
  (:refer-clojure :exclude [+ - * /]))

(defn- add-hashcode [hash x]
  (core/+ hash (core/* 37 hash) (Float/floatToIntBits x)))

(defprotocol
    CoordinateProtocol
  (^double x [this])
  (^double y [this])
  (^double z [this]))

(deftype CoordinateXYZ [^double x-field ^double y-field ^double z-field]
  CoordinateProtocol
  (x [_] x-field)
  (y [_] y-field)
  (z [_] z-field)

  clojure.lang.Counted
  (count [_] 3)

  clojure.lang.Sequential

  clojure.lang.Seqable
  (seq [_] (list x-field y-field z-field))

  clojure.lang.ILookup
  (valAt [c i]
    (.valAt c i nil))
  (valAt [_ i not-found]
    (case i
      (0 :x) x-field
      (1 :y) y-field
      (2 :z) z-field
      (if (number? i)
        (case (int i) 0 x-field 1 y-field 2 z-field not-found)
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
      (0 :x) (clojure.lang.MapEntry. 0 x-field)
      (1 :y) (clojure.lang.MapEntry. 1 y-field)
      (2 :z) (clojure.lang.MapEntry. 2 z-field)
      (when (number? k)
        (case (int k)
          0 (clojure.lang.MapEntry. 0 x-field)
          1 (clojure.lang.MapEntry. 1 y-field)
          2 (clojure.lang.MapEntry. 2 z-field)
          nil))))
  (assoc [_ i v]
    (case i
      (0 :x) (CoordinateXYZ. v y-field z-field)
      (1 :y) (CoordinateXYZ. x-field v z-field)
      (2 :z) (CoordinateXYZ. x-field y-field v)
      (when (number? i)
        (case (int i)
          0 (CoordinateXYZ. v y-field z-field)
          1 (CoordinateXYZ. x-field v z-field)
          2 (CoordinateXYZ. x-field y-field v)))))

  clojure.lang.IFn
  (invoke [c i]
    (.valAt c i))

  Object
  (toString [_]
    (str "#quantity.coordinate/xyz [" x-field " " y-field " " z-field "]"))
  (hashCode [_]
    (-> 17 (add-hashcode x-field)
        (add-hashcode y-field)
        (add-hashcode z-field)))
  (equals [self c]
    (or (identical? self c)
        (and (instance? CoordinateXYZ c)
             (== x-field (.x ^CoordinateXYZ c))
             (== y-field (.y ^CoordinateXYZ c))
             (== z-field (.z ^CoordinateXYZ c)))
        (and (counted? c)
             (== (count c) 3)
             (== x-field (c 0))
             (== y-field (c 1))
             (== z-field (c 2))))))

(deftype CoordinateXY [^double x-field ^double y-field]
  CoordinateProtocol
  (x [_] x-field)
  (y [_] y-field)

  clojure.lang.Counted
  (count [_] 2)

  clojure.lang.Sequential

  clojure.lang.Seqable
  (seq [_] (list x-field y-field))

  clojure.lang.ILookup
  (valAt [c i]
    (.valAt c i nil))
  (valAt [_ i not-found]
    (case i
      (0 :x) x-field
      (1 :y) y-field
      (if (number? i)
        (case (int i) 0 x-field 1 y-field not-found)
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
      (0 :x) (clojure.lang.MapEntry. 0 x-field)
      (1 :y) (clojure.lang.MapEntry. 1 y-field)
      (when (number? k)
        (case (int k)
          0 (clojure.lang.MapEntry. 0 x-field)
          1 (clojure.lang.MapEntry. 1 y-field)
          nil))))
  (assoc [_ i v]
    (case i
      (0 :x) (CoordinateXY. v y-field)
      (1 :y) (CoordinateXY. x-field v)
      (2 :z) (CoordinateXYZ. x-field y-field v)
      (when (number? i)
        (case (int i)
          0 (CoordinateXY. v y-field)
          1 (CoordinateXY. x-field v)
          2 (CoordinateXYZ. x-field y-field v)))))

  clojure.lang.IFn
  (invoke [c i]
    (.valAt c i))

  Object
  (toString [_]
    (str "#quantity.coordinate/xy [" x-field " " y-field "]"))
  (hashCode [_]
    (-> 17 (add-hashcode x-field)
        (add-hashcode y-field)))
  (equals [self c]
    (or (identical? self c)
        (and (instance? CoordinateXY c)
             (== x-field (.x ^CoordinateXY c))
             (== y-field (.y ^CoordinateXY c)))
        (and (counted? c)
             (== (count c) 2)
             (== x-field (c 0))
             (== y-field (c 1))))))

(defn- add-xy [^CoordinateXY c1 ^CoordinateXY c2]
  (CoordinateXY. (core/+ (.x c1) (.x c2))
                 (core/+ (.y c1) (.y c2))))

(defn- sub-xy [^CoordinateXY c1 ^CoordinateXY c2]
  (CoordinateXY. (core/- (.x c1) (.x c2))
                 (core/- (.y c1) (.y c2))))

(defn- mult-xy [^CoordinateXY c1 ^CoordinateXY c2]
  (CoordinateXY. (core/* (.x c1) (.x c2))
                 (core/* (.y c1) (.y c2))))

(defn- div-xy [^CoordinateXY c1 ^CoordinateXY c2]
  (CoordinateXY. (core// (.x c1) (.x c2))
                 (core// (.y c1) (.y c2))))

(defn- scale-xy [^CoordinateXY c ^double f]
  (CoordinateXY. (core/* (.x c) f)
                 (core/* (.y c) f)))

(defn- dot-xy [^CoordinateXY c1 ^CoordinateXY c2]
  (core/+ (core/* (.x c1) (.x c2))
          (core/* (.y c1) (.y c2))))

(defn- magnitude-xy [^CoordinateXY c]
  (let [x (.x c)
        y (.y c)]
    (Math/sqrt (core/+ (core/* x x) (core/* y y)))))

(defn- add-xyz [^CoordinateXYZ c1 ^CoordinateXYZ c2]
  (CoordinateXYZ. (core/+ (.x c1) (.x c2))
                  (core/+ (.y c1) (.y c2))
                  (core/+ (.z c1) (.z c2))))

(defn- sub-xyz [^CoordinateXYZ c1 ^CoordinateXYZ c2]
  (CoordinateXYZ. (core/- (.x c1) (.x c2))
                  (core/- (.y c1) (.y c2))
                  (core/- (.z c1) (.z c2))))

(defn- mult-xyz [^CoordinateXYZ c1 ^CoordinateXYZ c2]
  (CoordinateXYZ. (core/* (.x c1) (.x c2))
                  (core/* (.y c1) (.y c2))
                  (core/* (.z c1) (.z c2))))

(defn- div-xyz [^CoordinateXYZ c1 ^CoordinateXYZ c2]
  (CoordinateXYZ. (core// (.x c1) (.x c2))
                  (core// (.y c1) (.y c2))
                  (core// (.z c1) (.z c2))))

(defn- scale-xyz [^CoordinateXYZ c ^double f]
  (CoordinateXYZ. (core/* (.x c) f)
                  (core/* (.y c) f)
                  (core/* (.z c) f)))

(defn- dot-xyz [^CoordinateXYZ c1 ^CoordinateXYZ c2]
  (core/+ (core/* (.x c1) (.x c2))
          (core/* (.y c1) (.y c2))
          (core/* (.z c1) (.z c2))))

(defn- magnitude-xyz [^CoordinateXYZ c]
  (let [x (.x c)
        y (.y c)
        z (.z c)]
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
  (magnitude [c] (magnitude-xyz c)))

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
  (CoordinateXYZ. (- (* (.y c1) (.z c2))
                     (* (.z c1) (.y c2)))
                  (- (* (.z c1) (.x c2))
                     (* (.x c1) (.z c2)))
                  (- (* (.x c1) (.y c2))
                     (* (.y c1) (.x c2)))))

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

(do
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

(defmethod xy [java.lang.Number
               java.lang.Number]
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

(defmethod qm/* [CoordinateXY java.lang.Number]
  [^CoordinateXY a b]
  (scale a b))

(defmethod qm/* [java.lang.Number CoordinateXY]
  [a ^CoordinateXY b]
  (scale b a))

(defmethod qm/divide [CoordinateXY CoordinateXY]
  [^CoordinateXY a ^CoordinateXY b]
  (/ a b))

(defmethod qm/divide [CoordinateXY java.lang.Number]
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
  (qa/atan2 (.y c) (.x c)))

;;; XYZ

(defmulti xyz
  "Create a coordinateXYZ"
  (fn [x y z] [(type x) (type y) (type z)]))

(defmethod xyz [java.lang.Number
                java.lang.Number
                java.lang.Number]
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

(defmethod qm/* [CoordinateXYZ java.lang.Number]
  [^CoordinateXYZ a b]
  (scale a b))

(defmethod qm/* [java.lang.Number CoordinateXYZ]
  [a ^CoordinateXYZ b]
  (scale b a))

(defmethod qm/divide [CoordinateXYZ CoordinateXYZ]
  [^CoordinateXYZ a ^CoordinateXYZ b]
  (/ a b))

(defmethod qm/divide [CoordinateXYZ java.lang.Number]
  [^CoordinateXYZ a b]
  (scale a (core// b)))

(ns quantus.coordinates
  (:require [quantus.core :refer [Quantity] :as q]
            [quantus.angles :refer [AngleQuantity] :as qa]
            [quantus.math :as qm]
            [clojure.core :as core]
            [utilis.js :as j])
  #_(:refer-clojure :exclude [+ - * /]))

#_(defn- add-hashcode [hash x]
    (core/+ hash (core/* 37 hash) (Float/floatToIntBits x)))

(defprotocol
    iCoordinateXYZ
  (^double x [this])
  (^double y [this])
  (^double z [this]))

(deftype CoordinateXYZ [^double x-field ^double y-field ^double z-field]
  iCoordinateXYZ
  (x [_] x-field)
  (y [_] y-field)
  (z [_] z-field)

  Object
  (toString [_]
    (str "#quantity/coordinate-xyz [" x-field " " y-field " " z-field "]"))

  IEquiv
  (-equiv [self c]
    (or (identical? self c)
        (and (instance? CoordinateXYZ c)
             (== x-field (x ^CoordinateXYZ c))
             (== y-field (y ^CoordinateXYZ c))
             (== z-field (z ^CoordinateXYZ c)))
        (and (counted? c)
             (== (count c) 3)
             (== x-field (nth c 0))
             (== y-field (nth c 1))
             (== y-field (nth c 2))))))

#_(defprotocol
      iCoordinateXY
    (^double x [this])
    (^double y [this]))

(deftype CoordinateXY [^double x-field ^double y-field]
  iCoordinateXYZ
  (x [_] x-field)
  (y [_] y-field)

  Object
  (toString [_]
    (str "#quantity/coordinate-xy [" x-field " " y-field "]"))

  IEquiv
  (-equiv [self c]
    (or (identical? self c)
        (and (instance? CoordinateXY c)
             (== x-field (x ^CoordinateXY c))
             (== y-field (y ^CoordinateXY c)))
        (and (counted? c)
             (== (count c) 2)
             (== x-field (nth c 0))
             (== y-field (nth c 1))))))

(extend-protocol IPrintWithWriter
  quantus.coordinates.CoordinateXY
  (-pr-writer [obj writer _]
    (write-all writer "#quantity/coordinate-xy [" (x obj) " " (y obj) "]" ))
  quantus.coordinates.CoordinateXYZ
  (-pr-writer [obj writer _]
    (write-all writer "#quantity/coordinate-xyz [" (x obj) " " (y obj) " " (z obj) "]" )))

(defn coordinate
  "Create a new XY or XYZ coordinate."
  ([^double x ^double y]
   (CoordinateXY. x y))
  ([^double x ^double y ^double z]
   (CoordinateXYZ. x y z)))

(defmulti xy
  "Create a coordinateXY"
  qm/arity-dispatch)

(defmethod xy [js/Number
               js/Number]
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

(defmethod qm/+ [CoordinateXY CoordinateXY]
  [^CoordinateXY a ^CoordinateXY b]
  (xy (+ (x a) (x b))
      (+ (y a) (y b))))

(defmethod qm/+ [CoordinateXY js/Number]
  [^CoordinateXY a b]
  (xy (+ (x a) b)
      (+ (y a) b)))

(defmethod qm/+ [js/Number CoordinateXY]
  [a ^CoordinateXY b]
  (xy (+ a (x b))
      (+ a (y b))))

(defmethod qm/- [CoordinateXY CoordinateXY]
  [^CoordinateXY a ^CoordinateXY b]
  (xy (- (x a) (x b))
      (- (y a) (y b))))

(defmethod qm/- [CoordinateXY js/Number]
  [^CoordinateXY a b]
  (xy (- (x a) b)
      (- (y a) b)))

(defmethod qm/- [js/Number CoordinateXY]
  [a ^CoordinateXY b]
  (xy (- a (x b))
      (- a (y b))))

(defmethod qm/* [CoordinateXY CoordinateXY]
  [^CoordinateXY a ^CoordinateXY b]
  (xy (* (x a) (x b))
      (* (y a) (y b))))

(defmethod qm/* [CoordinateXY js/Number]
  [^CoordinateXY a b]
  (xy (* (x a) b)
      (* (y a) b)))

(defmethod qm/* [js/Number CoordinateXY]
  [a ^CoordinateXY b]
  (xy (* a (x b))
      (* a (y b))))

(defmethod qm/divide [CoordinateXY CoordinateXY]
  [^CoordinateXY a ^CoordinateXY b]
  (xy (/ (x a) (x b))
      (/ (y a) (y b))))

(defmethod qm/divide [CoordinateXY js/Number]
  [^CoordinateXY a b]
  (xy (/ (x a) b)
      (/ (y a) b)))


(defmulti xyz
  "Create a coordinateXYZ"
  (fn [x y z] [(type x) (type y) (type z)]))

(defmethod xyz [js/Number
                js/Number
                js/Number]
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

(defmethod qm/+ [CoordinateXYZ CoordinateXYZ]
  [^CoordinateXYZ a ^CoordinateXYZ b]
  (xyz (+ (x a) (x b))
       (+ (y a) (y b))
       (+ (z a) (z b))))

(defmethod qm/+ [CoordinateXYZ js/Number]
  [^CoordinateXYZ a b]
  (xyz (+ (x a) b)
       (+ (y a) b)
       (+ (z a) b)))

(defmethod qm/+ [js/Number CoordinateXYZ]
  [a ^CoordinateXYZ b]
  (xyz (+ a (x b))
       (+ a (y b))
       (+ a (z b))))

(defmethod qm/- [CoordinateXYZ CoordinateXYZ]
  [^CoordinateXYZ a ^CoordinateXYZ b]
  (xyz (- (x a) (x b))
       (- (y a) (y b))
       (- (z a) (z b))))

(defmethod qm/- [CoordinateXYZ js/Number]
  [^CoordinateXYZ a b]
  (xyz (- (x a) b)
       (- (y a) b)
       (- (z a) b)))

(defmethod qm/- [js/Number CoordinateXYZ]
  [a ^CoordinateXYZ b]
  (xyz (- a (x b))
       (- a (y b))
       (- a (z b))))

(defmethod qm/* [CoordinateXYZ CoordinateXYZ]
  [^CoordinateXYZ a ^CoordinateXYZ b]
  (xyz (* (x a) (x b))
       (* (y a) (y b))
       (* (z a) (z b))))

(defmethod qm/* [CoordinateXYZ js/Number]
  [^CoordinateXYZ a b]
  (xyz (* (x a) b)
       (* (y a) b)
       (* (z a) b)))

(defmethod qm/* [js/Number CoordinateXYZ]
  [a ^CoordinateXYZ b]
  (xyz (* a (x b))
       (* a (y b))
       (* a (z b))))

(defmethod qm/divide [CoordinateXYZ CoordinateXYZ]
  [^CoordinateXYZ a ^CoordinateXYZ b]
  (xyz (/ (x a) (x b))
       (/ (y a) (y b))
       (/ (z a) (z b))))

(defmethod qm/divide [CoordinateXYZ js/Number]
  [^CoordinateXYZ a b]
  (xyz (/ (x a) b)
       (/ (y a) b)
       (/ (z a) b)))

(defn magnitude
  [^CoordinateXYZ c]
  (Math/sqrt (+ (* (x c) (x c))
                (* (y c) (y c)))))

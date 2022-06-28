(ns quantus.frame-of-reference
  (:require [quantus.core :as q]
            [quantus.math :as qm]
            [quantus.coordinates :as qc])
  (:import [quantus.core Quantity]))

(defprotocol FrameOfReferenceProtocol
  (type [this])
  (data [this]))

(defrecord FrameOfReference [type-field data-field]
  FrameOfReferenceProtocol
  (type [_] type-field)
  (data [_] data-field))

(defprotocol QuantityFrameOfReferenceProtocol
  (quantity [this])
  (frame-of-reference [this]))

(defrecord QuantityFrameOfReference [^Quantity quantity-field
                                     ^FrameOfReference frame-of-reference-field]
  QuantityFrameOfReferenceProtocol
  (quantity [_] quantity-field)
  (frame-of-reference [_] frame-of-reference-field)

  FrameOfReferenceProtocol
  (type [_] (type frame-of-reference-field))
  (data [_] (data frame-of-reference-field)))

(defmulti convert
  "Converts a given QFoR to another FoR."
  (fn [quantity-frame-of-reference frame-of-reference]
    (if (and (= (type quantity-frame-of-reference) (type frame-of-reference))
             (= (data quantity-frame-of-reference) (data frame-of-reference)))
      ::identity
      [(type quantity-frame-of-reference) (type frame-of-reference)])))

(defmethod convert ::identity
  [qfor for]
  qfor)

(defmethod qm/> [QuantityFrameOfReference QuantityFrameOfReference]
  [^QuantityFrameOfReference a ^QuantityFrameOfReference b]
  (let [c (convert b (frame-of-reference a))]
    (qm/> (quantity a) (quantity c))))

(defmethod qm/< [QuantityFrameOfReference QuantityFrameOfReference]
  [^QuantityFrameOfReference a ^QuantityFrameOfReference b]
  (let [c (convert b (frame-of-reference a))]
    (qm/< (quantity a) (quantity c))))

(defmethod qm/+ [QuantityFrameOfReference Quantity]
  [^QuantityFrameOfReference qfor ^Quantity q]
  (QuantityFrameOfReference. (qm/+ (quantity qfor) q)
                             (frame-of-reference qfor)))

(defmethod qm/- [QuantityFrameOfReference Quantity]
  [^QuantityFrameOfReference qfor ^Quantity q]
  (QuantityFrameOfReference. (qm/- (quantity qfor) q)
                             (frame-of-reference qfor)))

(defmethod qm/- [QuantityFrameOfReference QuantityFrameOfReference]
  [^QuantityFrameOfReference a ^QuantityFrameOfReference b]
  "Returns a Quantity"
  (qm/- (quantity a)
        (quantity (convert b (frame-of-reference a)))))

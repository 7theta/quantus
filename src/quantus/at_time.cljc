(ns quantus.at-time
  (:import [quantus.core Quantity]))

(defProtocol AtTimeProtocol
  (value [this])
  (time [this]))

(deftype AtTime [value-field ^Quantity time-quantity]
  Object
  (toString [^AtTime this]
    (str "#at-time [" (pr-str time-quantity) " " (pr-str value-field) "]"))
  #?(:cljs IHash)
  (#?(:clj hashCode :cljs -hash) [_]
    (hash [value-field time-quantity]))
  #?(:cljs IEquiv)
  (#?(:clj equals :cljs -equiv) [self q]
    (or (identical? self q)
        (and (instance? AtTime q)
             (= value-field (value q))
             (= time-quantity (time q)))))

  AtTimeProtocol
  (value [_] value-field)
  (time [_] time-quantity)

  #?@(:clj
      [clojure.lang.ILookup
       (valAt [q i]
              (Quantity. (.valAt value-field i) unit-type-field))
       (valAt [q i not-found]
              (Quantity. (.valAt value-field i not-found) unit-type-field))]))

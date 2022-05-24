(ns quantus.coordinates-test
  (:require [quantus.coordinates :as sut]
            [quantus.core :as q]
            [quantus.transit]
            [cognitect.transit :as transit]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck :refer [times]]
            [com.gfredericks.test.chuck.clojure-test #?(:clj :refer :cljs :refer-macros) [checking]]
            #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing] :include-macros true])
            [quantus.math :as qm])
  #?(:clj (:import [quantus.core Quantity]
                   [java.io ByteArrayInputStream ByteArrayOutputStream])))

(def units-list [[identity identity]
                 [q/meters q/->meters]
                 [q/kilometers q/->kilometers]
                 [q/feet q/->feet]
                 [q/centimeters q/->centimeters]
                 [q/inches q/->inches]
                 [q/meters-per-second q/->meters-per-second]
                 [q/knots q/->knots]
                 [q/feet-per-minute q/->feet-per-minute]
                 [q/unitless q/->unitless]])

(defn approx=
  ([v1 v2] (approx= v1 v2 1E-5))
  ([v1 v2 e]
   (< (sut/magnitude (qm/- v1 v2)) (* (sut/magnitude v1) e))))

(deftest unit-coordinate-test
  (let [double-features {:NaN? false :min 1E-8 :max 1E50}]
    (checking
     "Single and dual unit coordinate operations: "
     [d1 (gen/double* double-features)
      d2 (gen/double* double-features)]
     (doseq [xy-or-xyz [sut/xy
                        (fn [x y] (sut/xyz x y y))]]
       (let [c12 (xy-or-xyz d1 d2)
             c21 (xy-or-xyz d2 d1)]
         (doseq [[unit-a ->unit-a] units-list]
           (testing "equality"
             (is (= (unit-a c12) (unit-a c12)) "Two instances with the same unit-type and value should be equal."))
           (testing "hash equality"
             (is (= (hash (unit-a d1)) (hash (unit-a d1))) "The hashes of two instances with the same value should be equal."))
           (testing "round trip through a unit"
             (is (approx= c12 (->unit-a (unit-a c12))) "Unit values should make it in and out of a Quantity."))
           #?(:clj (is (= (unit-a c12) (read-string (pr-str (unit-a c12)))) "Quantities should be converted to string and back."))
           (testing "transit encoding and decoding"
             (let [write-handlers {:handlers (:write quantus.transit/handlers)}
                   read-handlers {:handlers (:read quantus.transit/handlers)}
                   uad (unit-a c12)]
               #?(:clj
                  (let [out (ByteArrayOutputStream. 4096)
                        w (transit/writer out :json write-handlers)
                        _ (transit/write w uad)
                        in (ByteArrayInputStream. (.toByteArray out))
                        r (transit/reader in :json read-handlers)]
                    (is (= uad (transit/read r))
                        "Quantities should be converted to transit and back."))
                  :cljs
                  (let [encoded-transit-data (transit/write (transit/writer :json write-handlers) uad)
                        data (transit/read (transit/reader :json read-handlers) encoded-transit-data)]
                    (is (= uad data) "Quantities should be converted to transit and back.")))))


           #?(:clj
              (doseq [[unit-b ->unit-b] units-list
                      :when (and (not= unit-a identity)
                                 (not= unit-b identity))]
                (let [qa (unit-a c12)
                      qb (unit-b c12)]
                  (if (q/unit-type-match? qa qb)
                    (testing "Two quantities with matching unit-types should work together."
                      (let [ua1 (unit-a c12)
                            ub1 (unit-b c12)
                            ua2 (unit-a c21)
                            ub2 (unit-b c21)]
                        (is (approx= (qm/+ c12 c21) (->unit-a (qm/+ ua1 ua2))))
                        (is (approx= (qm/+ c12 c21) (->unit-b (qm/+ ub1 ub2))))
                        (is (->unit-a (qm/+ ua1 ub1 ua2 ub2)))
                        (is (->unit-b (qm/+ ua1 ub1 ua2 ub2))))))
                  (when (contains? (:multiplications q/allowed-operations)
                                   [(q/unit-type qa) (q/unit-type qb)])
                    (testing "Two quantities that may be multiplied"
                      (is (qm/* (unit-a d1) (unit-b d2)))
                      (is (qm/* (unit-b d2) (unit-a d1)))))
                  (when (contains? (:divisions q/allowed-operations)
                                   [(q/unit-type qa) (q/unit-type qb)])
                    (testing "Two quantities that may be divided"
                      (is (qm// (unit-a d1) (unit-b d2))))))))))))))

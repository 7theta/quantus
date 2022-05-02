(ns quantus.angles-test
  (:require [quantus.angles :as sut]
            [quantus.math :as qm]
            [quantus.core-test :refer [approx=]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck :refer [times]]
            [com.gfredericks.test.chuck.clojure-test #?(:clj :refer :cljs :refer-macros) [checking]]
            #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing] :include-macros true])
            [cognitect.transit :as transit]
            #?(:cljs [cljs.reader :refer [read-string]])
            [clojure.core :as core])
  #?(:clj (:import [java.io ByteArrayInputStream ByteArrayOutputStream])))

(def units-list
  [[sut/degrees sut/->degrees]
   [sut/radians sut/->radians]])

(deftest angles-test
  (let [double-features {:NaN? false :min 1E-5 :max 3.14}]
    (checking
     "Single and dual angle operations: "
     [d1 (gen/double* double-features)
      d2 (gen/double* double-features)]
     (doseq [[unit-a ->unit-a] units-list]
       (is (approx= d1 (->unit-a (unit-a d1))) "Unit values should make it in and out of a Quantity.")

       #?(:clj (is (= (unit-a d1) (read-string (pr-str (unit-a d1)))) "Quantities should be converted to string and back."))
       (testing "Quantities should be converted to transit and back."

         (let [write-handlers {:handlers (:write quantus.transit/handlers)}
               read-handlers {:handlers (:read quantus.transit/handlers)}
               uad (unit-a d1)]
           #?(:clj
              (let [out (ByteArrayOutputStream. 4096)
                    w (transit/writer out :json write-handlers)
                    _ (transit/write w uad)
                    in (ByteArrayInputStream. (.toByteArray out))
                    r (transit/reader in :json read-handlers)]
                (is (= uad
                       (transit/read r))))
              :cljs
              (let [encoded-transit-data (transit/write (transit/writer :json write-handlers) uad)
                    data (transit/read (transit/reader :json read-handlers) encoded-transit-data)]
                (is (= uad data))))))
       (doseq [[unit-b ->unit-b] units-list]
         (testing "Angle Quantities should work together."
           (is (approx= d1 (-> d1
                               unit-a
                               ->unit-b
                               unit-b
                               ->unit-a)))
           (is (approx= d1 (-> d1
                               unit-b
                               ->unit-a
                               unit-a
                               ->unit-b)))
           (let [d1h (/ d1 2)
                 d2h (/ d2 2)
                 ua1 (unit-a d1h)
                 ub1 (unit-b d1h)
                 ua2 (unit-a d2h)
                 ub2 (unit-b d2h)
                 a180 (sut/degrees 180)]
             (is (approx= (+ d1h d2h) (->unit-a (qm/+ ua1 ua2)) 1E-4))
             (is (approx= (+ d1h d2h) (->unit-b (qm/+ ub1 ub2)) 1E-4))
             (is (approx= (+ d1h d2h) (->unit-b (qm/+ ub1 a180 ub2 a180)) 1E-4)))))))))

(ns quantus.core-test
  (:require [quantus.core :as sut]
            [quantus.math :refer [abs] :as qm]
            [quantus.transit :refer [handlers]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck :refer [times]]
            [com.gfredericks.test.chuck.clojure-test #?(:clj :refer :cljs :refer-macros) [checking]]
            #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing] :include-macros true])
            [cognitect.transit :as transit]
            #?(:cljs [cljs.reader :refer [read-string]])
            [quantus.angles :as qa])
  #?(:clj (:import [java.io ByteArrayInputStream ByteArrayOutputStream])))

(def units-list [[sut/meters sut/to-meters]
                 [sut/feet sut/to-feet]
                 [sut/mps sut/to-mps]
                 [sut/kn sut/to-kn]
                 [sut/fpm sut/to-fpm]
                 [sut/kg sut/to-kg]
                 [sut/lb sut/to-lb]
                 [sut/seconds sut/to-seconds]
                 [sut/minutes sut/to-minutes]
                 [sut/hours sut/to-hours]
                 [sut/kelvin sut/to-kelvin]
                 [sut/rankine sut/to-rankine]
                 [sut/celsius sut/to-celsius]
                 [sut/fahrenheit sut/to-fahrenheit]
                 [sut/unitless sut/to-unitless]])

(defn approx=
  ([x y]
   #_(if (zero? x)
       (< (abs y) 1E-50)
       (< (/ (abs (- x y)) (+ x y)) 1E-5))
   (qm/approx= x y (+ (abs (* x 1E-5)) 1E-50)))
  ([x y e] (qm/approx= x y e)))

(deftest unit-test ;har-har
  (let [double-features {:NaN? false :min 1E-8 :max 1E50}]
    (checking
     "Single and dual unit operations: "
     [d1 (gen/double* double-features)
      d2 (gen/double* double-features)]
     (doseq [[unit-a to-unit-a] units-list]
       (is (approx= d1 (to-unit-a (unit-a d1))) "Unit values should make it in and out of a Quantity.")
       #?(:clj (is (= (unit-a d1) (read-string (pr-str (unit-a d1)))) "Quantities should be converted to string and back."))
       (let [write-handlers {:handlers (:write quantus.transit/handlers)}
             read-handlers {:handlers (:read quantus.transit/handlers)}
             uad (unit-a d1)]
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
              (is (= uad data) "Quantities should be converted to transit and back."))))


       (doseq [[unit-b to-unit-b] units-list]
         (let [qa (unit-a 1)
               qb (unit-b 1)]
           (if (sut/unit-type-match? qa qb)
             (testing "Two quantities with matching unit-types should work together."
               (is (approx= d1 (-> d1
                                   unit-a
                                   to-unit-b
                                   unit-b
                                   to-unit-a)))
               (is (approx= d1 (-> d1
                                   unit-b
                                   to-unit-a
                                   unit-a
                                   to-unit-b)))
               (when-not (or (#{sut/celsius sut/fahrenheit} unit-a)
                             (#{sut/celsius sut/fahrenheit} unit-b))
                 (let [ua1 (unit-a d1)
                       ub1 (unit-b d1)
                       ua2 (unit-a d2)
                       ub2 (unit-b d2)]
                   (is (approx= (+ d1 d2) (to-unit-a (qm/+ ua1 ua2))))
                   (is (approx= (+ d1 d2) (to-unit-b (qm/+ ub1 ub2))))
                   (is (to-unit-a (qm/+ ua1 ub1 ua2 ub2)))
                   (is (to-unit-b (qm/+ ua1 ub1 ua2 ub2))))))
             (testing "Two quantities with different unit-types shouldn't work together."
               (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                                     #"Quantities must have the same unit type."
                                     (qm/+ (unit-a d1) (unit-b d2))))
               (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                                     #"Quantities must have the same unit type."
                                     (qm/- (unit-a d1) (unit-b d2))))))
           (if (contains? (:multiplications sut/allowed-operations)
                          [(:unit-type qa) (:unit-type qb)])
             (testing "Two quantities that may be multiplied"
               (is (qm/* (unit-a d1) (unit-b d2)))
               (is (qm/* (unit-b d2) (unit-a d1))))
             (testing "Two quantities that may not be multiplied"
               (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                                     #"Multiplying two Quantities must result in a known unit-type"
                                     (qm/* (unit-a d1) (unit-b d2))))
               (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                                     #"Multiplying two Quantities must result in a known unit-type"
                                     (qm/* (unit-b d2) (unit-a d1))))))
           (if (contains? (:divisions sut/allowed-operations)
                          [(:unit-type qa) (:unit-type qb)])
             (testing "Two quantities that may be divided"
               (when-not (zero? d2)
                 (is (qm// (unit-a d1) (unit-b d2)))))
             (testing "Two quantities that may not be divided"
               (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                                     #"Dividing two Quantities must result in a known unit-type"
                                     (qm// (unit-a d1) (unit-b d2))))))))))))

(deftest temperatures
  (testing "Basic temperature conversions"
    (is (approx= 273.15 (sut/to-kelvin (sut/fahrenheit 32))))
    (is (approx= -273.15 (sut/to-celsius (sut/rankine 0))))))

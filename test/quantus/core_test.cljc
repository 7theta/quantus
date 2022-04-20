(ns quantus.core-test
  (:require [quantus.core :as sut]
            [quantus.math :refer [abs] :as qm]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck :refer [times]]
            [com.gfredericks.test.chuck.clojure-test #?(:clj :refer :cljs :refer-macros) [checking]]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is] :include-macros true])))
#_(ns
      (:require
       #?(:clj [clojure.test :as t]
          :cljs [cljs.test :as t :include-macros true])))

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
  [x y]
  #_(if (zero? x)
      (< (abs y) 1E-50)
      (< (/ (abs (- x y)) (+ x y)) 1E-5))
  (qm/approx= x y (+ (abs (* x 1E-5)) 1E-50)))

(deftest unit-test ;har-har
  (let [double-features {:NaN? false :min 1E-8 :max 1E50}]
    (doseq [[unit-a to-unit-a] units-list]
      (checking "Unit values should make it in and out of a Quantity."
                [d (gen/double* double-features)]
                (is (approx= d (to-unit-a (unit-a d)))))
      (checking "Quantities should be converted to string and back."
                [d (gen/double* double-features)]
                (is (= (unit-a d)
                       (-> (unit-a d)
                           pr-str
                           read-string))))
      (doseq [[unit-b to-unit-b] units-list]
        (let [qa (unit-a 1)
              qb (unit-b 1)]
          (if (sut/unit-type-match? qa qb)
            (checking "Two quantities with matching unit-types should work together."
                      [d1 (gen/double* double-features)
                       d2 (gen/double* double-features)]
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
                          (is (approx= (+ d1 d2) (to-unit-b (qm/+ ub1 ub2)))))))
            (checking "Two quantities with different unit-types shouldn't work together."
                      [d1 (gen/double* double-features)
                       d2 (gen/double* double-features)]
                      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                                            #"Quantities must have the same unit type."
                                            (qm/+ (unit-a d1) (unit-b d2))))
                      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                                            #"Quantities must have the same unit type."
                                            (qm/- (unit-a d1) (unit-b d2))))))
          (checking "Two quantities may be multiplied or divided"
                    [d1 (gen/double* double-features)
                     d2 (gen/double* double-features)]
                    (if (contains? (:multiplications sut/allowed-operations)
                                   [(:unit-type qa) (:unit-type qb)])
                      (do (is (qm/* (unit-a d1) (unit-b d2)))
                          (is (qm/* (unit-b d2) (unit-a d1))))
                      (do (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                                                #"Multiplying two Quantities must result in a known unit-type"
                                                (qm/* (unit-a d1) (unit-b d2))))
                          (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                                                #"Multiplying two Quantities must result in a known unit-type"
                                                (qm/* (unit-b d2) (unit-a d1))))))
                    (if (contains? (:divisions sut/allowed-operations)
                                   [(:unit-type qa) (:unit-type qb)])
                      (when-not (zero? d2)
                        (is (qm// (unit-a d1) (unit-b d2))))
                      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                                            #"Dividing two Quantities must result in a known unit-type"
                                            (qm// (unit-a d1) (unit-b d2)))))))))))

#_(deftest temperatures
    (testing "Basic temperature conversions"
      (is (= 273.15 (sut/to-kelvin)))))

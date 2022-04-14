(ns quantus.units)

(def kg-per-lb 0.453592)
(defn lb->kg [lb] (* lb kg-per-lb))
(defn kg->lb [kg] (/ kg kg-per-lb))

(def meters-per-foot 0.3048)
(defn feet->meters [feet] (* feet meters-per-foot))
(defn meters->feet [meters] (/ meters meters-per-foot))

(def seconds-per-minute 60.0)
(defn minutes->seconds [minutes] (* minutes seconds-per-minute))
(defn seconds->minutes [seconds] (/ seconds seconds-per-minute))

(def seconds-per-hour 3600.0)
(defn hours->seconds [hours] (* hours seconds-per-hour))
(defn seconds->hours [seconds] (/ seconds seconds-per-hour))

(def mps-per-kn 0.51444444)
(defn kn->mps [kn] (* kn mps-per-kn))
(defn mps->kn [mps] (/ mps mps-per-kn))

(def mps-per-fpm (/ meters-per-foot seconds-per-minute))
(defn fpm->mps [fpm] (* fpm mps-per-fpm))
(defn mps->fpm [mps] (/ mps mps-per-fpm))

;; (def kn-per-fpm (/ mps-per-kn mps-per-fpm))
;; (defn kn->fpm [kn] (* kn kn-per-fpm))
;; (defn fpm->kn [fpm] (/ fpm kn-per-fpm))

(defn- multiplication
  [combos new-combo unit-1 unit-2]
  (-> combos
      (update :types conj new-combo unit-1 unit-2)
      (update :multiplications assoc [unit-1 unit-2] new-combo)
      (update :multiplications assoc [unit-2 unit-1] new-combo)
      (update :divisions assoc [new-combo unit-1] unit-2)
      (update :divisions assoc [new-combo unit-2] unit-1)))

(defn- division
  [combos new-combo numerator-unit denominator-unit]
  (-> combos
      (update :types conj new-combo numerator-unit denominator-unit)
      (update :divisions assoc [numerator-unit denominator-unit] new-combo)
      (update :divisions assoc [numerator-unit new-combo] denominator-unit)
      (update :multiplications assoc [new-combo denominator-unit] numerator-unit)
      (update :multiplications assoc [denominator-unit new-combo] numerator-unit)))

(def ^:private reserved-types #{:angle})

(def unit
  (-> {:multiplications {} :divisions {} :types #{}}
      (multiplication :area :length :length)
      (division :speed :length :time)
      (division :acceleration :speed :time)
      ((fn add-unitless-operations
         [{:keys [types] :as u}]
         (reduce (fn [uu t]
                   (multiplication uu t t :unitless))
                 u
                 types)))
      (division :frequency :unitless :time)
      ((fn check-for-reserved-unit-types
         [{:keys [types] :as u}]
         (when-let [used-reserved-types (seq (clojure.set/intersection types reserved-types))]
           (throw (ex-info "A reserved type was used." {:used-reserved-types used-reserved-types})))
         u))))

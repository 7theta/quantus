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

(def kelvin-per-rankine (/ 5.0 9.0))
(def celsius-kelvin-offset 273.15)
(def fahrenheit-rankine-offset 459.67)
(defn celsius->kelvin [celsius] (+ celsius celsius-kelvin-offset))
(defn kelvin->celsius [kelvin] (- kelvin celsius-kelvin-offset))
(defn fahrenheit->kelvin [fahrenheit] (* (+ fahrenheit fahrenheit-rankine-offset) kelvin-per-rankine))
(defn kelvin->fahrenheit [kelvin] (- (/ kelvin kelvin-per-rankine) fahrenheit-rankine-offset))
(defn rankine->kelvin [rankine] (* rankine kelvin-per-rankine))
(defn kelvin->rankine [kelvin] (/ kelvin kelvin-per-rankine))

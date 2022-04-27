(ns quantus.units)

(def ^:private kilograms-per-pound 0.453592)
(defn pounds->kilograms [pounds] (* pounds kilograms-per-pound))
(defn kilograms->pounds [kilograms] (/ kilograms kilograms-per-pound))

(def ^:private meters-per-foot 0.3048)
(defn feet->meters [feet] (* feet meters-per-foot))
(defn meters->feet [meters] (/ meters meters-per-foot))

(def ^:private seconds-per-minute 60.0)
(defn minutes->seconds [minutes] (* minutes seconds-per-minute))
(defn seconds->minutes [seconds] (/ seconds seconds-per-minute))

(def ^:private seconds-per-hour 3600.0)
(defn hours->seconds [hours] (* hours seconds-per-hour))
(defn seconds->hours [seconds] (/ seconds seconds-per-hour))

(def ^:private meters-per-second-per-knot 0.51444444)
(defn knots->meters-per-second [knots] (* knots meters-per-second-per-knot))
(defn meters-per-second->knots [meters-per-second]
  (/ meters-per-second meters-per-second-per-knot))

(def ^:private meters-per-second-per-foot-per-minute (/ meters-per-foot seconds-per-minute))
(defn feet-per-minute->meters-per-second [feet-per-minute]
  (* feet-per-minute meters-per-second-per-foot-per-minute))
(defn meters-per-second->feet-per-minute [meters-per-second]
  (/ meters-per-second meters-per-second-per-foot-per-minute))

(def ^:private knots-per-foot-per-minute
  (/ meters-per-second-per-knot meters-per-second-per-foot-per-minute))
(defn knots->feet-per-minute [knots] (* knots knots-per-foot-per-minute))
(defn feet-per-minute->knots [feet-per-minute] (/ feet-per-minute knots-per-foot-per-minute))

(def ^:private kelvin-per-rankine (/ 5.0 9.0))
(def ^:private celsius-kelvin-offset 273.15)
(def ^:private fahrenheit-rankine-offset 459.67)
(defn celsius->kelvin [celsius] (+ celsius celsius-kelvin-offset))
(defn kelvin->celsius [kelvin] (- kelvin celsius-kelvin-offset))
(defn fahrenheit->kelvin [fahrenheit] (* (+ fahrenheit fahrenheit-rankine-offset) kelvin-per-rankine))
(defn kelvin->fahrenheit [kelvin] (- (/ kelvin kelvin-per-rankine) fahrenheit-rankine-offset))
(defn rankine->kelvin [rankine] (* rankine kelvin-per-rankine))
(defn kelvin->rankine [kelvin] (/ kelvin kelvin-per-rankine))

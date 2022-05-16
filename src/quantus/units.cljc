(ns quantus.units)

(def ^:private kilograms-per-pound 0.453592)
(defn pounds->kilograms [pounds] (* pounds kilograms-per-pound))
(defn kilograms->pounds [kilograms] (/ kilograms kilograms-per-pound))

(def ^:private grams-per-kilogram 1000.0)
(defn kilograms->grams [kilograms] (* kilograms grams-per-kilogram))
(defn grams->kilograms [grams] (/ grams grams-per-kilogram))

(def ^:private ounces-per-kilogram 35.274)
(defn kilograms->ounces [kilograms] (* kilograms ounces-per-kilogram))
(defn ounces->kilograms [ounces] (/ ounces ounces-per-kilogram))

(def ^:private ounces-per-pound 16.0)
(defn pounds->ounces [pounds] (* pounds ounces-per-pound))
(defn ounces->pounds [ounces] (/ ounces ounces-per-pound))

(def ^:private grams-per-ounce (/ (* kilograms-per-pound grams-per-kilogram) ounces-per-pound))
(defn ounces->grams [ounces] (* ounces grams-per-ounce))
(defn grams->ounces [grams] (/ grams grams-per-ounce))

(def ^:private grains-per-ounce 437.5)
(defn ounces->grains [ounces] (* ounces grains-per-ounce))
(defn grains->ounces [grains] (/ grains grains-per-ounce))

(def ^:private grains-per-kilogram (* grains-per-ounce ounces-per-kilogram))
(defn kilograms->grains [kilograms] (* kilograms grains-per-kilogram))
(defn grains->kilograms [grains] (/ grains grains-per-kilogram))

(def ^:private centimeters-per-meter 100.0)
(defn meters->centimeters [meters] (* meters centimeters-per-meter))
(defn centimeters->meters [centimeters] (/ centimeters centimeters-per-meter))

(def ^:private meters-per-foot 0.3048)
(defn feet->meters [feet] (* feet meters-per-foot))
(defn meters->feet [meters] (/ meters meters-per-foot))

(def ^:private centimeters-per-inch 2.54)
(defn inches->centimeters [inches] (* inches centimeters-per-inch))
(defn centimeters->inches [centimeters] (/ centimeters centimeters-per-inch))

(def ^:private inches-per-foot 12.0)
(defn feet->inches [feet] (* feet inches-per-foot))
(defn inches->feet [inches] (/ inches inches-per-foot))

(def ^:private centimeters-squared-per-meter-squared 10000.0)
(defn meters-squared->centimeters-squared [meters-squared] (* meters-squared centimeters-squared-per-meter-squared))
(defn centimeters-squared->meters-squared [centimeters-squared] (/ centimeters-squared centimeters-squared-per-meter-squared))

(def ^:private inches-squared-per-meter-squared 1550.0)
(defn meters-squared->inches-squared [meters-squared] (* meters-squared inches-squared-per-meter-squared))
(defn inches-squared->meters-squared [inches-squared] (/ inches-squared inches-squared-per-meter-squared))

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

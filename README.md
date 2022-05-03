# com.7theta/quantus

[![Current Version](https://img.shields.io/clojars/v/com.7theta/quantus.svg)](https://clojars.org/com.7theta/quantus)
[![GitHub license](https://img.shields.io/github/license/7theta/quantus.svg)](LICENSE)

Library for handing quantities with units.

# Adding new Unit Types and Units
There are several files that need to be updated to include a new unit type, or a new unit for an existing type.

## New Unit Type (e.g. pressure)
* Identify the base unit, most likely an SI unit (e.g. pascals).  All quantities for this unit type will be converted to this SI unit.
* The base unit quantities must be compatible with each other.  For instance, 1 meter divided by 1 second is 1 meter-per-second.
  * Can a test be added for this?  What happens if this rule is broken?
* Add it to `si-units` in [core.cljc](src/quantus/core.cljc) (e.g. `:pressure “pascals”`)
* Add any `allowed-operations` in [core.cljc](src/quantus/core.cljc)
  * How is it related to other unit types? (e.g. `(division :pressure :force :area)`)
* If the unit type doesn’t follow normal math rules (e.g. like angles), this it can’t be added to core.cljc and will require a new namespace with a new deftype and separate math implementations.  See [angles.cljc](src/quantus/angles.cljc).

## New Units (e.g. atmospheres, mmHg)
* Identify the unit-type (e.g. pressure).  Implement the new unit type if necessary.
* If it’s not a base/SI unit, add the conversion constants and functions to/from in [units.cljc](src/quantus/units.cljc).
  * Conversions to/from non-base units is not required for quantus, but may be convenient for anyone using the conversions directly.
* Add the function to create a quantity and extract the value in [core.cljc](src/quantus/core.cljc).  Remember to put the correct conversion function and unit type.  If it’s a base/SI unit, then you don’t need the unit conversions.
```clojure
(defn millimeters-Mercury [v] (Quantity. (u/millimeters-Mercury->pascals v) :pressure))
(defn ->millimeters-Mercury [^Quantity q] (assert-unit-type q :pressure) (u/pascals->millimeters-Mercury (get-value q)))
```
* Add the new unit to [data_readers.cljc](src/data_readers.cljc).
* Add the new unit to the `units-list` in [core_test.cljc](test/quantus/core_test.cljc).  This will add it to the suite of tests performed on each unit type, as well as every pair of unit types.
  * Note that units that have an offset (like temperature) will need to be skipped for some tests (celsius and fahrenheit are already excluded from some tests, so add it there).
* Run the tests `lein test` and `npx shadow-cljs release test`

## Copyright and License

Copyright © 2021 7theta

Distributed under the MIT License.


;;   Copyright (c) 7theta. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   MIT License (https://opensource.org/licenses/MIT) which can also be
;;   found in the LICENSE file at the root of this distribution.
;;
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any others, from this software.

(defproject com.7theta/quantus "0.8.0"
  :description "Library of handling quantities with units"
  :url "https://github.com/7theta/quantus"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[com.7theta/utilis "2.3.1"]]
  :profiles {:dev {:global-vars {*warn-on-reflection* true}
                   :dependencies [[org.clojure/clojure "1.11.1"]
                                  [org.clojure/clojurescript "1.11.54"]
                                  [org.clojure/tools.namespace "1.3.0"]
                                  [org.clojure/test.check "1.1.1"]
                                  [com.gfredericks/test.chuck "0.2.13"]
                                  [thheller/shadow-cljs "2.19.0"]
                                  [criterium "0.4.6"]]
                   :source-paths ["dev" "example/src"]}}
  :clean-targets ^{:protect false} ["out" "target"]
  :scm {:name "git"
        :url "https://github.com/7theta/quantus"})

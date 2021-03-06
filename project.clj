(defproject simple_recommendations "0.1.0-SNAPSHOT"
  :description "Collection of simple recommendations"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                  [org.clojure/math.numeric-tower "0.0.2"]]
  :plugins [[lein-midje "3.0.0"]]
  :profiles {:dev {:dependencies [[midje "1.6.0"]
                                   [clojure-csv "2.0.1"]
                                   [criterium "0.4.2"]]}})

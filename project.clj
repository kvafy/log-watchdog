(defproject log-watchdog "0.1.4-SNAPSHOT"
  :description "Watches log files for suspicious lines and notifies when something is found."
  :url "https://github.com/kvafy/log-watchdog"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [prismatic/schema "0.4.3"]]
  :main ^:skip-aot log-watchdog.ui
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.11"]]}
             :uberjar {:aot :all}})

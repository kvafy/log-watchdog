(defproject log-watchdog "0.1.1-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 ;[bouncer "0.3.3"] ; bouncer is too big a library, adds 13 MB to uberjar
                 ]
  :main ^:skip-aot log-watchdog.ui
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.11"]]}
             :uberjar {:aot :all}})

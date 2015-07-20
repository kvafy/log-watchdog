(ns log-watchdog.config-test
  (:require [clojure.test :refer :all]
            [schema.core :as s]
            [log-watchdog.config :as config]
            [log-watchdog.validators :as validators]))

(let [cfg-raw "{ :check-interval-ms 5000
                 :nagging-interval-ms 60000
                 :files
                   { \"file1\"
                       { :line-regex \"^ERROR.*$\" }
                     \"file2\"
                       { :line-regex \"^WARN.*$\"}
                   }
               }"]
  (deftest load-configuration-test
    (testing "verification of loaded configuration against schema"
      (s/validate validators/configuration (config/load-configuration cfg-raw)))
    (testing "correct values are loaded"
      (let [cfg (config/load-configuration cfg-raw)]
        (is (= 5000 (:check-interval-ms cfg)))
        (is (= 60000 (:nagging-interval-ms cfg)))
        (is (= 2 (count (:files cfg))))
        (is (= #{"file1" "file2"} (set (keys (:files cfg)))))))))

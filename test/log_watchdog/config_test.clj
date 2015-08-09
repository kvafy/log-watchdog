(ns log-watchdog.config-test
  (:require [clojure.test :refer :all]
            [schema.core :as s]
            [log-watchdog.config :as config]
            [log-watchdog.validators :as validators]))


(def valid-configuration-raw
  "{ :check-interval-ms 5000
     :nagging-interval-ms 60000
     :files
       { \"file1\"
           { :line-regex \"^ERROR.*$\" :file-group \"file-group-1\" }
         \"file2\"
           { :line-regex \"^WARN.*$\"}
       }
   }")


(def valid-configuration
  { :check-interval-ms 5000
    :nagging-interval-ms 60000
    :files
      { "file1"
          { :line-regex #"^ERROR.*$" :file-group "file-group-1"}
        "file2"
          { :line-regex #"^WARN.*$" :file-group config/default-watched-file-group-name}}})


(deftest configuration-validator-test
  (testing "valid configuration is accepted by the validator (failure means either the validator or the valid-configuration is off)"
    (s/validate validators/configuration valid-configuration))
  (testing "zero files is a valid scenario"
    (s/validate validators/configuration (assoc valid-configuration :files {})))
  (testing "missing entries are detected as errors"
    (is (thrown? Exception (s/validate validators/configuration (dissoc valid-configuration :check-interval-ms))))
    (is (thrown? Exception (s/validate validators/configuration (dissoc valid-configuration :nagging-interval-ms))))
    (is (thrown? Exception (s/validate validators/configuration (dissoc valid-configuration :files))))
    (is (thrown? Exception (s/validate validators/configuration (update-in valid-configuration [:files "file_path.log"] dissoc :line-regex)))))
  (testing "extra entries are detected as errors"
    (is (thrown? Exception (s/validate validators/configuration (assoc valid-configuration :extra-key "extra value"))))))


(deftest load-configuration-test
  (testing "verification of loaded configuration against schema"
    (s/validate validators/configuration (config/load-configuration valid-configuration-raw)))
  (testing "correct values are loaded"
    (let [cfg (config/load-configuration valid-configuration-raw)]
      (is (= 5000 (:check-interval-ms cfg)))
      (is (= 60000 (:nagging-interval-ms cfg)))
      (is (= 2 (count (:files cfg))))
      (is (= #{"file1" "file2"} (set (keys (:files cfg)))))
      (is (= "file-group-1" (get-in cfg [:files "file1" :file-group])))
      (is (= config/default-watched-file-group-name (get-in cfg [:files "file2" :file-group]))))))

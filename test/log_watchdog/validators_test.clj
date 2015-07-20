(ns log-watchdog.validators-test
  (:require [clojure.test :refer :all]
            [schema.core :as s]
            [log-watchdog.validators :as validators]))


(let [valid-cfg {:check-interval-ms 5000
                 :nagging-interval-ms 60000
                 :files {"file_path.log" {:line-regex #"^ERROR.*$"}}}]
  (deftest configuration-validator-test
    (testing "valid configuration is accepted by the validator"
      (s/validate validators/configuration valid-cfg))
    (testing "zero files is a valid scenario"
      (s/validate validators/configuration (assoc valid-cfg :files {})))
    (testing "missing entries are detected as errors"
      (is (thrown? Exception (s/validate validators/configuration (dissoc valid-cfg :check-interval-ms))))
      (is (thrown? Exception (s/validate validators/configuration (dissoc valid-cfg :nagging-interval-ms))))
      (is (thrown? Exception (s/validate validators/configuration (dissoc valid-cfg :files))))
      (is (thrown? Exception (s/validate validators/configuration (update-in valid-cfg [:files "file_path.log"] dissoc :line-regex)))))
    (testing "extra entries are detected as errors"
      (is (thrown? Exception (s/validate validators/configuration (assoc valid-cfg :extra-key "extra value")))))))

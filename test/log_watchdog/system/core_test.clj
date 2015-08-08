(ns log-watchdog.system.core-test
  (:require [clojure.test :refer :all]
            [schema.core :as s]
            [log-watchdog.system.core :as core]
            [log-watchdog.config :as config]
            [log-watchdog.validators :as validators]
            [log-watchdog.utils :as utils]
            [log-watchdog.config-test :as config-test]))


;; definition of a testing system

(def config-id (utils/uuid))
(def file1-id (utils/uuid))
(def file2-id (utils/uuid))
(def file-group1-id (utils/uuid))
(def file-group-default-id (utils/uuid))
(def alert1-id (utils/uuid))
(def alert2-id (utils/uuid))
(def alert3-id (utils/uuid))
(def alert4-id (utils/uuid))
(def notifications-id (utils/uuid))

(def system-orig
  { config-id
      { :type :configuration
        :check-enabled true
        :check-interval-ms 5000
        :nagging-interval-ms 60000}

    file1-id
      { :type :watched-file
        :file (java.io.File. "file-path-1")
        :line-regex #".*"
        :last-check-failed false
        :watched-file-group-id file-group1-id}
    alert1-id
      { :type :alert
        :matched-line "line-1"
        :acknowledged false
        :watched-file-id file1-id}
    alert2-id
      { :type :alert
        :matched-line "line-2"
        :acknowledged true
        :watched-file-id file1-id}

    file2-id
      { :type :watched-file
        :file (java.io.File. "file-path-2")
        :line-regex #".*"
        :last-check-failed true
        :watched-file-group-id file-group-default-id}
    alert3-id
      { :type :alert
        :matched-line "line-3"
        :acknowledged true
        :watched-file-id file2-id}
    alert4-id
      { :type :alert
        :matched-line "line-4"
        :acknowledged false
        :watched-file-id file2-id}

    file-group1-id
      { :type :watched-file-group
        :name "group1"}
    file-group-default-id
      { :type :watched-file-group
        :name nil}

    notifications-id
      { :type :notifications
        :last-notification-timestamp 0N}
  })


(deftest system-validator-test
  (testing "valid system is accepted by the validator (failure means either the system is invalid or the validator is invalid)"
    (validators/validate-system system-orig)))


(deftest create-system-test
  (let [created-system (core/create-system config-test/valid-configuration)]
    (testing "creating a system based on valid configuration yields a structurally valid system"
      (validators/validate-system created-system))))


(deftest query-test
  (testing "Impossible predicate results in no entity being found."
    (let [result (core/query system-orig (core/entity-pred :type (partial = :alert)
                                                           :type (partial = :watched-file)))]
      (is (map? result))
      (is (empty? result))))
  (testing "Empty predicate returns all entities (the whole system)."
    (let [all-entities (core/query system-orig (core/entity-pred))]
      (is (map? all-entities))
      (is (= all-entities system-orig))))
  (testing "Fetch files"
    (let [files (core/query system-orig (core/entity-pred :type (partial = :watched-file)))]
      (is (map? files))
      (let [file-ids (map first files)]
        (is (= #{file1-id file2-id} (set file-ids))))))
  (testing "Fetch alerts"
    (let [alerts (core/query system-orig (core/entity-pred :type (partial = :alert)))]
      (is (map? alerts))
      (let [alert-ids (map first alerts)]
        (is (= #{alert1-id alert2-id alert3-id alert4-id} (set alert-ids)))))))

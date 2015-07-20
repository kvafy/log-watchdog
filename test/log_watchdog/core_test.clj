(ns log-watchdog.core-test
  (:require [clojure.test :refer :all]
            [schema.core :as s]
            [log-watchdog.core :as core]
            [log-watchdog.config :as config]
            [log-watchdog.validators :as validators]
            [log-watchdog.config-test :as config-test])
  (:import [java.io BufferedReader StringReader]))

;; definition of testing systems

(def valid-system
  { :check-interval-ms 5000
    :nagging-interval-ms 60000
    :last-notification-timestamp 123456789
    :files
      { "file-path-1"
          { :line-regex #"^ERROR.*"
            :alerts
              { "line-1"
                  { :last-seen-timestamp 123456799
                    :acknowledged false}
                "line-2"
                  { :last-seen-timestamp 123456799
                    :acknowledged true}}}
        "file-path-2"
          { :line-regex #"xy"
            :alerts
              { "line-3"
                  { :last-seen-timestamp 0
                    :acknowledged true}
                "line-4"
                  { :last-seen-timestamp 0
                    :acknowledged true}}}}})


(deftest system-validator-test
  (testing "valid system is accepted by the validator"
    (s/validate validators/system valid-system)))


(deftest create-system-test
  (testing "creating a system based on valid configuration yields a valid system"
    (s/validate validators/system (core/create-system config-test/valid-configuration))))


(deftest file-paths-test
  (testing "get all file paths"
    (is (= #{"file-path-1" "file-path-2"} (set (core/file-paths valid-system)))))
  (testing "get file paths matching predicate"
    (is (= #{"file-path-1"} (set (core/file-paths valid-system core/file-has-unacknowledged-alert?))))
    (is (= #{"file-path-2"} (set (core/file-paths valid-system (complement core/file-has-unacknowledged-alert?)))))))


(deftest alerts-test
  (testing "get all alerts from all files"
    (is (= #{"line-1" "line-2" "line-3" "line-4"} (set (keys (core/alerts valid-system))))))
  (testing "get all alerts from specified files"
    (is (= #{"line-1" "line-2" "line-3" "line-4"} (set (keys (core/alerts valid-system ["file-path-1" "file-path-2"]))))))
  (testing "get all alerts from specified file"
    (is (= #{"line-1" "line-2"} (set (keys (core/alerts valid-system ["file-path-1"]))))))
  (testing "get alerts from specified files matching predicate"
    (is (= #{"line-1"} (set (keys (core/alerts valid-system ["file-path-1"] core/unacknowledged-alert?)))))
    (is (= #{"line-2"} (set (keys (core/alerts valid-system ["file-path-1"] (complement core/unacknowledged-alert?))))))))


(deftest has-new-alert?-test
  (testing "unchanged system cannot have a new alert"
    (is (not (core/has-new-alert? valid-system valid-system))))
  (testing "new alert detected when there is one"
    (let [new-system (assoc-in valid-system
                               [:files "file-path-1" :alerts]
                               {"line-new" {:last-seen-timestamp 123456799, :acknowledged true}})]
      (s/validate validators/system new-system) ; make sure the hand-crafted system is valid
      (is (core/has-new-alert? valid-system new-system)))))


(deftest update-system-by-checking-files-test
  (with-redefs-fn {#'clojure.java.io/reader (fn [file-path & _]
                                              (let [file-data (if (= file-path "file-path-1")
                                                                "ERROR new error"
                                                                "")]
                                                (BufferedReader. (StringReader. file-data))))}
    (fn []
      (let [new-system (core/update-system-by-checking-files valid-system)]
        (s/validate validators/system new-system)
        (is (contains? (core/alerts new-system) "ERROR new error"))))))


(deftest update-system-by-acknowledging-alerts-test
  (let [new-system (core/update-system-by-acknowledging-alerts valid-system)]
    (s/validate validators/system new-system)
    (is (empty? (core/alerts new-system ["file-path-1"] core/unacknowledged-alert?)))))


(deftest update-system-by-setting-last-notification-timestamp-test
  (let [old-timestamp (:last-notification-timestamp valid-system)
        new-timestamp (inc (* 2 old-timestamp))
        new-system (core/update-system-by-setting-last-notification-timestamp valid-system new-timestamp)]
    (s/validate validators/system new-system)
    (is (= new-timestamp (:last-notification-timestamp new-system)))))

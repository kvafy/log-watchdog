(ns log-watchdog.system-test
  (:require [clojure.test :refer :all]
            [schema.core :as s]
            [log-watchdog.system :as system]
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


(deftest create-test
  (testing "creating a system based on valid configuration yields a valid system"
    (s/validate validators/system (system/create config-test/valid-configuration))))


(deftest file-paths-test
  (testing "get all file paths"
    (is (= #{"file-path-1" "file-path-2"} (set (system/file-paths valid-system)))))
  (testing "get file paths matching predicate"
    (is (= #{"file-path-1"} (set (system/file-paths valid-system system/file-has-unacknowledged-alert?))))
    (is (= #{"file-path-2"} (set (system/file-paths valid-system (complement system/file-has-unacknowledged-alert?)))))))


(deftest alerts-test
  (testing "get all alerts from all files"
    (is (= #{"line-1" "line-2" "line-3" "line-4"} (set (keys (system/alerts valid-system))))))
  (testing "get all alerts from specified files"
    (is (= #{"line-1" "line-2" "line-3" "line-4"} (set (keys (system/alerts valid-system ["file-path-1" "file-path-2"]))))))
  (testing "get all alerts from specified file"
    (is (= #{"line-1" "line-2"} (set (keys (system/alerts valid-system ["file-path-1"]))))))
  (testing "get alerts from specified files matching predicate"
    (is (= #{"line-1"} (set (keys (system/alerts valid-system ["file-path-1"] system/unacknowledged-alert?)))))
    (is (= #{"line-2"} (set (keys (system/alerts valid-system ["file-path-1"] (complement system/unacknowledged-alert?))))))))


(deftest has-new-alert?-test
  (testing "unchanged system cannot have a new alert"
    (is (not (system/has-new-alert? valid-system valid-system))))
  (testing "new alert detected when there is one"
    (let [new-system (assoc-in valid-system
                               [:files "file-path-1" :alerts]
                               {"line-new" {:last-seen-timestamp 123456799, :acknowledged true}})]
      (s/validate validators/system new-system) ; make sure the hand-crafted system is valid
      (is (system/has-new-alert? valid-system new-system)))))


(deftest check-files-test
  (let [updated-file "file-path-1"
        new-alert-line "ERROR This is a new error"]
    (with-redefs-fn {#'clojure.java.io/reader (fn [file-path & _]
                                                (let [file-data (if (= file-path updated-file)
                                                                  new-alert-line
                                                                  "")]
                                                  (BufferedReader. (StringReader. file-data))))}
      (fn []
        (let [new-system (system/check-files valid-system)]
          (s/validate validators/system new-system)
          (let [new-alerts (system/alerts new-system [updated-file])]
            (is (contains? new-alerts new-alert-line))
            (is (not (get-in new-alerts [new-alert-line :acknowledged])))))))))


(deftest acknowledge-alerts-test
  (let [new-system (system/acknowledge-alerts valid-system)]
    (s/validate validators/system new-system)
    (is (empty? (system/alerts new-system ["file-path-1"] system/unacknowledged-alert?)))))


(deftest set-last-notification-timestamp-test
  (let [old-timestamp (system/last-notification-timestamp valid-system)
        new-timestamp (inc (* 2 old-timestamp))
        new-system (system/set-last-notification-timestamp valid-system new-timestamp)]
    (s/validate validators/system new-system)
    (is (= new-timestamp (system/last-notification-timestamp new-system)))))


(deftest set-tray-icon-test
  (let [image (.getImage (java.awt.Toolkit/getDefaultToolkit)
                         (clojure.java.io/resource "icon.png"))
        tray-icon (java.awt.TrayIcon. image)
        new-system (system/set-tray-icon valid-system tray-icon)]
    (s/validate validators/system new-system)))

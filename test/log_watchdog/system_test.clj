(ns log-watchdog.system-test
  (:require [clojure.test :refer :all]
            [schema.core :as s]
            [log-watchdog.system :as system]
            [log-watchdog.config :as config]
            [log-watchdog.validators :as validators]
            [log-watchdog.config-test :as config-test])
  (:import [java.io BufferedReader StringReader]))

;; definition of a testing system

(def orig-system
  { :check-enabled true
    :check-interval-ms 5000
    :nagging-interval-ms 60000
    :last-notification-timestamp 111
    :files
      { "file-path-1"
          { :line-regex #".*"
            :last-check-failed false
            :alerts
              { "line-1"
                  { :acknowledged false}
                "line-2"
                  { :acknowledged true}}}
        "file-path-2"
          { :line-regex #".*"
            :last-check-failed true
            :alerts
              { "line-3"
                  { :acknowledged true}
                "line-4"
                  { :acknowledged true}}}}})


(deftest system-validator-test
  (testing "valid system is accepted by the validator (failure means either the system is invalid or the validator is invalid)"
    (s/validate validators/system orig-system)))


(deftest create-test
  (testing "creating a system based on valid configuration yields a valid system"
    (let [created-system (system/create config-test/valid-configuration)]
      (s/validate validators/system created-system)
      (is (= 2 (count (system/file-paths created-system)))))))


(deftest file-paths-test
  (testing "get all file paths"
    (is (= #{"file-path-1" "file-path-2"} (set (system/file-paths orig-system)))))
  (testing "get file paths filtered by 'file-has-unacknowledged-alert?' predicate"
    (is (= #{"file-path-1"} (set (system/file-paths orig-system system/file-has-unacknowledged-alert?))))
    (is (= #{"file-path-2"} (set (system/file-paths orig-system (complement system/file-has-unacknowledged-alert?))))))
  (testing "get file paths filtered by 'file-has-last-check-failed?' predicate"
    (is (= #{"file-path-2"} (set (system/file-paths orig-system system/file-has-last-check-failed?))))
    (is (= #{"file-path-1"} (set (system/file-paths orig-system (complement system/file-has-last-check-failed?)))))))


(deftest alerts-test
  (testing "get all alerts from all files"
    (is (= #{"line-1" "line-2" "line-3" "line-4"} (set (keys (system/alerts orig-system))))))
  (testing "get all alerts from specified files"
    (is (= #{"line-1" "line-2" "line-3" "line-4"} (set (keys (system/alerts orig-system ["file-path-1" "file-path-2"]))))))
  (testing "get all alerts from specified file"
    (is (= #{"line-1" "line-2"} (set (keys (system/alerts orig-system ["file-path-1"]))))))
  (testing "get alerts from specified files matching predicate"
    (is (= #{"line-1"} (set (keys (system/alerts orig-system ["file-path-1"] system/unacknowledged-alert?)))))
    (is (= #{"line-2"} (set (keys (system/alerts orig-system ["file-path-1"] (complement system/unacknowledged-alert?))))))))


(deftest has-new-alert?-test
  (testing "unchanged system cannot have a new alert"
    (is (not (system/has-new-alert? orig-system orig-system))))
  (testing "new alert detected when there is one"
    (let [new-system (assoc-in orig-system
                               [:files "file-path-1" :alerts]
                               {"line-new" {:acknowledged true}})]
      (s/validate validators/system new-system) ; make sure the hand-crafted system is valid
      (is (system/has-new-alert? orig-system new-system)))))

(deftest has-new-failed-file?-test
  (testing "unchanged system cannot have a new failed file"
    (is (not (system/has-new-failed-file? orig-system orig-system))))
  (testing "new failed file detected when there is one"
    (let [new-system (system/set-file-property orig-system "file-path-1" :last-check-failed true)]
      (s/validate validators/system new-system)
      (is (system/has-new-failed-file? orig-system new-system)))))


(deftest check-files-test
  (testing "New alert is detected"
    (let [tested-file "file-path-1"
          tested-line "This is a new error"
          orig-alerts (system/alerts orig-system [tested-file])]
      ; pre-conditions
      (is (not (contains? orig-alerts tested-line)))
      (with-redefs-fn {#'clojure.java.io/reader (fn [file-path & _]
                                                  (BufferedReader.
                                                    (StringReader. (if (= file-path tested-file)
                                                                     tested-line
                                                                     ""))))}
        (fn []
          (let [new-system (system/check-files orig-system)]
            ; post-conditions
            (s/validate validators/system new-system)
            (let [new-alerts (system/alerts new-system [tested-file])]
              (is (contains? new-alerts tested-line))
              (is (= false (get-in new-alerts [tested-line :acknowledged])))))))))
  (testing "Existing alerts are updated"
    (let [tested-file "file-path-1"
          tested-line1 "line-1"
          tested-line2 "line-2"]
      (with-redefs-fn {#'clojure.java.io/reader (fn [file-path & _]
                                                  (BufferedReader.
                                                    (StringReader. (if (= file-path tested-file)
                                                                     (clojure.string/join "\r\n" [tested-line1 tested-line2])
                                                                     ""))))}
        (fn []
          (let [new-system (system/check-files orig-system)]
            ; post-conditions
            (s/validate validators/system new-system)
            ; 'acknowledged' property must stay the same
            (is (= (system/alert-property orig-system tested-file tested-line1 :acknowledged)
                   (system/alert-property new-system tested-file tested-line1 :acknowledged)))
            (is (= (system/alert-property orig-system tested-file tested-line2 :acknowledged)
                   (system/alert-property new-system tested-file tested-line2 :acknowledged))))))))
  (testing "Previously seen alert not found now"
    (let [tested-file "file-path-1"
          tested-line "line-1"
          orig-alerts (system/alerts orig-system [tested-file])]
      ; pre-conditions
      (is (contains? orig-alerts tested-line))
      (with-redefs-fn {#'clojure.java.io/reader (fn [file-path & _] (BufferedReader. (StringReader. "")))}
        (fn []
          (let [new-system (system/check-files orig-system)
                new-alerts (system/alerts new-system [tested-file])]
            ; post-conditions
            (s/validate validators/system new-system)
            (is (empty? new-alerts)))))))
  (testing "Non-existing file is detected"
    (let [tested-file "file-path-1"]
      (with-redefs-fn {#'clojure.java.io/reader (fn [file-path & _] (throw (java.io.IOException. "")))}
        (fn []
          (let [new-system (system/check-files orig-system)]
            (is (= false (system/file-property orig-system tested-file :last-check-failed))) ; pre-condition
            (is (= true (system/file-property new-system tested-file :last-check-failed)))))))) ; post-condition
  (testing "Existing file is detected"
    (let [tested-file "file-path-2"]
      ; pre-conditions
      (is (= true (system/file-property orig-system tested-file :last-check-failed)))
      (with-redefs-fn {#'clojure.java.io/reader (fn [file-path & _] (BufferedReader. (StringReader. "")))}
          (fn []
          (let [new-system (system/check-files orig-system)]
            ; post-conditions
            (s/validate validators/system new-system)
            (is (= false (system/file-property new-system tested-file :last-check-failed)))))))))


;; verify that the system stays in a consistent state wrt. system schema when changed

(deftest acknowledge-alerts-test
  (let [new-system (system/acknowledge-alerts orig-system)]
    (s/validate validators/system new-system)
    (is (empty? (system/alerts new-system ["file-path-1"] system/unacknowledged-alert?)))))


(deftest set-last-notification-timestamp-test
  (let [old-timestamp (system/last-notification-timestamp orig-system)
        new-timestamp (inc (* 2 old-timestamp))
        new-system (system/set-last-notification-timestamp orig-system new-timestamp)]
    (s/validate validators/system new-system)
    (is (= new-timestamp (system/last-notification-timestamp new-system)))))

(deftest set-file-property-test
  (let [tested-file "file-path-1"
        tested-property :last-check-failed]
    ; pre-conditions
    (is (contains? (set (system/file-paths orig-system)) tested-file))
    (let [old-value (system/file-property orig-system tested-file tested-property)
          new-system (system/set-file-property orig-system tested-file tested-property (not old-value))
          new-value (system/file-property new-system tested-file tested-property)]
      ; post-conditions
      (s/validate validators/system new-system)
      (is (not= old-value new-value)))))

(deftest set-alert-property-test
  (let [tested-file "file-path-1"
        tested-alert "line-1"
        tested-property :acknowledged]
    ; pre-conditions
    (is (contains? (set (system/file-paths orig-system)) tested-file))
    (is (contains? (system/alerts orig-system [tested-file]) tested-alert))
    (let [old-value (system/alert-property orig-system tested-file tested-alert tested-property)
          new-system (system/set-alert-property orig-system tested-file tested-alert tested-property (not old-value))
          new-value (system/alert-property new-system tested-file tested-alert tested-property)]
      ; post-conditions
      (s/validate validators/system new-system)
      (is (not= old-value new-value)))))

(deftest set-ui-property-test
  (let [image (.getImage (java.awt.Toolkit/getDefaultToolkit)
                         (clojure.java.io/resource "icon.png"))
        tray-icon (java.awt.TrayIcon. image)
        new-system (system/set-ui-property orig-system :tray-icon tray-icon)]
    (s/validate validators/system new-system)))

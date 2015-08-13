(ns log-watchdog.system.helpers-test
  (:require [clojure.test :refer :all]
            [schema.core :as s]
            [log-watchdog.system.core :as core]
            [log-watchdog.system.helpers :as helpers]
            [log-watchdog.validators :as validators]
            [log-watchdog.system.core-test :refer :all])
  (:import [java.io BufferedReader StringReader]))


; Note: A lot of definitions are imported from log-watchdog.system.core-test namespace

(defn reader-for-string! [string]
  (BufferedReader. (StringReader. string)))


(deftest unacknowledged-alerts-test
  (let [unacked-alerts (helpers/unacknowledged-alerts system-orig)
        unacked-alert-ids (map first unacked-alerts)]
    (is (= #{alert1-id alert4-id} (set unacked-alert-ids)))))

(deftest unreadable-files-test
  (let [unreadable-files (helpers/unreadable-files system-orig)
        unreadable-file-ids (map first unreadable-files)]
    (is (= #{file2-id} (set unreadable-file-ids)))))

(deftest referenced-entity-test
  (let [alert-entity (core/query-by-id system-orig alert1-id)
        file-entity  (helpers/referenced-entity system-orig alert-entity :watched-file-id)
        group-entity (helpers/referenced-entity system-orig alert-entity :watched-file-id :watched-file-group-id)]
    (is (= (core/query-by-id system-orig file1-id) file-entity))
    (is (= (core/query-by-id system-orig file-group1-id) group-entity))))


(deftest toggle-check-enabled-test
  (let [system-new (helpers/toggle-check-enabled system-orig)]
    (validators/validate-system system-new)
    (let [[_ cfg-data-orig] (core/query-singleton system-orig (core/entity-pred :type (partial = :configuration)))
          [_ cfg-data-new]  (core/query-singleton system-new  (core/entity-pred :type (partial = :configuration)))]
      (is (not= (:check-enabled cfg-data-orig) (:check-enabled cfg-data-new))))))


(deftest acknowledge-alert-test
  (let [[_ alert-data-orig :as alert-orig] (core/query-by-id system-orig alert1-id)
        system-new (helpers/acknowledge-alert system-orig alert-orig)
        [_ alert-data-new :as alert-new] (core/query-by-id system-new alert1-id)]
    (testing "New system is structurally correct"
      (validators/validate-system system-new))
    (testing "Alert is acknowledged"
      (is (false? (:acknowledged alert-data-orig)))
      (is (true? (:acknowledged alert-data-new))))
    (testing "No other property of alert changes"
      (is (= (dissoc alert-data-orig :acknowledged) (dissoc alert-data-new :acknowledged))))
    (testing "No other entity in the system changes"
      (is (= (set (core/remove-entity system-orig alert-orig))
             (set (core/remove-entity system-new alert-new)))))))

(deftest acknowledge-alerts-test
  (let [file1 (core/query-by-id system-orig file1-id)
        file2 (core/query-by-id system-orig file2-id)
        system-new (helpers/acknowledge-alerts system-orig [file1 file2])]
    (testing "New system is structurally correct"
      (validators/validate-system system-new))
    (testing "Non-alert entities are not changed at all"
      (is (= (set (core/query system-orig (core/entity-pred :type (partial not= :alert))))
             (set (core/query system-new (core/entity-pred :type (partial not= :alert)))))))
    (testing "all alerts are acknowledged"
      (let [[_ alert1-data-orig] (core/query-by-id system-orig alert1-id)
            [_ alert1-data-new]  (core/query-by-id system-new alert1-id)
            [_ alert2-data-orig] (core/query-by-id system-orig alert2-id)
            [_ alert2-data-new]  (core/query-by-id system-new alert2-id)
            [_ alert3-data-orig] (core/query-by-id system-orig alert3-id)
            [_ alert3-data-new]  (core/query-by-id system-new alert3-id)
            [_ alert4-data-orig] (core/query-by-id system-orig alert4-id)
            [_ alert4-data-new]  (core/query-by-id system-new alert4-id)]
        (is (= (dissoc alert1-data-orig :acknowledged) (dissoc alert1-data-new :acknowledged)))
        (is (false? (:acknowledged alert1-data-orig)))
        (is (true? (:acknowledged alert1-data-new)))
        (is (= alert2-data-orig alert2-data-new))
        (is (= alert3-data-orig alert3-data-new))
        (is (= (dissoc alert4-data-orig :acknowledged) (dissoc alert4-data-new :acknowledged)))
        (is (false? (:acknowledged alert4-data-orig)))
        (is (true? (:acknowledged alert4-data-new))))))
  (let [file1 (core/query-by-id system-orig file1-id)
        system-new (helpers/acknowledge-alerts system-orig [file1])]
    (testing "alerts for only given files are acknowledged"
      (let [[_ alert1-data-orig] (core/query-by-id system-orig alert1-id)
            [_ alert1-data-new]  (core/query-by-id system-new alert1-id)
            [_ alert2-data-orig] (core/query-by-id system-orig alert2-id)
            [_ alert2-data-new]  (core/query-by-id system-new alert2-id)
            [_ alert3-data-orig] (core/query-by-id system-orig alert3-id)
            [_ alert3-data-new]  (core/query-by-id system-new alert3-id)
            [_ alert4-data-orig] (core/query-by-id system-orig alert4-id)
            [_ alert4-data-new]  (core/query-by-id system-new alert4-id)]
        (is (= (dissoc alert1-data-orig :acknowledged) (dissoc alert1-data-new :acknowledged)))
        (is (false? (:acknowledged alert1-data-orig)))
        (is (true? (:acknowledged alert1-data-new)))
        (is (= alert2-data-orig alert2-data-new))
        (is (= alert3-data-orig alert3-data-new))
        (is (= alert4-data-orig alert4-data-new))))))


(deftest check-files-test
  (testing "New alert is detected"
    (let [tested-file-id file1-id
          tested-line "This is a new error"
          [_ tested-file-data] (core/query-by-id system-orig tested-file-id)
          tested-file-path (:file tested-file-data)]
      ; pre-conditions
      (is (empty? (core/query system-orig (core/entity-pred :type (partial = :alert)
                                                            :matched-line (partial = tested-line)))))
      (with-redefs-fn {#'clojure.java.io/reader (fn [file-path & _]
                                                  (if (= file-path tested-file-path)
                                                    (BufferedReader. (StringReader. tested-line))
                                                    (BufferedReader. (StringReader. ""))))}
        (fn []
          (let [system-new (helpers/check-files system-orig)]
            ; post-conditions
            (validators/validate-system system-new)
            (let [alerts-orig (core/query system-orig (core/entity-pred :type (partial = :alert)))
                  alerts-new  (core/query system-new  (core/entity-pred :type (partial = :alert)))]
              (= (count alerts-orig)
                 (dec (count alerts-new))))
            (let [new-alert-query-result (core/query system-new (core/entity-pred :type (partial = :alert)
                                                                                  :matched-line (partial = tested-line)))]
              (is (= 1 (count new-alert-query-result)))
              (let [[new-alert-id new-alert-data] (first new-alert-query-result)]
                (is (false? (:acknowledged new-alert-data))))))))))
  (testing "Existing alerts are updated"
    (let [tested-file-id file1-id
          tested-line1 "line-1"
          tested-line2 "line-2"
          [_ tested-file-data] (core/query-by-id system-orig tested-file-id)
          tested-file-path (:file tested-file-data)]
      (with-redefs-fn {#'clojure.java.io/reader (fn [file-path & _]
                                                  (if (= file-path tested-file-path)
                                                    (BufferedReader. (StringReader. (clojure.string/join "\r\n" [tested-line1 tested-line2])))
                                                    (BufferedReader. (StringReader. ""))))}
        (fn []
          (let [system-new (helpers/check-files system-orig)]
            ; post-conditions
            (validators/validate-system system-new)
            (let [alerts-orig (core/query system-orig (core/entity-pred :type (partial = :alert)))
                  alerts-new  (core/query system-new  (core/entity-pred :type (partial = :alert)))]
              ; no alert is added or removed
              (is (= (count alerts-orig) (count alerts-new)))
              ; for now, all properties stay the same for known alerts
              (is (= alerts-orig alerts-new))))))))
  (testing "File becomes unavailable"
    (let [[tested-file-id tested-file-data-orig] (core/query-by-id system-orig file1-id)]
      ; pre-condition
      (is (false? (:last-check-failed tested-file-data-orig)))
      (with-redefs-fn {#'clojure.java.io/reader (fn [file-path & _] (throw (java.io.IOException. "")))}
        (fn []
          (let [system-new (helpers/check-files system-orig)
                [_ tested-file-data-new] (core/query-by-id system-new tested-file-id)]
            ; post-condition
            (validators/validate-system system-new)
            (is (true? (:last-check-failed tested-file-data-new))))))))
  (testing "File becomes available"
    (let [[tested-file-id tested-file-data-orig] (core/query-by-id system-orig file2-id)]
      ; pre-condition
      (is (true? (:last-check-failed tested-file-data-orig)))
      (with-redefs-fn {#'clojure.java.io/reader (fn [file-path & _] (BufferedReader. (StringReader. "")))}
        (fn []
          (let [system-new (helpers/check-files system-orig)
                [_ tested-file-data-new] (core/query-by-id system-new tested-file-id)]
            ; post-conditions
            (validators/validate-system system-new)
            (is (false? (:last-check-failed tested-file-data-new))))))))
  (testing "File not checked when last modified timestamp and file size don't change"
    (let [last-modified-before-test 42
          last-modified-during-test 42
          size-before-test 100
          size-during-test 100
          [tested-file-id tested-file-data-orig :as tested-file] (core/query-by-id system-orig file1-id)]
      ; Prepare the tested file for test and craft a new system for test
      (let [tested-file-data-before (assoc tested-file-data-orig :file-last-size-b size-before-test
                                                                 :file-last-modified-ms last-modified-before-test
                                                                 :always-check-override false)
            system-before (core/add-entity system-orig [tested-file-id tested-file-data-before])
            file-before (core/query-by-id system-before tested-file-id)
            file-read? (atom false)]
        (validators/validate-system system-before)
        (with-redefs-fn {#'log-watchdog.io/file-exists? (fn [file-path] true)
                         #'log-watchdog.io/file-size (fn [file-path] size-during-test)
                         #'log-watchdog.io/file-last-modified-ms (fn [file-path] last-modified-during-test)
                         #'clojure.java.io/reader (fn [file-path & _] (reset! file-read? true) (reader-for-string! ""))}
          (fn []
            (let [system-new (helpers/check-files system-before [file-before])]
              (validators/validate-system system-new)
              (is (false? @file-read?))))))))
  (testing "When :always-check-override is true, file is checked"
    (let [last-modified-before-test 42
          last-modified-during-test 42
          size-before-test 100
          size-during-test 100
          [tested-file-id tested-file-data-orig :as tested-file] (core/query-by-id system-orig file1-id)]
      ; Prepare the tested file for test and craft a new system for test
      (let [tested-file-data-before (assoc tested-file-data-orig :file-last-size-b size-before-test
                                                                 :file-last-modified-ms last-modified-before-test
                                                                 :always-check-override true)
            system-before (core/add-entity system-orig [tested-file-id tested-file-data-before])
            file-before (core/query-by-id system-before tested-file-id)
            file-read? (atom false)]
        (validators/validate-system system-before)
        (with-redefs-fn {#'log-watchdog.io/file-exists? (fn [file-path] true)
                         #'log-watchdog.io/file-size (fn [file-path] size-during-test)
                         #'log-watchdog.io/file-last-modified-ms (fn [file-path] last-modified-during-test)
                         #'clojure.java.io/reader (fn [file-path & _] (reset! file-read? true) (reader-for-string! ""))}
          (fn []
            (let [system-new (helpers/check-files system-before [file-before])]
              (validators/validate-system system-new)
              (is (true? @file-read?))))))))
  (testing "File checked when last modified timestamp changes"
    (let [last-modified-before-test 42
          last-modified-during-test 43
          size-before-test 100
          size-during-test 100
          [tested-file-id tested-file-data-orig :as tested-file] (core/query-by-id system-orig file1-id)]
      ; Prepare the tested file for test and craft a new system for test
      (let [tested-file-data-before (assoc tested-file-data-orig :file-last-size-b size-before-test
                                                                 :file-last-modified-ms last-modified-before-test
                                                                 :always-check-override false)
            system-before (core/add-entity system-orig [tested-file-id tested-file-data-before])
            file-before (core/query-by-id system-before tested-file-id)
            file-read? (atom false)]
        (validators/validate-system system-before)
        (with-redefs-fn {#'log-watchdog.io/file-exists? (fn [file-path] true)
                         #'log-watchdog.io/file-size (fn [file-path] size-during-test)
                         #'log-watchdog.io/file-last-modified-ms (fn [file-path] last-modified-during-test)
                         #'clojure.java.io/reader (fn [file-path & _] (reset! file-read? true) (reader-for-string! ""))}
          (fn []
            (let [system-new (helpers/check-files system-before [file-before])]
              (validators/validate-system system-new)
              (is (true? @file-read?))))))))
  (testing "File checked when file size changes"
    (let [last-modified-before-test 42
          last-modified-during-test 42
          size-before-test 100
          size-during-test 11
          [tested-file-id tested-file-data-orig :as tested-file] (core/query-by-id system-orig file1-id)]
      ; Prepare the tested file for test and craft a new system for test
      (let [tested-file-data-before (assoc tested-file-data-orig :file-last-size-b size-before-test
                                                                 :file-last-modified-ms last-modified-before-test
                                                                 :always-check-override false)
            system-before (core/add-entity system-orig [tested-file-id tested-file-data-before])
            file-before (core/query-by-id system-before tested-file-id)
            file-read? (atom false)]
        (validators/validate-system system-before)
        (with-redefs-fn {#'log-watchdog.io/file-exists? (fn [file-path] true)
                         #'log-watchdog.io/file-size (fn [file-path] size-during-test)
                         #'log-watchdog.io/file-last-modified-ms (fn [file-path] last-modified-during-test)
                         #'clojure.java.io/reader (fn [file-path & _] (reset! file-read? true) (reader-for-string! ""))}
          (fn []
            (let [system-new (helpers/check-files system-before [file-before])]
              (validators/validate-system system-new)
              (is (true? @file-read?)))))))))

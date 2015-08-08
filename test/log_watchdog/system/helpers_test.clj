(ns log-watchdog.system.helpers-test
  (:require [clojure.test :refer :all]
            [schema.core :as s]
            [log-watchdog.system.core :as core]
            [log-watchdog.system.helpers :as helpers]
            [log-watchdog.validators :as validators]
            [log-watchdog.system.core-test :refer :all])
  (:import [java.io BufferedReader StringReader]))


; Note: A lot of definitions are imported from log-watchdog.system.core-test namespace


(deftest unacknowledged-alerts-test
  (let [unacked-alerts (helpers/unacknowledged-alerts system-orig)
        unacked-alert-ids (map first unacked-alerts)]
    (is (= #{alert1-id alert4-id} (set unacked-alert-ids)))))

(deftest files-having-unacknowledged-alerts-test
  (let [unacked-files (helpers/files-having-unacknowledged-alerts system-orig)
        unacked-file-ids (map first unacked-files)]
    (is (= #{file1-id file2-id} (set unacked-file-ids)))))

(deftest unreadable-files-test
  (let [unreadable-files (helpers/unreadable-files system-orig)
        unreadable-file-ids (map first unreadable-files)]
    (is (= #{file2-id} (set unreadable-file-ids)))))

(deftest referenced-entity-test
  (let [alert-entity [alert1-id (get system-orig alert1-id)]
        file-entity  (helpers/referenced-entity system-orig alert-entity :watched-file-id)
        group-entity (helpers/referenced-entity system-orig alert-entity :watched-file-id :watched-file-group-id)]
    (is (= [file1-id (get system-orig file1-id)] file-entity))
    (is (= [file-group1-id (get system-orig file-group1-id)] group-entity))))


(deftest toggle-check-enabled-test
  (let [system-new (helpers/toggle-check-enabled system-orig)]
    (validators/validate-system system-new)
    (let [[_ cfg-data-orig] (first (core/query system-orig (core/entity-pred :type (partial = :configuration))))
          [_ cfg-data-new]  (first (core/query system-new  (core/entity-pred :type (partial = :configuration))))]
      (is (not= (get cfg-data-orig :check-enabled) (get cfg-data-new :check-enabled))))))

(deftest acknowledge-alerts-test
  (let [system-new (helpers/acknowledge-alerts system-orig [file1-id file2-id])]
    (testing "new system is structurally correct"
      (validators/validate-system system-new))
    (testing "set of entity IDs in the system doesn't change"
      (is (= (keys system-orig) (keys system-new))))
    (testing "non-alert entities are not changed at all"
      (is (= (core/query system-orig (core/entity-pred :type (partial not= :alert)))
             (core/query system-new (core/entity-pred :type (partial not= :alert))))))
    (testing "all alerts are acknowledged"
      (let [alert1-orig (get system-orig alert1-id)
            alert1-new (get system-new alert1-id)
            alert2-orig (get system-orig alert2-id)
            alert2-new (get system-new alert2-id)
            alert3-orig (get system-orig alert3-id)
            alert3-new (get system-new alert3-id)
            alert4-orig (get system-orig alert4-id)
            alert4-new (get system-new alert4-id)]
        (is (dissoc alert1-orig :acknowledged) (dissoc alert1-new :acknowledged))
        (is (false? (:acknowledged alert1-orig)))
        (is (true? (:acknowledged alert1-new)))
        (is (= alert2-orig alert2-new))
        (is (= alert3-orig alert3-new))
        (is (dissoc alert4-orig :acknowledged) (dissoc alert4-new :acknowledged))
        (is (false? (:acknowledged alert4-orig)))
        (is (true? (:acknowledged alert4-new))))))
  (let [system-new (helpers/acknowledge-alerts system-orig [file1-id])]
    (testing "alerts for only given files are acknowledged"
      (let [alert1-orig (get system-orig alert1-id)
            alert1-new (get system-new alert1-id)
            alert2-orig (get system-orig alert2-id)
            alert2-new (get system-new alert2-id)
            alert3-orig (get system-orig alert3-id)
            alert3-new (get system-new alert3-id)
            alert4-orig (get system-orig alert4-id)
            alert4-new (get system-new alert4-id)]
        (is (dissoc alert1-orig :acknowledged) (dissoc alert1-new :acknowledged))
        (is (false? (:acknowledged alert1-orig)))
        (is (true? (:acknowledged alert1-new)))
        (is (= alert2-orig alert2-new))
        (is (= alert3-orig alert3-new))
        (is (= alert4-orig alert4-new))))))


(deftest check-files-test
  (testing "New alert is detected"
    (let [tested-file-id file1-id
          tested-line "This is a new error"
          tested-file-data (get system-orig tested-file-id)
          tested-file-path (get tested-file-data :file)]
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
            (let [orig-alerts (core/query system-orig (core/entity-pred :type (partial = :alert)))
                  new-alerts (core/query system-new (core/entity-pred :type (partial = :alert)))]
              (= (count orig-alerts)
                 (dec (count new-alerts))))
            (let [new-alert-query-result (core/query system-new (core/entity-pred :type (partial = :alert)
                                                                                  :matched-line (partial = tested-line)))]
              (is (= 1 (count new-alert-query-result)))
              (let [[new-alert-id new-alert-data] (first new-alert-query-result)]
                (is (false? (get new-alert-data :acknowledged))))))))))
  (testing "Existing alerts are updated"
    (let [tested-file-id file1-id
          tested-line1 "line-1"
          tested-line2 "line-2"
          tested-file-data (get system-orig tested-file-id)
          tested-file-path (get tested-file-data :file)]
      (with-redefs-fn {#'clojure.java.io/reader (fn [file-path & _]
                                                  (if (= file-path tested-file-path)
                                                    (BufferedReader. (StringReader. (clojure.string/join "\r\n" [tested-line1 tested-line2])))
                                                    (BufferedReader. (StringReader. ""))))}
        (fn []
          (let [system-new (helpers/check-files system-orig)]
            ; post-conditions
            (validators/validate-system system-new)
            (let [orig-alerts (core/query system-orig (core/entity-pred :type (partial = :alert)))
                  new-alerts (core/query system-new (core/entity-pred :type (partial = :alert)))]
              ; no alert is added or removed
              (is (= (count orig-alerts) (count new-alerts)))
              ; for now, all properties stay the same for known alerts
              (is (= orig-alerts new-alerts))))))))
  (testing "File becomes unavailable"
    (let [tested-file-id file1-id
          tested-file-data-orig (get system-orig tested-file-id)]
      ; pre-condition
      (is (false? (get tested-file-data-orig :last-check-failed)))
      (with-redefs-fn {#'clojure.java.io/reader (fn [file-path & _] (throw (java.io.IOException. "")))}
        (fn []
          (let [system-new (helpers/check-files system-orig)
                tested-file-data-new (get system-new tested-file-id)]
            ; post-condition
            (validators/validate-system system-new)
            (is (true? (get tested-file-data-new :last-check-failed))))))))
  (testing "File becomes available"
    (let [tested-file-id file2-id
          tested-file-data-orig (get system-orig tested-file-id)]
      ; pre-condition
      (is (true? (get tested-file-data-orig :last-check-failed)))
      (with-redefs-fn {#'clojure.java.io/reader (fn [file-path & _] (BufferedReader. (StringReader. "")))}
        (fn []
          (let [system-new (helpers/check-files system-orig)
                tested-file-data-new (get system-new tested-file-id)]
            ; post-conditions
            (validators/validate-system system-new)
            (is (false? (get tested-file-data-new :last-check-failed)))))))))

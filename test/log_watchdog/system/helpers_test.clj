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
  (letfn [(alerts->alert-lines [alerts] (->> alerts
                                             (map (fn [[_ alert-data]] (:matched-line alert-data)))
                                             (set)))
          (alert-change-test-fn
           [{:keys [tested-file-id
                    lines-during-test
                    lines-during-test-already-have-alerts
                    lines-not-present-after]}]
             ; Prepare the tested file for test (force always check, never seek)
             (let [[_ tested-file-data-orig] (core/query-by-id system-orig tested-file-id)
                   tested-file-data-before (assoc tested-file-data-orig :always-check-override true
                                                                        :never-seek-override true)
                   system-before (core/add-entity system-orig [tested-file-id tested-file-data-before])]
               (validators/validate-system system-before)
               ; pre-conditions
               (let [alerts-before (core/query system-before (core/entity-pred :type (partial = :alert)
                                                                               :watched-file-id (partial = tested-file-id)))
                     alert-lines-before (alerts->alert-lines alerts-before)]
                 (if lines-during-test-already-have-alerts
                   (is (every?   #(alert-lines-before %) lines-during-test))
                   (is (not-any? #(alert-lines-before %) lines-during-test)))
                 (is (every? #(alert-lines-before %) lines-not-present-after)))
               (with-redefs-fn {#'clojure.java.io/reader (fn [file-path & _] (reader-for-string! (clojure.string/join "\n" lines-during-test)))}
                 (fn []
                   (let [system-after (helpers/check-files system-before)]
                     ; post-conditions
                     (validators/validate-system system-after)
                     (let [alerts-after  (core/query system-after  (core/entity-pred :type (partial = :alert)
                                                                                     :watched-file-id (partial = tested-file-id)))
                           alert-lines-after (alerts->alert-lines alerts-after)]
                       (is (every? #(alert-lines-after %) lines-during-test))
                       (is (every? (fn [[alert-id alert-data-after]]
                                     (if lines-during-test-already-have-alerts
                                       ; all alerts for tested lines are already existing alerts
                                       (let [[_ alert-data-before] (core/query-by-id system-after alert-id)]
                                         (= alert-data-before alert-data-after))
                                       ; all alerts for tested lines are new alerts
                                       (false? (:acknowledged alert-data-after))))
                                   alerts-after))
                       (is (not-any? #(alert-lines-after %) lines-not-present-after))))))))]
    (testing "New alert is detected"
      (alert-change-test-fn {:tested-file-id file2-id
                             :lines-during-test ["This is a new error"]
                             :lines-during-test-already-have-alerts false
                             :lines-not-present-after []}))
    (testing "Existing alerts are updated"
      (alert-change-test-fn {:tested-file-id file1-id
                             :lines-during-test ["line-1" "line-2"]
                             :lines-during-test-already-have-alerts true
                             :lines-not-present-after []}))
    (testing "Alerts not present anymore are removed from the system"
      (alert-change-test-fn {:tested-file-id file1-id
                             :lines-during-test []
                             :lines-during-test-already-have-alerts false
                             :lines-not-present-after ["line-1" "line-2"]})))
  (letfn [(file-availability-test-fn
          [{:keys [tested-file-id
                   file-exists? size-during-test last-modified-during-test
                   reader-behavior
                   expected-last-check-failed-before expected-last-check-failed-after
                   expected-last-modified-ms-after expected-last-size-b-after]}]
           (let [[_ tested-file-data-orig] (core/query-by-id system-orig tested-file-id)]
             ; pre-condition
             (is (= expected-last-check-failed-before (:last-check-failed tested-file-data-orig)))
             (with-redefs-fn {#'log-watchdog.io/file-exists? (fn [file-path] file-exists?)
                              #'log-watchdog.io/file-size (fn [file-path] size-during-test)
                              #'log-watchdog.io/file-last-modified-ms (fn [file-path] last-modified-during-test)
                              #'clojure.java.io/reader (fn [file-path & _] (reader-behavior))}
               (fn []
                 (let [system-new (helpers/check-files system-orig)]
                   ; post-conditions
                   (validators/validate-system system-new)
                   (let [[_ tested-file-data-new] (core/query-by-id system-new tested-file-id)]
                     (is (= expected-last-check-failed-after (:last-check-failed tested-file-data-new)))
                     (is (= expected-last-modified-ms-after (:file-last-modified-ms tested-file-data-new)))
                     (is (= expected-last-size-b-after (:file-last-size-b tested-file-data-new)))))))))]
    (testing "File becomes unavailable (non-existent)"
      (file-availability-test-fn {:tested-file-id file1-id
                                  :file-exists? false
                                  :reader-behavior (fn [] (throw (java.io.FileNotFoundException.)))
                                  :expected-last-check-failed-before false
                                  :expected-last-check-failed-after true
                                  :expected-last-modified-ms-after  nil
                                  :expected-last-size-b-after nil}))
    (testing "File exists but IOException occurs"
      (file-availability-test-fn {:tested-file-id file1-id
                                  :file-exists? true
                                  :reader-behavior (fn [] (throw (java.io.IOException. "Some unexpected error")))
                                  :expected-last-check-failed-before false
                                  :expected-last-check-failed-after true
                                  :expected-last-modified-ms-after  nil
                                  :expected-last-size-b-after nil}))
    (testing "File becomes available"
      (file-availability-test-fn {:tested-file-id file2-id
                                  :file-exists? true
                                  :last-modified-during-test 100
                                  :size-during-test 42
                                  :reader-behavior (fn [] (reader-for-string! ""))
                                  :expected-last-check-failed-before true
                                  :expected-last-check-failed-after false
                                  :expected-last-modified-ms-after  100
                                  :expected-last-size-b-after 42})))
  (letfn [(file-physically-read?-test-fn
           [{:keys [last-modified-before-test last-modified-during-test
                    size-before-test size-during-test
                    expected-file-read?
                    always-check-override]}]
            (let [tested-file-id file1-id
                  [_ tested-file-data-orig :as tested-file] (core/query-by-id system-orig tested-file-id)]
              ; Prepare the tested file for test and craft a new system for test
              (let [tested-file-data-before (assoc tested-file-data-orig :file-last-size-b size-before-test
                                                                         :file-last-modified-ms last-modified-before-test
                                                                         :always-check-override always-check-override)
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
                      (is (= expected-file-read? @file-read?))))))))]
    (testing "File not checked when last modified timestamp and file size don't change"
      (file-physically-read?-test-fn {:last-modified-before-test 42
                                      :last-modified-during-test 42
                                      :size-before-test 100
                                      :size-during-test 100
                                      :expected-file-read? false
                                      :always-check-override false}))
    (testing "When :always-check-override is true, file is checked"
      (file-physically-read?-test-fn {:last-modified-before-test 42
                                      :last-modified-during-test 42
                                      :size-before-test 100
                                      :size-during-test 100
                                      :expected-file-read? true
                                      :always-check-override true}))
    (testing "File checked when last modified timestamp changes"
      (file-physically-read?-test-fn {:last-modified-before-test 42
                                      :last-modified-during-test 43
                                      :size-before-test 100
                                      :size-during-test 100
                                      :expected-file-read? true
                                      :always-check-override false}))
    (testing "File checked when file size changes"
      (file-physically-read?-test-fn {:last-modified-before-test 42
                                      :last-modified-during-test 42
                                      :size-before-test 100
                                      :size-during-test 11
                                      :expected-file-read? true
                                      :always-check-override false}))
    (testing "File checked when previous file size is unknown"
      (file-physically-read?-test-fn {:last-modified-before-test 42
                                      :last-modified-during-test 42
                                      :size-before-test nil
                                      :size-during-test 100
                                      :expected-file-read? true
                                      :always-check-override false}))
    (testing "File checked when previous last modified timestamp is unknown"
      (file-physically-read?-test-fn {:last-modified-before-test nil
                                      :last-modified-during-test 42
                                      :size-before-test 100
                                      :size-during-test 100
                                      :expected-file-read? true
                                      :always-check-override false})))
  (letfn [(file-seeked?-test-fn
           [{:keys [size-before-test size-during-test
                    expected-file-reader-seek-offset
                    never-seek-override]}]
            (let [last-modified-before-test 42
                  last-modified-during-test 4200 ; always want to advance time so that the seeking decisions need to be made
                  tested-file-id file1-id
                  [_ tested-file-data-orig :as tested-file] (core/query-by-id system-orig tested-file-id)]
              ; Prepare the tested file for test and craft a new system for test
              (let [tested-file-data-before (assoc tested-file-data-orig :file-last-size-b size-before-test
                                                                         :file-last-modified-ms last-modified-before-test
                                                                         ; Force the check to be sure - want to test the seeking logic.
                                                                         :always-check-override true
                                                                         :never-seek-override never-seek-override)
                    system-before (core/add-entity system-orig [tested-file-id tested-file-data-before])
                    file-before (core/query-by-id system-before tested-file-id)
                    file-reader-seek-offset (atom nil)]
                (validators/validate-system system-before)
                (with-redefs-fn {#'log-watchdog.io/file-exists? (fn [file-path] true)
                                 #'log-watchdog.io/file-size (fn [file-path] size-during-test)
                                 #'log-watchdog.io/file-last-modified-ms (fn [file-path] last-modified-during-test)
                                 #'clojure.java.io/reader (fn [file-path & _] (reader-for-string! ""))
                                 #'log-watchdog.io/seek-reader! (fn [reader offset] (reset! file-reader-seek-offset offset))}
                  (fn []
                    (let [system-new (helpers/check-files system-before [file-before])]
                      (validators/validate-system system-new)
                      (is (= expected-file-reader-seek-offset @file-reader-seek-offset))))))))]
    (testing "File reader seeked when file size increases"
      (file-seeked?-test-fn {:size-before-test 100
                             :size-during-test 1000
                             :expected-file-reader-seek-offset 100
                             :never-seek-override false}))
    (testing "File reader NOT seeked when the override is set"
      (file-seeked?-test-fn {:size-before-test 100
                             :size-during-test 1000
                             :expected-file-reader-seek-offset nil
                             :never-seek-override true}))
    (testing "File reader NOT seeked when file size doesn't change"
      (file-seeked?-test-fn {:size-before-test 100
                             :size-during-test 100
                             :expected-file-reader-seek-offset nil
                             :never-seek-override false}))
    (testing "File reader NOT seeked when file size decreases"
      (file-seeked?-test-fn {:size-before-test 100
                             :size-during-test 10
                             :expected-file-reader-seek-offset nil
                             :never-seek-override false}))
    (testing "File reader NOT seeked when previous file size is unknown"
      (file-seeked?-test-fn {:size-before-test nil
                             :size-during-test 100
                             :expected-file-reader-seek-offset nil
                             :never-seek-override false}))))

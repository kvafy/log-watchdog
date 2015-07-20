(ns log-watchdog.core-test
  (:require [clojure.test :refer :all]
            [log-watchdog.core :as core]
            [log-watchdog.config :as config]))

;; definition of testing systems

(def system-1-file-str
  "{ :check-interval-ms 10000
     :nagging-interval-ms 60000
     :files
       { \"error.log\"
         { :line-regex \"^ERROR.*$\" }
       }
   }")
(def system-1-file-cfg (config/load-configuration system-1-file-str))

(def system-2-files-str
  "{ :check-interval-ms 10000
     :nagging-interval-ms 60000
     :files
       { \"error.log\"
         { :line-regex \"^ERROR.*$\" }
         \"warn.log\"
         { :line-regex \"^WARN.*$\" }
       }
   }")
(def system-2-files-cfg (config/load-configuration system-2-files-str))


(deftest create-system-test
  ; the fact that systems contain valid data is ensured by bouncer validators
  ;;TODO put the validators in place
  ;;TODO negative testing (invalid regexp etc.)
  (core/create-system system-1-file-cfg)
  (core/create-system system-2-files-cfg))


(def system-1-file  (core/create-system system-1-file-cfg))
(def system-2-files (core/create-system system-2-files-cfg))

(deftest all-file-paths-test
  (is (= #{"error.log"} (set (core/file-paths system-1-file))))
  (is (= #{"error.log" "warn.log"} (set (core/file-paths system-2-files)))))



(comment
(deftest alert-detection-single-file
  (let [file         nil
        problem-A    "problem A"
        problem-B    "problem B"
        results-nil  [(core/->CheckResult file #{})]
        results-A    [(core/->CheckResult file #{problem-A})]
        results-B    [(core/->CheckResult file #{problem-B})]
        results-AB   [(core/->CheckResult file #{problem-A problem-B})]]
    (testing "Alerts don't change"
      (let [results-nil->nil  (core/filter-out-seen-alerts results-nil results-nil)
            results-A->A      (core/filter-out-seen-alerts results-A results-A)
            results-AB->AB    (core/filter-out-seen-alerts results-AB results-AB)]
        (is (empty results-nil->nil))
        (is (empty results-A->A))
        (is (empty results-AB->AB))))
    (testing "One new alert"
      (let [results-nil->A  (core/filter-out-seen-alerts results-A results-nil)
            results-A->AB   (core/filter-out-seen-alerts results-AB results-A)]
        (is (and (= 1 (count results-nil->A))
                 (= #{problem-A} (:alerts (first results-nil->A)))))
        (is (and (= 1 (count results-A->AB))
                 (= #{problem-B} (:alerts (first results-A->AB)))))))
    (testing "Two new alerts"
      (let [results-nil->AB  (core/filter-out-seen-alerts results-AB results-nil)]
        (is (and (= 1 (count results-nil->AB))
                 (= #{problem-A problem-B} (:alerts (first results-nil->AB)))))))
    (testing "Alert goes away"
      (let [results-AB->A    (core/filter-out-seen-alerts results-A results-AB)
            results-AB->nil  (core/filter-out-seen-alerts results-nil results-AB)]
        (is (empty results-AB->A))
        (is (empty results-AB->nil))))
    (testing "One alert goes away, one new alert"
      (let [results-A->B  (core/filter-out-seen-alerts results-B results-A)]
        (is (and (= 1 (count results-A->B))
                 (is (= #{problem-B} (:alerts (first results-A->B))))))))))

(deftest alert-detection-two-files
  (let [file-1              (core/->WatchedFile "file-1" #".*")
        file-2              (core/->WatchedFile "file-2" #".*")
        problem-A           "problem A"
        problem-B           "problem B"
        problems-1nil_2nil  [(core/->CheckResult file-1 #{}) (core/->CheckResult file-2 #{})]
        problems-1A_2nil    [(core/->CheckResult file-1 #{problem-A}) (core/->CheckResult file-2 #{})]
        problems-1A_2A      [(core/->CheckResult file-1 #{problem-A}) (core/->CheckResult file-2 #{problem-A})]]
    (testing "Alerts don't change"
      (let [results-1nil_2nil->1nil_2nil (core/filter-out-seen-alerts problems-1nil_2nil problems-1nil_2nil)]
        (is (empty results-1nil_2nil->1nil_2nil))))
    (testing "One file new alert"
      (let [results-1nil_2nil->1A_2nil (core/filter-out-seen-alerts problems-1A_2nil problems-1nil_2nil)]
        (is (and (= 1 (count results-1nil_2nil->1A_2nil))
                 (= file-1 (:watched-file (first results-1nil_2nil->1A_2nil)))
                 (= #{problem-A} (:alerts (first results-1nil_2nil->1A_2nil)))))))
    (testing "Both files new alert"
      (let [results-1nil_2nil->1A_2A (core/filter-out-seen-alerts problems-1A_2A problems-1nil_2nil)]
        (is (and (= 2 (count results-1nil_2nil->1A_2A))
                 (every? #(= #{problem-A} %) (map #(:alerts %) results-1nil_2nil->1A_2A))))))))
)

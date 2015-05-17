(ns scratch
  (:require [clojure.repl :refer :all]
            [clojure.tools.namespace.repl :refer [refresh]]
            [log-watchdog.config :as config]
            [log-watchdog.core :as core]
            [log-watchdog.ui :as ui]))


;; refresh the required namespaces if they changed
(refresh)


;; configuration loading
(config/load-configuration)


;; test the file-checking functionality

(let [configuration (config/load-configuration)
      file (first (:files configuration))]
  (core/check-file file))

(let [configuration (config/load-configuration)
      files (:files configuration)]
  (core/check-files files))

(let [file1 (core/->WatchedFile "file1" #"regexp")
      cur  [(core/->CheckResult file1 #{"problem 1" "problem 2"})]
      prev [(core/->CheckResult file1 #{"problem 1" "problem 2"})]]
  (core/filter-out-seen-alerts cur prev))


;; reset the watcher agent
(send core/watcher (fn [_] #{}))

;; start the watcher agent
(let [configuration (config/load-configuration)
      files (:files configuration)
      notifier-fn (fn [new-problems] (println new-problems))
      intervalMs (:checkIntervalMs configuration)]
  (send core/watcher-running (fn [_] true))
  (send core/watcher (core/run-watcher-until-stopped-action-creator files notifier-fn intervalMs core/watcher-running)))

;; turn off the agent
(send core/watcher-running (fn [_] false))

;; check the agent states
@core/watcher-running
@core/watcher


;; start the app

(ui/-main)

(ns scratch
  (:require [clojure.repl :refer :all]
            [log-watchdog.config :as config]
            [log-watchdog.core :as core]
            [clojure.tools.namespace.repl :refer [refresh]]))


;; refresh the required namespaces if they changed
(refresh)

;; test the file-checking functionality

(let [file (first (config/files-to-watch))]
  (core/check-file file))

(let [files (config/files-to-watch)]
  (core/check-files files))

(let [file1 (core/->WatchedFile. "file1" #"regexp")
      cur  [(core/->CheckResult. file1 #{"problem 1" "problem 2"})]
      prev [(core/->CheckResult. file1 #{"problem 1" "problem 3"})]]
  (core/extract-new-problems cur prev))


;; reset the watcher agent
(send core/watcher (fn [_] #{}))

;; start the watcher agent
(let [files (config/files-to-watch)
      notifier-fn (fn [new-problems] (println new-problems))
      intervalMs 1000]
  (send core/watcher-running (fn [_] true))
  (send core/watcher (core/run-watcher-until-stopped-creator files notifier-fn intervalMs core/watcher-running)))

;; turn off the agent
(send core/watcher-running (fn [_] false))

;; check the agent states
@core/watcher-running
@core/watcher

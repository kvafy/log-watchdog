(ns log-watchdog.config
  (:require [log-watchdog.core :as core])
  (:gen-class))

; TODO not hard-coded, move to a config file
(defn files-to-watch []
  [(core/->WatchedFile "resources/sample.log" #"^(?<timestamp>[0-9.,: -]+) (?<loglevel>ERROR) (?<origin>[^-]+) - (?<message>.*)$")])

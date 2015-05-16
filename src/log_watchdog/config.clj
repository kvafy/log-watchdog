(ns log-watchdog.config
  (:require [log-watchdog.core :as core]
            [clojure.edn :as edn])
  (:gen-class))

(def config-file "configuration.edn")

; TODO supply default if config doesn't exist
(defn load-configuration
  ([]
    (load-configuration (slurp config-file)))
  ([config-string]
    (let [raw-edn (edn/read-string config-string)]
      {:checkIntervalMs (:checkIntervalMs raw-edn)
       :files (map #(core/->WatchedFile (:path %)
                                         (re-pattern (:regex %)))
                   (:files raw-edn))})))

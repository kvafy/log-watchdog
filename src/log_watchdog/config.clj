(ns log-watchdog.config
  (:require [log-watchdog.core :as core]
            [clojure.edn :as edn])
  (:import [java.io IOException])
  (:gen-class))

(def config-file "configuration.edn")


(defn load-configuration
  "Loads the configuration, either from the default file, or from a given EDN string.
  Returns a map {:checkIntervalMs <longValue>, :files <list-of-WatchedFile-records>}."
  ([]
    (try
      (load-configuration (slurp config-file))
      (catch IOException ex
        (throw (RuntimeException. (str "Error while reading the configuration file: " (.toString ex)))))))
  ([edn-string]
    (let [raw-edn (edn/read-string edn-string)]
      {:checkIntervalMs (:checkIntervalMs raw-edn)
       :files (map #(core/->WatchedFile (:path %)
                                         (re-pattern (:regex %)))
                   (:files raw-edn))})))

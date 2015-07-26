(ns log-watchdog.config
  (:require [clojure.edn]
            [schema.core :as s]
            [log-watchdog.validators :as validators])
  (:import [java.io IOException]))

(def config-file "configuration.edn")

(defn load-configuration
  "Loads the configuration, either from the default file, or from a given EDN string.
  Returns the following map:
    { :check-interval-ms <value>
      :nagging-interval-ms <value>
      :files
        { <file-path-1> { :line-regex <pattern>}
          ...
        }
    }.
  Throws an exception if the configuration cannot be read as edn or the edn doesn't
  meet criteria of the validator."
  ([]
    (try
      (load-configuration (slurp config-file))
      (catch IOException ex
        (throw (RuntimeException. (str "Error while reading the configuration file: " (.toString ex)))))))
  ([edn-string]
    (let [raw-edn (clojure.edn/read-string edn-string)]
      (s/validate validators/configuration-edn raw-edn)
      (let [check-interval-ms (long (:check-interval-ms raw-edn))
            nagging-interval-ms (long (:nagging-interval-ms raw-edn))
            files-map (into {}
                            (for [file-name (keys (get-in raw-edn [:files]))]
                              (let [pattern (re-pattern (get-in raw-edn [:files file-name :line-regex]))]
                                {file-name {:line-regex pattern}})))]
        {:check-interval-ms check-interval-ms
         :nagging-interval-ms nagging-interval-ms
         :files files-map}))))

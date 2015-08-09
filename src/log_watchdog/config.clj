(ns log-watchdog.config
  (:require [clojure.edn]
            [schema.core :as s]
            [log-watchdog.validators :as validators])
  (:import [java.io IOException]))

(def config-file "configuration.edn")

(def default-watched-file-group-name "Default")

(defn load-configuration
  "Loads the configuration, either from the default file, or from a given EDN string,
  adding default values when not specified.

  Returns the following map:
    { :check-interval-ms <value>
      :nagging-interval-ms <value>
      :files
        { <file-path-1>
          { :line-regex <pattern>
            :file-group <string>}
          ...
        }
    }.

  Throws an exception if the configuration cannot be read as edn or the edn doesn't
  meet criteria of the schema validator."
  ([]
    (try
      (load-configuration (slurp config-file))
      (catch IOException ex
        (throw (RuntimeException. (str "Error while reading the configuration file: " (.toString ex)))))))
  ([edn-string]
    (let [raw-edn (clojure.edn/read-string edn-string)]
      (s/validate validators/configuration-edn raw-edn)
      (let [files-map (into {}
                            (for [file-name (keys (get-in raw-edn [:files]))]
                              (let [pattern (re-pattern (get-in raw-edn [:files file-name :line-regex]))
                                    file-group (get-in raw-edn [:files file-name :file-group] default-watched-file-group-name)]
                                {file-name {:line-regex pattern :file-group file-group}})))]
        {:check-interval-ms (long (:check-interval-ms raw-edn))
         :nagging-interval-ms (long (:nagging-interval-ms raw-edn))
         :files files-map}))))

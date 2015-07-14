(ns log-watchdog.core
  (:require [clojure.set]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]))

;;TODO add map structure constraints/hints

; 'system' is the following data structure holding complete application state:
;
;   { :check-interval-ms value
;     :files
;       {  "file-path-A"
;            { :line-regex "pattern-instance"
;              :alert-lines #{line1 line2 ... lineN}
;              :unacknowledged-alert-lines #{line1 line2 ... lineM}}
;          "file-path-B"
;            {...}
;          ...
;       }
;   }

; holds instance of the system
(def system (atom nil))

(defn create-system
  "Creates a system instance based on a configuration map as returned by log-watchdod.config/load-configuration."
  [config]
  (let [config-map {:check-interval-ms (get-in config [:check-interval-ms])}
        files-map (into {}
                        (for [file-name (keys (get-in config [:files]))]
                          {file-name {:line-regex (get-in config [:files file-name :line-regex])
                                      :alert-lines #{}
                                      :unacknowledged-alert-lines #{}}}))]
    (-> {}
        (into config-map)
        (into {:files files-map}))))

(defn all-file-paths [system]
  (keys (get-in system [:files])))

;; logic of checking files

(defn check-file
  "Checks current alerts in given file and updates part of the system which represents this file."
  [system file-path]
  {:pre (contains? system file-path)}
  (try
    (with-open [reader (io/reader file-path)]
      (let [regex (get-in system [:files file-path :line-regex])
            prev-alert-lines (get-in system [:files file-path :alert-lines])
            cur-alert-lines (->> (line-seq reader)
                                 (filter #(re-matches regex %))
                                 (set))
            new-alert-lines (clojure.set/difference cur-alert-lines prev-alert-lines)]
        (-> system
            (update-in [:files file-path :alert-lines] clojure.set/union new-alert-lines)
            (update-in [:files file-path :unacknowledged-alert-lines] clojure.set/union new-alert-lines))))
    (catch java.io.IOException ex
      (do
        (log/error ex (str "Failed to read file " file-path))
        system))))

(defn update-system-by-checking-files
  "Updates the system by checking all files and updating their maps with newly found alert lines."
  [system]
  (reduce (fn [sys file-path]
            (check-file sys file-path))
          system
          (all-file-paths system)))

(defn update-system-by-acknowledging-alerts
  "Updates the system by acknowledging alerts for given files. If no files are given, then
  acknowledges acknowledges alerts for all files."
  [system & file-paths]
  (let [file-paths (if (empty? file-paths)
                     (all-file-paths system)
                     file-paths)]
    (reduce (fn [sys file-path]
              (update-in system [:files file-path :unacknowledged-alert-lines] empty))
            system
            file-paths)))


(defn reset-system!
  "Clears current state of the watcher agent and sets up new state, saying that
  the agent is watching given set of files and so far has seen nothing."
  [config]
  (let [new-system (create-system config)]
    (swap! system (fn [_] new-system))))

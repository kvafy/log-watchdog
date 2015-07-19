(ns log-watchdog.core
  (:require [clojure.set]
            [clojure.zip :as zip]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [log-watchdog.util :as util]))

;;TODO add map structure constraints/hints using the bouncer

; 'system' is the following data structure holding complete application state:
;
;   { :check-interval-ms value
;
;     :files
;       {  "file-path-A"
;            { :line-regex "pattern-instance"
;              :alerts
;                { "<line1>"
;                    { :last-seen-timestamp <timestampMs>
;                      :acknowledged <true/false>}
;                  ...
;                  "<lineN>"
;                    { ... }
;                }
;          "file-path-B"
;            {...}
;          ...
;       }
;   }
;

; holds instance of the system
(def system (atom nil))

(defn create-system
  "Creates a system instance based on a configuration map as returned by log-watchdog.config/load-configuration."
  [config]
  (let [base-map {:last-unacknowledged-notification-timestamp nil}
        config-map {:check-interval-ms (get-in config [:check-interval-ms])}
        files-map (into {}
                        (for [file-name (keys (get-in config [:files]))]
                          {file-name {:line-regex (get-in config [:files file-name :line-regex])
                                      :alerts {}}}))]
    (-> base-map
        (into config-map)
        (into {:files files-map}))))

(defn reset-system!
  "Clears current state of the 'system' atom and sets up a fresh system in which no check has been performed yet."
  [config]
  (let [new-system (create-system config)]
    (swap! system (fn [_] new-system))))


;; creating parts of a system

(defn create-alert [timestamp-ms]
  {:last-seen-timestamp timestamp-ms
   :acknowledged false})


;; inspecting/modifying a system

(defn file-paths
  ([system]
    (file-paths system (fn [_] true)))
  ([system pred]
    (->> (get-in system [:files])
         (filter pred)
         (map (fn [[file-path file-data]] file-path)))))

(defn alerts
  ([system]
    (alerts system (file-paths system)))
  ([system file-paths]
    (alerts system file-paths (fn [_] true)))
  ([system file-paths pred]
    {:pre (seq? file-paths)}
    (->> file-paths
         (map (fn [file-path] (get-in system [:files file-path :alerts])))
         (apply merge)
         (filter pred)
         (into {}))))

; predicates to use with 'file-paths' and 'alerts' functions
(declare unacknowledged-alert?)
(defn file-has-unacknowledged-alert? [[file-path {:keys [alerts]}]]
  (some unacknowledged-alert? alerts))

(defn unacknowledged-alert? [[alert-line alert-data]]
  (not (:acknowledged alert-data)))


;; logic of checking files

(defn check-file
  "Checks current alerts in given file and updates part of the system which represents this file."
  [system file-path]
  {:pre (contains? system file-path)}
  (try
    (with-open [reader (io/reader file-path)]
      (let [now (util/current-time-ms)
            regex (get-in system [:files file-path :line-regex])
            cur-alert-lines (->> (line-seq reader)
                                 (filter #(re-matches regex %)))]
        (reduce (fn [sys alert-line]
                  (if (contains? (alerts sys [file-path]) alert-line)
                    sys
                    (assoc-in sys [:files file-path :alerts alert-line] (create-alert now))))
                system
                cur-alert-lines)))
    (catch java.io.IOException ex
      (do
        (log/error ex (str "Failed to read file " file-path))
        system))))

(defn update-system-by-checking-files
  "Updates the system by checking all files and updating their maps with newly found alert lines."
  [system]
  (reduce (fn [sys file-path] (check-file sys file-path))
          system
          (file-paths system)))


;; acknowledging alerts

(defn update-system-by-acknowledging-alerts
  "Updates the system by acknowledging all alerts for given files. If no files are given, then
  acknowledges alerts for all files."
  [system & file-paths-to-ack]
  (let [file-paths-to-ack (if (empty? file-paths-to-ack)
                     (file-paths system)
                     file-paths-to-ack)]
    ;TODO use zippers for more idiomatic solution
    (reduce (fn [sys [file-path alert-line]]
              (assoc-in sys [:files file-path :alerts alert-line :acknowledged] true))
            system
            (for [file-path file-paths-to-ack
                  [alert-line _] (alerts system [file-path])]
              (vector file-path alert-line)))))

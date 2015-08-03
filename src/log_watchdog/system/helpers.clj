(ns log-watchdog.system.helpers
  (:require [clojure.set]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [log-watchdog.system.core :as core]))

;; Contains complex or specific operations to read/modify a system.
;; These operations don't logically belong into the log-watchdog.system.core
;; namespace which is minimalistic.


;; read operations

(defn last-notification-timestamp [system]
  (let [[notifications-id notifications-data] (first (core/query system (core/entity-pred :type (partial = :notifications))))]
    (get notifications-data :last-notification-timestamp)))

(defn- configuration [system]
  (core/query system (core/entity-pred :type (partial = :configuration))))

(defn configuration-data [system]
  (let [[config-id config-data] (first (configuration system))]
    config-data))

(defn unacknowledged-alerts [system]
  (core/query system (core/entity-pred :type (partial = :alert)
                                       :acknowledged false?)))


(defn- files-for-alerts [system alerts]
  (let [file-ids (->> alerts
                      (map (fn [[alert-id alert-data]] (get alert-data :watched-file-id)))
                      (set))]
    (into {} (filter (fn [[file-id _]] (file-ids file-id)) system))))


(defn files-having-unacknowledged-alerts [system]
  (files-for-alerts system (unacknowledged-alerts system)))


(defn files-having-last-check-failed [system]
  (core/query system (core/entity-pred :type (partial = :watched-file)
                                       :last-check-failed true?)))


;; compare predicates

(defn has-new-alert? [prev-system cur-system]
  (let [prev-alerts (core/query prev-system (core/entity-pred :type (partial = :alert)))
        cur-alerts  (core/query cur-system  (core/entity-pred :type (partial = :alert)))
        prev-alert-ids (map first prev-alerts)
        cur-alert-ids  (map first cur-alerts)
        new-alert-ids (clojure.set/difference (set cur-alert-ids) (set prev-alert-ids))]
    (not-empty new-alert-ids)))

(defn has-new-failed-file? [prev-system cur-system]
  (let [prev-failed-files (files-having-last-check-failed prev-system)
        cur-failed-files  (files-having-last-check-failed cur-system)
        prev-failed-file-ids (map first prev-failed-files)
        cur-failed-file-ids  (map first cur-failed-files)
        new-failed-file-ids (clojure.set/difference (set cur-failed-file-ids) (set prev-failed-file-ids))]
    (not-empty new-failed-file-ids)))


;; update operations

(defn set-last-notification-timestamp [system value]
  (let [[notifications-id notifications-data] (first (core/query system (core/entity-pred :type (partial = :notifications))))]
    (assoc system notifications-id (assoc notifications-data :last-notification-timestamp value))))

(defn toggle-check-enabled [system]
  (let [[cfg-id cfg-data] (first (configuration system))
        old-check-enabled (get cfg-data :check-enabled)]
    (assoc system cfg-id (assoc cfg-data :check-enabled (not old-check-enabled)))))


(defn acknowledge-alerts
  "Updates the system by acknowledging all alerts for given files."
  ([system]
    (let [all-files (core/query system (core/entity-pred :type (partial = :watched-file)))
          all-file-ids (map (fn [[file-id file-data]] file-id) all-files)]
      (acknowledge-alerts system all-file-ids)))
  ([system file-ids-to-ack]
    (let [alerts-to-ack (core/query system (core/entity-pred :type (partial = :alert)
                                                             :watched-file-id (partial contains? (apply hash-set file-ids-to-ack))))]
      (reduce (fn [sys [alert-id alert-data]]
                (assoc sys alert-id (assoc alert-data :acknowledged true)))
              system
              alerts-to-ack))))

(defn- create-or-update-alerts [system file-id matched-alert-lines]
  (reduce (fn [sys matched-line]
            (if-let [[alert-id alert-data] (first (core/query sys (core/entity-pred :type (partial = :alert)
                                                                                    :matched-line (partial = matched-line)
                                                                                    :watched-file-id (partial = file-id))))]
              sys
              (core/add-entity sys
                               (core/create-entity :alert
                                                   :matched-line matched-line
                                                   :acknowledged false
                                                   :watched-file-id file-id))))
          system
          matched-alert-lines))

(defn- check-file
  "Checks current alerts in given file and updates part of the system which represents this file."
  [system file-id]
  (let [file-data (get system file-id)
        file-path (get file-data :file)
        regex (get file-data :line-regex)]
    (try
      (with-open [reader (io/reader file-path)]
        (let [matched-lines (->> (line-seq reader)
                                 (filter #(re-matches regex %)))]
          (-> system
              (create-or-update-alerts file-id matched-lines)
              (assoc file-id (assoc file-data :last-check-failed false)))))
      (catch java.io.IOException ex
        (do
          (log/warn (format "Failed to read file '%s': %s" file-path ex))
          (assoc system file-id (assoc file-data :last-check-failed true)))))))

(defn check-files
  "Updates the system by checking all files or only specified files and updating the system with findings."
  ([system]
    (let [all-files (core/query system (core/entity-pred :type (partial = :watched-file)))
          all-file-ids (map first all-files)]
      (check-files system all-file-ids)))
  ([system file-ids]
    (reduce (fn [sys file-id] (check-file sys file-id))
            system
            file-ids)))

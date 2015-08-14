(ns log-watchdog.system.helpers
  (:require [clojure.set]
            [clojure.java.io]
            [clojure.tools.logging :as log]
            [log-watchdog.system.core :as core]
            [log-watchdog.config :as config]
            [log-watchdog.io]))

;; Contains complex or specific operations to read/modify a system.
;; These operations don't logically belong into the log-watchdog.system.core
;; namespace which is minimalistic.


;; read operations

(defn last-notification-timestamp [system]
  (let [[notifications-id notifications-data] (core/query-singleton system (core/entity-pred :type (partial = :notifications)))]
    (:last-notification-timestamp notifications-data)))

(defn configuration-data [system]
  (let [[config-id config-data] (core/query-singleton system (core/entity-pred :type (partial = :configuration)))]
    config-data))


(defn referenced-entity
  "Crawls the entity graph starting from 'base-entity' which
  references another entity by first key in referenced-entity-id-keys.
  Rest of the entities along the dependency graph reference another
  entities by the rest of the referenced-entity-id-keys.
  Returns the final entity when no more keys are left."
  [system base-entity & referenced-entity-id-keys]
  (if (empty? referenced-entity-id-keys)
    base-entity
    (let [[_ base-data] base-entity
          ref-id (get base-data (first referenced-entity-id-keys))
          ref-data (get system ref-id)
          ref-entity [ref-id ref-data]]
      (recur system ref-entity (next referenced-entity-id-keys)))))

(defn group-entities-by-referenced-entity
  "Each of the given entities has a 'referenced-entity-id-key' in its data
  and this key references an entity.
  The function groups the given entities by referenced entity into a map
  of form {referenced-entity [entities-to-group]}"
  [system entities-to-group & referenced-entity-id-keys]
  (letfn [(grouping-fn [ent]
            (apply referenced-entity system ent referenced-entity-id-keys))]
    (group-by grouping-fn entities-to-group)))

(defn files-by-file-group
  "Returns a map {watched-file-group-entity [watched-file-entity]}."
  [system]
  (group-entities-by-referenced-entity system
                                       (core/query system (core/entity-pred :type (partial = :watched-file)))
                                       :watched-file-group-id))

(defn unacknowledged-alerts [system]
  (core/query system (core/entity-pred :type (partial = :alert)
                                       :acknowledged false?)))

(defn unacknowledged-alerts-by-file
  "Returns a map {watched-file-entity [alert-entity]}."
  [system]
  (group-entities-by-referenced-entity system
                                       (unacknowledged-alerts system)
                                       :watched-file-id))

(defn unacknowledged-alerts-by-file-group
  "Returns a map {watched-file-group-entity [alert-entity]}."
  [system]
  (group-entities-by-referenced-entity system
                                       (unacknowledged-alerts system)
                                       :watched-file-id :watched-file-group-id))

(defn unreadable-files [system]
  (core/query system (core/entity-pred :type (partial = :watched-file)
                                       :last-check-failed true?)))

(defn unreadable-files-by-file-group
  "Returns a map {watched-file-group-entity [watched-file-entity]}."
  [system]
  (group-entities-by-referenced-entity system
                                       (unreadable-files system)
                                       :watched-file-group-id))

(defn uses-defeault-file-group-only? [system]
  (let [file-groups (core/query system (core/entity-pred :type (partial = :watched-file-group)))
        file-group-names (map (fn [[_ group-data]] (:name group-data)) file-groups)]
    (= #{config/default-watched-file-group-name} (set file-group-names))))


;; systems-comparing predicates

(defn has-new-alert? [prev-system cur-system]
  (let [prev-alerts (core/query prev-system (core/entity-pred :type (partial = :alert)))
        cur-alerts  (core/query cur-system  (core/entity-pred :type (partial = :alert)))
        prev-alert-ids (map first prev-alerts)
        cur-alert-ids  (map first cur-alerts)
        new-alert-ids (clojure.set/difference (set cur-alert-ids) (set prev-alert-ids))]
    (not-empty new-alert-ids)))

(defn has-new-unreadable-file? [prev-system cur-system]
  (let [prev-unreadable-files (unreadable-files prev-system)
        cur-unreadable-files  (unreadable-files cur-system)
        prev-unreadable-file-ids (map first prev-unreadable-files)
        cur-unreadable-file-ids  (map first cur-unreadable-files)
        new-unreadable-file-ids (clojure.set/difference (set cur-unreadable-file-ids) (set prev-unreadable-file-ids))]
    (not-empty new-unreadable-file-ids)))


;; update operations

(defn set-last-notification-timestamp [system value]
  (let [[notifications-id notifications-data] (core/query-singleton system (core/entity-pred :type (partial = :notifications)))]
    (core/add-entity system [notifications-id (assoc notifications-data :last-notification-timestamp value)])))

(defn toggle-check-enabled [system]
  (let [[cfg-id cfg-data] (core/query-singleton system (core/entity-pred :type (partial = :configuration)))
        old-check-enabled (:check-enabled cfg-data)]
    (core/add-entity system [cfg-id (assoc cfg-data :check-enabled (not old-check-enabled))])))

(defn acknowledge-alert [system alert]
  (let [[alert-id alert-data] alert]
    (core/add-entity system [alert-id (assoc alert-data :acknowledged true)])))

(defn acknowledge-alerts
  "Updates the system by acknowledging all alerts for given files."
  ([system]
    (let [all-files (core/query system (core/entity-pred :type (partial = :watched-file)))]
      (acknowledge-alerts system all-files)))
  ([system files-to-ack]
    (let [file-ids-to-ack (set (map first files-to-ack))
          alerts-to-ack (core/query system (core/entity-pred :type (partial = :alert)
                                                             :watched-file-id (partial contains? file-ids-to-ack)))]
      (reduce acknowledge-alert system alerts-to-ack))))

(defn- create-or-update-alerts [system file-id matched-alert-lines]
  (reduce (fn [sys matched-line]
            (if-let [[alert-id alert-data] (core/query-singleton sys (core/entity-pred :type (partial = :alert)
                                                                                       :matched-line (partial = matched-line)
                                                                                       :watched-file-id (partial = file-id)))]
              sys
              (core/add-entity sys
                               (core/create-entity :alert
                                                   :matched-line matched-line
                                                   :acknowledged false
                                                   :watched-file-id file-id))))
          system
          matched-alert-lines))

(defn- perform-file-check? [file-data]
  (let [file (:file file-data)]
    (if-not (log-watchdog.io/file-exists? file)
      true ; perform the check - the checking mechanism needs to fail and take a note about it
      (or (not= (log-watchdog.io/file-last-modified-ms file) (:file-last-modified-ms file-data))
          (not= (log-watchdog.io/file-size file) (:file-last-size-b file-data))
          (:always-check-override file-data)))))

(defn- check-file
  "Checks current alerts in given file and updates part of the system which represents this file."
  [system file-entity]
  (let [[file-id file-data] file-entity
        perform-file-check? (perform-file-check? file-data)]
    (log/debugf "(perform-file-check? \"%s\") -> %s" (:file file-data) perform-file-check?)
    (if-not perform-file-check?
      system
      (try
        (with-open [reader (clojure.java.io/reader (:file file-data))]
          (let [matched-lines (->> (line-seq reader)
                                   (filter #(re-matches (:line-regex file-data) %)))]
            (-> system
                (create-or-update-alerts file-id matched-lines)
                (core/add-entity [file-id (assoc file-data :last-check-failed false
                                                           :file-last-size-b (log-watchdog.io/file-size (:file file-data))
                                                           :file-last-modified-ms (log-watchdog.io/file-last-modified-ms (:file file-data)))]))))
        (catch java.io.IOException ex
          (do
            (log/warnf "Failed to read file '%s': %s" (:file file-data) ex)
            (core/add-entity system [file-id (assoc file-data :last-check-failed true
                                                              :file-last-size-b nil
                                                              :file-last-modified-ms nil)])))))))

(defn check-files
  "Updates the system by checking all files or only specified files and updating the system with findings."
  ([system]
    (let [all-files (core/query system (core/entity-pred :type (partial = :watched-file)))]
      (check-files system all-files)))
  ([system files]
    (reduce check-file system files)))

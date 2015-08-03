(ns log-watchdog.system.core
  (:require [log-watchdog.utils :as utils]))


;; An 'entity' is a 2-tuple (uuid entity-data).
;; A 'system' is a uuid->entity-data map holding complete application state.
;; This namespace defines CRUD operations for (parts of) systems.


; Crud

(declare add-entity)

(defn create-entity [entity-type & kvs]
  (into {:type entity-type} (apply hash-map kvs)))

(defn create-system
  "Creates a system instance based on a configuration map as returned by log-watchdog.config/load-configuration."
  [config]
  (let [cfg-entity (create-entity :configuration
                                  :check-interval-ms    (get-in config [:check-interval-ms])
                                  :nagging-interval-ms  (get-in config [:nagging-interval-ms])
                                  :check-enabled        true)
        notifications-entity (create-entity :notifications
                                            :last-notification-timestamp 0N)
        file-entities (for [file-name (keys (get-in config [:files]))]
                             (create-entity :watched-file
                                            :file              (java.io.File. file-name)
                                            :line-regex        (get-in config [:files file-name :line-regex])
                                            :last-check-failed false))
        all-entities (concat [cfg-entity notifications-entity] file-entities)]
    (reduce add-entity {} all-entities)))


; cRud

(defn query
  "Queries system for entities matching given entity predicate.
  Returns map of entity-id->entity-data."
  [system pred]
  (into {} (filter pred system)))

(defn entity-pred
  "Expects sequence of keys and value predicates 'k vpred k vpred k vpred ...'.
  Creates a composite predicate that returns true for entity such that
  all '(vpred (get entity-data k))' return true."
  [& kvpreds]
  (fn [[entity-id entity-data]]
    (every? identity (map (fn [[k vpred]]
                           (vpred (get entity-data k)))
                         (partition 2 kvpreds)))))


; crUd

(defn add-entity [system entity-data]
  (let [uuid (utils/uuid)]
    (assoc system uuid entity-data)))


; cruD

(defn remove-entity [system entity-id]
  (dissoc system entity-id))

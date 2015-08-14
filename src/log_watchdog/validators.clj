(ns log-watchdog.validators
  (:require [schema.core :as s]))

;; Validators for log-watchdog.configuration

(def configuration-edn
  { :files
      { s/Str
        { :line-regex s/Str
          (s/optional-key :file-group) s/Str
          (s/optional-key :always-check-override) s/Bool
          (s/optional-key :never-seek-override) s/Bool}}
    :check-interval-ms s/Int
    :nagging-interval-ms s/Int})

(def configuration
  { :check-interval-ms s/Int
    :nagging-interval-ms s/Int
    :files
      { s/Str
        { :line-regex s/Regex
          :file-group s/Str
          :always-check-override s/Bool
          :never-seek-override s/Bool}}})


;; Validators for system entities

(defmulti entity-validator #(:type %))

(defn validate-system [system]
  (doseq [[entity-id entity-data] system]
    (s/validate s/Uuid entity-id)
    (s/validate (entity-validator entity-data) entity-data)))

; validators for entities from log-watchdog.system.*

(defmethod entity-validator :configuration [entity]
  { :type s/Keyword
    :check-interval-ms s/Int
    :nagging-interval-ms s/Int
    :check-enabled s/Bool})

(defmethod entity-validator :watched-file [entity]
  { :type s/Keyword
    :file java.io.File
    :line-regex s/Regex
    :last-check-failed s/Bool
    :file-last-size-b (s/maybe s/Int)
    :file-last-modified-ms (s/maybe s/Int)
    :watched-file-group-id s/Uuid
    :always-check-override s/Bool
    :never-seek-override s/Bool})

(defmethod entity-validator :watched-file-group [entity]
  { :type s/Keyword
    :name (s/maybe s/Str)}) ; default file group has nil for name

(defmethod entity-validator :alert [entity]
  { :type s/Keyword
    :matched-line s/Str
    :acknowledged s/Bool
    :watched-file-id s/Uuid})

; validators for entities from log-watchdog.ui.*

(defmethod entity-validator :notifications [entity]
  { :type s/Keyword
    :last-notification-timestamp s/Int})

(defmethod entity-validator :ui-tray-icon [entity]
  { :type s/Keyword
    :value java.awt.TrayIcon})

(defmethod entity-validator :ui-ack-alerts-menu-button [entity]
  { :type s/Keyword
    :value java.awt.MenuItem
    :linked-entity-id (s/maybe s/Uuid)})

(defmethod entity-validator :ui-toggle-check-enabled-menu-button [entity]
  { :type s/Keyword
    :value java.awt.MenuItem})

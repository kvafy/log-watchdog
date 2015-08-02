(ns log-watchdog.validators
  (:require [schema.core :as s]))

;; Validators for log-watchdog.configuration

(def configuration-edn
  { :check-interval-ms s/Int
    :nagging-interval-ms s/Int
    :files
      { s/Str
        { :line-regex s/Str}}})

(def configuration
  { :check-interval-ms s/Int
    :nagging-interval-ms s/Int
    :files
      { s/Str {:line-regex s/Regex}}})


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
    :last-check-failed s/Bool})

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

(defmethod entity-validator :ui-ack-all-alerts-menu-button [entity]
  { :type s/Keyword
    :value java.awt.MenuItem})

(defmethod entity-validator :ui-toggle-check-enabled-menu-button [entity]
  { :type s/Keyword
    :value java.awt.MenuItem})

(ns log-watchdog.validators
  (:require [schema.core :as s]))

(def configuration
  { :check-interval-ms s/Int
    :nagging-interval-ms s/Int
    :files
      {s/Str {:line-regex s/Regex}}})

; Structure of a system and semantics of its parts:
;   { :check-enabled <true/false>
;     :check-interval-ms <timestamp-ms>
;     :nagging-interval-ms <timestamp-ms>
;
;     :last-notification-timestamp <timestamp-ms>
;
;     :ui
;       {  :tray-icon <instance>}
;
;     :files
;       {  "file-path-A"
;            { :line-regex "pattern-instance"
;              :last-check-failed <false/true>
;              :alerts
;                { "<line1>"
;                    { :acknowledged <true/false>}
;                  ...
;                  "<lineN>"
;                    { ... }
;                }
;          "file-path-B"
;            {...}
;          ...
;       }
;   }

(def system
  { :check-enabled s/Bool
    :check-interval-ms s/Int
    :nagging-interval-ms s/Int
    :last-notification-timestamp s/Int
    (s/optional-key :ui) clojure.lang.PersistentArrayMap
    :files
      { s/Str
        { :line-regex s/Regex
          :last-check-failed s/Bool
          :alerts
            { s/Str
               { :acknowledged s/Bool}}
          (s/optional-key :ui)
            { :menu-item java.awt.MenuItem}}}})

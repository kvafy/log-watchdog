(ns log-watchdog.validators
  (:require [schema.core :as s]))

(def configuration
  { :check-interval-ms s/Int
    :nagging-interval-ms s/Int
    :files
      {s/Str {:line-regex s/Regex}}})

; Structure of a system and semantics of its parts:
;   { :check-interval-ms <timestamp-ms>
;     :nagging-interval-ms <timestamp-ms>
;
;     :last-notification-timestamp <timestamp-ms>
;
;     :files
;       {  "file-path-A"
;            { :line-regex "pattern-instance"
;              :alerts
;                { "<line1>"
;                    { :last-seen-timestamp <timestamp-ms>
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

(def system
  { :check-interval-ms s/Int
    :nagging-interval-ms s/Int
    :last-notification-timestamp s/Int
    :files
      { s/Str
        { :line-regex s/Regex
          :alerts
            { s/Str
               { :last-seen-timestamp s/Int
                 :acknowledged s/Bool}}}}})

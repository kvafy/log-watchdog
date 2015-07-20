(ns log-watchdog.validators
  (:require [schema.core :as s]))

(def configuration
  {:check-interval-ms s/Int
   :nagging-interval-ms s/Int
   :files {s/Str {:line-regex java.util.regex.Pattern}}})

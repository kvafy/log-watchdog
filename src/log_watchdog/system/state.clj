(ns log-watchdog.system.state
  (:require [log-watchdog.system.core :as system-core]))

; Atom holding instance of the system.
; State of the system is modified using functions in 'log-watchdog.system.core'
; and 'log-watchdog.system.helpers' namespaces.
(def system (atom nil))

(defn reset-system!
  "Clears current state of the 'system' atom and sets up a fresh system in which no check has been performed yet."
  [config]
  (let [new-system (system-core/create-system config)]
    (swap! system (fn [_] new-system))))

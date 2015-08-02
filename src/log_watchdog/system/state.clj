(ns log-watchdog.system.state
  (:require [log-watchdog.system.core :as system-core]))

; atom holding instance of the system
(def system (atom nil))

(defn reset-system!
  "Clears current state of the 'system' atom and sets up a fresh system in which no check has been performed yet."
  [config]
  (let [new-system (system-core/create-system config)]
    (swap! system (fn [_] new-system))))

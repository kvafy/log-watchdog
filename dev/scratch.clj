(ns scratch
  (:require [clojure.repl :refer :all]
            [clojure.tools.namespace.repl :refer [refresh]]
            [log-watchdog.system.core :as system-core]
            [log-watchdog.system.helpers :as system-helpers]
            [log-watchdog.system.state :as system-state]
            [log-watchdog.config :as config]
            [log-watchdog.ui.core :as ui-core]
            [clojure.java.io]))


;; refresh the required namespaces if they changed
(refresh)

;; configuration loading
(config/load-configuration)

;; creating initial system from configuration
(system-core/create-system (config/load-configuration))

;; test the file-checking functionality
(let [configuration (config/load-configuration)
      system-instance (system-core/create-system configuration)]
  (system-helpers/check-files system-instance))


;; initialize the system
(let [configuration (config/load-configuration)]
  (system-state/reset-system! configuration))

;; explicitly invoke single system update
(swap! system-state/system system-helpers/check-files)

;; enable/disable the file checking
(swap! system-state/system system-helpers/toggle-check-enabled)

;; inspect the system state
@system-state/system


;; start the swing UI app
(ui-core/-main)

;; stop the app
(System/exit 0)

(ns scratch
  (:require [clojure.repl :refer :all]
            [clojure.tools.namespace.repl :refer [refresh]]
            [log-watchdog.system :as system]
            [log-watchdog.config :as config]
            [log-watchdog.ui :as ui]))


;; refresh the required namespaces if they changed
(refresh)

;; configuration loading
(config/load-configuration)

;; creating initial system from configuration
(system/create (config/load-configuration))

;; test the file-checking functionality
(let [configuration (config/load-configuration)
      system-instance (system/create configuration)]
  (system/check-files system-instance))



;; initialize the system
(let [configuration (config/load-configuration)]
  (system/reset! configuration))

;; explicitly invoke single system update
(swap! system/system system/check-files)

;; enable/disable the file checking
(swap! system/system system/toggle-check-enabled)

;; inspect the system state
@system/system


;; start the swing UI app
(ui/-main)

;; stop the app
(System/exit 0)

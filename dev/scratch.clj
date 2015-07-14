(ns scratch
  (:require [clojure.repl :refer :all]
            [clojure.tools.namespace.repl :refer [refresh]]
            [log-watchdog.core :as core]
            [log-watchdog.config :as config]
            [log-watchdog.ui :as ui]))


;; refresh the required namespaces if they changed
(refresh)

;; configuration loading
(config/load-configuration)

;; creating initial system from configuration
(core/create-system (config/load-configuration))

;; test the file-checking functionality
(let [configuration (config/load-configuration)
      system (core/create-system configuration)]
  (core/update-system-by-checking-files system))



;; initialize the system
(let [configuration (config/load-configuration)]
  (core/reset-system! configuration))

;; explicitly invoke single system update
(swap! core/system core/update-system-by-checking-files)

;; enable/disable the watcher thread
(swap! ui/watcher-enabled (fn [state] (not state)))

;; inspect the system and watcher state
@core/system
@ui/watcher-enabled


;; start the swing UI app
(ui/-main)

;; stop the app
(System/exit 0)

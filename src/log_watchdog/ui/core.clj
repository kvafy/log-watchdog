(ns log-watchdog.ui.core
  (:require [clojure.set]
            [clojure.tools.logging :as log]
            [log-watchdog.system.core :as system-core]
            [log-watchdog.system.helpers :as system-helpers]
            [log-watchdog.system.state :as system-state]
            [log-watchdog.utils :as utils]
            [log-watchdog.ui.utils :as ui-utils]
            [log-watchdog.ui.messages :as messages]
            [log-watchdog.config :as config]
            [log-watchdog.validators :as validators])
  (:import [java.awt SystemTray TrayIcon TrayIcon$MessageType PopupMenu])
  (:gen-class)) ; needed for uberjar because this package contains -main function


;; CRUD for ui objects represented as entities of the system

(defn create-ui-entity [entity-type value]
  (system-core/create-entity entity-type
                             :value value))

(defn ui-entity-value
  "A 'ui property' is a singleton entity holding instance of a UI Java object.
  This accessor extracts instance of the Java object from the entity."
  [system entity-type]
  (let [[entity-id entity-data] (first (system-core/query system
                                                          (system-core/entity-pred :type (partial = entity-type))))]
    (get entity-data :value)))


;; periodic actions

(defn start-watcher-thread!
  "Initializes and starts a thread that periodically updates the system by performing check of watched files."
  []
  (letfn [(system-updating-fn []
            (loop []
              (let [config-data (system-helpers/configuration-data @system-state/system)]
                (when (get config-data :check-enabled)
                  (log/info "Checking files...")
                  (swap! system-state/system system-helpers/check-files)
                  (log/trace @system-state/system))
                (Thread/sleep (get config-data :check-interval-ms))
                (recur))))]
    (doto (Thread. system-updating-fn)
      (.setDaemon true)
      (.start))))


;; observers of the system state and reacting to the current state

(defn update-tray-icon! [{:keys [cur-system config-data unacked-files unreadable-files]}]
  (let [paused? (not (get config-data :check-enabled))
        any-alert? (not-empty unacked-files)
        any-warning? (not-empty unreadable-files)]
    (let [icon-name (str "icon" (if paused? "P" "") (if any-alert? "A" "") (if any-warning? "W" "") ".png")]
      (.setImage (ui-entity-value cur-system :ui-tray-icon)
                 (ui-utils/load-image icon-name)))))

(defn update-tray-tooltip! [{:keys [cur-system] :as info-map}]
  (.setToolTip (ui-entity-value cur-system :ui-tray-icon)
               (messages/short-status info-map)))

(defn update-menu-items! [{:keys [cur-system config-data unacked-alerts]}]
  (doto (ui-entity-value cur-system :ui-ack-all-alerts-menu-button)
    (.setLabel (format "Acknowledge all alerts (%d)" (count unacked-alerts)))
    (.setEnabled (not (empty? unacked-alerts))))
  (doto (ui-entity-value cur-system :ui-toggle-check-enabled-menu-button)
    (.setLabel (if (get config-data :check-enabled)
                 "Disable file checking"
                 "Enable file checking"))))

(defn show-balloon-notification! [{:keys [cur-system] :as info-map}]
  (.displayMessage (ui-entity-value cur-system :ui-tray-icon)
                   (messages/short-status info-map)
                   (messages/long-status info-map)
                   TrayIcon$MessageType/WARNING))

(defn maybe-show-balloon-notification! [{:keys [prev-system cur-system config-data unreadable-files unacked-alerts] :as info-map}]
  (let [has-new-alert? (system-helpers/has-new-alert? prev-system cur-system)
        has-new-unreadable-file? (system-helpers/has-new-unreadable-file? prev-system cur-system)
        can-nag-now? (< (+ (system-helpers/last-notification-timestamp cur-system)
                           (get config-data :nagging-interval-ms))
                        (utils/current-time-ms))]
    (when (or has-new-unreadable-file?
              (and (not-empty unacked-alerts)
                   (or has-new-alert? can-nag-now?)))
      (show-balloon-notification! info-map)
      (swap! system-state/system system-helpers/set-last-notification-timestamp (utils/current-time-ms)))))

(defn process-new-system-notification [_ _ prev-system cur-system]
  ; extract commonly used values into a map and pass that map around
  (let [info-map {:prev-system    prev-system
                  :cur-system     cur-system
                  :config-data    (system-helpers/configuration-data cur-system)
                  :uses-groups?   (not (system-helpers/uses-defeault-file-group-only? cur-system))
                  :unacked-alerts-by-file (system-helpers/unacknowledged-alerts-by-file cur-system)
                  :unacked-alerts-by-group (system-helpers/unacknowledged-alerts-by-file-group cur-system)
                  :unacked-alerts (system-helpers/unacknowledged-alerts cur-system)
                  :unreadable-files   (system-helpers/unreadable-files cur-system)
                  :unreadable-files-by-group (system-helpers/unreadable-files-by-file-group cur-system)}]
    (update-tray-icon! info-map)
    (update-tray-tooltip! info-map)
    (update-menu-items! info-map)
    (maybe-show-balloon-notification! info-map)))


;; menu actions

(defn ack-all-alerts []
  (swap! system-state/system system-helpers/acknowledge-alerts))

(defn show-status []
  (let [cur-system @system-state/system
        info-map   {:cur-system     cur-system
                    :config-data    (system-helpers/configuration-data cur-system)
                    :uses-groups?   (not (system-helpers/uses-defeault-file-group-only? cur-system))
                    :unacked-alerts-by-file (system-helpers/unacknowledged-alerts-by-file cur-system)
                    :unacked-alerts-by-group (system-helpers/unacknowledged-alerts-by-file-group cur-system)
                    :unacked-alerts (system-helpers/unacknowledged-alerts cur-system)
                    :unreadable-files   (system-helpers/unreadable-files cur-system)
                    :unreadable-files-by-group (system-helpers/unreadable-files-by-file-group cur-system)}]
  (show-balloon-notification! info-map)))

(defn toggle-check-enabled []
  (swap! system-state/system system-helpers/toggle-check-enabled))

(defn exit []
  (System/exit 0))

(defn open-files-with-alerts []
  (let [unacked-alerts-by-file (system-helpers/unacknowledged-alerts-by-file @system-state/system)
        unacked-files (keys unacked-alerts-by-file)
        unacked-file-paths (map (fn [[file-id file-data]] (get file-data :file)) unacked-files)]
    (apply ui-utils/open-files unacked-file-paths)))

(defn initialize-ui! []
  (let [tray (SystemTray/getSystemTray)
        image (ui-utils/load-image "icon.png")
        tray-icon (TrayIcon. image)
        ack-all-alerts-menu (ui-utils/create-menu-item "Acknowledge all alerts" ack-all-alerts)
        show-status-menu (ui-utils/create-menu-item "Show status" show-status)
        toggle-check-enabled-menu (ui-utils/create-menu-item "Disable file checking" toggle-check-enabled)
        exit-menu (ui-utils/create-menu-item "Exit" exit)
        version-menu (ui-utils/create-menu-label (str "Version " (utils/project-version)))
        popup (PopupMenu.)]
    (doto popup
      (.add ack-all-alerts-menu)
      (.add show-status-menu)
      (.add toggle-check-enabled-menu)
      (.addSeparator)
      (.add exit-menu)
      (.add version-menu))
    (doto tray-icon
      (.setPopupMenu popup)
      (.setImageAutoSize true)
      (.addActionListener (ui-utils/create-action-listener open-files-with-alerts))
      (.addMouseListener (ui-utils/create-mouse-listener :mdl-callback
                                                         (fn [] (show-status)))))
    (doto tray
      (.add tray-icon))
    (swap! system-state/system (fn [sys]
                                 (-> sys
                                     (system-core/add-entity (create-ui-entity :ui-tray-icon tray-icon))
                                     (system-core/add-entity (create-ui-entity :ui-ack-all-alerts-menu-button ack-all-alerts-menu))
                                     (system-core/add-entity (create-ui-entity :ui-toggle-check-enabled-menu-button toggle-check-enabled-menu)))))))


;; main entry point

(defn -main [& args]
  (utils/try-let [configuration (config/load-configuration)]
    (do
      (system-state/reset-system! configuration)
      (initialize-ui!)
      (validators/validate-system @system-state/system)
      (add-watch system-state/system "ui-system-change-watch" process-new-system-notification)
      (start-watcher-thread!))
    (catch Exception ex
      (log/error ex "Failed to read the configuration file")
      (ui-utils/show-error-message "Critical error"
                                   (format "Following error occurred while reading the configuration file:\n%s" (.toString ex))))))

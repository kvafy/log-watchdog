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
  (:import [java.awt SystemTray TrayIcon TrayIcon$MessageType PopupMenu Menu])
  (:gen-class)) ; needed for uberjar because this package contains -main function


;; CRUD for ui objects represented as entities of the system

(defn create-ui-entity [entity-type value & kvs]
  (apply system-core/create-entity entity-type
                                   (concat [:value value] kvs)))

(defn ui-entity-value
  "A 'ui property' is a singleton entity holding instance of a UI Java object.
  This accessor extracts instance of the Java object from the entity."
  [system entity-type]
  (let [[entity-id entity-data] (system-core/query-singleton system
                                                             (system-core/entity-pred :type (partial = entity-type)))]
    (:value entity-data)))


;; periodic actions

(defn start-watcher-thread!
  "Initializes and starts a thread that periodically updates the system by performing check of watched files."
  []
  (letfn [(system-updating-fn []
            (loop []
              (let [config-data (system-helpers/configuration-data @system-state/system)]
                (when (:check-enabled config-data)
                  (log/info "Checking files...")
                  (swap! system-state/system system-helpers/check-files)
                  (log/trace @system-state/system))
                (Thread/sleep (:check-interval-ms config-data))
                (recur))))]
    (doto (Thread. system-updating-fn)
      (.setDaemon true)
      (.start))))


;; observers of the system state and reacting to the current state

(defn update-tray-icon! [{:keys [cur-system config-data unacked-alerts unreadable-files]}]
  (let [paused? (not (:check-enabled config-data))
        any-alert? (not-empty unacked-alerts)
        any-warning? (not-empty unreadable-files)]
    (let [icon-name (str "icon" (if paused? "P" "") (if any-alert? "A" "") (if any-warning? "W" "") ".png")]
      (.setImage (ui-entity-value cur-system :ui-tray-icon)
                 (ui-utils/load-image icon-name)))))

(defn update-tray-tooltip! [{:keys [cur-system] :as info-map}]
  (doto (ui-entity-value cur-system :ui-tray-icon)
    (.setToolTip (messages/short-status info-map))))

(defn update-alert-acknowledging-menu-items! [{:keys [cur-system unacked-alerts unacked-alerts-by-file unacked-alerts-by-group]}]
  (let [ack-btn-entities (system-core/query cur-system (system-core/entity-pred :type (partial = :ui-ack-alerts-menu-button)))]
    (doseq [[_ ack-btn-data] ack-btn-entities]
      (let [[_ linked-entity-data :as linked-entity] (system-core/query-by-id cur-system (:linked-entity-id ack-btn-data))]
        (condp = (:type linked-entity-data)
          :watched-file
            (let [[file-name _] (ui-utils/file-name-and-dir (:file linked-entity-data))
                  file-alerts (get unacked-alerts-by-file linked-entity)]
              (doto (:value ack-btn-data)
                (.setLabel (format "In '%s' file (%d)" file-name (count file-alerts)))
                (.setEnabled (not (empty? file-alerts)))))
          :watched-file-group
            (let [group-name (:name linked-entity-data)
                  group-alerts (get unacked-alerts-by-group linked-entity)]
              (doto (:value ack-btn-data)
                (.setLabel (format "In '%s' group (%d)" group-name (count group-alerts)))
                (.setEnabled (not (empty? group-alerts)))))
          ; the 'ack all alerts everywhere' button has no linked entity
          (doto (:value ack-btn-data)
            (.setLabel (format "Everywhere (%d)" (count unacked-alerts)))
            (.setEnabled (not (empty? unacked-alerts)))))))))

(defn update-menu-items! [{:keys [cur-system config-data] :as info-map}]
  (update-alert-acknowledging-menu-items! info-map)
  (doto (ui-entity-value cur-system :ui-toggle-check-enabled-menu-button)
    (.setLabel (if (:check-enabled config-data)
                 "Disable file checking"
                 "Enable file checking"))))

(defn show-balloon-notification! [{:keys [cur-system] :as info-map}]
  (.displayMessage (ui-entity-value cur-system :ui-tray-icon)
                   (messages/short-status info-map)
                   (messages/long-status info-map)
                   TrayIcon$MessageType/WARNING))

(defn maybe-show-balloon-notification! [{:keys [prev-system cur-system config-data unacked-alerts] :as info-map}]
  (let [has-new-alert? (system-helpers/has-new-alert? prev-system cur-system)
        has-new-unreadable-file? (system-helpers/has-new-unreadable-file? prev-system cur-system)
        can-nag-now? (< (+ (system-helpers/last-notification-timestamp cur-system)
                           (:nagging-interval-ms config-data))
                        (utils/current-time-ms))]
    (when (or has-new-unreadable-file?
              (and (not-empty unacked-alerts)
                   (or has-new-alert? can-nag-now?)))
      (show-balloon-notification! info-map)
      (swap! system-state/system system-helpers/set-last-notification-timestamp (utils/current-time-ms)))))

(defn process-new-system-notification [_ _ prev-system cur-system]
  ; extract commonly used values into a map and pass that map around
  (let [info-map {:prev-system               prev-system
                  :cur-system                cur-system
                  :config-data               (system-helpers/configuration-data cur-system)
                  :uses-groups?              (not (system-helpers/uses-defeault-file-group-only? cur-system))
                  :unacked-alerts            (system-helpers/unacknowledged-alerts cur-system)
                  :unacked-alerts-by-file    (system-helpers/unacknowledged-alerts-by-file cur-system)
                  :unacked-alerts-by-group   (system-helpers/unacknowledged-alerts-by-file-group cur-system)
                  :unreadable-files          (system-helpers/unreadable-files cur-system)
                  :unreadable-files-by-group (system-helpers/unreadable-files-by-file-group cur-system)}]
    (update-tray-icon! info-map)
    (update-tray-tooltip! info-map)
    (update-menu-items! info-map)
    (maybe-show-balloon-notification! info-map)))


;; menu actions

(defn ack-alerts
  ([]
    (swap! system-state/system system-helpers/acknowledge-alerts))
  ([files]
    (swap! system-state/system system-helpers/acknowledge-alerts files)))

(defn show-status []
  (let [cur-system @system-state/system
        info-map   {:cur-system                cur-system
                    :config-data               (system-helpers/configuration-data cur-system)
                    :uses-groups?              (not (system-helpers/uses-defeault-file-group-only? cur-system))
                    :unacked-alerts            (system-helpers/unacknowledged-alerts cur-system)
                    :unacked-alerts-by-file    (system-helpers/unacknowledged-alerts-by-file cur-system)
                    :unacked-alerts-by-group   (system-helpers/unacknowledged-alerts-by-file-group cur-system)
                    :unreadable-files          (system-helpers/unreadable-files cur-system)
                    :unreadable-files-by-group (system-helpers/unreadable-files-by-file-group cur-system)}]
  (show-balloon-notification! info-map)))

(defn toggle-check-enabled []
  (swap! system-state/system system-helpers/toggle-check-enabled))

(defn exit []
  (System/exit 0))

(defn open-all-files-with-alerts []
  (let [unacked-alerts-by-file (system-helpers/unacknowledged-alerts-by-file @system-state/system)
        unacked-files (keys unacked-alerts-by-file)
        unacked-file-paths (map (fn [[_ file-data]] (:file file-data)) unacked-files)]
    (apply ui-utils/open-files unacked-file-paths)))


;; UI creation

(defn create-menu-button-entity-to-ack-alerts
  ([label]
    ; acknowledge all alerts in all files in the system
    (let [menu-item (ui-utils/create-menu-item label (fn [] (ack-alerts)))]
      (create-ui-entity :ui-ack-alerts-menu-button
                        menu-item
                        :linked-entity-id nil)))
  ; acknowledge alerts only in specified files (used for file groups & individual files)
  ([label files linked-entity-id]
    (let [menu-item (ui-utils/create-menu-item label (fn [] (ack-alerts files)))]
      (create-ui-entity :ui-ack-alerts-menu-button
                        menu-item
                        :linked-entity-id linked-entity-id))))

(defn create-menu-button-entities-to-ack-alerts-in-group
  "Creates following menu buttons for a file group of n files (in the order bellow):
    * 1 button to acknowledge alerts in all files in the group
    * n buttons to acknowledge alerts in individual files.
  If the group has only one file, creates only the per-group button."
  [group group-files]
  (let [[gr-id gr-data] group
        group-button (create-menu-button-entity-to-ack-alerts (format "In '%s' group" (:name gr-data))
                                                              group-files
                                                              gr-id)
        file-buttons (map (fn [[f-id f-data :as file]]
                            (let [[file-name _] (ui-utils/file-name-and-dir (:file f-data))]
                              (create-menu-button-entity-to-ack-alerts (format "In '%s' file" file-name)
                                                                       [file]
                                                                       f-id)))
                          group-files)]
    (if (= 1 (count group-files))
      [group-button]
      (cons group-button file-buttons))))

(defn create-and-add-menu-buttons-to-ack-alerts! [system root-menu]
  ; First, add buttons for file groups and individual files...
  (let [files-by-group (system-helpers/files-by-file-group system)
        [system _] (reduce (fn [[sys add-separator?] [group files]]
                             (when add-separator?
                               (.addSeparator root-menu))
                             (let [ack-btn-entities (create-menu-button-entities-to-ack-alerts-in-group group files)
                                   sys'' (reduce (fn [sys' [btn-id btn-data :as btn-entity]]
                                                   (.add root-menu (:value btn-data))
                                                   (system-core/add-entity sys' btn-entity))
                                                 sys
                                                 ack-btn-entities)]
                               [sys'' true]))
                           [system false]
                           files-by-group)]
    ; ... then add the "ack everything button, but only if there is more than one file group
    (if (>= 1 (count files-by-group))
      system
      (let [ack-all-btn-entity (create-menu-button-entity-to-ack-alerts "Everywhere")
            [ack-all-btn-id ack-all-btn-data] ack-all-btn-entity
            system-with-ack-all-btn (system-core/add-entity system ack-all-btn-entity)]
        (doto root-menu
          (.insertSeparator 0)
          (.insert (:value ack-all-btn-data) 0))
        system-with-ack-all-btn))))

(defn initialize-ui! []
  (let [tray (SystemTray/getSystemTray)
        image (ui-utils/load-image "icon.png")
        tray-icon (TrayIcon. image)
        open-files-menu (Menu. "Open files")
        ack-alerts-menu (Menu. "Acknowledge alerts")
        show-status-menu (ui-utils/create-menu-item "Show status" show-status)
        toggle-check-enabled-menu (ui-utils/create-menu-item "Disable file checking" toggle-check-enabled)
        exit-menu (ui-utils/create-menu-item "Exit" exit)
        version-menu (ui-utils/create-menu-label (str "Version " (utils/project-version)))
        popup (PopupMenu.)]
    (doto open-files-menu
      )
    (doto popup
      ;(.add open-files-menu)
      (.add ack-alerts-menu)
      (.add show-status-menu)
      (.add toggle-check-enabled-menu)
      (.addSeparator)
      (.add exit-menu)
      (.add version-menu))
    (doto tray-icon
      (.setPopupMenu popup)
      (.setImageAutoSize true)
      (.addActionListener (ui-utils/create-action-listener open-all-files-with-alerts))
      (.addMouseListener (ui-utils/create-mouse-listener :mdl-callback show-status)))
    (doto tray
      (.add tray-icon))
    (swap! system-state/system (fn [sys]
                                 (-> sys
                                     (system-core/add-entity (create-ui-entity :ui-tray-icon tray-icon))
                                     (create-and-add-menu-buttons-to-ack-alerts! ack-alerts-menu)
                                     (system-core/add-entity (create-ui-entity :ui-toggle-check-enabled-menu-button toggle-check-enabled-menu)))))))


;; main entry point

(defn -main [& args]
  (utils/configure-logging!)
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

(ns log-watchdog.ui.core
  (:require [clojure.set]
            [clojure.tools.logging :as log]
            [log-watchdog.system.core :as system-core]
            [log-watchdog.system.helpers :as system-helpers]
            [log-watchdog.system.state :as system-state]
            [log-watchdog.utils :as utils]
            [log-watchdog.ui.utils :as ui-utils]
            [log-watchdog.config :as config]
            [log-watchdog.validators :as validators])
  (:import [java.awt SystemTray TrayIcon TrayIcon$MessageType PopupMenu])
  (:gen-class)) ; needed for uberjar because this package contains -main function


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

;;TODO this should go somewhere else

(defn- ui-property-instance [system entity-type]
  (let [[entity-id entity-data] (first (system-core/query system
                                                          (system-core/entity-pred :type (partial = entity-type))))]
    (get entity-data :value)))

(defn- tray-icon-instance [system]
  (ui-property-instance system :ui-tray-icon))

(defn- ack-all-alerts-menu-button-instance [system]
  (ui-property-instance system :ui-ack-all-alerts-menu-button))

(defn- toggle-check-enabled-menu-button-instance [system]
  (ui-property-instance system :ui-toggle-check-enabled-menu-button))


(defn update-tray-icon! [{:keys [cur-system config-data unacked-files failed-files]}]
  (let [paused? (not (get config-data :check-enabled))
        any-alert? (not-empty unacked-files)
        any-warning? (not-empty failed-files)]
    (let [icon-name (str "icon"
                         (if paused? "P" "")
                         (if any-alert? "A" "")
                         (if any-warning? "W" "")
                         ".png")]
      (.setImage (tray-icon-instance cur-system)
                 (ui-utils/load-image icon-name)))))

(defn update-tray-tooltip! [{:keys [cur-system unacked-files failed-files unacked-alerts]}]
  (.setToolTip (tray-icon-instance cur-system)
               (if (every? empty? [unacked-files failed-files])
                     "Everything is fine."
                     (let [alerts-msg (if (empty? unacked-files)
                                            ""
                                            (format "%d unacknowledged %s."
                                                    (count unacked-alerts)
                                                    (ui-utils/plural-of-word "alert" (count unacked-alerts))))
                           failed-checks-msg (if (empty? failed-files)
                                               ""
                                               (format "%d undreadable %s"
                                                       (count failed-files)
                                                       (ui-utils/plural-of-word "file" (count failed-files))))]
                       (clojure.string/join " " (filter not-empty [alerts-msg failed-checks-msg]))))))

(defn update-menu-items! [{:keys [cur-system config-data unacked-alerts]}]
  (doto (ack-all-alerts-menu-button-instance cur-system)
    (.setLabel (format "Acknowledge all alerts (%d)" (count unacked-alerts)))
    (.setEnabled (not (empty? unacked-alerts))))
  (doto (toggle-check-enabled-menu-button-instance cur-system)
    (.setLabel (if (get config-data :check-enabled)
                 "Disable file checking"
                 "Enable file checking"))))

(defn show-balloon-notification!
  ([cur-system just-a-nag?]
    (let [info-map {:cur-system     cur-system
                    :config-data    (system-helpers/configuration-data cur-system)
                    :unacked-alerts (system-helpers/unacknowledged-alerts cur-system)
                    :unacked-files  (system-helpers/files-having-unacknowledged-alerts cur-system)
                    :failed-files   (system-helpers/files-having-last-check-failed cur-system)
                    :just-a-nag?    just-a-nag?}]
      (show-balloon-notification! info-map)))
  ([{:keys [cur-system unacked-files failed-files unacked-alerts just-a-nag?]}]
    (let [balloon-caption (format "%d unacknowledged %s in %d %s. %d unreadable %s."
                                  (count unacked-alerts)
                                  (ui-utils/plural-of-word "alert" (count unacked-alerts))
                                  (count unacked-files)
                                  (ui-utils/plural-of-word "file" (count unacked-files))
                                  (count failed-files)
                                  (ui-utils/plural-of-word "file" (count failed-files)))
          unacked-files-msg (->> unacked-files
                                (map (fn [[file-id file-data]]
                                       (let [[name dir] (ui-utils/file-name-and-dir (get file-data :file))
                                             unacked-file-alerts (filter (fn [[alert-id alert-data]]
                                                                           (= file-id (get alert-data :watched-file-id)))
                                                                         unacked-alerts)]
                                         (format "%s (%d %s)"
                                                 name
                                                 (count unacked-file-alerts)
                                                 (ui-utils/plural-of-word "alert" (count unacked-file-alerts))))))
                                (clojure.string/join "\n"))
          failed-files-msg (->> failed-files
                                (map (fn [[file-id file-data]]
                                       (let [[name dir] (ui-utils/file-name-and-dir (get file-data :file))]
                                         (format "%s (check failed)" name))))
                                (clojure.string/join "\n"))
          balloon-text (clojure.string/join "\n---\n" (filter #(< 0 (count %)) [unacked-files-msg failed-files-msg]))]
      (.displayMessage (tray-icon-instance cur-system)
                       balloon-caption
                       balloon-text
                       TrayIcon$MessageType/WARNING))))

(defn maybe-show-balloon-notification! [{:keys [prev-system cur-system config-data failed-files unacked-alerts] :as info-map}]
  (let [has-new-alert? (system-helpers/has-new-alert? prev-system cur-system)
        has-new-failed-file? (system-helpers/has-new-failed-file? prev-system cur-system)
        can-nag-now? (< (+ (system-helpers/last-notification-timestamp cur-system)
                           (get config-data :nagging-interval-ms))
                        (utils/current-time-ms))]
    (when (or has-new-failed-file?
              (and (not-empty unacked-alerts)
                   (or has-new-alert? can-nag-now?)))
      (show-balloon-notification! (assoc info-map :just-a-nag? (not has-new-alert?)))
      (swap! system-state/system system-helpers/set-last-notification-timestamp (utils/current-time-ms)))))

(defn process-new-system-notification [_ _ prev-system cur-system]
  ; extract commonly used values into a map and pass that map around
  (let [info-map {:prev-system prev-system
                  :cur-system     cur-system
                  :config-data    (system-helpers/configuration-data cur-system)
                  :unacked-alerts (system-helpers/unacknowledged-alerts cur-system)
                  :unacked-files  (system-helpers/files-having-unacknowledged-alerts cur-system)
                  :failed-files   (system-helpers/files-having-last-check-failed cur-system)}]
    (update-tray-icon! info-map)
    (update-tray-tooltip! info-map)
    (update-menu-items! info-map)
    (maybe-show-balloon-notification! info-map)))


;; menu actions

(defn ack-all-alerts []
  (swap! system-state/system system-helpers/acknowledge-alerts))

(defn show-status []
  (show-balloon-notification! @system-state/system false))

(defn toggle-check-enabled []
  (swap! system-state/system system-helpers/toggle-check-enabled))

(defn exit []
  (System/exit 0))

(defn open-files-with-alerts []
  (let [unacked-files (system-helpers/files-having-unacknowledged-alerts @system-state/system)
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
                                                         (fn [] (show-balloon-notification! @system-state/system false)))))
    (doto tray
      (.add tray-icon))
    (swap! system-state/system (fn [sys]
                                 (-> sys
                                     (system-core/add-entity (system-core/create-entity :ui-tray-icon
                                                                                        :value tray-icon))
                                     (system-core/add-entity (system-core/create-entity :ui-ack-all-alerts-menu-button
                                                                                        :value ack-all-alerts-menu))
                                     (system-core/add-entity (system-core/create-entity :ui-toggle-check-enabled-menu-button
                                                                                        :value toggle-check-enabled-menu)))))))


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

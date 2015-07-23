(ns log-watchdog.ui
  (:require [clojure.set]
            [clojure.java.io :refer [resource]]
            [clojure.tools.logging :as log]
            [log-watchdog.system :as system]
            [log-watchdog.util :as util]
            [log-watchdog.config :as config])
  (:import [java.awt SystemTray TrayIcon TrayIcon$MessageType PopupMenu MenuItem Toolkit Desktop]
           [java.awt.event ActionListener MouseEvent MouseAdapter])
  (:gen-class)) ; needed for uberjar because this package contains -main function


;; UI properties stored in the 'system'
(def tray-icon-property :tray-icon)
(def show-alert-details-menu-property :show-alert-details-menu)
(def ack-all-alerts-menu-property :ack-all-alerts-menu)
(def toggle-check-enabled-menu-property :toggle-check-enabled-menu)


;; periodic actions

(defn start-watcher-thread!
  "Initializes and starts a thread that periodically updates the system by performing checks of watched files."
  []
  (letfn [(system-updating-fn []
            (loop []
              (let [cur-system @system/system
                    watcher-enabled (system/check-enabled cur-system)
                    delay-ms (system/check-interval-ms cur-system)]
                (when watcher-enabled
                  (log/info "Checking files...")
                  (swap! system/system system/check-files)
                  (log/trace (str @system/system)))
                (Thread/sleep delay-ms)
                (recur))))]
    (doto (Thread. system-updating-fn)
      (.setDaemon true)
      (.start))))


;; observers of the system state and reacting to the current state

(defn update-tray-tooltip! [{:keys [cur-system file-paths-with-unacked-alerts unacked-alerts]}]
  (.setToolTip (system/ui-property cur-system tray-icon-property)
               (if (empty? file-paths-with-unacked-alerts)
                 "No unacknowledged alerts"
                 (format "%d unacknowledged %s in %s %s. Double click to open."
                         (count unacked-alerts)
                         (util/plural-of-word "alert" (count unacked-alerts))
                         (count file-paths-with-unacked-alerts)
                         (util/plural-of-word "file" (count file-paths-with-unacked-alerts))))))

(defn update-menu-items! [{:keys [cur-system unacked-alerts]}]
  (doto (system/ui-property cur-system ack-all-alerts-menu-property)
    (.setLabel (format "Acknowledge all alerts (%d)" (count unacked-alerts)))
    (.setEnabled (not (empty? unacked-alerts))))
  (doto (system/ui-property cur-system toggle-check-enabled-menu-property)
    (.setLabel (if (system/check-enabled cur-system)
                 "Disable file checking"
                 "Enable file checking"))))

(defn show-balloon-notification!
  ([cur-system just-a-nag?]
    (let [file-paths-with-unacked-alerts (system/file-paths cur-system system/file-has-unacknowledged-alert?)
        unacked-alerts (system/alerts cur-system file-paths-with-unacked-alerts system/unacknowledged-alert?)
        info-map {:cur-system cur-system
                  :file-paths-with-unacked-alerts file-paths-with-unacked-alerts
                  :unacked-alerts unacked-alerts
                  :just-a-nag? just-a-nag?}]
      (show-balloon-notification! info-map)))
  ([{:keys [cur-system file-paths-with-unacked-alerts unacked-alerts just-a-nag?]}]
    (let [balloon-caption (format "You%s have %d unacknowledged %s in %d %s"
                                  (if just-a-nag? " still" "")
                                  (count unacked-alerts)
                                  (util/plural-of-word "alert" (count unacked-alerts))
                                  (count file-paths-with-unacked-alerts)
                                  (util/plural-of-word "file" (count file-paths-with-unacked-alerts)))
          balloon-text (clojure.string/join "\n" file-paths-with-unacked-alerts)]
      (.displayMessage (system/ui-property cur-system tray-icon-property)
                       balloon-caption
                       balloon-text
                       TrayIcon$MessageType/WARNING))))

(defn maybe-show-balloon-notification! [{:keys [prev-system cur-system file-paths-with-unacked-alerts unacked-alerts] :as info-map}]
  (let [has-new-alert? (system/has-new-alert? prev-system cur-system)
        can-nag-now? (< (+ (system/last-notification-timestamp cur-system)
                           (system/nagging-interval-ms cur-system))
                        (util/current-time-ms))]
    (when (and (not-empty unacked-alerts)
               (or has-new-alert? can-nag-now?))
      (show-balloon-notification! (assoc info-map :just-a-nag? (not has-new-alert?)))
      (swap! system/system system/set-last-notification-timestamp (util/current-time-ms)))))

(defn process-new-system-notification [key system-ref prev-system cur-system]
  ; extract commonly used values into a map and pass that map
  (let [file-paths-with-unacked-alerts (system/file-paths cur-system system/file-has-unacknowledged-alert?)
        unacked-alerts (system/alerts cur-system file-paths-with-unacked-alerts system/unacknowledged-alert?)
        info-map {:prev-system prev-system
                  :cur-system cur-system
                  :file-paths-with-unacked-alerts file-paths-with-unacked-alerts
                  :unacked-alerts unacked-alerts}]
    (update-tray-tooltip! info-map)
    (update-menu-items! info-map)
    (maybe-show-balloon-notification! info-map)))



;; menu & tray icon

(defn create-action-listener [callback]
  (proxy [ActionListener] []
    (actionPerformed [event] (callback))))

(defn create-mouse-listener [& options]
  (let [no-op (fn [] nil)
        callback-map (into {:sgl-callback no-op :dbl-callback no-op :mdl-callback no-op :enter-callback no-op}
                           (apply hash-map options))
        {:keys [sgl-callback dbl-callback mdl-callback enter-callback]} callback-map]
    (proxy [MouseAdapter] []
      (mouseClicked [event]
        (let [click-count (.getClickCount event)]
          (condp = (.getButton event)
            MouseEvent/BUTTON1
              (condp = click-count
                ; this doesn't really work well in Java...
                1 (sgl-callback)
                2 (dbl-callback)
                :else nil)
            MouseEvent/BUTTON2
              (when (= click-count 1)
                (mdl-callback))
            nil)))
      (mouseEntered [event]
        (enter-callback)))))

(defn create-menu-item [label callback]
  (let [menu (MenuItem. label)]
    (.addActionListener menu (create-action-listener callback))
    menu))

(defn open-files [& files]
  (let [desktop (Desktop/getDesktop)]
    (doseq [file files]
      (.open desktop (java.io.File. file)))))


;; menu actions

(defn ack-all-alerts []
  (swap! system/system system/acknowledge-alerts))

(defn show-alert-details []
  (show-balloon-notification! @system/system false))

(defn toggle-check-enabled []
  (swap! system/system system/toggle-check-enabled))

(defn exit []
  (System/exit 0))

(defn open-files-with-alerts []
  (let [file-paths-to-open (system/file-paths @system/system system/file-has-unacknowledged-alert?)]
    (apply open-files file-paths-to-open)))

(defn initialize-ui! []
  (let [tray (SystemTray/getSystemTray)
        image (.getImage (Toolkit/getDefaultToolkit)
                         (resource "icon.png"))
        tray-icon (TrayIcon. image)
        ack-all-alerts-menu (create-menu-item "Acknowledge all alerts" ack-all-alerts)
        show-alert-details-menu (create-menu-item "Show alert details" show-alert-details)
        toggle-check-enabled-menu (create-menu-item "Disable file checking" toggle-check-enabled)
        exit-menu (create-menu-item "Exit" exit)
        popup (PopupMenu.)]
    (doto popup
      (.add ack-all-alerts-menu)
      (.add show-alert-details-menu)
      (.add toggle-check-enabled-menu)
      (.add exit-menu))
    (doto tray-icon
      (.setPopupMenu popup)
      (.setImageAutoSize true)
      (.addActionListener (create-action-listener open-files-with-alerts))
      (.addMouseListener (create-mouse-listener :mdl-callback
                                                (fn [] (show-balloon-notification! @system/system false)))))
    (doto tray
      (.add tray-icon))
    (swap! system/system system/set-ui-property ack-all-alerts-menu-property ack-all-alerts-menu)
    (swap! system/system system/set-ui-property show-alert-details-menu-property show-alert-details-menu)
    (swap! system/system system/set-ui-property toggle-check-enabled-menu-property toggle-check-enabled-menu)
    (swap! system/system system/set-ui-property tray-icon-property tray-icon)))


;; main entry point

(defn -main [& args]
  (util/try-let [configuration (config/load-configuration)]
    (do
      (system/reset! configuration)
      (initialize-ui!)
      (add-watch system/system "ui-system-change-watch" process-new-system-notification)
      (start-watcher-thread!))
    (catch Exception ex
      (log/error ex "Failed to read the configuration file"))))

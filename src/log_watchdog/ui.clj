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


;; periodic actions

; allows to disable the watcher thread from doing any work
(def watcher-enabled (atom true))

(defn start-watcher-thread!
  "Initializes and starts a thread that periodically updates the system by performing checks of watched files."
  []
  (letfn [(system-updating-fn []
            (loop []
              (let [cur-system @system/system
                    delay-ms (get-in cur-system [:check-interval-ms])]
                (when @watcher-enabled
                  (log/info "Running watcher")
                  (swap! system/system system/check-files)
                  (log/trace (str @system/system)))
                (Thread/sleep delay-ms)
                (recur))))]
    (doto (Thread. system-updating-fn)
      (.setDaemon true)
      (.start))))


;; observers of the system state and reacting to the current state

(defn update-tray-tooltip! [{:keys [cur-system file-paths-with-unacked-alerts unacked-alerts]}]
  (.setToolTip (system/tray-icon cur-system)
               (if (empty? file-paths-with-unacked-alerts)
                 "No unacknowledged alerts"
                 (format "%d unacknowledged %s in %s %s. Double click to open."
                         (count unacked-alerts)
                         (util/plural-of-word "alert" (count unacked-alerts))
                         (count file-paths-with-unacked-alerts)
                         (util/plural-of-word "file" (count file-paths-with-unacked-alerts))))))

(defn update-menu-items! [info-map]
  )

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
      (.displayMessage (system/tray-icon cur-system)
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

(defn create-mouse-listener [sgl-callback dbl-callback]
  (proxy [MouseAdapter] []
    (mouseClicked [event]
      (when (= (.getButton event) MouseEvent/BUTTON1)
        (condp = (.getClickCount event)
          1 (sgl-callback)
          2 (dbl-callback))))))

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
        exit-menu (create-menu-item "Exit" exit)
        popup (PopupMenu.)]
    (.add popup ack-all-alerts-menu)
    (.add popup exit-menu)
    (.setPopupMenu tray-icon popup)
    (.setImageAutoSize tray-icon true)
    ;(.addActionListener tray-icon (create-action-listener open-files-with-alerts))
    (.addMouseListener tray-icon (create-mouse-listener (fn [] (show-balloon-notification! @system/system false))
                                                        open-files-with-alerts))
    (.add tray tray-icon)
    (swap! system/system system/set-tray-icon tray-icon)))


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

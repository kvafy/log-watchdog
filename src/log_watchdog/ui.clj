(ns log-watchdog.ui
  (:require [clojure.set]
            [clojure.java.io :refer [resource]]
            [clojure.tools.logging :as log]
            [log-watchdog.system :as system]
            [log-watchdog.util :as util]
            [log-watchdog.config :as config])
  (:import [java.awt SystemTray TrayIcon TrayIcon$MessageType PopupMenu MenuItem Toolkit Desktop]
           [java.awt.event ActionListener])
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


;; observers of the system state

(defn update-menu-items []
  )

(defn update-tray-tooltip []
  )

(defn process-new-system-notification [key system-ref prev-system cur-system]
  (let [tray-icon (system/tray-icon cur-system)
        file-paths-with-unacked-alerts (system/file-paths cur-system system/file-has-unacknowledged-alert?)
        unacked-alerts (system/alerts cur-system file-paths-with-unacked-alerts system/unacknowledged-alert?)]
    (let [tooltip (if (empty? file-paths-with-unacked-alerts)
                    "No unacknowledged alerts"
                    (format "%d unacknowledged %s in %s %s. Double click to open."
                            (count unacked-alerts)
                            (util/plural-of-word "alert" (count unacked-alerts))
                            (count file-paths-with-unacked-alerts)
                            (util/plural-of-word "file" (count file-paths-with-unacked-alerts))))]
      (.setToolTip tray-icon tooltip))
    (let [has-new-alert? (system/has-new-alert? prev-system cur-system)
          last-notification-timestamp (:last-notification-timestamp cur-system)
          nagging-interval-ms (:nagging-interval-ms cur-system)]
      (when (and (not-empty unacked-alerts)
                 (or has-new-alert?
                     (< (+ last-notification-timestamp nagging-interval-ms)
                        (util/current-time-ms))))
        (swap! system/system system/set-last-notification-timestamp (util/current-time-ms))
        (let [balloon-caption (format "You%s have %d unacknowledged %s in %d %s"
                                      (if has-new-alert? "" " still")
                                      (count unacked-alerts)
                                      (util/plural-of-word "alert" (count unacked-alerts))
                                      (count file-paths-with-unacked-alerts)
                                      (util/plural-of-word "file" (count file-paths-with-unacked-alerts)))
              balloon-text (clojure.string/join "\n" file-paths-with-unacked-alerts)]
          (.displayMessage tray-icon
                           balloon-caption
                           balloon-text
                           TrayIcon$MessageType/WARNING))))))


;; menu & tray icon

(defn create-action-listener [callback]
  (proxy [ActionListener] []
    (actionPerformed [event] (callback))))

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
    (.addActionListener tray-icon (create-action-listener open-files-with-alerts))
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

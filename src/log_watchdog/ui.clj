(ns log-watchdog.ui
  (:require [clojure.set]
            [clojure.java.io :refer [resource]]
            [clojure.tools.logging :as log]
            [log-watchdog.core :as core]
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
              (let [cur-system @core/system
                    delay-ms (get-in cur-system [:check-interval-ms])]
                (when @watcher-enabled
                  (log/info "Running watcher")
                  (swap! core/system core/update-system-by-checking-files)
                  (log/trace (str @core/system)))
                (Thread/sleep delay-ms)
                (recur))))]
    (doto (Thread. system-updating-fn)
      (.setDaemon true)
      (.start))))


;; observers of the system state

(defn notify-new-system [prev-system cur-system tray-icon]
  (let [file-paths-with-unacked-alerts (core/file-paths cur-system core/file-has-unacknowledged-alert?)
        unacked-alerts (core/alerts cur-system file-paths-with-unacked-alerts core/unacknowledged-alert?)]
    (let [tooltip (if (empty? file-paths-with-unacked-alerts)
                    "No unacknowledged alerts"
                    (format "%d unacknowledged %s in: %s"
                            (count unacked-alerts)
                            (util/plural-of-word "alert" (count unacked-alerts))
                            (clojure.string/join ", " file-paths-with-unacked-alerts)))]
      (.setToolTip tray-icon tooltip))
    (let [has-new-alert? (core/has-new-alert? prev-system cur-system)
          last-notification-timestamp (:last-notification-timestamp cur-system)
          nagging-interval-ms (:nagging-interval-ms cur-system)]
      (when (and (not-empty unacked-alerts)
                 (or has-new-alert?
                     (< (+ last-notification-timestamp nagging-interval-ms)
                        (util/current-time-ms))))
        (swap! core/system core/update-system-by-setting-last-notification-timestamp (util/current-time-ms))
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

(defn create-system-watch-fn [tray-icon]
  (letfn [(system-watch-fn [key system-ref prev-system cur-system]
            (notify-new-system prev-system cur-system tray-icon))]
    system-watch-fn))


;; menu & tray icon

(defn create-action-listener [callback]
  (proxy [ActionListener] []
    (actionPerformed [event] (callback))))

(defn create-menu-item [label callback]
  (let [menu (MenuItem. label)]
    (.addActionListener menu (create-action-listener callback))
    menu))

(defn ack-all-alerts []
  (swap! core/system core/update-system-by-acknowledging-alerts))

(defn exit []
  (System/exit 0))

(defn open-files-with-alerts []
  (let [cur-system @core/system
        file-paths-to-open (core/file-paths cur-system core/file-has-unacknowledged-alert?)
        desktop (Desktop/getDesktop)]
    (doseq [file-path-to-open file-paths-to-open]
      (.open desktop (java.io.File. file-path-to-open)))))

(defn register-tray-icon! []
  (let [tray (SystemTray/getSystemTray)
        image (.getImage (Toolkit/getDefaultToolkit)
                         (resource "icon64.png"))
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
    tray-icon))

(defn initialize-ui! []
  (let [tray-icon (register-tray-icon!)
        system-watch-fn (create-system-watch-fn tray-icon)]
    (add-watch core/system "ui-system-change-watch" system-watch-fn)))


;; main entry point

(defn -main [& args]
  (util/try-let [configuration (config/load-configuration)]
    (do
      (initialize-ui!)
      (core/reset-system! configuration)
      (start-watcher-thread!))
    (catch Exception ex
      (log/error ex "Failed to read the configuration file"))))

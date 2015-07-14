(ns log-watchdog.ui
  (:require [clojure.set]
            [clojure.java.io :refer [resource]]
            [clojure.tools.logging :as log]
            [log-watchdog.core :as core]
            [log-watchdog.util :as util]
            [log-watchdog.config :as config])
  (:import [java.awt SystemTray TrayIcon TrayIcon$MessageType PopupMenu MenuItem Toolkit]
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

(defn notify-new-system [system tray-icon]
  (let [all-file-paths (keys (get-in system [:files]))
        file-paths-with-unacked-alert-lines (filter (fn [file-path]
                                                      (not-empty
                                                       (get-in system [:files file-path :unacknowledged-alert-lines])))
                                                    all-file-paths)
        unacked-alert-lines (reduce (fn [alerts file-path]
                                      (clojure.set/union alerts (get-in system [:files file-path :unacknowledged-alert-lines])))
                                    #{}
                                    all-file-paths)]
    (let [tooltip (if (empty? file-paths-with-unacked-alert-lines)
                    "No unacknowledged alerts"
                    (format "%d unacknowledged %s in: %s"
                            (count unacked-alert-lines)
                            (util/plural-of-word "alert" (count unacked-alert-lines))
                            (clojure.string/join ", " file-paths-with-unacked-alert-lines)))]
      (.setToolTip tray-icon tooltip))
    (when (not-empty file-paths-with-unacked-alert-lines)
      (let [balloon-caption (format "You have %d unacknowledged %s in %d %s"
                                    (count unacked-alert-lines)
                                    (util/plural-of-word "alert" (count unacked-alert-lines))
                                    (count file-paths-with-unacked-alert-lines)
                                    (util/plural-of-word "file" (count file-paths-with-unacked-alert-lines)))
            balloon-text (clojure.string/join "\n" file-paths-with-unacked-alert-lines)]
        (.displayMessage tray-icon
                         balloon-caption
                         balloon-text
                         TrayIcon$MessageType/WARNING)))))

(defn create-system-watch-fn [tray-icon]
  (letfn [(system-watch-fn [key system-ref prev-system cur-system]
            (notify-new-system cur-system tray-icon))]
    system-watch-fn))


;; menu

(defn menu-item [label callback]
  (let [menu (MenuItem. label)
        listener (proxy [ActionListener] []
                   (actionPerformed [event] (callback)))]
    (.addActionListener menu listener)
    menu))

(defn ack-all-alerts []
  (swap! core/system core/update-system-by-acknowledging-alerts))

(defn exit []
  (System/exit 0))

(defn register-tray-icon! []
  (let [tray (SystemTray/getSystemTray)
        image (.getImage (Toolkit/getDefaultToolkit)
                         (resource "icon64.png"))
        tray-icon (TrayIcon. image)
        ack-all-alerts-menu (menu-item "Acknowledge all alerts" ack-all-alerts)
        ;forget-all-menu (menu-item "Forget all alerts" forget-all-alerts)
        exit-menu (menu-item "Exit" exit)
        popup (PopupMenu.)]
    ;(.add popup forget-all-menu)
    (.add popup ack-all-alerts-menu)
    (.add popup exit-menu)
    (.setPopupMenu tray-icon popup)
    (.setImageAutoSize tray-icon true)
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
      (do
        (log/error ex "Failed to read the configuration file")
        (println (str "Failed to read the configuration file: " (.getMessage ex)))))))

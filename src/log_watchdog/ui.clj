(ns log-watchdog.ui
  (:require [clojure.java.io :refer [resource]]
            [log-watchdog.core :as core]
            [log-watchdog.util :as util]
            [log-watchdog.config :as config])
  (:import [java.awt SystemTray TrayIcon TrayIcon$MessageType PopupMenu MenuItem Toolkit]
           [java.awt.event ActionListener])
  (:gen-class)) ; needed for uberjar because this package contains -main


;; miscellaneous

(defn menu-item [label callback]
  (let [menu (MenuItem. label)
        listener (proxy [ActionListener] []
                   (actionPerformed [event] (callback)))]
    (.addActionListener menu listener)
    menu))


;; agent interaction

(defn format-check-result [check-result]
  (let [filename (:path (:watched-file check-result))
        alert-count (count (:alerts check-result))]
    (format "%s: %d new %s"
            filename
            alert-count
            (util/plural-of-word "alert" alert-count))))

(defn notify-unseen-check-results-creator
  "Creates a closure function that can be passed as a notification function
  to an agent watching the files."
  [tray-icon]
  (letfn [(notify-unseen-check-results [unseen-check-results]
            (let [alert-count (reduce + (map #(count (:alerts %)) unseen-check-results))]
              (let [files-with-alerts (map #(get-in % [:watched-file :path]) unseen-check-results)
                    tooltip (if (empty? files-with-alerts)
                              "No alerts detected"
                              (str "Last "
                                   (util/plural-of-word "alert" alert-count)
                                   " detected in: " (clojure.string/join ", " files-with-alerts)))]
                (.setToolTip tray-icon tooltip))
              (let [file-count (count unseen-check-results)
                    balloon-caption (format "Detected %d %s in %d %s"
                                            alert-count
                                            (util/plural-of-word "alert" alert-count)
                                            file-count
                                            (util/plural-of-word "file" file-count))
                    balloon-text (->> unseen-check-results
                                      (sort-by #(get-in % [:watched-file :path]))
                                      (map format-check-result)
                                      (clojure.string/join "\n"))]
                (.displayMessage tray-icon
                                 balloon-caption
                                 balloon-text
                                 TrayIcon$MessageType/WARNING))))]
    notify-unseen-check-results))

(defn start-agents! [tray-icon files intervalMs]
  (let [notify-fn (notify-unseen-check-results-creator tray-icon)
        watcher-agent-action-fn (core/run-watcher-until-stopped-action-creator files
                                                                               notify-fn
                                                                               intervalMs
                                                                               core/watcher-running)]
    (send core/watcher watcher-agent-action-fn)))


;; menu actions

(defn forget-all-errors
  "Clears the watcher agent, so that it thinks it hasn't seen anything yet in the watched files."
  []
  (send core/watcher (fn [_] #{})))

(defn exit []
  (shutdown-agents)
  (System/exit 0))

(defn register-tray-icon! []
  (let [tray (SystemTray/getSystemTray)
        image (.getImage (Toolkit/getDefaultToolkit)
                         (resource "icon64.png"))
        tray-icon (TrayIcon. image)
        forget-all-menu (menu-item "Forget all errors" forget-all-errors)
        exit-menu (menu-item "Exit" exit)
        popup (PopupMenu.)]
    (.add popup forget-all-menu)
    (.add popup exit-menu)
    (.setPopupMenu tray-icon popup)
    (.setImageAutoSize tray-icon true)
    (.add tray tray-icon)
    tray-icon))

;; main entry point

(defn -main [& args]
  (let [configuration (config/load-configuration)
        tray-icon (register-tray-icon!)]
    (start-agents! tray-icon
                   (:files configuration)
                   (:checkIntervalMs configuration))))

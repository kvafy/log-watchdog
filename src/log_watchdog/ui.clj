(ns log-watchdog.ui
  (:require [clojure.java.io :refer [resource]]
            [log-watchdog.core :as core]
            [log-watchdog.config :as config])
  (:import [java.awt SystemTray TrayIcon TrayIcon$MessageType PopupMenu MenuItem Toolkit]
           [java.awt.event ActionListener]))


;; miscellaneous

(defn menu-item [label callback]
  (let [menu (MenuItem. label)
        listener (proxy [ActionListener] []
                   (actionPerformed [event] (callback)))]
    (.addActionListener menu listener)
    menu))


;; agent interaction

(defn check-result-to-html [check-result]
  (format "<strong>%s</strong><ul>%s</ul>"
          (:path (:watched-file check-result))
          (->> (:alerts check-result)
               (sort)
               (map #(str "<li>" % "</li>")))))

(defn notify-new-alerts-creator [tray-icon]
  (letfn [(notify-new-alerts [alerts]
            (let [caption "New errors detected"
                  text (->> alerts
                           (sort-by #(:path (:watched-file %)))
                           (map check-result-to-html)
                           (clojure.string/join "<br/><br/>"))
                  files-with-alerts (map #(:path (:watched-file %)) alerts)]
              (.setToolTip tray-icon
                           (clojure.string/join "<br/>" files-with-alerts))
              (.displayMessage tray-icon
                               caption
                               text
                               TrayIcon$MessageType/WARNING)))]
    notify-new-alerts))

(defn start-agents! [tray-icon files intervalMs]
  (send core/watcher (core/run-watcher-until-stopped-creator files
                                                             (notify-new-alerts-creator tray-icon)
                                                             intervalMs
                                                             core/watcher-running)))


;; menu actions

(defn reset
  "Clears the watcher agent, so that it thinks it hasn't seen anything yet."
  []
  (send core/watcher (fn [_] #{})))

(defn exit []
  (shutdown-agents)
  (System/exit 0))


;; main entry point

(defn -main [& args]
  (let [configuration (config/load-configuration)
        files (:files configuration)
        checkIntervalMs (:checkIntervalMs configuration)
        tray (SystemTray/getSystemTray)
        image (.getImage (Toolkit/getDefaultToolkit)
                         (resource "icon64.png"))
        icon (TrayIcon. image)
        reset-menu (menu-item "Reset" reset)
        exit-menu (menu-item "Exit" exit)]
    (.setImageAutoSize icon true)
    (.add tray icon)
    (let [popup (PopupMenu.)]
      (.add popup reset-menu)
      (.add popup exit-menu)
      (.setPopupMenu icon popup)
      (start-agents! icon files checkIntervalMs))))

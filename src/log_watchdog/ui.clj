(ns log-watchdog.ui
  (:require [clojure.java.io :refer [resource]]
            [log-watchdog.core :as core]
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

(defn check-result-to-html [check-result]
  (format "<strong>%s</strong><ul>%s</ul>"
          (:path (:watched-file check-result))
          (->> (:alerts check-result)
               (sort)
               (map #(str "<li>" % "</li>"))
               (clojure.string/join))))

(defn notify-unseen-check-results-creator
  "Creates a closure function that can be passed as a notification function
  to an agent watching the files."
  [tray-icon]
  (letfn [(notify-unseen-check-results [unseen-check-results]
            (let [caption "New errors detected"
                  text (->> unseen-check-results
                           (sort-by #(:path (:watched-file %)))
                           (map check-result-to-html)
                           (clojure.string/join "<br/><br/>"))
                  files-with-alerts (map #(:path (:watched-file %)) unseen-check-results)]
              (.setToolTip tray-icon
                           (clojure.string/join "<br/>" files-with-alerts))
              (.displayMessage tray-icon
                               caption
                               text
                               TrayIcon$MessageType/WARNING)))]
    notify-unseen-check-results))

(defn start-agents! [tray-icon files intervalMs]
  (let [notify-fn (notify-unseen-check-results-creator tray-icon)
        watcher-agent-action-fn (core/run-watcher-until-stopped-action-creator files
                                                                               notify-fn
                                                                               intervalMs
                                                                               core/watcher-running)]
    (send core/watcher watcher-agent-action-fn)))


;; menu actions

(defn reset
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
        reset-menu (menu-item "Reset" reset)
        exit-menu (menu-item "Exit" exit)
        popup (PopupMenu.)]
    (.add popup reset-menu)
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

(ns log-watchdog.system
  (:require [clojure.set]
            [clojure.zip :as zip]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [log-watchdog.util :as util]))

; A 'system' is a data structure holding complete application state.
; To understand how the system is organized, see 'system' schema in 'validators' namespace.

; holds instance of the system
(def system (atom nil))


;; accessors of system properties

(defn- check-interval-ms-accessor []
  [:check-interval-ms])
(defn- nagging-interval-ms-accessor []
  [:nagging-interval-ms])
(defn- last-notification-timestamp-accessor []
  [:last-notification-timestamp])
(defn- tray-icon-accessor []
  [:ui :tray-icon])
(defn- files-map-accessor []
  [:files])
(defn- file-data-accessor [file-path]
  [:files file-path])
(defn- file-property-accessor [file-path property]
  (conj (file-data-accessor file-path) property))
(defn- alerts-map-accessor [file-path]
  (file-property-accessor file-path :alerts))
(defn- alert-data-accessor [file-path alert-line]
  (conj (alerts-map-accessor file-path) alert-line))
(defn- alert-property-accessor [file-path alert-line property]
  (conj (alert-data-accessor file-path alert-line) property))


;; creating (parts of) a system

(defn create-alert-data [timestamp-ms]
  {:last-seen-timestamp timestamp-ms
   :acknowledged false})

(defn create-file-data [line-regex alerts]
  {:line-regex line-regex
   :alerts alerts})

(defn create
  "Creates a system instance based on a configuration map as returned by log-watchdog.config/load-configuration."
  [config]
  (let [base-map   (-> {}
                       (assoc-in (last-notification-timestamp-accessor) 0))
        config-map (-> {}
                       (assoc-in (check-interval-ms-accessor) (get-in config [:check-interval-ms]))
                       (assoc-in (nagging-interval-ms-accessor) (get-in config [:nagging-interval-ms])))
        files-map (apply util/merge-recursive
                         (for [file-name (keys (get-in config [:files]))]
                             (assoc-in {} (file-data-accessor file-name) (create-file-data (get-in config [:files file-name :line-regex]) {}))))]
    (-> base-map
        (into config-map)
        (into files-map))))

(defn reset!
  "Clears current state of the 'system' atom and sets up a fresh system in which no check has been performed yet."
  [config]
  (let [new-system (create config)]
    (swap! system (fn [_] new-system))))


;; inspecting a system

(defn file-paths
  ([system]
    (file-paths system (fn [_] true)))
  ([system pred]
    (->> (get-in system (files-map-accessor))
         (filter pred)
         (map (fn [[file-path file-data]] file-path)))))

(defn alerts
  ([system]
    (alerts system (file-paths system)))
  ([system file-paths]
    (alerts system file-paths (fn [_] true)))
  ([system file-paths pred]
    {:pre (seq? file-paths)}
    (->> file-paths
         (map (fn [file-path] (get-in system (alerts-map-accessor file-path))))
         (apply merge)
         (filter pred)
         (into {}))))

; predicates to use with 'file-paths' and 'alerts' functions
(declare unacknowledged-alert?)
(defn file-has-unacknowledged-alert? [[file-path {:keys [alerts]}]]
  (some unacknowledged-alert? alerts))

(defn unacknowledged-alert? [[alert-line alert-data]]
  (not (:acknowledged alert-data)))


;; read a system property / update system by an action

(defn- check-file
  "Checks current alerts in given file and updates part of the system which represents this file."
  [system file-path]
  {:pre (contains? system file-path)}
  (try
    (with-open [reader (io/reader file-path)]
      (let [now (util/current-time-ms)
            regex (get-in system [:files file-path :line-regex])
            cur-alert-lines (->> (line-seq reader)
                                 (filter #(re-matches regex %)))]
        (reduce (fn [sys alert-line]
                  (if (contains? (alerts sys [file-path]) alert-line)
                    sys
                    (assoc-in sys (alert-data-accessor file-path alert-line) (create-alert-data now))))
                system
                cur-alert-lines)))
    (catch java.io.IOException ex
      (do
        (log/error ex (str "Failed to read file " file-path))
        system))))

(defn check-files
  "Updates the system by checking all files and updating their maps with newly found alert lines."
  [system]
  (reduce (fn [sys file-path] (check-file sys file-path))
          system
          (file-paths system)))

(defn acknowledge-alerts
  "Updates the system by acknowledging all alerts for given files. If no files are given, then
  acknowledges alerts for all files."
  [system & file-paths-to-ack]
  (let [file-paths-to-ack (if (empty? file-paths-to-ack)
                     (file-paths system)
                     file-paths-to-ack)]
    ;TODO use zippers for more idiomatic solution
    (reduce (fn [sys [file-path alert-line]]
              (assoc-in sys (alert-property-accessor file-path alert-line :acknowledged) true))
            system
            (for [file-path file-paths-to-ack
                  [alert-line _] (alerts system [file-path])]
              (vector file-path alert-line)))))

(defn check-interval-ms [system]
  (get-in system (check-interval-ms-accessor)))

(defn nagging-interval-ms [system]
  (get-in system (nagging-interval-ms-accessor)))

(defn set-last-notification-timestamp [system timestamp]
  (assoc-in system (last-notification-timestamp-accessor) timestamp))
(defn last-notification-timestamp [system]
  (get-in system (last-notification-timestamp-accessor)))

(defn set-tray-icon
  "Updates the system by adding a tray icon."
  [system tray-icon]
  (assoc-in system (tray-icon-accessor) tray-icon))
(defn tray-icon
  [system]
  (get-in system (tray-icon-accessor)))


;; comparing systems

(defn has-new-alert? [prev-system cur-system]
  (let [prev-alert-lines (keys (alerts prev-system))
        cur-alert-lines  (keys (alerts cur-system))]
    (not (every? (set prev-alert-lines) cur-alert-lines))))

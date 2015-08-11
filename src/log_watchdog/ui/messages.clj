(ns log-watchdog.ui.messages
  (:require [log-watchdog.ui.utils :as ui-utils]))


(defn short-status [{:keys [unacked-alerts-by-file unacked-alerts-by-group unreadable-files-by-group uses-groups?] :as info-map}]
  (if (every? empty? [unacked-alerts-by-file unreadable-files-by-group])
    "Everything is fine."
    (let [alerts-msg (cond
                       (empty? unacked-alerts-by-group)
                         ""
                       uses-groups?
                         (format "%s in %s."
                                 (ui-utils/plural-of-word (count (apply concat (vals unacked-alerts-by-group)))
                                                          "unacknowledged"
                                                          "alert")
                                 (ui-utils/plural-of-word (count (keys unacked-alerts-by-group))
                                                          "group"))
                       :else
                         (format "%s in %s."
                                 (ui-utils/plural-of-word (count (vals unacked-alerts-by-file))
                                                          "unacknowledged"
                                                          "alert")
                                 (ui-utils/plural-of-word (count (keys unacked-alerts-by-file))
                                                          "file")))
          unreadable-files-msg (cond
                              (empty? unreadable-files-by-group)
                                ""
                              uses-groups?
                                (format "%s in %s."
                                        (ui-utils/plural-of-word (count (apply concat (vals unreadable-files-by-group)))
                                                                 "unreadable"
                                                                 "file")
                                        (ui-utils/plural-of-word (count (keys unreadable-files-by-group))
                                                                 "group"))
                              :else
                                (format "%s."
                                        (ui-utils/plural-of-word (count (apply concat (vals unreadable-files-by-group)))
                                                                 "unreadable"
                                                                 "file")))]
    (clojure.string/join " " (filter not-empty [alerts-msg unreadable-files-msg])))))


(declare long-status-without-groups long-status-with-groups)

(defn long-status [{:keys [uses-groups?] :as info-map}]
  (if uses-groups?
    (long-status-with-groups info-map)
    (long-status-without-groups info-map)))

(defn long-status-without-groups [{:keys [unacked-alerts-by-file unreadable-files-by-group]}]
  (let [unacked-files-msg (->> (keys unacked-alerts-by-file)
                               (map (fn [[file-id file-data :as file-entity]]
                                      (let [[name dir] (ui-utils/file-name-and-dir (:file file-data))]
                                        (format "%s (%s)"
                                                name
                                                (ui-utils/plural-of-word (count (get unacked-alerts-by-file file-entity))
                                                                         "alert")))))
                               (clojure.string/join "\n"))
        unreadable-files-msg (->> (apply concat (vals unreadable-files-by-group))
                                  (map (fn [[file-id file-data]]
                                         (let [[name dir] (ui-utils/file-name-and-dir (:file file-data))]
                                           (format "%s (unreadable)" name))))
                                  (clojure.string/join "\n"))]
    (clojure.string/join "\n-----\n" (filter not-empty [unacked-files-msg unreadable-files-msg]))))

(defn long-status-with-groups [{:keys [unacked-alerts-by-group unreadable-files-by-group]}]
  (let [unacked-groups (->> unacked-alerts-by-group
                            (keys)
                            (sort-by (fn [[gr-id gr-data]] (:name gr-data))))
        unreadable-groups (->> unreadable-files-by-group
                               (keys)
                               (sort-by (fn [[gr-id gr-data]] (:name gr-data)))
                               (filter (fn [gr] (not (contains? unacked-alerts-by-group gr)))))
        groups-to-report (concat unacked-groups unreadable-groups)
        per-group-msgs (map (fn [[gr-id gr-data :as gr]]
                              (let [alert-count (count (get unacked-alerts-by-group gr))
                                    alerts-msg (if (zero? alert-count)
                                                 ""
                                                 (ui-utils/plural-of-word alert-count "alert"))
                                    unreadable-count (count (get unreadable-files-by-group gr))
                                    unreadables-msg (if (zero? unreadable-count)
                                                      ""
                                                      (ui-utils/plural-of-word unreadable-count "unreadable" "file"))
                                    msgs (filter not-empty [alerts-msg unreadables-msg])]
                              (format "'%s' group (%s)" (:name gr-data) (clojure.string/join ", " msgs))))
                            groups-to-report)]
    (clojure.string/join "\n" per-group-msgs)))

(ns log-watchdog.core
  (:require [clojure.set]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [log-watchdog.util :as util]))

;; define core data structures

;;TODO include a timestamp in CheckResult and read the file contents
;;     only if last modification time of the file changes

(defrecord WatchedFile [path line-regex])

; Contains
;  - WatchedFile record
;  - set of detected alerts (matched lines in the file)
(defrecord CheckResult [watched-file alerts])


;; logic of checking files

(defn check-file
  "Returns CheckResult for given file."
  [watched-file]
  (try
    (with-open [reader (io/reader (:path watched-file))]
      (let [all-lines (line-seq reader)
            alert-lines (filter #(re-matches (:line-regex watched-file) %) all-lines)]
          (CheckResult. watched-file (set alert-lines))))
    (catch java.io.IOException e
      (do
        (log/errorf "Failed to read file '%s'" (:path watched-file))
        (CheckResult. watched-file #{})))))

(defn check-files
  "Returns CheckResults for given files."
  [watched-files]
  (->> watched-files
       (map check-file)
       (filter #(not-empty (:alerts %)))))

(defn filter-out-seen-alerts
  "Returns non-empty check results that don't contain alerts from the prev-check-results."
  [cur-check-results prev-check-results]
  (let [known-alerts-by-file (apply hash-map (mapcat #(list (:watched-file %) (:alerts %)) prev-check-results))]
    (->> cur-check-results
         (map #(CheckResult. (:watched-file %)
                             (clojure.set/difference (:alerts %) (known-alerts-by-file (:watched-file %)))))
         (filter #(not-empty (:alerts %)))
         (vec))))

(defn run-watcher-until-stopped-action-creator
  "Creates a function dispatcheable to an agent. This function periodically checks given files
   for new interesting lines. If such a line appears, it invokes the notifier function,
   passing it the set of new interesting lines.
   Runs for as long as the switch agent contains a logically true value."
  [watched-files notifier intervalMs switch]
  (letfn [(run-watcher-until-stopped-action [prev-check-results]
            (log/info "Running watcher")
            (let [cur-check-results (check-files watched-files)
                  unseen-check-results (filter-out-seen-alerts cur-check-results prev-check-results)]
              (when (not-empty unseen-check-results)
                (log/infof "Found new problems in %d %s"
                           (count unseen-check-results)
                           (util/plural-of-word "file" (count unseen-check-results)))
                (notifier unseen-check-results))
              (Thread/sleep intervalMs)
              (when @switch
                (send-off *agent* run-watcher-until-stopped-action))
              (set cur-check-results)))]
    run-watcher-until-stopped-action))



;; watcher is an agent holding the set of all problems seen so far
(def watcher (agent #{}))

;; watcher-running allows to shutdown the watcher agent
(def watcher-running (agent true))

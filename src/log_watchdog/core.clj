(ns log-watchdog.core
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as log])
  (:gen-class))

;; define core data structures

;;TODO include a timestamp in CheckResult and read the file only if last modification time of the file changes

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
            positive-lines-lazy (filter #(re-matches (:line-regex watched-file) %) all-lines)
            positive-lines (doall (set positive-lines-lazy))]
          (CheckResult. watched-file positive-lines)))
    (catch java.io.IOException e
      (do
        (log/errorf "Failed to read file '%s'" (:path watched-file))
        (CheckResult. watched-file #{})))))

(defn check-files
  "Returns positive CheckResults for given files."
  [watched-files]
  (let [check-results (map check-file watched-files)]
    (filter #(not-empty (:alerts %)) check-results)))


(defn- extract-problems-from-new-files
  [cur-check-results prev-check-results]
  (let [known-files (set (map #(:watched-file %) prev-check-results))
        new-check-results (filter #(not (contains? known-files (:watched-file %))) cur-check-results)]
    new-check-results))

(defn- extract-problems-from-already-seen-files
  [cur-check-results prev-check-results]
  (let [known-files (set (map #(:watched-file %) prev-check-results))
        known-problems (apply hash-map (mapcat #(list (:watched-file %) (:alerts %)) prev-check-results))
        updated-check-results (filter #(contains? known-files (:watched-file %)) cur-check-results)
        new-problems (map #(CheckResult. (:watched-file %)
                                         (clojure.set/difference (:alerts %) (known-problems (:watched-file %))))
                          updated-check-results)]
    (filter #(not-empty (:alerts %)) new-problems)))

(defn extract-new-problems
  "Returns non-empty check results that don't contain any lines from the prev-check-results."
  [cur-check-results prev-check-results]
  (concat (extract-problems-from-new-files cur-check-results prev-check-results)
          (extract-problems-from-already-seen-files cur-check-results prev-check-results)))

(defn run-watcher-until-stopped-creator
  "Creates a function applicable to an agent that periodically checks given files
   for new interesting lines. If such a line appears, invokes the notifier function,
   passing the set of new interesting lines."
  [watched-files notifier intervalMs switch]
  (letfn [(run-watcher-until-stopped
           [prev-check-results]
            (log/info "Running watcher")
            (when @switch
              (send-off *agent* run-watcher-until-stopped))
            (let [cur-check-results (set (check-files watched-files))
                  new-problems (extract-new-problems cur-check-results prev-check-results)]
              (when (not-empty new-problems)
                (log/infof "Found %d new problems" (count new-problems))
                (notifier new-problems))
              (Thread/sleep intervalMs)
              cur-check-results))]
    run-watcher-until-stopped))



;; watcher is an agent holding the set of all problems seen so far
(def watcher (agent #{}))

;; watcher-running allows to shutdown the watcher agent
(def watcher-running (agent true))

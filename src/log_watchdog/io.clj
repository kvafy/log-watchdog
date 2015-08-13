(ns log-watchdog.io)

(defn file-exists? [^java.io.File file]
  (.exists file))

(defn file-size [^java.io.File file]
  (.length file))

(defn file-last-modified-ms [^java.io.File file]
  (.lastModified file))

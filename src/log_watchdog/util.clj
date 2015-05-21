(ns log-watchdog.util)

(defn plural-of-word [word count]
  (if (> count 1)
    (str word "s")
    word))

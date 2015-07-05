(ns log-watchdog.util)

(defn plural-of-word [word count]
  (let [plural-suffix (if (and (not-empty word)
                               (.endsWith word "s"))
                        "es" "s")]
    (if (> count 1)
      (str word plural-suffix)
      word)))

(ns log-watchdog.ui.utils
  (:require [clojure.java.io])
  (:import [java.awt SystemTray MenuItem Toolkit Desktop]
           [java.awt.event ActionListener MouseEvent MouseAdapter]
           [javax.swing JOptionPane]))


;; AWT / SWING

(defn create-action-listener [callback]
  (proxy [ActionListener] []
    (actionPerformed [event] (callback))))

(defn create-mouse-listener [& options]
  (let [no-op (fn [] nil)
        callback-map (into {:sgl-callback no-op :dbl-callback no-op :mdl-callback no-op :enter-callback no-op}
                           (apply hash-map options))
        {:keys [sgl-callback dbl-callback mdl-callback enter-callback]} callback-map]
    (proxy [MouseAdapter] []
      (mouseClicked [event]
        (let [click-count (.getClickCount event)]
          (condp = (.getButton event)
            MouseEvent/BUTTON1
              (condp = click-count
                ; this doesn't really work well in Java...
                1 (sgl-callback)
                2 (dbl-callback)
                nil)
            MouseEvent/BUTTON2
              (when (= click-count 1)
                (mdl-callback))
            nil)))
      (mouseEntered [event]
        (enter-callback)))))

(defn create-menu-item [label callback]
  (let [item (MenuItem. label)]
    (.addActionListener item (create-action-listener callback))
    item))

(defn create-menu-label [label]
  (let [item (MenuItem. label)]
    (.setEnabled item false)
    item))

(defn update-menu-item! [menu-item & kvs]
  (let [{:keys [label enabled]} kvs]
    (when (and (not (nil? label))
               (not= label (.getLabel menu-item)))
      (.setLabel menu-item label))
    (when (and (not (nil? enabled))
               (not= enabled (.isEnabled menu-item)))
      (.setEnabled menu-item enabled))))

(defn load-image [resource-name]
  (.getImage (Toolkit/getDefaultToolkit) (clojure.java.io/resource resource-name)))

(defn show-error-message [title msg]
  (JOptionPane/showMessageDialog nil msg title JOptionPane/ERROR_MESSAGE))

(defn open-files [& files]
  (let [desktop (Desktop/getDesktop)]
    (doseq [file files]
      (.open desktop file))))


;; other utilities

(defn plural-of-word
  ([count word]
    (plural-of-word count nil word))
  ([count adjective word]
    (let [plural-suffix (if (and (not-empty word)
                                 (.endsWith word "s"))
                          "es" "s")
          plural-word (if (not= count 1) (str word plural-suffix) word)]
      (if (nil? adjective)
        (format "%d %s" count plural-word)
        (format "%d %s %s" count adjective plural-word)))))

(defn file-name-and-dir [^java.io.File file]
  (let [name (.getName file)
        dir (-> file
                (.getAbsoluteFile)
                (.getParent))]
    [name dir]))

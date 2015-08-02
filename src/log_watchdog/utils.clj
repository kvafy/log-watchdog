(ns log-watchdog.utils)

(defn current-time-ms []
  (System/currentTimeMillis))

(defn uuid []
  (java.util.UUID/randomUUID))

(defn project-version []
  ; the project.clj is just a Clojure data file which can be easily read
  (-> (clojure.java.io/resource "project.clj") slurp read-string (nth 2)))

(defn merge-recursive [& maps]
  (reduce (fn [m1 m2]
            (if (empty? m2)
              m1
              (let [[k v2] (first m2)
                    v1 (get m1 k)]
                (if (every? map? [v1 v2])
                  (recur (assoc m1 k (merge-recursive v1 v2))
                         (dissoc m2 k))
                  (recur (assoc m1 k v2)
                         (dissoc m2 k))))))
          {}
          maps))


; Following code taken from https://gist.github.com/ataggart/377278

(defmacro try-let
  "A combination of try and let such that exceptions thrown in the binding or
   body can be handled by catch clauses in the body, and all bindings are
   available to the catch and finally clauses. If an exception is thrown while
   evaluating a binding value, it and all subsequent binding values will be nil.

   Example:
   (try-let [x (f a)
             y (f b)]
     (g x y)
     (catch Exception e (println a b x y e)))"
  {:arglists '([[bindings*] exprs* catch-clauses* finally-clause?])}
  [bindings & exprs]
  (when-not (even? (count bindings))
    (throw (IllegalArgumentException. "try-let requires an even number of forms in binding vector")))
  (let [names  (take-nth 2 bindings)
        values (take-nth 2 (next bindings))
        ex     (gensym "ex__")]
    `(let [~ex nil
           ~@(interleave names (repeat nil))
           ~@(interleave
               (map vector names (repeat ex))
               (for [v values]
                 `(if ~ex
                    [nil ~ex]
                    (try [~v nil]
                      (catch Throwable ~ex [nil ~ex])))))]
      (try
        (when ~ex (throw ~ex))
        ~@exprs))))

(comment
; turns this:
(try-let [x (f a)
          y (f b)]
  (g x y)
  (catch Exception e (println a b x y e)))

; into something like this (where ex is a gensym):
(let [ex     nil ; holds the exception thrown when evaluating binding values
      x      nil
      y      nil
      [x ex] (if ex [nil ex] (try [(f a) nil] (catch Throwable ex [nil ex])))
      [y ex] (if ex [nil ex] (try [(f b) nil] (catch Throwable ex [nil ex])))]
  (try
    (when ex (throw ex))
    (g x y)
    (catch Exception e (println a b x y e))))
)

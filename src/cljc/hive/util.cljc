(ns hive.util)

(defn dissoc-in
  "Dissociate this keyseq from m, removing any empty maps created as a result
   (including at the top-level)."
  [m [k & ks]]
  (when m
    (if-let [res (and ks (dissoc-in (m k) ks))]
      (assoc m k res)
      (let [res (dissoc m k)]
        (when-not (empty? res)
          res)))))

(defn check-argument
  "Throw a platform-appropriate exception if valid? is false"
  [valid? msg]
  (when-not valid?
    #?(:clj (throw (IllegalArgumentException. msg))
       :cljs (throw (js/Error. msg)))))


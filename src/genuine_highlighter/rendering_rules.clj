(ns genuine-highlighter.rendering-rules)

(defn- maybe [x & args]
  (when-not (nil? x)
    (apply x args)))

(defn compose-rules [r1 r2]
  (fn [type]
    (fn [part]
      (fn [x v]
        (let [app (fn [r v] (-> r (maybe type) (maybe part) (maybe x v)))
              v (or (app r1 v) v)]
          (or (app r2 v) v))))))

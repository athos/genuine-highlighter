(ns genuine-highlighter.core
  (require [genuine-highlighter.parser :refer [parse]]
           [genuine-highlighter.analyzer :refer [analyze]]
           [genuine-highlighter.renderer :refer [render]]
           [clojure.tools.reader.reader-types :refer [string-push-back-reader]]))

(defn highlight
  ([rule] (highlight rule *in*))
  ([rule rdr]
     (->> (parse rdr)
          analyze
          (render rule))))

(defn highlight-string [rule s]
  (let [r (string-push-back-reader s)]
    (highlight rule r)))

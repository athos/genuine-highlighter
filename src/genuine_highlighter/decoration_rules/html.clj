(ns genuine-highlighter.decoration-rules.html)

(defn ^:private colorful-symbol [x v]
  (let [type (name (:type (:usage x)))]
    (format "<span class=\"%s\">%s</span>" type v)))

(def colorful-symbols-rule
  {:symbol {:content colorful-symbol}})

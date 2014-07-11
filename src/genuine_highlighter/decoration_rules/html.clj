(ns genuine-highlighter.decoration-rules.html)

(defn ^:private colorful-symbol [x v]
  (when-let [type (some-> x :usage :type name)]
    (format "<span class=\"%s\">%s</span>" type v)))

(def colorful-symbols-rule
  {:symbol {:content colorful-symbol}})

(ns genuine-highlighter.analyzer
  (:require [clojure.walk :refer [postwalk]]
            [genuine-highlighter.converter :refer [convert]]
            [genuine-highlighter.extractor :refer [extract]])
  (:import net.cgrand.parsley.Node))

;;
;; Annotation
;;
(defn- annotate [node info]
  (if (string? node)
    node
    (let [{:keys [content]} node
          node' (assoc node :content (mapv #(annotate % info) content))]
      (if-let [usage (and (= (:tag node) :symbol) (info (:id node)))]
        (assoc node' :usage usage)
        node'))))

;;
;; Entry point
;;
(defn analyze
  ([x] (analyze *ns* x))
  ([ns x]
     (->> (convert x)
          (extract ns)
          (annotate x))))

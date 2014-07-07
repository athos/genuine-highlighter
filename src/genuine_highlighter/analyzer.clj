(ns genuine-highlighter.analyzer
  (:require [clojure.walk :refer [postwalk]]
            [genuine-highlighter.conversion :refer [convert]]
            [genuine-highlighter.extraction :refer [extract]])
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
  ([root] (analyze *ns* root))
  ([ns root]
     (let [sexps (convert root)
           info (reduce (fn [info sexp]
                          (eval sexp)
                          (merge info (extract ns sexp)))
                        {}
                        sexps)]
       (annotate root info))))

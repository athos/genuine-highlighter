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
(defn analyze [root & {:keys [ns suppress-eval?]}]
  (let [ns (or ns *ns*)
        sexps (convert root)
        ext (fn [info sexp]
              (when-not suppress-eval?
                (binding [*ns* ns]
                  (eval sexp)))
              (merge info (extract ns sexp)))
        info (reduce ext {} sexps)]
    (annotate root info)))

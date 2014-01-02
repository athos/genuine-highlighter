(ns genuine-highlighter.analyzer
  (:require [clojure.walk :refer [postwalk]]
            [genuine-highlighter.converter :refer [convert]]
            [genuine-highlighter.extractor :refer [extract]]))

;;
;; Annotation
;;
(defn- annotate [x info]
  (letfn [(annotate-info [x]
            (if (and (map? x) (= (:type x) :symbol))
              (assoc x :usage (info (:id x)))
              x))]
    (postwalk annotate-info x)))

;;
;; Entry point
;;
(defn analyze
  ([x] (analyze *ns* x))
  ([ns x]
     (->> (convert x)
          (extract ns)
          (annotate x))))

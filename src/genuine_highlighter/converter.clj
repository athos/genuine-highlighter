(ns genuine-highlighter.converter
  (:require [genuine-highlighter.parser :as p])
  (:import net.cgrand.parsley.Node))

(defn get-id [x]
  (::id (meta x)))

(defn- remove-whitespaces [content]
  (filterv #(or (not (instance? Node %))
                (not (#{:whitespace :newline} (p/node-tag %))))
           content))

(defn- essential-content [x]
  (remove-whitespaces (p/node-content* x)))

(defmulti ^:private convert* p/node-tag)

(defmethod convert* :root [x]
  (mapv convert* (essential-content x)))

(defmethod convert* :symbol [x]
  (let [[maybe-ns _ maybe-name] (p/node-content* x)
        sym (if maybe-name
              (symbol (p/node-content maybe-ns) (p/node-content maybe-name))
              (symbol (p/node-content maybe-ns)))]
    (with-meta sym
      {::id (:id x)})))

(defmethod convert* :number [x]
  (read-string (p/node-content x)))

(declare convert-seq)

(defmethod convert* :list [x]
  (convert-seq x))

(defmethod convert* :vector [x]
  (vec (convert-seq x)))

(defmethod convert* :map [x]
  (into {} (map vec (partition 2 (convert-seq x)))))

(defmethod convert* :set [x]
  (set (convert-seq x)))

(defn convert [root]
  (convert* root))

(defn- convert-seq [x]
  (->> (essential-content x)
       butlast
       rest
       (map convert*)))

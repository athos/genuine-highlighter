(ns genuine-highlighter.conversion
  (:require [genuine-highlighter.parsing :as p])
  (:import net.cgrand.parsley.Node))

(defn get-id [x]
  (::id (meta x)))

(defn- remove-whitespaces [content]
  (filterv #(or (not (instance? Node %))
                (not (#{:whitespace :newline :comment :discard} (p/node-tag %))))
           content))

(defn- essential-content [x]
  (remove-whitespaces (p/node-content* x)))

(defmulti ^:private convert* p/node-tag)

(defmethod convert* :root [x]
  (mapv convert* (essential-content x)))

(defmethod convert* :nil [x]
  nil)

(defmethod convert* :boolean [x]
  ({"true" true, "false" false} x))

(defmethod convert* :symbol [x]
  (let [[maybe-ns _ maybe-name] (p/node-content* x)
        sym (if maybe-name
              (symbol (p/node-content maybe-ns) (p/node-content maybe-name))
              (symbol (p/node-content maybe-ns)))]
    (with-meta sym
      {::id (:id x)})))

(defmethod convert* :keyword [x]
  (let [[colon maybe-ns _ maybe-name] (p/node-content* x)]
    (cond maybe-name
          #_=> (keyword (p/node-content maybe-ns) (p/node-content maybe-name))
          (= colon "::")
          #_=> (keyword (name (ns-name *ns*)) (p/node-content maybe-ns))
          :else (keyword (p/node-content maybe-ns)))))

(defmethod convert* :number [x]
  (read-string (p/node-content x)))

(defmethod convert* :char [x]
  (read-string (p/node-content x)))

(defmethod convert* :string [x]
  (let [[_ s _] (p/node-content* x)]
    s))

(defmethod convert* :regex [x]
  (let [[_ _ s _] (p/node-content* x)]
    (re-pattern s)))

(defmethod convert* :fn [x])

(defmethod convert* :meta [x])

(defmethod convert* :var [x]
  (let [[_ maybe-ns _ maybe-name] (essential-content x)
        sym (if maybe-name
              (symbol (convert* maybe-ns) (p/node-content maybe-name))
              (symbol (convert* maybe-ns)))]
    (list 'var sym)))

(defn- wrap [sym node]
  (let [[_ v] (essential-content node)]
    (list sym (convert* v))))

(defmethod convert* :deref [x]
  (wrap 'clojure.core/deref x))

(defmethod convert* :quote [x]
  (wrap 'quote x))

(defmethod convert* :syntax-quote [x])

(defmethod convert* :unquote [x]
  (wrap 'clojure.core/unquote x))

(defmethod convert* :unquote-splicing [x]
  (wrap 'clojure.core/unquote-splicing x))

(defmethod convert* :eval [x])

(defmethod convert* :reader-literal [x])

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

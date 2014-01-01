(ns genuine-highlighter.converter)

(defn get-id [x]
  (::id (meta x)))

(declare convert)

(defmulti ^:private convert* :type)

(defmethod convert* :symbol [x]
  (with-meta (:symbol x)
    {::id (:id x)}))

(defmethod convert* :number [x]
  (:number x))

(defmethod convert* :list [x]
  (map convert* (filter #(not= (:type %) :whitespaces) (:nodes x))))

(defmethod convert* :vector [x]
  (vec (convert* (assoc x :type :list))))

(defmethod convert* :map [x]
  (into {} (map vec (partition 2 (convert* (assoc x :type :list))))))

(defmethod convert* :set [x]
  (set (convert* (assoc x :type :list))))

(defn convert [xs]
  (when-let [x (first (drop-while #(= (:type %) :whitespaces) xs))]
    (convert* x)))

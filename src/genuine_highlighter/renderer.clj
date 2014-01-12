(ns genuine-highlighter.renderer)

(defn- maybe [x & args]
  (when-not (nil? x)
    (apply x args)))

(defn apply-rule [r x]
  (if-let [f (r (:type x))]
    (assoc x :contents
           (mapcat (fn [[p v]] [p (or (-> f (maybe p) (maybe x v)) v)])
                   (partition 2 (:contents x))))
    x))

(declare render)

(defmulti ^:private prepare (fn [_ x] (:type x)))

(defmethod prepare :whitespaces [r x]
  [:raw (:raw x)])

(defmethod prepare :symbol [r x]
  [:symbol (:symbol x)])

(defmethod prepare :number [r x]
  [:number (:number x)])

(defmethod prepare :list [r x]
  [:lparen "(" :nodes (render r (:nodes x)) :rparen ")"])

(defmethod prepare :vector [r x]
  [:lbracket "[" :nodes (render r (:nodes x)) :rbracket "]"])

(defn- render* [r x]
  (->> (prepare r x)
       (assoc x :contents)
       (apply-rule r)
       :contents
       (partition 2)
       (map second)
       (apply str)))

(defn render [rule nodes]
  (apply str (map #(render* rule %) nodes)))

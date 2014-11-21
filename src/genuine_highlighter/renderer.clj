(ns genuine-highlighter.renderer
  (:require [genuine-highlighter.parsing :as p]))

(defn- maybe [x & args]
  (when-not (nil? x)
    (apply x args)))

(defn apply-rule [r x]
  (if-let [f (r (:tag x))]
    (assoc x :content
           (mapcat (fn [[p v]] [p (or (-> f (maybe p) (maybe x v)) v)])
                   (partition 2 (:content x))))
    x))

(declare render-seq)

(defmulti ^:private prepare (fn [_ x] (p/node-tag x)))

(defmethod prepare :default [r x]
  [:content (p/node-content x)])

(defmethod prepare :net.cgrand.sjacket.parser/root [r x]
  [:content (render-seq r (p/node-content* x))])

(defmethod prepare :symbol [r x]
  (let [[maybe-ns _ maybe-name] (p/node-content* x)
        sym (if maybe-name
              (symbol (p/node-content maybe-ns) (p/node-content maybe-name))
              (symbol (p/node-content maybe-ns)))]
    [:content sym]))

(defn- prepare-nested [r x open close]
  `[:open ~open
    ~@(let [[open & maybe-content] (p/node-content* x)
            content (take-while #(not= % close) maybe-content)]
        (when content
          [:nodes (render-seq r content)]))
    :close ~close])

(defmethod prepare :list [r x]
  (prepare-nested r x "(" ")"))

(defmethod prepare :vector [r x]
  (prepare-nested r x "[" "]"))

(defmethod prepare :map [r x]
  (prepare-nested r x "{" "}"))

(defmethod prepare :set [r x]
  (prepare-nested r x "#{" "}"))

(defn render [rule node]
  (->> (prepare rule node)
       (assoc node :content)
       (apply-rule rule)
       :content
       (partition 2)
       (map second)
       (apply str)))

(defn- render-seq [rule nodes]
  (apply str (map #(render rule %) nodes)))

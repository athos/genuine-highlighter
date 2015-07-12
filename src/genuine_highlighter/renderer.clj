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

(defmethod prepare :keyword [r x]
  (let [[pre maybe-ns _ maybe-name] (p/node-content* x)
        keyword (if maybe-name
                  (str pre (p/node-content maybe-ns) \/ (p/node-content maybe-name))
                  (str pre (p/node-content maybe-ns)))]
    [:content keyword]))

(defmethod prepare :string [r x]
  (let [[_ s _] (p/node-content* x)]
    [:content (str "\"" s "\"")]))

(defmethod prepare :char [r x]
  (let [[_ char] (p/node-content* x)]
    [:content (str "\\" char)]))

(defmethod prepare :quote [r x]
  (let [[quote & contents] (p/node-content* x)]
    [:quote quote
     :nodes (render-seq r contents)]))

(defmethod prepare :var [r x]
  (let [[var & contents] (p/node-content* x)]
    [:var var
     :nodes (render-seq r contents)]))

(defmethod prepare :meta [r x]
  (let [[meta & contents] (p/node-content* x)]
    [:meta meta
     :nodes (render-seq r contents)]))

(defmethod prepare :deref [r x]
  (let [[deref & contents] (p/node-content* x)]
    [:deref deref
     :nodes (render-seq r contents)]))

(defmethod prepare :comment [r x]
  (let [[_ comment] (p/node-content* x)]
    [:content (str ";" comment)]))

;; FIXME: The preparation implementation for discard is INCOMPLETE
;; Discarded forms should be rendered differently from ordinary ones
(defmethod prepare :discard [r x]
  (let [[discard & contents] (p/node-content* x)]
    [:discard discard
     :nodes (render-seq r contents)]))

(defmethod prepare :regex [r x]
  (let [[_ _ p _] (p/node-content* x)]
    [:content (str "#\"" p "\"")]))

(defmethod prepare :syntax-quote [r x]
  (let [[syntax-quote & contents] (p/node-content* x)]
    [:syntax-quote syntax-quote
     :nodes (render-seq r contents)]))

(defmethod prepare :unquote [r x]
  (let [[unquote & contents] (p/node-content* x)]
    [:unquote unquote
     :nodes (render-seq r contents)]))

(defmethod prepare :unquote-splicing [r x]
  (let [[unquote-splicing & contents] (p/node-content* x)]
    [:unquote-splicing unquote-splicing
     :nodes (render-seq r contents)]))

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

(defmethod prepare :fn [r x]
  (prepare-nested r x "#(" ")"))

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

(ns genuine-highlighter.renderer)

(defn- maybe [x & args]
  (when-not (nil? x)
    (apply x args)))

(defn- compound-rules [r1 r2]
  (fn [type]
    (fn [part]
      (fn [x v]
        (let [app (fn [r v] (-> r (maybe type) (maybe part) (maybe x v)))
              v (or (app r1 v) v)]
          (or (app r2 v) v))))))

(defn- apply-rule [r x]
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

(def white-symbols-rule
  {:symbol {:symbol (fn [x v] (str "\033[37m" v "\033[39m"))}})

(defn ^:private colorful-symbol [_]
  (fn [x v]
    (let [code {:special 36, :macro 33, :var 35, :local 34}
          color (fn [x]
                  (if-let [c (code (:type (:usage x)))]
                    (str "\033[" c "m" v "\033[39m")
                    (str (:symbol x))))]
      (color x))))

(def colorful-symbols-rule
  {:symbol colorful-symbol})

(def rainbow-parens-rule
  (let [colors (atom (cycle (range 31 38)))
        color-stack (atom nil)
        open-fn (fn [x v]
                  (let [[color] @colors]
                    (swap! colors rest)
                    (swap! color-stack conj color)
                    (str "\033[" color "m" v "\033[39m")))
        close-fn (fn [x v]
                   (let [[color] @color-stack]
                     (swap! color-stack rest)
                     (str "\033[" color "m" v "\033[39m")))]
    {:list {:lparen open-fn, :rparen close-fn}
     :vector {:lbracket open-fn, :rbracket close-fn}
     :map {:lbrace open-fn, :rbrace close-fn}}))

(def fadeout-parens-rule
  (letfn [(count-preceding-parens [x]
            (loop [x x n 0]
              (if (#{:list :vector :map} (:type x))
                (recur (last (:nodes x)) (inc n))
                n)))
          (fadeout-parens [x v]
            (let [colors {1 255, 2 255, 3 247, 4 242}
                  color (or (colors (count-preceding-parens x)) 237)]
              (str "\033[38;5;" color  "m" v "\033[0m")))]
    {:list {:rparen fadeout-parens}
     :vector {:rbracket fadeout-parens}
     :map {:rbrace fadeout-parens}}))

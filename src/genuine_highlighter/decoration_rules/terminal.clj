(ns genuine-highlighter.decoration-rules.terminal)

(defn color-ansi [c x]
  (format "\033[%dm%s\033[39m" c x))

(defn color-256 [c x]
  (format "\033[38;5;%dm%s\033[0m" c x))

(def white-symbols-rule
  {:symbol {:symbol #(color-ansi 37 %2)}})

(defn ^:private colorful-symbol [_]
  (fn [x v]
    (let [code {:special 36, :macro 33, :var 35, :local 34}
          color (fn [x]
                  (if-let [c (code (:type (:usage x)))]
                    (color-ansi c v)
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
                    (color-ansi color v)))
        close-fn (fn [x v]
                   (let [[color] @color-stack]
                     (swap! color-stack rest)
                     (color-ansi color v)))]
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
              (color-256 color v)))]
    {:list {:rparen fadeout-parens}
     :vector {:rbracket fadeout-parens}
     :map {:rbrace fadeout-parens}}))

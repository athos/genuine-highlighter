(ns leiningen.highlight
  (:require [clojure.main :as m]
            [genuine-highlighter.parser :as p]
            [genuine-highlighter.analyzer :as a]
            [genuine-highlighter.renderer :as r]
            [genuine-highlighter.converter :as c]
            [genuine-highlighter.decoration-rules.terminal :as t]))

(defn- drop-proceeding-newlines [nodes]
  (drop-while #(and (= (:type %) :whitespaces)
                    (= (:raw %) "\n"))
              nodes))

(defn- read-and-highlight [in]
  (let [p (drop-proceeding-newlines (p/parse in))]
    (printf "  #_== %s\n" (r/render t/colorful-symbols-rule (a/analyze p)))
    (c/convert p)))

(defn highlight
  "Highlight Clojure source code."
  [project & args]
  (m/repl :read (fn [_ _] (read-and-highlight *in*))
          :need-prompt #(do true)))

(ns leiningen.highlight
  (:require [clojure.main :as m]
            [genuine-highlighter.core :as hl]
            [genuine-highlighter.parser :as p]
            [genuine-highlighter.analyzer :as a]
            [genuine-highlighter.renderer :as r]
            [genuine-highlighter.converter :as c]
            [genuine-highlighter.decoration-rules.terminal :as t]))

(defn- drop-proceeding-newlines [s]
  (clojure.string/replace s #"^\n+" ""))

(defn- read-and-highlight [^java.io.BufferedReader in]
  (loop [code ""]
    (let [line (.readLine in)
          code' (str code \newline line)]
      (if-let [result (hl/highlight t/colorful-symbols-rule code')]
        (let [result (->> (clojure.string/split result #"\n")
                          (drop-while #(= "" %))
                          (map #(str "  #_== " %))
                          (clojure.string/join \newline))]
          (println result)
          (flush)
          (read-string code'))
        (do (print "  #_=> ")
            (flush)
            (recur code'))))))

(defn highlight
  "Highlight Clojure source code."
  [project & args]
  (m/repl :read (fn [_ _] (read-and-highlight (clojure.java.io/reader *in*)))
          :need-prompt #(do true)))

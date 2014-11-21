(ns genuine-highlighter.core
  (:require [genuine-highlighter [parsing :as p]
                                 [renderer :as renderer]]
            [symbol-analyzer.core :as ana]))

(defn highlight [rule s & {:keys [ns unfinished suppress-eval?]
                           :or {ns *ns*}}]
  (let [ast (p/parse s)]
    (cond (p/unfinished? ast)
          #_=> unfinished
          (p/unexpected? ast)
          #_=> (throw (ex-info "unexpected" {::type :unexpected :data s}))
          :else (->> (ana/analyze ast :ns ns :suppress-eval? suppress-eval?)
                     (renderer/render rule)))))

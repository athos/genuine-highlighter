(ns genuine-highlighter.core
  (:require [genuine-highlighter [parsing :as p]
                                 [analyzer :as analyzer]
                                 [renderer :as renderer]]))

(defn highlight [rule s & {:keys [ns unfinished suppress-eval?]}]
  (let [ast (p/parse s)]
    (cond (p/unfinished? ast)
          #_=> unfinished
          (p/unexpected? ast)
          #_=> (throw (ex-info "unexpected" {::type :unexpected :data s}))
          :else (->> (analyzer/analyze ast :ns ns :suppress-eval? suppress-eval?)
                     (renderer/render rule)))))

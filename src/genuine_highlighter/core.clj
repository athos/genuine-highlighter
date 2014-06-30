(ns genuine-highlighter.core
  (:require [genuine-highlighter [parser :as parser]
                                 [analyzer :as analyzer]
                                 [renderer :as renderer]]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io BufferedReader]))

(defn highlight
  ([rule s] (highlight rule s nil))
  ([rule s unfinished]
     (let [ast (parser/parse s)]
       (cond (parser/unfinished? ast)
             #_=> unfinished
             (parser/unexpected? ast)
             #_=> (throw (ex-info "unexpected" {::type :unexpected :data s}))
             :else (renderer/render rule (analyzer/analyze ast))))))

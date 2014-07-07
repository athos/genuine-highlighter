(ns genuine-highlighter.core
  (:require [genuine-highlighter [parsing :as p]
                                 [analyzer :as analyzer]
                                 [renderer :as renderer]]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io BufferedReader]))

(defn highlight
  ([rule s] (highlight rule s nil))
  ([rule s unfinished]
     (let [ast (p/parse s)]
       (cond (p/unfinished? ast)
             #_=> unfinished
             (p/unexpected? ast)
             #_=> (throw (ex-info "unexpected" {::type :unexpected :data s}))
             :else (renderer/render rule (analyzer/analyze ast))))))

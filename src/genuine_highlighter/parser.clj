(ns genuine-highlighter.parser
  (:require [net.cgrand.parsley :as p]
            [net.cgrand.sjacket.parser :as s]))

(def new-id
  (let [n (atom 0)]
    (fn []
      (swap! n inc)
      @n)))

(def ^{:arglist '[s]} parse
  (letfn [(make-node [tag content]
            (let [node (p/->Node tag content)]
              (if (= tag :symbol)
                (assoc node :id (new-id))
                node)))]
    (p/make-parser {:main :sexpr*
                    :space [s/space-nodes :*]
                    :root-tag :root
                    :make-node make-node
                    }
                   s/rules)))

(defn node-tag [node]
  (:tag node))

(defn node-content* [node]
  (:content node))

(defn node-content [node]
  (first (node-content* node)))

(ns genuine-highlighter.parsing
  (:require [net.cgrand.sjacket.parser :as p]))

(defn parse [s]
  (p/parser s))

(defn node-tag [node]
  (:tag node))

(defn node-content* [node]
  (:content node))

(defn node-content [node]
  (first (node-content* node)))

(defn unfinished? [node]
  (= (:tag node) :net.cgrand.parsley/unfinished))

(defn unexpected? [node]
  (some #(= (:tag %) :net.cgrand.parsley/unexpected)
        (tree-seq node-tag node-content* node)))

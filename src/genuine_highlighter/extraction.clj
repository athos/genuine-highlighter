(ns genuine-highlighter.extraction
  (:refer-clojure :exclude [extend])
  (:require [genuine-highlighter.conversion :refer [get-id]]
            [clojure.core.match :refer [match]]))

;;
;; Environment
;;
(defn- toplevel-env
  ([] (toplevel-env *ns*))
  ([ns]
   {:ns (the-ns ns) :locals {}}))

(defn- lookup [env n]
  (or ((:locals env) n)
      (ns-resolve (:ns env) n)))

(defn- extend [env n v]
  (assoc-in env [:locals n] v))

;;
;; Extraction
;;
(def ^:private specials
  '#{def if do quote set! try catch finally throw var monitor-enter monitor-exit
     new . let* fn* loop* recur letfn* case* reify* deftype* clojure.core/import*})

(defn- special? [[op]]
  (boolean (specials op)))

(defn- extract-from-symbol [env sym]
  (or (when-let [m (get-id sym)]
        (let [e (lookup env sym)]
          (cond (var? e) {m {:type :var :usage :ref :var e}}
                (class? e) {m {:type :class :class e}}
                e {m {:type :local :usage :ref :binding e}}
                :else nil)))
      {}))

(declare extract*)

(defn- extract-from-forms [env forms]
  (into {} (map #(extract* env %) forms)))

(defmulti ^:private extract-from-special (fn [env [op]] op))
(defmethod extract-from-special :default [env [op & args]]
  (apply conj {}
         (when-let [m (get-id op)]
           {m {:type :special :op op}})
         (extract-from-forms env args)))

(defn- extract-from-seq [env [maybe-op :as seq]]
  (cond (special? seq)
        #_=> (extract-from-special env seq)
        (symbol? maybe-op)
        #_=> (let [e (lookup env maybe-op)]
               (if (or (var? e) (nil? e))
                 ;; op may be a macro or .method or Class. or Class/method
                 (let [expanded (macroexpand seq)]
                   (cond (= expanded seq)
                         #_=> (extract-from-forms env seq)
                         (var? e)
                         #_=> (apply conj {}
                                     (when-let [m (get-id maybe-op)]
                                       {m {:type :macro :macro e}})
                                     (extract* env expanded))
                         :else (extract* env expanded)))
                 (extract-from-forms env seq)))
        :else (extract-from-forms env seq)))

(defn- extract* [env form]
  (cond (symbol? form)
        #_=> (extract-from-symbol env form)
        (seq? form)
        #_=> (extract-from-seq env form)
        (vector? form)
        #_=> (extract-from-forms env form)
        (map? form)
        #_=> (merge (extract-from-forms env (keys form))
                    (extract-from-forms env (vals form)))
        :else {}))

(defn extract
  ([form] (extract *ns* form))
  ([ns form]
     (extract* (toplevel-env ns) form)))

;;
;; Implementation of etraction methods
;; for each special form (related to bindings)
;;

(defmacro def-special-extractor [op & clauses]
  `(defmethod extract-from-special '~op [~'env ~'form]
     (match ~'form
       ~@(->> (for [[pat maybe-map maybe-expr] clauses
                    :let [map (if (map? maybe-map) maybe-map {})
                          expr (if (map? maybe-map) maybe-expr maybe-map)]]
                 [`(~(vec pat) :seq)
                  `(apply conj {}
                          (let [op# (first ~'form)]
                            (when-let [m# (get-id op#)]
                              {m# {:type :special :op op#}}))
                          ~@(for [[name usage] map]
                              `(when-let [m# (and (symbol? ~name) (get-id ~name))]
                                 {m# ~usage}))
                          ~expr)])
              (apply concat))
       :else nil)))

(defn- collect-symbols [ret x]
  (cond (symbol? x) (conj ret x)
        (map? x) (-> ret
                     (collect-symbols (keys x))
                     (collect-symbols (vals x)))
        (coll? x) (reduce collect-symbols ret x)
        :else ret))

(def-special-extractor quote
  [(_ sexp)
   (for [sym (collect-symbols [] sexp)
         :let [m (get-id sym)]
         :when m]
     {m {:type :quote}})])

(def-special-extractor def
  [(_ name expr)
   {name {:type :var :usage :def :name name}}
   (extract* env expr)])

(defn- extract-from-bindings [env bindings]
  (loop [env env, [[name expr] & more :as bindings] (partition 2 bindings), ret {}]
    (if (empty? bindings)
      [ret env]
      (let [m (get-id name)
            e {:type :local :usage :def}]
        (recur (extend env name e)
               more
               (conj ret (if m {m e} {}) (extract* env expr)))))))

(def-special-extractor let*
  [(_ bindings & body)
   (let [[info env] (extract-from-bindings env bindings)]
     (merge info (extract-from-forms env body)))])

(def-special-extractor loop*
  [(_ bindings & body)
   (let [[info env] (extract-from-bindings env bindings)]
     (merge info (extract-from-forms env body)))])

(defn- extract-from-args [env args]
  (loop [env env, [name & more :as args] args, ret {}]
    (if (empty? args)
      [ret env]
      (let [m (get-id name)
            e {:type :local :usage :def}]
        (recur (extend env name e)
               more
               (if m (assoc ret m e) ret))))))

(defn- extract-from-clauses [env clauses]
  (->> (for [[args & body] clauses
             :let [[info env] (extract-from-args env args)]]
         (merge info (extract-from-forms env body)))
       (into {})))

(def-special-extractor fn*
  [(_ (args :guard vector?) & body)
   (extract-from-special env `(fn* (~args ~@body)))]
  [(_ (fname :guard symbol?) (args :guard vector?) & body)
   (extract-from-special env `(fn* fname (~args ~@body)))]
  [(_ (clause :guard seq?) & clauses)
   (extract-from-special env `(fn* nil ~clause ~@clauses))]
  [(_ fname & clauses)
   {fname {:type :local :usage :def}}
   (let [env' (if fname
                (extend env fname {:type :local :usage :def})
                env)]
     (extract-from-clauses env' clauses))])

(defmethod extract-from-special 'letfn* [env [op & more]])

(defmethod extract-from-special 'catch [env [op exn e & body]])

(defmethod extract-from-special 'new [env [op class & args]])

(defmethod extract-from-special '. [env [op target field-or-method]])

(defmethod extract-from-special 'case* [env form])

(defmethod extract-from-special 'reify* [env form])

(defmethod extract-from-special 'deftype* [env form])

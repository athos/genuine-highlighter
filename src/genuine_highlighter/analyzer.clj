(ns genuine-highlighter.analyzer
  (:refer-clojure :exclude [extend])
  (:require [clojure.walk :refer [postwalk]]))

;;
;; Marks
;;
(defn- gen-mark []
  (gensym 'mark))

(defn- add-mark [x]
  (vary-meta x assoc ::mark (gen-mark)))

(defn- get-mark [x]
  (::mark (meta x)))

(defn- add-marks [form]
  (postwalk #(if (symbol? %) (add-mark %) %) form))

;;
;; Environment
;;
(defn- default-env
  ([] (default-env *ns*))
  ([ns]
   {:ns ns :locals {}}))

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
  (specials op))

(defn- extract-from-symbol [env sym]
  (or (when-let [m (get-mark sym)]
        (let [e (lookup env sym)]
          (cond (var? e) {m {::type :var ::usage ::ref :var e}}
                (class? e) {m {::type :class ::class e}}
                e {m {::type :local ::usage :ref ::binding e}}
                :else nil)))
      {}))

(declare extract)

(defn- extract-from-forms [env forms]
  (into {} (map #(extract env %) forms)))

(defmulti ^:private extract-from-special (fn [env [op]] op))
(defmethod extract-from-special :default [env [op & args]]
  (apply conj {}
         (when-let [m (get-mark op)]
           {m {::type :special ::op op}})
         (extract-from-forms env args)))

(defn- extract-from-seq [env [op :as seq]]
  (if (special? seq)
    (extract-from-special env seq)
    (let [e (lookup env op)]
      (if (or (var? e) (nil? e))
        ;; op may be a macro or .method or Class. or Class/method
        (let [expanded (macroexpand seq)]
          (cond (= expanded seq)
                #_=> (extract-from-forms env seq)
                (var? e)
                #_=> (apply conj {}
                            (when-let [m (get-mark op)]
                              {m {::type :macro ::macro e}})
                            (extract env expanded))
                :else (extract env expanded)))
        (extract-from-forms env seq)))))

(defn- extract [env form]
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

;;
;; Annotation
;;
(defn- annotate [form info]
  (letfn
      [(annotate-info [x]
         (if (symbol? x)
           (let [m (get-mark x)]
             (-> x
                 (vary-meta dissoc m)
                 (vary-meta into (info m))))
           x))]
    (postwalk annotate-info form)))

;;
;; Entry point
;;
(defn analyze
  ([form] (analyze *ns* form))
  ([ns form]
     (let [form (add-marks form)]
       (->> form
            (extract (default-env ns))
            (annotate form)))))

;;
;; Implementation of etraction methods
;; for each special form (related to bindings)
;;
(defmethod extract-from-special 'def [env [op name expr]]
  (apply conj {}
         (when-let [m (get-mark op)]
           {m {::type :special ::op op}})
         (when-let [m (get-mark name)]
           {m {::type :var ::usage :def ::name name}})
         (extract env expr)))

(defn- extract-from-bindings [env bindings]
  (loop [env env, [[name expr] & more :as bindings] (partition 2 bindings), ret {}]
    (if (empty? bindings)
      [ret env]
      (let [m (get-mark name)
            e {::type :local ::usage :def}]
        (recur (extend env name e)
               more
               (conj ret (if m {m e} {}) (extract env expr)))))))

(defmethod extract-from-special 'let* [env [op bindings & body]]
  (apply conj {}
         (when-let [m (get-mark op)]
           {m {::type :special ::op op}})
         (let [[info env] (extract-from-bindings env bindings)]
           (merge info (extract-from-forms env body)))))

(defmethod extract-from-special 'loop* [env [op bindings & body]]
  (apply conj {}
         (when-let [m (get-mark op)]
           {m {::type :special ::op op}})
         (let [[info env] (extract-from-bindings env bindings)]
           (merge info (extract-from-forms env body)))))

(defmethod extract-from-special 'fn* [env [op & more]]
  #_(let [maybe-name (first more)
        fname (if (symbol? maybe-name) maybe-name nil)
        clauses (if fname (rest more) more)
        clauses (if (vector? (first clauses))
                  (list clauses)
                  clauses)
        e {::type :local ::usage :def}
        env (if fname (extend env fname e) env)]
    (apply conj {}
           (when-let [m (get-mark op)]
             {m {::type :special ::op op}})
           (when-let [m (get-mark fname)]
             {m e})
           )))

(defmethod extract-from-special 'letfn* [env [op & more]])

(defmethod extract-from-special 'catch [env [op exn e & body]])

(defmethod extract-from-special '. [env [op target field-or-method]])

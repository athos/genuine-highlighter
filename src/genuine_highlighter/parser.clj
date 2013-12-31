(ns genuine-highlighter.parser
  (:require [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as types]
            [clojure.tools.reader.impl.utils :as utils]
            [clojure.tools.reader.impl.commons :as commons]))

(defn- whitespace? [^Character ch]
  (Character/isWhitespace ch))

(defn- parse-whitespaces [rdr initch]
  (loop [ws [initch]]
    (let [ch (types/read-char rdr)]
      (if (whitespace? ch)
        (recur (conj ws ch))
        (do (types/unread rdr ch)
            {:type :whitespaces :raw (apply str ws)})))))

(defn- parse-comment [rdr initch])

(defn- not-implemented [m]
  (fn [& args]
    (throw (Exception. (str "reader macro " m " not implemented yet")))))

(defn- dispatch-macros [ch]
  (case ch
    \^ (not-implemented "#^") #_read-meta
    \' (not-implemented "#'") #_(wrapping-reader 'var)
    \( (not-implemented "#(") #_read-fn
    \= (not-implemented "#=") #_read-eval
    \{ (not-implemented "#{") #_read-set
    \< (not-implemented "#<") #_(throwing-reader "Unreadable form")
    \" (not-implemented "#\"") #_read-regex
    \! (not-implemented "#!") #_read-comment
    \_ (not-implemented "#_") #_read-discard
    nil))

(defn- read-dispatch
  [rdr _]
  (if-let [ch (types/read-char rdr)]
    (if-let [dm (dispatch-macros ch)]
      (dm rdr ch)
      (types/reader-error rdr "No dispatch macro for " ch)
      #_(if-let [obj (read-tagged (doto rdr (unread ch)) ch)]
          obj
          (types/reader-error rdr "No dispatch macro for " ch)))
    (types/reader-error rdr "EOF while reading character")))

(defn- macros [ch]
  (case ch
    \" (not-implemented \") #_read-string*
    \: (not-implemented \:) #_read-keyword
    \; (not-implemented \;) #_read-comment
    \' (not-implemented \') #_(wrapping-reader 'quote)
    \@ (not-implemented \@) #_(wrapping-reader 'clojure.core/deref)
    \^ (not-implemented \^) #_read-meta
    \` (not-implemented \`) #_read-syntax-quote ;;(wrapping-reader 'syntax-quote)
    \~ (not-implemented \~) #_read-unquote
    \( (not-implemented \() #_read-list
    \) (not-implemented \)) #_read-unmatched-delimiter
    \[ (not-implemented \[) #_read-vector
    \] (not-implemented \]) #_read-unmatched-delimiter
    \{ (not-implemented \{) #_read-map
    \} (not-implemented \}) #_read-unmatched-delimiter
    \\ (not-implemented \\) #_read-char*
    \% (not-implemented \%) #_read-arg
    \# read-dispatch
    nil))

(defn parse
  ([reader] (parse reader true))
  ([reader eof-error?]
     (lazy-seq
       (try
         (let [ch (types/read-char reader)]
           (cond (nil? ch)
                 #_=> (if eof-error? (types/reader-error reader "EOF") nil)
                 (whitespace? ch)
                 #_=> (cons (parse-whitespaces reader ch) (parse reader eof-error?))
                 (commons/number-literal? reader ch)
                 #_=> (list {:type :number :number (#'r/read-number reader ch)})
                 (utils/comment-prefix? ch)
                 #_=> (cons (parse-comment reader ch) (parse reader eof-error?))
                 :else
                 #_=> (let [f (macros ch)]
                        (-> (if f
                              (let [res (f reader ch)]
                                (if (identical? res reader)
                                  (parse reader eof-error?)
                                  res))
                              {:type :symbol :symbol (#'r/read-symbol reader ch)})
                            list))))
         (catch Exception e
           (if (utils/ex-info? e)
             (throw e)
             (throw (ex-info (.getMessage e)
                             (merge {:type :reader-exception}
                                    (if (types/indexing-reader? reader)
                                      {:line (types/get-line-number reader)
                                       :column (types/get-column-number reader)}))
                             e))))))))

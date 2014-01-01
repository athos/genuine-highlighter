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

(declare dispatch-macros macros parse)

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

(defn- parse-delimited [delim rdr recursive?]
  (let [first-line (when (types/indexing-reader? rdr)
                     (types/get-line-number rdr))
        delim (utils/char delim)]
    (loop [a []]
      (let [ch (types/read-char rdr)]
        (cond (nil? ch)
              #_=> (types/reader-error
                     rdr
                     "EOF while reading"
                     (when first-line
                       (str ", starting at line " first-line)))
              (identical? delim (utils/char ch))
              #_=> a
              (whitespace? ch)
              #_=> (recur (conj a (parse-whitespaces rdr ch)))
              (macros ch)
              #_=> (let [macrofn (macros ch)
                         mret (types/log-source-unread rdr (macrofn rdr ch))]
                     (recur (if-not (identical? mret rdr) (conj a mret) a)))
              :else
              #_=> (let [o (doall (parse (doto rdr (types/unread ch)) true))]
                     (recur (if-not (identical? o rdr) (into a o) a))))))))

(defn- parse-list
  [rdr _]
  (let [[start-line start-column]
        (when (types/indexing-reader? rdr)
          [(types/get-line-number rdr) (int (dec (types/get-column-number rdr)))])
        the-list (parse-delimited \) rdr true)
        [end-line end-column]
        (when (types/indexing-reader? rdr)
          [(types/get-line-number rdr) (int (types/get-column-number rdr))])]
    (with-meta {:type :list, :nodes the-list}
      (when start-line
        {:line start-line
         :column start-column
         :end-line end-line
         :end-column end-column}))))

(defn- parse-vector
  [rdr _]
  (let [[start-line start-column]
        (when (types/indexing-reader? rdr)
          [(types/get-line-number rdr) (int (dec (types/get-column-number rdr)))])
        the-vector (parse-delimited \] rdr true)
        [end-line end-column]
        (when (types/indexing-reader? rdr)
          [(types/get-line-number rdr) (int (types/get-column-number rdr))])]
    (with-meta {:type :vector, :nodes the-vector}
      (when start-line
        {:line start-line
         :column start-column
         :end-line end-line
         :end-column end-column}))))

(defn- parse-map
  [rdr _]
  (let [[start-line start-column]
        (when (types/indexing-reader? rdr)
          [(types/get-line-number rdr) (int (dec (types/get-column-number rdr)))])
        the-map (parse-delimited \} rdr true)
        ;map-count (count the-map)
        [end-line end-column]
        (when (types/indexing-reader? rdr)
          [(types/get-line-number rdr) (int (dec (types/get-column-number rdr)))])]
    #_(when (odd? map-count)
      (types/reader-error rdr "Map literal must contain an even number of forms"))
    (with-meta {:type :map, :nodes the-map}
      (when start-line
        {:line start-line
         :column start-column
         :end-line end-line
         :end-column end-column}))))

(defn- parse-set [rdr _]
  {:type :set, :nodes (parse-delimited \} rdr true)})

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
    \( parse-list
    \) (not-implemented \)) #_read-unmatched-delimiter
    \[ parse-vector
    \] (not-implemented \]) #_read-unmatched-delimiter
    \{ parse-map
    \} (not-implemented \}) #_read-unmatched-delimiter
    \\ (not-implemented \\) #_read-char*
    \% (not-implemented \%) #_read-arg
    \# read-dispatch
    nil))

(defn- dispatch-macros [ch]
  (case ch
    \^ (not-implemented "#^") #_read-meta
    \' (not-implemented "#'") #_(wrapping-reader 'var)
    \( (not-implemented "#(") #_read-fn
    \= (not-implemented "#=") #_read-eval
    \{ parse-set
    \< (not-implemented "#<") #_(throwing-reader "Unreadable form")
    \" (not-implemented "#\"") #_read-regex
    \! (not-implemented "#!") #_read-comment
    \_ (not-implemented "#_") #_read-discard
    nil))

(def new-id
  (let [n (atom 0)]
    (fn []
      (swap! n inc)
      @n)))

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
                              {:type :symbol
                               :symbol (#'r/read-symbol reader ch)
                               :id (new-id)})
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

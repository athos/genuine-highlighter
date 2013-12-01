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
                 #_=> (let [f (#'r/macros ch)]
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

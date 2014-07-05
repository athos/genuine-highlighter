(ns leiningen.highlight
  (:require [clojure.main :as m]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.browse :as browse]
            [genuine-highlighter.core :as hl]
            [genuine-highlighter.parser :as p]
            [genuine-highlighter.analyzer :as a]
            [genuine-highlighter.renderer :as r]
            [genuine-highlighter.converter :as c]
            [genuine-highlighter.decoration-rules [terminal :as t]
                                                  [html :as html]]
            [compojure [core :refer [defroutes GET]]
                       [route :as route]]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.reload :refer [wrap-reload]]
            [hiccup [page :as hiccup]]))

(defn- drop-proceeding-newlines [s]
  (clojure.string/replace s #"^\n+" ""))

(defn- read-and-highlight [^java.io.BufferedReader in]
  (loop [code ""]
    (let [line (.readLine in)
          code' (str code \newline line)]
      (if-let [result (hl/highlight t/colorful-symbols-rule code')]
        (let [result (->> (clojure.string/split result #"\n")
                          (drop-while #(= "" %))
                          (map #(str "  #_== " %))
                          (clojure.string/join \newline))]
          (println result)
          (flush)
          (read-string code'))
        (do (print "  #_=> ")
            (flush)
            (recur code'))))))

(defn- html-template [body]
  (hiccup/html5
    [:head
      [:link {:href "css/highlight.css", :rel "stylesheet", :type "text/css"}]]
    [:body
      [:pre
        body]]))

(defroutes routes
  (GET "/:nsname" [nsname]
    (let [filename (-> (#'clojure.core/root-resource nsname)
                       (str/replace #"^/" "")
                       (str ".clj"))]
      (->> (hl/highlight html/colorful-symbols-rule (slurp filename))
           html-template)))
  (route/resources "/"))

(def app
  (-> routes
      (wrap-reload '[leiningen.highlight])))

(defn- browse-highlighted-code [filename]
  (if filename
    (try
      (let [port (Integer/parseInt (get (System/getenv) "PORT" "8080"))
            server (jetty/run-jetty app {:port port, :join? false})]
        (browse/browse-url (str "http://localhost:" port "/" (str/replace filename #"/" "%2f")))
        (.join server)))
    (throw (Exception. "specify namespace to be highlighted"))))

(defn highlight
  "Highlight Clojure source code."
  ([project] (highlight project "repl"))
  ([project command & args]
     (case command
       "browse"
       #_=> (browse-highlighted-code (first args))
       "repl"
       #_=> (m/repl :read (fn [_ _] (read-and-highlight (io/reader *in*)))
                    :need-prompt #(do true))
       #_else (throw (Exception. (str "unknown subcommand: " command))))))

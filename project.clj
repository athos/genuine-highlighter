(defproject genuine-highlighter "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojars.trptcolin/sjacket "0.1.4" :exclusions [[org.clojure/clojure]]]
                 ;; for lein highlight browse functionality
                 [compojure "1.1.8"]
                 [hiccup "1.0.5"]]
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.4"]]
                   :source-paths ["src" "dev" "examples"]}})

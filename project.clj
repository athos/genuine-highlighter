(defproject genuine-highlighter "0.1.0-SNAPSHOT"
  :description "a macro-aware syntax highlighter for Clojure"
  :url "https://github.com/athos/genuine-highlighter"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [symbol-analyzer "0.1.1"]
                 [org.clojars.trptcolin/sjacket "0.1.4" :exclusions [[org.clojure/clojure]]]
                 [org.clojure/core.match "0.2.2"]]
  :profiles {:1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}
             :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :dev {:dependencies [[org.clojure/tools.namespace "0.2.10"]]
                   :source-paths ["src" "dev"]}}
  :aliases {"all" ["with-profile" "dev:1.5:1.6"]}
  :global-vars {*warn-on-reflection* true})

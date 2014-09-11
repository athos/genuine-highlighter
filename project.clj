(defproject genuine-highlighter "0.1.0-SNAPSHOT"
  :description "a macro-aware syntax highlighter for Clojure"
  :url "https://github.com/athos/genuine-highlighter"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojars.trptcolin/sjacket "0.1.4" :exclusions [[org.clojure/clojure]]]
                 [org.clojure/core.match "0.2.2"]
                 ;; for lein highlight browse functionality
                 [compojure "1.1.9"]
                 [ring/ring-jetty-adapter "1.3.1"]
                 [ring/ring-devel "1.3.1"]
                 [hiccup "1.0.5"]]
  :resource-paths ["resources"]
  :profiles {:1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}
             :dev {:dependencies [[org.clojure/tools.namespace "0.2.4"]]
                   :source-paths ["src" "dev" "examples"]}}
  :aliases {"all" ["with-profile" "dev:1.5"]}
  :global-vars {*warn-on-reflection* true})

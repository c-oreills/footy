(defproject footy "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [hiccups "0.1.1"]]
  :plugins [[lein-cljsbuild "0.3.2"]]
  :cljsbuild
  {:builds
   [{:source-paths ["src"],
     :compiler
     {:pretty-print true,
      :output-to "out/main.js",
      :optimizations :whitespace}}]})

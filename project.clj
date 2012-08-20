(defproject footy "1.0.0-SNAPSHOT"
            :dependencies [[org.clojure/clojure "1.4.0"]]
            :plugins [[lein-cljsbuild "0.2.5"]]
            :cljsbuild {
                        :builds [{:source-path "src"
                                  :compiler {:output-to "resources/public/js/main.js"
                                             :optimizations :whitespace
                                             :pretty-print true}}]})

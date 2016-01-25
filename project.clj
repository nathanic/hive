(defproject hive "0.1.0-SNAPSHOT"
  :description "Hive with People on the Internet"
  :url "https://github.com/nathanic/hive"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.analyzer "0.6.7"]
                 [org.clojure/tools.analyzer.jvm "0.6.9"]
                 ;; [org.clojure/core.async "0.2.374" :exclusions [org.clojure/tools.analyzer.jvm]]
                 [ring-server "0.4.0"]
                 [reagent "0.5.1" :exclusions [org.clojure/tools.reader]]
                 [reagent-forms "0.5.13"]
                 [reagent-utils "0.1.7"]
                 [ring "1.4.0"]
                 [ring/ring-defaults "0.1.5"]
                 [compojure "1.4.0" :exclusions [instaparse]]
                 [hiccup "1.0.5"]
                 [environ "1.0.1"]
                 [org.clojure/clojurescript "1.7.228" :scope "provided"]
                 [secretary "1.2.3"]
                 [venantius/accountant "0.1.6" :exclusions [org.clojure/tools.reader]]
                 ;; [org.clojure/core.typed "0.2.87"]
                 ;; [swiss-arrows "1.0.0"] ; might be unused
                 [aysylu/loom "0.5.4"] ; 0.5.0
                 [quil "2.3.0"]
                 [org.clojure/core.match "0.2.2" :exclusions [tools.analyzer.jvm]]
                 ;; [org.clojure/core.logic "0.8.10" :exclusions [tools.analyzer]] ; possibly unused
                 [org.clojure/tools.logging "0.3.1"]
                 ]

  :plugins [[lein-environ "1.0.1"]
            [lein-cljsbuild "1.1.1"]
            [lein-asset-minifier "0.2.4"
             :exclusions [org.clojure/clojure]]
]

  :ring {:handler hive.handler/app
         :uberwar-name "hive.war"}

  :min-lein-version "2.5.3"

  :uberjar-name "hive.jar"

  :main hive.server

  :clean-targets ^{:protect false} [:target-path
                                    [:cljsbuild :builds :app :compiler :output-dir]
                                    [:cljsbuild :builds :app :compiler :output-to]]

  :source-paths ["src/clj" "src/cljc"]
  :resource-paths ["resources" "target/cljsbuild"]

  :minify-assets
  {:assets
   {"resources/public/css/site.min.css" "resources/public/css/site.css"}}

  :cljsbuild {:builds {:app {:source-paths ["src/cljs" "src/cljc"]
                             :compiler {:output-to "target/cljsbuild/public/js/app.js"
                                        :output-dir "target/cljsbuild/public/js/out"
                                        :asset-path   "js/out"
                                        :optimizations :none
                                        :pretty-print  true}}}}


  :profiles {:dev {:repl-options {:init-ns hive.gui}

                   :dependencies [[ring/ring-mock "0.3.0"]
                                  [ring/ring-devel "1.4.0"]
                                  [prone "1.0.1"]
                                  [lein-figwheel "0.5.0-4"
                                   :exclusions [org.clojure/core.memoize
                                                ring/ring-core
                                                org.clojure/clojure
                                                org.ow2.asm/asm-all
                                                commons-codec
                                                org.clojure/data.priority-map
                                                org.clojure/tools.reader
                                                org.clojure/clojurescript
                                                org.clojure/core.async
                                                org.clojure/tools.analyzer.jvm]]
                                  [org.clojure/clojurescript "1.7.228"
                                   :exclusions [org.clojure/clojure org.clojure/tools.reader]]
                                  [org.clojure/tools.nrepl "0.2.12"]
                                  [com.cemerick/piggieback "0.2.1"]
                                  [pjstadig/humane-test-output "0.7.1"]
                                  [midje "1.6.3"]
                                  ]

                   :source-paths ["env/dev/clj"]
                   :test-paths ["test/clj"]
                   :plugins [[lein-figwheel "0.5.0-4"
                              :exclusions [org.clojure/core.memoize
                                           ring/ring-core
                                           org.clojure/clojure
                                           org.ow2.asm/asm-all
                                           commons-codec
                                           org.clojure/data.priority-map
                                           org.clojure/tools.reader
                                           org.clojure/clojurescript
                                           org.clojure/core.async
                                           org.clojure/tools.analyzer.jvm]]
                             [org.clojure/clojurescript "1.7.170"]
                             [lein-midje "3.2"]
                             ]

                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]

                   :figwheel {:http-server-root "public"
                              :server-port 3449
                              :nrepl-port 7002
                              :nrepl-middleware ["cemerick.piggieback/wrap-cljs-repl"
                                                 ]
                              :css-dirs ["resources/public/css"]
                              :ring-handler hive.handler/app}

                   :env {:dev true}

                   :cljsbuild {:builds {:app {:source-paths ["env/dev/cljs"]
                                              :compiler {:main "hive.dev"
                                                         :source-map true}}
                                        }}}

             :uberjar {:hooks [minify-assets.plugin/hooks]
                       :source-paths ["env/prod/clj"]
                       :prep-tasks ["compile" ["cljsbuild" "once"]]
                       :env {:production true}
                       :aot :all
                       :omit-source true
                       :cljsbuild {:jar true
                                   :builds {:app
                                            {:source-paths ["env/prod/cljs"]
                                             :compiler
                                             {:optimizations :advanced
                                              :pretty-print false}}}}}})


(defproject hive "0.1.0-SNAPSHOT"
  :description "Hive Game Computations"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.typed "0.2.87"]
                 [swiss-arrows "1.0.0"]
                 [aysylu/loom "0.5.0"]
                 [quil "2.2.5"]
                 [org.clojure/core.match "0.2.2"]
                 [org.clojure/core.logic "0.8.10"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/tools.logging "0.3.1"]
                 ]
  :profiles  {:dev  {:dependencies  [[midje "1.6.3"]]}}
  :main hive.gui
  )

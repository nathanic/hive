(ns hive.handler
  (:require [clojure.java.io :as io]
            [compojure.core :refer [GET defroutes]]
            [compojure.route :refer [not-found resources]]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [include-js include-css]]
            [hive.middleware :refer [wrap-middleware]]
            [cognitect.transit :as transit]
            [environ.core :refer [env]])
  (:import [java.io.ByteArrayOutputStream])
  )

; probably move this to util
(defn write-json [v]
  (let [os (new java.io.ByteArrayOutputStream)]
    (transit/write (transit/writer os :json) v)
    (str os)))

(def mount-target
  [:div#app
      [:h3 "ClojureScript has not been compiled!"]
      [:p "please run "
       [:b "lein figwheel"]
       " in order to start the compiler"]])

(def loading-page
  (html
   [:html
    [:head
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport"
             :content "width=device-width, initial-scale=1"}]
     (include-css (if (env :dev) "css/site.css" "css/site.min.css"))]
    [:body
     mount-target
     (include-js "js/app.js")]]))


(defroutes routes
  (GET "/" [] loading-page)
  (GET "/about" [] loading-page)
  (GET "/games" []
       (write-json [{:title "a test game"}
                    {:title "another test game"}
                    ]))

  (resources "/")
  (not-found "Not Found"))

(def app (wrap-middleware #'routes))

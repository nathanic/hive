(ns hive.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r :refer [atom]]
            [reagent.session :as session]
            [cljs.core.async :as async :refer [<!]]
            [cljs-http.client :as http]
            [cognitect.transit :as transit]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]))

; NEXT GOAL
; show a list of open games
; make a 'start new game' button

; oh javascript
(def json-reader (transit/reader :json))
(defn read-json [s] 
  (transit/read json-reader s))

(def games* (r/atom []))

;; -------------------------
;; Views

(defn game-summary [game]
  [:li
   [:span (:title game)]
   ])

(defn game-list []
  [:div 
   [:span "Open Games"] 
   [:ul (for [g @games*]
          (game-summary g))]])

(defn home-page []
  [:div [:h2 "Welcome to hive"]
   (game-list)
   [:div [:a {:href "/about"} "go to about page"]]])

(defn about-page []
  [:div [:h2 "About hive"]
   [:div [:a {:href "/"} "go to the home page"]]])

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page #'home-page)
  
  )

(secretary/defroute "/about" []
  (session/put! :current-page #'about-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (println "init!")
  (go (reset! games* ;[{:title "foo"} {:title "bar"}] 
              (-> (http/get "/games")
                  <!
                  :body
                  read-json
                  )))
  (accountant/configure-navigation!)
  (accountant/dispatch-current!)
  (mount-root))

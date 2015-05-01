(ns hive.game-server
  (:require [hive.game :as game]
            [clojure.string :as string]
            [clojure.core.match :refer [match]]
            [clojure.core.typed :as t
             :refer [ann U Vec Map Set Seq Option Keyword Any All]]
            ))

; temporary game state "persistence"
; hash map of game ids to game structures
; for short term could just serialize to edn file
(defonce games* (atom {}))

; request game state from [fake] server
(defn get-game [id]
  (get @games* id))

(defn create-game []
  (let [g (game/new-game)]
    (swap! games* assoc (:id g) g)
    g)) ; 201 created

(defn send-move
  "simulates posting a move request to the game server"
  [game-id {:keys [piece position], :as move}]
  (println "Recevied move for game-id " game-id ": " move)
  (if-let [g (get-game game-id)]
    (let [{:keys [board possible-moves]} g]
      (if (game/valid-move? g move)
        ; move is valid, modify board state
        (let [g' (game/apply-move g move)]
          (swap! games* assoc game-id g')
          g') ; 200 OK
        ; move is invalid, 400 bad request
        (do (println "rejecting invalid move: " move)
            nil)))
    ; 404 game not found
    (do
      (println "game" game-id "not found!")
      nil)))

(comment
  (def names (-> "/home/nathan/tmp/at-midnight-guests-2015-04-24"
                 slurp
                 (string/split-lines)
                 ))

  (->> names
      frequencies
      ;; seq
      (sort-by second)
      reverse
      clojure.pprint/pprint
    )
  (count (frequencies names))
  (->> names
       frequencies
       seq
       (filter (fn [[k v]] (= v 1)))
       count
       )


  )

(ns hive.t-game
  (:require [midje.sweet :refer :all])
  (:require [hive.game :as fixture]))

; assert various game state stuff

; super lame one to get me started
(fact "a new game doesn't have any moves"
      (:moves (fixture/new-game)) => []
      )


(facts  "we can keep track of a whole 2 players"
       (fact "opposites"
             (fixture/opposing-player :white) => :black
             (fixture/opposing-player :black) => :white
             (fixture/opposing-player :wumpus) => nil)
       )

(facts "we understand pieces"
       (fact "piece->player works"
             (fixture/piece->player :bA2) => :black
             (fixture/piece->player :wQ) => :white
             (fixture/piece->player :MALFORMED) => nil)

       (fact "we can identify species"
             (fixture/piece->species :bA2) => :ant
             (fixture/piece->species :bB2) => :beetle
             (fixture/piece->species :wG3) => :grasshopper
             (fixture/piece->species :bQ) => :queen-bee
             (fixture/piece->species :bL) => :ladybug
             (fixture/piece->species :wM) => :mosquito
             (fixture/piece->species :wP) => :pillbug
             (fixture/piece->species :wS2) => :spider
             (fixture/piece->species :xxxBROKEN) => nil
             )
       )

; set up some example boards
; and assert about valid moves

(def BOARD-WHITE-QUEEN-ONLY
  {[1 1] [:wQ]})

(def BOARD-SPIDERS-AND_QUEENS
  ; wQ wS bS
  ;          bQ
  {[1 1] [:wS1] 
   [0 1] [:wQ]
   [2 1] [:bS1]
   [2 2] [:bQ]
   })

(facts "board calculations"
       (fact "spawn points"
             ; yay this one found a bug!
             (fixture/spawn-positions BOARD-WHITE-QUEEN-ONLY :black) 
             => #{[0 1] [1 0] [2 0] [2 1] [1 2] [0 2]} ; all neighbors of queen

             (fixture/spawn-positions BOARD-SPIDERS-AND_QUEENS :white)
             => #{[1 0] [0 0] [-1 1] [-1 2] [0 2]}
             )
       )

(facts 
  "move calculations"
  (fact "queen moves"
        ; the white queen 
        (fixture/calculate-move-positions {:board BOARD-SPIDERS-AND_QUEENS} [0 1])
        => #{[0 2] [1 0]}
        ; the black queen
        (fixture/calculate-move-positions {:board BOARD-SPIDERS-AND_QUEENS} [2 2])
        => #{[1 2] [3 1]}
        ))

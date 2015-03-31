(ns hive.game
  (:use [loom graph alg attr])
  (:require [hive.hex-grid :as grid]
            [clojure.core.typed :as t
             :refer [defalias ann U Vec Map Seq]]
            ))

(def PIECES #{:bA1 :bA2 :bA3
              :bB1 :bB2
              :bG1 :bG2 :bG3
              :bL
              :bM
              :bP
              :bQ
              :bS1 :bS2
              :wA1 :wA2 :wA3
              :ww1 :ww2
              :wG1 :wG2 :wG3
              :wL
              :wM
              :wP
              :wQ
              :wS1 :wS2})

(defalias Piece ; is there a way to be DRY about this?
  (U ':bA1 ':bA2 ':bA3
     ':bB1 ':bB2
     ':bG1 ':bG2 ':bG3
     ':bL
     ':bM
     ':bP
     ':bQ
     ':bS1 ':bS2
     ':wA1 ':wA2 ':wA3
     ':ww1 ':ww2
     ':wG1 ':wG2 ':wG3
     ':wL
     ':wM
     ':wP
     ':wQ
     ':wS1 ':wS2
     ))

(defalias Species "a species-identifier keyword"
  (U ':ant ':beetle ':hopper ':ladybug ':mosquito ':pillbug ':queen-bee ':spider))

(defalias Player "a player-identifier keyword"
  (U ':white ':black))

(defalias Board
  (Map grid/AxialPoint Piece))

(ann LETTER->SPECIES (Map Character Species))
(def LETTER->SPECIES
  {\A :ant
   \B :beetle
   \G :hopper
   \L :ladybug
   \M :mosquito
   \P :pillbug
   \Q :queen-bee
   \S :spider})

(ann piece->species [Piece -> (U Species nil)])
(defn piece->species [piece]
  (->> piece name second LETTER->SPECIES))

(ann piece->player [Piece -> (U Player nil)])
(defn piece->player [piece]
  (->> piece name first {\w :white, \b :black}))

(comment
  (piece->species :wQ)
  (piece->player :wQ)
  (def board {:bQ  {:e  :bG1
                    :se :bG2 }
              :bG1 {:w  :bQ
                    :sw :bG2
                    :se :wA1 }
              :bG2 {:nw :bQ
                    :ne :bG1
                    :e  :wA1 }
              :wA1 {:nw :bG1
                    :w  :bG2
                    :se :bA1 }
              :bA1 {:nw :wA1 }
              })

  (def g (graph {:bQ [:bG1 :bG2],
                 :bG1 [:bQ :bG2]
                 :bG2 [:bQ :bG1 :wA1],
                 :wA1 [:bG2 :bA1]
                 :bA1 [:wA1]
                 }))
  (connected? g)
  (connected? (-> g (add-nodes :wQ)))
  (connected? (-> g (remove-nodes :bQ)))
  (connected? (-> g (remove-nodes :bG1)))
  (connected? (-> g (remove-nodes :bG2)))

  (free-pieces g)

  (successors g :wA1)
  (successors g :bG2)
  (prn g)
  (def q (graph :foo {:pos [0 0]}))
  (successors q :foo)
  (successors q :pos)
  (attr q :foo :pos)

  ; adding edges automatically creates nodes
  (-> (graph)
      (add-edges [:a :b] [:b :c])
      (nodes))

  ; however simply adding an attribute does not create the nodes
  (-> (graph)
      (add-attr [:a :b] :color :purple)
      (nodes))
  )

(ann occupied? [Board grid/AxialPoint -> bool])
(defn occupied?
  "query whether the given pq coordinates are occupied on the given board"
  [board pq]
  (contains? board pq))

(ann unoccupied? [Board grid/AxialPoint -> bool])
(def unoccupied?
  (comp not occupied?))

(ann occupied-neighbors [Board grid/AxialPoint -> '['[grid/AxialPoint Piece]]])
(defn occupied-neighbors
  "query immediate neighbors of this piece that are themselves occupied by pieces"
  [board pq]
  (for [nabe-pq (grid/neighbors pq)
        :when (occupied? board nabe-pq) ]
    [nabe-pq (get board nabe-pq)]))

(ann planar-passable? [Board grid/AxialPoint grid/AxialPoint -> bool])
(defn planar-passable?
  "determine if two adjacent positions are planar-passable (not gated)"
  [board from to]
  (let [board' (dissoc board from)]
    (and
      (unoccupied? board to)
      (some (partial occupied? board')   (grid/neighbors to))
      (some (partial unoccupied? board') (grid/gate-positions from to)))))

(defn piece-is-free?
  "given a graph of a Hive board, and a piece name keyword,
  decide if the piece is free to move."
  [board-graph piece]
  (connected? (remove-nodes board-graph piece)))

(comment
  (graph)
(def board
    {[4 5] :bA1, [2 3] :bQ, [3 5] :wQ, [3 4] :wS1, [2 5] :wA1, [2 4] :bA2}
    )
  (def bg (board->graph board))
  (prn bg)
  (connected? (remove-nodes bg :wQ))

  (prn hive.gui/board)
  (piece-is-free? bg :wQ)
  (piece-is-free? bg :bA)
  (free-pieces bg)
  (calculate-moves board [3 4])
  )

(defn free-pieces
  "return a list of names of free pieces in the given board graph"
  [board-graph]
  (filter (partial piece-is-free? board-graph) (nodes board-graph)))

; build a graph from the above map
; coords as node attributes
  ; compute coords from directions as we go
; dirs as edge attributes
#_(defn board->graph [board]
  ; fold over pieces/nodes
  (reduce (fn [g piece]
            ; fold over this piece's neighbors
            (reduce (fn [g [dir nabe-piece]]
                      (-> g
                          (add-edges [piece nabe-piece] )
                          (add-attr piece nabe-piece :dir dir )
                          (add-attr nabe-piece :pos (grid/neighbor (attr g piece :pos) dir))))
                    g
                    (get board piece)))
          ; prime the graph with the first node and a position at the origin
          (-> (graph (first (keys board)))
              (add-attr (first (keys board)) :pos [0 0]))
          (keys board)))
; i can't believe the above worked the first time!


; board is (Map '[Long Long] Keyword)
#_(defn board->graph [board]
  (reduce
    (fn [g [pq piece]]
      (assoc g
              piece
              (for [neighbor (grid/neighbors pq)
                    :when (occupied? board neighbor) ]
                (get board neighbor))))
    {}
    board))

(ann board->graph [Board -> loom.graph.BasicEditableGraph])
(defn board->graph [board]
  ; fold board structure into graph
  (reduce
    (fn [g [pq piece]]
      ; fold occupied neighbors of this piece into graph as edges
      (reduce
        (fn [g nabe-piece]
          (add-edges g [piece nabe-piece]))
        g
        (for [nabe-pq (grid/neighbors pq)
              :when (occupied? board nabe-pq) ]
          (get board nabe-pq))))
    (graph)
    board))

(comment
  ; about half a millisecond for small board
  ; probably not going to worry about caching this
  (time (board->graph b))

  (def gg (board->graph board))
  (def gg2 (board->graph board))
  (attr gg :bQ :bG2 :dir) ;=> :se
  (attr gg :bQ :pos)
  (attr gg :bG1 :pos)
  (attr gg :bA1 :pos)
  (attr gg :wA1 :pos)
  )

#_(defn graph->board [g]
  ; for each node
    ; create a node in the hashmap
      ; whose value is a hash map
      ; with keys that are the :dir attribute of the edges between this node and (successors g node)
      ; also :pos key
  )

; okay, maybe we have dual data structures
; one view is a map [p q] -> piece keyword
; another view is a [labeled] loom graph
; should be isomorphic...
; then again, do we really need map view?
; the maximal hive board is still quite a small N
; could just brute force search it...
; yeah, graph data structure will be primary
; secondary to that we can either brute force it or build maps from it as needed
; would also probably be handy to be able to build it from a map...


; move calculations
  ; figure out the set of all possible move destinations
  ; first check if piece is free.  if it's not free, don't bother calculating moves.
  ; first remove the piece to be moved from consideration of the game graph
  ; never allow a moveset to contain the starting position of the piece
    ; as that's not really a "move"
  ; concept: a "planar passable" connection is when you have two neighboring cells
    ; such that one has your piece-to-move
    ; and the other is unoccupied but has at least one occupied neighbor
    ; and their border is not gated
  ; a "gate" is when the connection between two cells (A and B) is occupied on both sides
    ; that is, the two hexes with vertices pointing onto the common border segment shared by A and B
    ; are both occupied at the current level (z-index)

; queen moves
  ; return all planar passable immediate neighbors


(ann allowed-moves [Board grid/AxialPoint -> (Set grid/AxialPoint)])
(defmulti allowed-moves
  "internal helper multimethod; you probably want to call calculate-moves instead"
  (fn [board pq] (piece->species (get board pq))))

; don't know what it is -> it gets no moves
(defmethod allowed-moves :default
  [_ _]
  #{})

; board data structure is a map from pq coords to pieces
; might do this as a multimethod
(defmethod allowed-moves :queen-bee
  [board pq]
  (assert (#{:wQ :bQ} (get board pq)))
  (set
    (for [neighbor-pq (grid/neighbors pq)
          :when (planar-passable? board pq neighbor-pq) ]
      neighbor-pq)))

; need unit tests for all of these

(comment
  (def b {[0 0] :wQ
          [0 1] :wS1
          [0 2] :bS1
          [0 3] :bQ
          [1 2] :bA1
          ;; [1 3] :bB1
          })
  (allowed-moves b [0 3])
  (allowed-moves b [0 0])
  (def pq [0 3])
  (filter #(planar-passable? b pq %) (grid/neighbors pq))
  )


; spider moves
  ; from the starting position
  ; find all planar passable neighbors
  ; find all planar passable 2nd order neighbors without reversing course
    ; that is, the planar passable neighbors of the planar passable neighbors
    ; except when you'd go back where you came from
  ; return all planar passable 3rd order neighbors without reversing course
(defmethod allowed-moves :spider
  [board pq]
  ;; (assert (#{:wS1 :wS2 :bS1 :bS2} (get board pq)))
  (set
    (let [board (dissoc board pq)]
      (for [dir       grid/DIRS
            :let      [nabe-pq (grid/neighbor pq dir)]
            :when     (planar-passable? board pq nabe-pq)
            dir2      (remove #{(grid/opposite-direction dir)} grid/DIRS)
            :let      [nabe2-pq (grid/neighbor nabe-pq dir2)]
            :when     (planar-passable? board nabe-pq nabe2-pq)
            dir3      (remove #{(grid/opposite-direction dir2)} grid/DIRS)
            :let      [nabe3-pq (grid/neighbor nabe2-pq dir3)]
            :when     (planar-passable? board nabe2-pq nabe3-pq)
            ]
        nabe3-pq))))

(comment
  (grid/neighbor [3 4] :ne)
  (def board (:board @hive.gui/state*))
  (allowed-moves board [2 4])
  (for [x (range 3), x (range 4 6)] x)
  )
; grasshopper moves
  ; for each cardinal direction (of the 6)
    ; walk that direction on the grid
      ; if the immediate neighbor is not occupied, continue to next direction
      ; if occupied, keep walking until unoccupied point found and return that point

; ant moves
  ; for each occupied point
    ; add all of its unoccupied neighbors to a graph as a node
  ; for each point in the graph
    ; add edges to all passable neighbors
  ; return all points connected to starting position

; beetle moves
  ; accept all occupied immediate neighbor cells
  ; plus all planar passable neighbors
(defmethod allowed-moves :beetle
  [board pq]
  (println "calculating beatle moves" pq board)
  (set
    (for [neighbor-pq (grid/neighbors pq)
          :when (or (occupied? board neighbor-pq)
                    (planar-passable? board pq neighbor-pq)) ]
      neighbor-pq)))

; ladybug moves
  ; for each occupied neighbor
    ; for each occupied 2nd order neighbor (allowing backtracking)
      ; accumulate all unoccupied 3rd order neighbors
(defmethod allowed-moves :ladybug
  [board pq]
  (set
    (let [board (dissoc board pq)]
      (for [[nabe-pq _]  (occupied-neighbors board pq)
            [nabe2-pq _] (occupied-neighbors board nabe-pq)
            nabe3-pq (grid/neighbors nabe2-pq)
            :when    (unoccupied? board nabe3-pq)
            ]
        nabe3-pq))))

; mosquito moves
  ; if mosquito is atop another piece, use beetle rules
  ; else if only neighbor is mosquito, return empty set
  ; else union the movesets from this position using the rules for all neighboring pieces
    ; might need special attention for pillbugging as mosquito

; pillbug moves
  ; queen movement rules
  ; plus special logic for moving other pieces...
    ; can't have moved in last turn
    ; hive must remain fully connected after movement to pillbug's neighbor
    ; be aware of higher level gating
    ; maybe handle pillbug moves as simply enabling extra destinations for the pieces that might be moved
      ; that is, pillbug logic is invoked for EVERY piece movement
      ; if piece is adjacent to pillbug
        ; and has not moved in last turn

(ann calculate-moves [Board grid/AxialPoint -> (Set grid/AxialPoint)])
(defn calculate-moves
  "calculate the set of possible destination coordinates for the bug at a given position"
  [board pq]
  (let [bg (board->graph board)]
    (if (piece-is-free? bg (get board pq))
      (allowed-moves board pq)
      #{})))


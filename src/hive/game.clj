(ns hive.game
  (:use [loom graph alg attr]) ; TODO: requireize
  (:require [hive.hex-grid :as grid]
            [clojure.core.match :refer [match]]
            [clojure.core.typed :as t
             :refer [ann U Vec Map Set Seq Option Keyword Any All]]
            )
  (:import [loom.graph BasicEditableGraph])
  )
(comment
  ; need types for loom stuff in order to proceed on typing this
  ; add-edges
  ; graph
  ; connected?
  ; remove-nodes
  ; bf-traverse
  (t/check-ns)
  (macroexpand '(U ~@PIECES))
  (defmacro make-union [set-name type-name & elems]
    (let [symbols (for [elem elems]
                    (->> elem (str \') symbol)
                    ;; (-> elem str symbol)
                    )]
     `(do
       (def ~set-name #{~@elems})
       (t/defalias ~type-name
         (U ~@symbols))
       )))
  (macroexpand-1 '(make-union fooset footype :a :b :c))
  (make-union fooset FooType :a :b :c)
  (t/cf :a FooType)
  )

(ann add-edges [BasicEditableGraph '[Any Any] * -> BasicEditableGraph])
(ann graph [Any * -> BasicEditableGraph])
(ann connected? [BasicEditableGraph -> boolean])
(ann remove-nodes [BasicEditableGraph Any * -> BasicEditableGraph])
(ann bf-traverse [BasicEditableGraph Any -> (Seq Any)])

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

(t/defalias Piece ; is there a way to be DRY about this?
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

(t/defalias Species "a species-identifier keyword"
  (U ':ant ':beetle ':grasshopper ':ladybug ':mosquito ':pillbug ':queen-bee ':spider))

(t/defalias Player "a player-identifier keyword"
  (U ':white ':black))

; the board is a map from axial coordinates to ordered lists of pieces at that position
; the first item in a given position list is the bottommost, the last item the topmost
(t/defalias Board
  (Map grid/AxialPoint '[Piece]))

; it is also sometimes more convenient to build a Loom graph representation
(t/defalias BoardGraph BasicEditableGraph)

(ann LETTER->SPECIES (Map Character Species))
(def LETTER->SPECIES
  {\A :ant
   \B :beetle
   \G :grasshopper
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

(ann top-piece-at-pos [Board grid/AxialPoint -> (Option Piece)])
(defn top-piece-at-pos
  "get the topmost piece of the stack at the given axial coordinates.
  if no piece is found, returns nil."
  [board pq]
  (let [[piece & _] (get board pq)]
    piece))

(ann remove-top-piece-at-pos [Board grid/AxialPoint -> Board])
(defn remove-top-piece-at-pos [board pq]
  (if-let [pieces (get board pq)]
    (assoc board pq (rest pieces))
    board))

(ann spawn-piece [Board Piece grid/AxialPoint -> Board])
(defn spawn-piece [board piece pq]
  (update-in board [pq] conj piece))

(defn move-piece [board from-pq to-pq]
  ;; remove the piece from from-pq, move it over
  ;; do move-validation here?
  )

(ann occupied? [Board grid/AxialPoint -> boolean])
(defn occupied?
  "query whether the given pq coordinates are occupied on the given board"
  [board pq]
  (not-empty (get board pq)))

(ann unoccupied? [Board grid/AxialPoint -> boolean])
(def unoccupied?
  (comp not occupied?))

(ann occupied-neighbors [Board grid/AxialPoint -> '['[grid/AxialPoint Piece]]])
(defn occupied-neighbors
  "query immediate neighbors of this piece that are themselves occupied by pieces"
  [board pq]
  (t/for [nabe-pq :- grid/AxialPoint (grid/neighbors pq)
        :when (occupied? board nabe-pq) ]
    :- '['[grid/AxialPoint Piece]]
    [nabe-pq (top-piece-at-pos board nabe-pq)]))

(ann planar-passable? [Board grid/AxialPoint grid/AxialPoint -> boolean])
(defn planar-passable?
  "determine if two adjacent positions are planar-passable (not gated)"
  [board from to]
  (let [board' (dissoc board from)]
    (and
      (unoccupied? board to)
      (some (partial occupied? board')   (grid/neighbors to))
      ; one gate position MUST be filled, other MUST NOT be filled
      (let [[g1 g2] (grid/gate-positions from to)
            occ1    (unoccupied? board' g1)
            occ2    (unoccupied? board' g2)]
        (or (and occ1 (not occ2))
            (and (not occ1) occ2))))))

(ann piece-is-free? [BoardGraph Piece -> boolean])
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

(ann free-pieces [BoardGraph -> (Vec Piece)])
(defn free-pieces
  "return a list of names of free pieces in the given board graph"
  [board-graph]
  (filter (partial piece-is-free? board-graph) (nodes board-graph)))

(ann board->graph [Board -> BoardGraph])
(defn board->graph [board]
  ; fold board structure into graph
  (reduce
    (fn [g [pq [piece & _]]]
      ; fold occupied neighbors of this piece into graph as edges
      (reduce
        (fn [g nabe-piece]
          (add-edges g [piece nabe-piece]))
        g
        (t/for [nabe-pq :- grid/AxialPoint (grid/neighbors pq)
              :when (occupied? board nabe-pq) ]
          :- Piece
          (top-piece-at-pos board nabe-pq))))
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
  (fn [board pq] (piece->species (top-piece-at-pos board pq))))

; don't know what it is -> it gets no moves
(defmethod allowed-moves :default
  [_ _]
  #{})

(defmethod allowed-moves :queen-bee
  [board pq]
  (set
    (t/for [neighbor-pq :- grid/AxialPoint (grid/neighbors pq)
          :when (planar-passable? board pq neighbor-pq) ]
      :- grid/AxialPoint
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
  (set
    (let [board (dissoc board pq)]
      (for [dir   grid/DIRS
            :let  [nabe-pq (grid/neighbor pq dir)]
            :when (planar-passable? board pq nabe-pq)
            dir2  (remove #{(grid/opposite-direction dir)} grid/DIRS)
            :let  [nabe2-pq (grid/neighbor nabe-pq dir2)]
            :when (planar-passable? board nabe-pq nabe2-pq)
            dir3  (remove #{(grid/opposite-direction dir2)} grid/DIRS)
            :let  [nabe3-pq (grid/neighbor nabe2-pq dir3)]
            :when (planar-passable? board nabe2-pq nabe3-pq)
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
(defmethod allowed-moves :grasshopper
  [board pq]
  (set
    (for [dir grid/DIRS
          :let  [nabe-pq (grid/neighbor pq dir)]
          :when (occupied? board nabe-pq) ]
      ; scan in this direction until we see an unoccupied hex
      (some (fn [pq] (and (unoccupied? board pq) pq))
            (iterate (fn [pq] (grid/neighbor pq dir)) nabe-pq))
      )))

(comment
  (calculate-moves b [1 6])

  )
; ant moves
  ; for each occupied point
    ; add all of its unoccupied neighbors to a graph as a node
  ; for each point in the graph
    ; add edges to all passable neighbors
  ; return all points connected to starting position
(defmethod allowed-moves :ant
  [board pq]
  ; TODO TODO change me for to support stacks!
  (-> (let [board (remove-top-piece-at-pos board pq)
            empties (set
                      (for [[ppq piece] board
                            nabe-pq     (grid/neighbors ppq)
                            :when       (unoccupied? board nabe-pq) ]
                        nabe-pq))
            surround-graph (apply graph
                                  (for [pq1 empties
                                        pq2 empties
                                        ;; :let [_ (println "(planar-passable? board" pq1 pq2 ") =>"
                                                         ;; (planar-passable? board pq1 pq2) )]
                                        :when (and (not= pq1 pq2)
                                                   (grid/neighbors? pq1 pq2)
                                                   (planar-passable? board pq1 pq2)) ]
                                    [pq1 pq2])) ]
        (bf-traverse surround-graph pq)
        )
      set
      (disj pq) ; don't allow starting position
      ))

(comment
  (def board b)
  (def pq [0 7])
  (calculate-moves b [0 7])

  (swap! state* assoc :valid-moves (calculate-moves b [0 7]))
  (planar-passable? board [2 7] [3 6])

  (def g (graph [:a :b] [:b :c] [:d :e]))
  (bf-traverse g :a)
  (loom.io/view g)
  (def empties (set
                 (for [[ppq piece] (dissoc board pq)
                       nabe-pq     (grid/neighbors ppq)
                       :when       (unoccupied? (dissoc board pq) nabe-pq) ]
                   nabe-pq)))

  (def surround-graph (apply graph
                             (for [pq1 empties
                                   pq2 empties
                                   :when (and (not= pq1 pq2)
                                              (grid/neighbors? pq1 pq2)
                                              (planar-passable? board pq1 pq2))
                                   ]
                               (do
                                 (println "(planar-passable? board" pq1 pq2 ") =>"
                                                    (planar-passable? board pq1 pq2) )
                                 [pq1 pq2]))))
  (clojure.pprint/pprint surround-graph)
  (count (connected-components surround-graph))
  (grid/neighbors? [2 7] [3 6])
  (grid/neighbors? [2 7] [4 6])
  (planar-passable? (dissoc board pq) [0 7] [0 8])

  (swap! state* assoc :valid-moves (bf-traverse surround-graph pq))
  )
; beetle moves
  ; accept all occupied immediate neighbor cells
  ; plus all planar passable neighbors
(defmethod allowed-moves :beetle
  [board pq]
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
(defmethod allowed-moves :mosquito
  [board pq]
  (set
    ; TODO: check if we are atop anything, use beetle rules
    (let [board (dissoc board pq)]
      ; could be more efficient by not repeating neighbor species
      (for [[nabe-pq nabe-piece]  (occupied-neighbors board pq)
            :when (not= (piece->species nabe-piece) :mosquito)
            ]
        (allowed-moves
          ; pretend the mosquito is another piece
          (assoc board pq (-> nabe-piece name (str "MOSQUITO") keyword ))
          pq)))))

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
(defmethod allowed-moves :pillbug
  [board pq]
  ; movement of other pieces will be handled for those other pieces
  ; here we just calculate queen moves for the pillbug's own movement
  (set
    (for [neighbor-pq (grid/neighbors pq)
          :when (planar-passable? board pq neighbor-pq) ]
      neighbor-pq)))

; might need more state... last-moved piece for pillbug
(ann calculate-moves [Board grid/AxialPoint -> (Set grid/AxialPoint)])
(defn calculate-moves
  "calculate the set of possible destination coordinates for the bug at a given position"
  [board pq]
  ; TODO: pillbug checks
  (let [bg (board->graph board)]
    (if (piece-is-free? bg (top-piece-at-pos board pq))
      (allowed-moves board pq)
      #{})))

(comment
  ; how to reach into the state of the currently running applet
  (defn current-board!
    []
    (-> hive.gui/hive-applet
        (quil.applet/with-applet (quil.core/state-atom))
        deref
        :board
        ))
  (swap! state* assoc-in [:board [3 4]] :bQ)
  (swap! state* assoc-in [:board [2 6]] :bL)
  (do
    (def state* (quil.applet/with-applet hive.gui/hive-applet (quil.core/state-atom)))
    ; this board has shown me several move calc errors already...
    (def b {[3 4] :bQ
            [2 5] :bS1
            [2 6] :wS1
            [1 7] :wQ
            [1 6] :wG1
            [2 4] :bL
            [4 4] :bP
            [0 7] :wA1

            [4 5] :bA2
            [4 6] :bA3
            [3 7] :wS2

            ;; [1 5] :wM
            })
    (swap! state* assoc :board b)
    )
    (keys @state*)
    (clojure.set/rename-keys b)
    (into {} (map (fn [[[p q] v]] [[q p] v]) b))

    ; shift the pieces along the q-axis
    (swap! state* assoc :board
           (into {} (map (fn [[[p q] v]]
                           [[p (dec q)] v])
                         b)))

  )


; spawn positions
; must be empty hexes
; must be neighbors of $player color
; must not be neighbors of $opposing-player color
; for each friendly piece
  ; for each unoccupied neighbor hex of piece
    ; accum hex if this empty neighbor has no pieces of opposing color
(ann spawn-positions [Board Player -> (Vec grid/AxialPoint)])
(defn spawn-positions [board player]
  (set
    (for [; find all friendly pieces
          [pq piece] board
          :when      (= player (piece->player piece))
          ; find all empty neighbors of friendly pieces
          empty-pq   (grid/neighbors pq)
          ; require that those empty neighbors are themselves not neighbors with any unfriendly pieces
          :when      (and (unoccupied? board empty-pq)
                          (every? true?
                                  (map (fn [[_ nabe-piece]]
                                         (= player (piece->player nabe-piece)))
                                       (occupied-neighbors board empty-pq))))
          ]
      empty-pq)))

(comment
  (swap! state* assoc :valid-moves (spawn-positions (current-board!) :white))
  (swap! state* assoc :valid-moves (spawn-positions (current-board!) :black))
  )



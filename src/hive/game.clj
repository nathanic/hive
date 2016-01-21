(ns hive.game
  (:use [loom graph alg attr]) ; TODO: requireize
  (:require [hive.hex-grid :as grid]
            [clojure.core.match :refer [match]]
            [clojure.core.typed :as t
             :refer [ann U Vec Map Set Seq Option Keyword Any All]]
            )
  (:import [loom.graph BasicEditableGraph])
  )

; BUG BUG most instances of (dissoc board ...) are probably bugs
; now that we have stacks
; need to hunt them all down and fix them
; in some cases, change to remove-top-piece-at-pos

(comment
  (t/check-ns)
  )

; TODO graph type could probably be generalized
(ann add-edges [BasicEditableGraph '[Any Any] * -> BasicEditableGraph])
(ann graph [Any * -> BasicEditableGraph])
(ann connected? [BasicEditableGraph -> boolean])
(ann remove-nodes [BasicEditableGraph Any * -> BasicEditableGraph])
(ann bf-traverse [BasicEditableGraph Any -> (Seq Any)])

(t/defalias Point hive.hex-grid/AxialPoint)

(def PIECES #{:bA1 :bA2 :bA3
              :bB1 :bB2
              :bG1 :bG2 :bG3
              :bL
              :bM
              :bP
              :bQ
              :bS1 :bS2
              :wA1 :wA2 :wA3
              :wB1 :wB2
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
; the first item in a given position list is the topmost, the last item the bottommost
(t/defalias Board
  (Map Point '[Piece]))

(ann top-piece-of-stack [(Vec Piece) -> (Option Piece)])
(defn top-piece-of-stack
  [pieces]
  (first pieces))

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

(ann opposing-player [Player -> Player])
(defn opposing-player [player]
  ({:white :black, :black :white} player))

(ann piece->species [Piece -> (U Species nil)])
(defn piece->species [piece]
  (->> piece name second LETTER->SPECIES))

(ann piece->player [Piece -> (U Player nil)])
(defn piece->player [piece]
  (when-not (keyword? piece)
    (println "piece->player" piece (type piece)))
  (->> piece name first {\w :white, \b :black}))

(ann valid-piece? [Piece -> Boolean])
(defn valid-piece?
  "decide if a given keyword is a proper Piece"
  [p]
  (contains? PIECES p))

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

(ann top-piece-at-pos [Board Point -> (Option Piece)])
(defn top-piece-at-pos
  "get the topmost piece of the stack at the given axial coordinates.
  if no piece is found, returns nil."
  [board pq]
  (let [piece (top-piece-of-stack (get board pq))]
    (when-not (or (nil? piece) (keyword? piece))
      (println "non-keyword piece" piece "found at" pq "in board:\n" board))
    piece))

(ann remove-top-piece-at-pos [Board Point -> Board])
(defn remove-top-piece-at-pos [board pq]
  (if-let [pieces (get board pq)]
    (assoc board pq (rest pieces))
    board))

(ann spawn-piece [Board Piece Point -> Board])
(defn spawn-piece [board piece pq]
  (update-in board [pq] conj piece))

(defn move-piece [board from-pq to-pq]
  ;; remove the piece from from-pq, move it over
  ;; do move-validation here?
  )

(ann occupied? [Board Point -> boolean])
(defn occupied?
  "query whether the given pq coordinates are occupied on the given board"
  [board pq]
  (not-empty (get board pq)))

(ann unoccupied? [Board Point -> boolean])
(def unoccupied?
  (comp not occupied?))

(ann occupied-neighbors [Board Point -> (Seq '[Point Piece])])
(defn occupied-neighbors
  "query immediate neighbors of this piece that are themselves occupied by pieces"
  [board pq]
  (t/for [nabe-pq :- Point (grid/neighbors pq)
        :when (occupied? board nabe-pq) ]
    :- '['[Point Piece]]
    [nabe-pq (top-piece-at-pos board nabe-pq)]))

(ann position-is-stacked? [Board Point -> boolean])
(defn position-is-stacked? [board pq]
  (> 1 (count (get board pq))))

(ann planar-passable? [Board Point Point -> boolean])
(defn planar-passable?
  "determine if two adjacent positions are planar-passable (not gated)
  NOTE: only considers the bottommost plane! does not consider movement atop the hive"
  [board from to]
  (let [board' (dissoc board from)] ; BUG: should be remove-top-piece-at-pos ?
    (and
      (unoccupied? board to)
      (some (partial occupied? board')   (grid/neighbors to))
      ; one gate position MUST be filled, other MUST NOT be filled
      (let [[g1 g2] (grid/gate-positions from to)
            occ1    (unoccupied? board' g1)
            occ2    (unoccupied? board' g2)]
        (or (and occ1 (not occ2))
            (and (not occ1) occ2))))))

; BUGBUG this doesn't account for pieces that are atop other pieces
; the topmost z>0 pieces are always free
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
        (t/for [nabe-pq :- Point (grid/neighbors pq)
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


; TODO better name, maybe allowed-move-positions-from-point?
(ann allowed-moves [Board Point -> (Set Point)])
(defmulti allowed-moves
  "internal helper multimethod; you probably want to call calculate-move-positions instead"
  (fn [board pq] (piece->species (top-piece-at-pos board pq))))

; if we don't know what it is, it gets no moves
(defmethod allowed-moves :default
  [_ _]
  #{})

(defmethod allowed-moves :queen-bee
  [board pq]
  (set
    (t/for [neighbor-pq :- Point (grid/neighbors pq)
          :when (planar-passable? board pq neighbor-pq) ]
      :- Point
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
    (let [board (dissoc board pq)] ; BUG: remove-top-piece-at-pos ?
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
          ; THIS SEEMS BUGGED TO SHIT
          ; 2016-01-20 coming back to this project, idle since May, above is a heartening comment, heh
          (assoc board pq (-> nabe-piece name (str "MOSQUITO") keyword vector))
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
(ann calculate-move-positions [Game Point -> (Set Point)])
(defn calculate-move-positions
  "calculate the set of possible destination coordinates for the bug at a given position"
  [g pq]
  ; TODO: pillbug checks
  ; if this piece neighbors a pillbug
  ; and this piece did not move in the last turn
  ; then add the unoccupied neighbors of the pillbug to the moveset
  ; if the path to each neighbor is not gated at the 2nd level
  (let [board (:board g)
        bg    (board->graph board)]
    (if (or (position-is-stacked? board pq)
            (piece-is-free? bg (top-piece-at-pos board pq)))
      (allowed-moves board pq)
      #{})))

; BUG doesn't account for player-relative moves
; e.g. a piece might be differently mobile for each player due to pillbuggery
(ann calculate-all-moves [Game -> (Map Piece (Vec Point))])
(defn calculate-all-moves
  [{:keys [board], :as g}]
  (into {}
        (filter
          second
          (t/for [[pq pieces] :- '[Point (Vec Piece)]   board
                  :let [positions (calculate-move-positions g pq)]
                  :when (not-empty positions)]
            :- '[Piece (Vec Point)]
            [(top-piece-of-stack pieces) positions]))))

(comment
  (def g (current-game!))
  (def m {:piece :wA1, :position [1 5]})
  (calculate-all-moves-for-player g :black)
  (def g (apply-move g m))

  (def player :black)
  (def board (:board g))

  (prn state*)
  ; how to reach into the state of the currently running applet
  (defn current-game!
    []
    (-> hive.gui/hive-applet
        (quil.applet/with-applet (quil.core/state-atom))
        deref
        :game
        ))
  (swap! state* assoc-in [:board [3 4]] :bQ)
  (swap! state* assoc-in [:board [2 6]] :bL)
  (clojure.pprint/pprint state*)
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
(ann spawn-positions [Board Player -> (Vec Point)])
(defn spawn-positions [board player]
  ; TODO: account for the situation where the board contains only a single enemy piece, and no friendlies
  (set
    (if (= 1 (count board))
      ; special case when there is only one piece on the board
      (let [[pq [piece]] (first board)]
        (assert (= 1 (-> board first second count)))
        (if (= (opposing-player player) (piece->player piece))
          (grid/neighbors pq)
          #{}))
      ; the rest of the time we do this more monstrous query
      (for [; find all friendly pieces
            [pq pieces] board
            :when      (= player (piece->player (top-piece-of-stack pieces)))
            ; find all empty neighbors of friendly pieces
            empty-pq   (grid/neighbors pq)
            ; require that those empty neighbors are themselves not neighbors with any unfriendly pieces
            :when      (and (unoccupied? board empty-pq)
                            (every? true?
                                    (map (fn [[_ nabe-piece]]
                                           ;; (println "investigating piece" nabe-piece)
                                           (= player (piece->player nabe-piece)))
                                         (occupied-neighbors board empty-pq))))
            ]
        empty-pq))))

(comment
  (swap! state* assoc :valid-moves (spawn-positions (current-board!) :white))
  (swap! state* assoc :valid-moves (spawn-positions (current-board!) :black))
  )

(ann surrounded? [Board Point -> boolean])
(defn surrounded?
  [board pq]
  (every? (partial occupied? board) (grid/neighbors pq)))

(ann find-piece [Board Piece -> Point])
(defn find-piece
  [board needle-piece]
  (first
    (t/for [[pq pieces] board
            piece pieces
            :when (= piece needle-piece) ]
      :- Point
      pq)))

(ann determine-winner [Board -> (U Player ':tie nil)])
(defn determine-winner
  [board]
  (let [wQ-pos (find-piece board :wQ)
        bQ-pos (find-piece board :bQ) ]
    (match [(and wQ-pos (surrounded? board wQ-pos))
            (and bQ-pos (surrounded? board bQ-pos))]
           [true true] :tie
           [true _]    :black
           [_ true]    :white
           :else       nil)))

(t/defalias Move
  (t/HMap :mandatory {:piece Piece
                      :position Point }
          :optional {:text String}))

(t/defalias Game (t/HMap
                   :mandatory {:id String
                               :board Board
                               :unplaced (Set Piece)
                               :moves (Vec Move)
                               :possible-moves (Map Piece (Vec Point))
                               :spawns (Vec Point)
                               :turn Player
                               :winner (U Player ':tie nil)
   ; TODO: player metadata
   ; name, id number, avatar url, etc.
                               }))

(ann new-game [-> Game])
(defn new-game
  "Create a fresh Game data structure"
  []
  {:id (str (gensym "g")) ; lazy ass
   :board {}
   :unplaced PIECES
   :moves []
   :possible-moves {}
   :spawns [] ; spawn anywhere, nothing is anchored yet
   :turn :white
   :winner nil ; no winner yet
   })

(ann valid-move? [Game Move -> boolean])
(defn valid-move?
  "determine if the proposed move is valid in the given game"
  [{:keys [unplaced spawns], :as g}
   {:keys [piece position], :as move}]
  (or
    ; spawn
    (and
      (contains? unplaced piece)
      (or (empty? spawns) (contains? spawns position)))
    ; movement
    (some-> g
            :possible-moves
            (get piece)
            (contains? position))))

; NB assuming move is valid...
(ann apply-move [Game Move -> Game])
(defn apply-move
  "apply the given move into a game, producing a new game state"
  [{:keys [board unplaced moves], :as g}
   {:keys [piece position], :as move}]
  (let [opponent (opposing-player (:turn g))
        board    (if (contains? unplaced piece)
                   (spawn-piece board piece position)
                   (move-piece board piece position))
        g        (assoc g
                        :board    board
                        :unplaced (disj unplaced piece)
                        :moves    (conj moves move) ; TODO calculate textual description of move
                        :spawns   (spawn-positions board opponent)
                        :turn     opponent
                        :winner   (determine-winner board)
                        )]
    (assoc g :possible-moves (calculate-all-moves g))
    ))


(comment
  ; trying to be DRYer about enum union types
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

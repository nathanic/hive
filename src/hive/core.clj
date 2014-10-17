(ns hive.core
  (:use [loom graph alg attr]))


(comment
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

  )


; build a graph from the above map
; coords as node attributes
  ; compute coords from directions as we go
; dirs as edge attributes
(defn board->graph [board]
  (let [g (-> (apply graph (keys board))
              (add-attr (first (keys board)) :pos [0 0])
              )]
    (reduce (fn [g piece]
              (reduce (fn [g [dir nabe-piece]]
                        (-> g
                            (add-edges [piece nabe-piece] )
                            (add-attr piece nabe-piece :dir dir )
                            (add-attr nabe-piece :pos (move-direction (attr g piece :pos) dir))))
                      g
                      (get board piece)))
            g
            (keys board))))
; i can't believe the above worked the first time!

(comment
  (def gg (board->graph board))
  (attr gg :bQ :bG2 :dir) ;=> :se
  (attr gg :bQ :pos)
  (attr gg :bG1 :pos)
  (attr gg :bA1 :pos)
  (attr gg :wA1 :pos)
  )


(defn free-pieces
  "return a list of names of free pieces in the given board graph"
  [board-graph]
  (filter (partial piece-is-free? board-graph) (nodes board-graph)))

(defn piece-is-free?
  "given a graph of a Hive board, and a piece name keyword,
  decide if the piece is free to move."
  [board-graph piece]
  (connected? (remove-nodes board-graph piece)))

(comment
  (free? g :bQ)
  (free? g :bG1)
  (free? g :bG2)
  (free? g :wA1)
  )


; http://www.redblobgames.com/grids/hexagons/#coordinates
; axial coordinates
;; neighbors = [
;;    [+1,  0], [+1, -1], [ 0, -1],
;;    [-1,  0], [-1, +1], [ 0, +1]
;;
;; d = neighbors [direction]
;; return Hex (q + d[0], r + d[1])

(def DIRS [:ne :e :se :sw :w :nw])
(def DIR-VECTORS {:ne [ 1 -1]
                  :e  [ 1  0]
                  :se [ 0  1]
                  :sw [-1  1]
                  :w  [-1  0]
                  :nw [ 0 -1]
                  })

(defn move-direction
  "given the coordinates of a hex and a direction, compute the
  coordinates of the neighbor hex in the given direction."
  [[x y] dir]
  (let [[dx dy] (get DIR-VECTORS dir)]
    [(+ x dx) (+ y dy)]))

(comment
  (move-direction [2 -2] :ne)

  (move-direction [2 -2] :nw)

  (neighbor-coords [2 -2])
  )

(defn neighbor-coords
  "given a coordinate pair [x y] in axial hex coordinates,
  compute the coordinates of all 6 neighboring points"
  [point]
  (map (partial move-direction point) DIRS))




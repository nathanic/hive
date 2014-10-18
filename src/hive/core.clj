(ns hive.core
  (:use [hive hex-grid]
        [loom graph alg attr]))

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

  ; adding edges automatically creates nodes
  (-> (graph)
      (add-edges [:a :b] [:b :c])
      (nodes))

  ; however simply adding an attribute does not create the nodes
  (-> (graph)
      (add-attr [:a :b] :color :purple)
      (nodes))
  )


(defn piece-is-free?
  "given a graph of a Hive board, and a piece name keyword,
  decide if the piece is free to move."
  [board-graph piece]
  (connected? (remove-nodes board-graph piece)))


(defn free-pieces
  "return a list of names of free pieces in the given board graph"
  [board-graph]
  (filter (partial piece-is-free? board-graph) (nodes board-graph)))


; build a graph from the above map
; coords as node attributes
  ; compute coords from directions as we go
; dirs as edge attributes
(defn board->graph [board]
  ; fold over pieces/nodes
  (reduce (fn [g piece]
            ; fold over this piece's neighbors
            (reduce (fn [g [dir nabe-piece]]
                      (-> g
                          (add-edges [piece nabe-piece] )
                          (add-attr piece nabe-piece :dir dir )
                          (add-attr nabe-piece :pos (move-direction (attr g piece :pos) dir))))
                    g
                    (get board piece)))
          ; prime the graph with the first node and a position at the origin
          (-> (graph (first (keys board)))
              (add-attr (first (keys board)) :pos [0 0])) 
          (keys board)))
; i can't believe the above worked the first time!

(comment
  (def gg (board->graph board))
  (def gg2 (board->graph board))
  (attr gg :bQ :bG2 :dir) ;=> :se
  (attr gg :bQ :pos)
  (attr gg :bG1 :pos)
  (attr gg :bA1 :pos)
  (attr gg :wA1 :pos)
  )

(defn graph->board [g]
  ; for each node
    ; create a node in the hashmap
      ; whose value is a hash map
      ; with keys that are the :dir attribute of the edges between this node and (successors g node)
      ; also :pos key
  )

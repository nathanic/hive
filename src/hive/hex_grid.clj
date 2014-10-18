(ns hive.hex-grid
  (:use [loom graph alg attr]))

; so we're using a pointy-top hex grid with an axial coordinate system

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




(ns hive.hex-grid
  (:use [loom graph alg attr])
  (:require [clojure.core.typed
             :as t
             :refer [defalias ann U Vec Map Seq]]))

(comment
  (t/check-ns)
  )

(ann hex-height-for-width [Number -> Number])
(defn hex-height-for-width [width]
  (/ (* 2 width) (Math/sqrt 3.0)))

(ann hex-side-length-for-width [Number -> Number])
(defn hex-side-length-for-width [width]
  (/ width (Math/sqrt 3.0)))

; so we're using a pointy-top hex grid with an axial coordinate system

; http://www.redblobgames.com/grids/hexagons/#coordinates
; axial coordinates
;; neighbors = [
;;    [+1,  0], [+1, -1], [ 0, -1],
;;    [-1,  0], [-1, +1], [ 0, +1]
;;
;; d = neighbors [direction]
;; return Hex (q + d[0], r + d[1])

(defalias Direction "One of the 6 cardinal directions of pointy-topped hexagons"
  (U ':ne ':e ':se ':sw ':w ':nw))

(defalias AxialVector "A 2-vector in axial hex coordinates"
  '[Number Number])

(defalias AxialPoint "A point in axial hex coordinates"
  '[Number Number])

(defalias CubePoint "A point in 3D cube hex coordinates"
  '[Number Number Number])

(defalias PixelPoint "A point in 2D pixel coordinates"
  '[Number Number])

(ann DIRS (Vec Direction))
(def DIRS [:ne :e :se :sw :w :nw])

(ann DIR-VECTORS (Map Direction AxialVector))
(def DIR-VECTORS {:ne [ 1 -1]
                  :e  [ 1  0]
                  :se [ 0  1]
                  :sw [-1  1]
                  :w  [-1  0]
                  :nw [ 0 -1]
                  })

(ann move-direction [AxialPoint Direction -> AxialPoint])
(defn move-direction
  "given the coordinates of a hex and a direction, compute the
  coordinates of the neighbor hex in the given direction."
  [[p q] dir]
  [0 0]
  #_(if-let [[dp dq] (get DIR-VECTORS dir)]
    [(+ p dp) (+ q dq)]
    (throw (new IllegalArgumentException (str "Invalid direction: " dir)))))

(comment
  (t/check-ns)
  (t/cf (get DIR-VECTORS :ne [1 -1]))
  (t/cf (move-direction [0 0] :ne))
  (t/cf [0 0] '[Number Number])
  (t/cf [0 0] AxialPoint)
  (t/cf [0 0] CubePoint)
  (t/cf (fn [[p q] d] [q p]) [AxialPoint Direction -> AxialPoint])
  (move-direction [2 -2] :ne)

  (move-direction [2 -2] :nw)

  (neighbor-coords [2 -2])
  )

(ann neighbor-coords [AxialPoint -> (Seq AxialPoint)])
(defn neighbor-coords
  "given a coordinate pair [p q] in axial hex coordinates,
  compute the coordinates of all 6 neighboring points"
  [point]
  (map (partial move-direction point) DIRS))



; q = (1/3*sqrt(3) * x - 1/3 * y) / size
; r = 2/3 * y / size
(ann pixel->axial [Number PixelPoint -> AxialPoint])
(defn pixel->axial [hex-width [x y]]
  [(/ (- (* (Math/sqrt 3.0) x) y)
      hex-width
      3.0)
   (-> y (* 2/3) (/ hex-width)) ])


(ann axial->pixel [Number AxialPoint -> PixelPoint])
(defn axial->pixel [hex-width [p q]]
  ; assuming both coordinate systems share the same origin
  [(* hex-width (Math/sqrt 3.0) (+ p (/ q 2.0)))
   (* hex-width 3/2 p)])

(ann axial->cube [AxialPoint -> CubePoint])
(defn axial->cube [[p q]]
  [p q (- 0 p q)])

(ann cube->axial [CubePoint -> AxialPoint])
(defn cube->axial [[x y z]]
  [x y])

(ann round [Number -> Long])
(defn round [x]
  (-> x double Math/round))

(ann abs [Number -> Double])
(defn abs [x]
  (-> x double Math/abs))

(comment
  (cube-round [1.1 1.2 1.3])
  )

(ann cube-round [CubePoint -> CubePoint])
(defn cube-round
  "round each cubic hex coordinate, see which one was the farthest off,
  and replace it from the other two coords given the cube constraint
  x + y + z = 0"
  [[x y z]]
  (let [rx (round x)
        ry (round y)
        rz (round z)
        dx (abs (- rx x))
        dy (abs (- ry y))
        dz (abs (- rz z)) ]
    (cond
      (and (> dx dy) (> dx dz)) [(- 0 ry rz) ry rz]
      (> dy dz)                 [rx (- 0 ry rz) rz]
      :else                     [rx ry (- 0 rx ry)])))

(ann axial-round [AxialPoint -> AxialPoint])
(defn axial-round
  "round the given point [p q] in axial hex coordinates to the nearest integer hex"
  [pt]
  (-> pt axial->cube cube-round cube->axial))

(ann pixel->nearest-hex [Number PixelPoint -> AxialPoint])
(defn pixel->nearest-hex [hex-width xy]
  (->> xy (pixel->axial hex-width) axial-round))


(comment
  (t/cf (pixel->axial 100.0 [1 1]))
  (t/cf (->> [1 1] (pixel->axial 100.0)))
  (t/cf (->> [1 1] (pixel->axial 100.0) axial-round))
  (do (def w 100) (def h (hex-height-for-width w)))
  (pixel->nearest-hex w [0 0]) ;=> [0 0]
  (pixel->nearest-hex w [w 0]) ;=> [1 0]
  (pixel->nearest-hex w [0 h]) ;=> [0 1]
  (pixel->nearest-hex w [0 (* 2 h)]) ;=> [-1 -1]   ;; NOPE
  (axial->pixel w [-1 -1]) ;=> [-259.8076211353316 -150N]  ;; also NOPE

  (pixel->axial )
  )



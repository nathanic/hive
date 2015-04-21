(ns hive.hex-grid
  (:use [loom graph alg attr])
  (:require [clojure.core.typed :as t
             :refer [defalias ann U Vec Map Seq Option]]))

; so we're using a pointy-top hex grid with an axial coordinate system
; http://www.redblobgames.com/grids/hexagons/#coordinates

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types and Constants
(defalias Direction "One of the 6 cardinal directions of pointy-topped hexagons"
  (U ':ne ':e ':se ':sw ':w ':nw))

(defalias Rotation "Clockwise or counterclockwise (widdershins)"
  (U ':cw ':ccw))

(defalias AxialVector "A 2-vector in axial hex coordinates"
  '[Number Number])

(defalias AxialPoint "A point in axial hex coordinates"
  '[Number Number])

(defalias CubePoint "A point in 3D cube hex coordinates"
  '[Number Number Number])

(defalias CubeVector "A 3-vector in 3D cube hex coordinates"
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

(ann DIR-OPPOSITES (Map Direction Direction))
(def DIR-OPPOSITES
  {:ne :sw
   :e  :w
   :se :nw
   :sw :ne
   :w  :e
   :nw :se
   })

(ann round [Number -> Long])
(defn- round [x]
  (-> x double Math/round))

(ann abs [Number -> Double])
(defn- abs [x]
  (-> x double Math/abs))

(ann opposite-direction [Direction -> Direction])
(defn opposite-direction
  [dir]
  (or (DIR-OPPOSITES dir) :e))

(ann neighbor [AxialPoint Direction -> AxialPoint])
(defn neighbor
  "given the coordinates of a hex and a direction, compute the
  coordinates of the neighbor hex in the given direction."
  [[p q] dir]
  ;; [0 0]
  (if-let [[dp dq] (get DIR-VECTORS dir)]
    [(+ p dp) (+ q dq)]
    (throw (new IllegalArgumentException (str "Invalid direction: " dir)))))

(comment
  (t/check-ns)
  (t/cf (get DIR-VECTORS :ne [1 -1]))
  (t/cf (neighbor [0 0] :ne))
  (t/cf [0 0] '[Number Number])
  (t/cf [0 0] AxialPoint)
  (t/cf [0 0] CubePoint)
  (t/cf (fn [[p q] d] [q p]) [AxialPoint Direction -> AxialPoint])
  (neighbor [2 -2] :ne)

  (neighbor [2 -2] :nw)

  (neighbors [2 -2])
  (t/ASeq)
  )

(ann neighbors [AxialPoint -> (Seq AxialPoint)])
(defn neighbors
  "given a coordinate pair [p q] in axial hex coordinates,
  compute the coordinates of all 6 neighboring points"
  [point]
  (map (partial neighbor point) DIRS))

(ann cube-distance [CubePoint CubePoint -> Number])
(defn cube-distance
  [[x1 y1 z1] [x2 y2 z2]]
  (-> (+ (abs (- x1 x2))
         (abs (- y1 y2))
         (abs (- z1 z2)))
      long
      (/ 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jommetry
(ann hex-height-from-width [Number -> Number])
(defn hex-height-from-width [width]
  (/ (* 2 width) (Math/sqrt 3.0)))

(ann hex-size-from-width [Number -> Number])
(defn hex-size-from-width [width]
  (/ (double width) (Math/sqrt 3.0)))

(ann hex-width-from-size [Number -> Number])
(defn hex-width-from-size [size]
  (* size (Math/sqrt 3.0)))

(ann hex-height-from-size [Number -> Number])
(defn hex-height-from-size [size]
  (* size 2.0))

(ann hex-dims-from-size [Number -> '[Number Number]])
(defn hex-dims-from-size
  "returns a tuple [width height] for a hexagon given the `size` (length of a single side)"
  [size]
  [(hex-width-from-size size) (hex-height-from-size size)])

(comment
  (hex-width-from-size (hex-size-from-width 100.0))
  (hex-height-from-size (hex-height-from-width 100.0))
  )

; q = (1/3*sqrt(3) * x - 1/3 * y) / size
; r = 2/3 * y / size
(ann pixel->axial [Number PixelPoint -> AxialPoint])
(defn pixel->axial
  "given the size (side length) of a hexagon in pixel coordinates,
  and a point in pixel coordinates, transform it to the corresponding point
  in axial hex grid space."
  [size [x y]]
  [(/ (- (* (Math/sqrt 3.0) x) y)
      size
      3.0)
   (-> y (* 2/3) (/ size)) ])

(comment
  (/ 100.0 2 2)
  )

(ann axial->pixel [Number AxialPoint -> PixelPoint])
(defn axial->pixel [size [p q]]
  ; assuming both coordinate systems share the same origin
  [(* size (Math/sqrt 3.0) (+ p (/ q 2.0)))
   (* size 3/2 q)])

(ann axial->cube [AxialPoint -> CubePoint])
(defn axial->cube [[p q]]
  [p q (- 0 p q)])

(ann cube->axial [CubePoint -> AxialPoint])
(defn cube->axial [[x y z]]
  [x y])


(comment
  (cube-round [1.1 1.2 1.3])
  )

(ann cube-round [CubePoint -> CubePoint])
(defn cube-round
  "round each cubic hex coordinate, determine which one is the most different
  from its pre-rounding value, and replace it from the other two coords given
  the planar cube constraint: x + y + z = 0"
  [[x y z]]
  (let [rx (round x)
        ry (round y)
        rz (round z)
        dx (abs (- rx x))
        dy (abs (- ry y))
        dz (abs (- rz z)) ]
    (cond
      (and (> dx dy) (> dx dz)) [(- 0 ry rz) ry rz]
      (> dy dz)                 [rx (- 0 rx rz) rz]
      :else                     [rx ry (- 0 rx ry)])))

; this is so much easier to do with a change of coordinate systems
; than to try to naively deal with hexagon tile geometry,
; which is probably what i would have done. (thanks Amit!)
(ann axial-round [AxialPoint -> AxialPoint])
(defn axial-round
  "round the given point [p q] in axial hex coordinates to the nearest integer hex."
  [pt]
  (-> pt axial->cube cube-round cube->axial))

(ann pixel->nearest-hex [Number PixelPoint -> AxialPoint])
(defn pixel->nearest-hex
  "compute the integer coordinates in axial hex space corresponding to the center
  of the hex on which the given pixel coordinates fall."
  [hex-size xy]
  (->> xy (pixel->axial hex-size) axial-round))

(ann axial-coords-covering-rect [Number Number Number -> (Seq AxialPoint)])
(defn axial-coords-covering-rect
  "returns a sequence of axial hex coordinates for all hexes that
  at least partially fit on a rectangle of the given dimensions"
  [hex-size cx cy]
  ; go right by hex-widths and down by 3/4 hex-heights
  (let [[width height] (hex-dims-from-size hex-size)]
    (t/for [y :- Number, (range 1 cy (* 3/4 height))
            x :- Number, (range 1 cx width) ] :- AxialPoint
      (pixel->nearest-hex hex-size [x y]))))

(comment
  (t/cf (pixel->axial 100.0 [1 1]))
  (t/cf (->> [1 1] (pixel->axial 100.0)))
  (t/cf (->> [1 1] (pixel->axial 100.0) axial-round))
  (do (def w 100) (def h (hex-height-from-width w)) (def s (hex-size-from-width w)))
  (pixel->nearest-hex s [0 0]) ;=> [0 0]
  (pixel->nearest-hex s [w 0]) ;=> [1 0]
  (pixel->nearest-hex s [0 h]) ;=> [0 1]
  (pixel->nearest-hex s [0 (* 2 h)]) ;=> [-1 -1]   ;; NOPE
  (axial->pixel s [-1 -1]) ;=> [-259.8076211353316 -150N]  ;; also NOPE

  (axial->pixel s [0 0])
  (axial->pixel s [1 0]) ;=> [100.0 86.60254037844388]
  (axial->pixel s [-1 2]) ;=> still nope
  [w h s]
  (/ (* -2.0 115) 300.0)
  (/ (* 4 h) 300.0)
  (axial-round [-0.7666 1.540])
  (* 2 h)
  (axial->cube [-0.7666 1.540]) ;=> [-0.7666 1.54 -0.7734000000000001]
  (cube-round [-0.7666 1.54 -0.7734000000000001]) ;=> [-1 -1 -1]
  (map round [-0.7666 1.54 -0.7734000000000001])
  (map (fn [x] (abs (- (round x) x))) [-0.7666 1.54 -0.7734000000000001])


  (pixel->axial s [0 0])
  (pixel->axial s [1 0])
  (pixel->axial s [10 0])
  (pixel->nearest-hex s [10 0])
  (pixel->nearest-hex s [w 0])
  (pixel->axial s [0 (* 2 h)])
  )

(comment
  (do (def w 100) (def h (hex-height-from-width w)) (def s (hex-size-from-width w)))
  (axial-coords-covering-rect s 400 300)
  (use 'clojure.pprint)

  (pprint
    (let [cx 400, cy 300
          [width height] (hex-dims-from-size s)]
      (t/for [y :- Number, (range 1 cy (* 3/4 height))
              x :- Number, (range 1 cx width) ] :- AxialPoint
        [[x y] (pixel->nearest-hex s [x y])])))
  )

(ann neighbors? [AxialPoint AxialPoint -> boolean])
(defn neighbors? [pq1 pq2]
  (let [cu1 (axial->cube pq1)
        cu2 (axial->cube pq2)
        dist (cube-distance cu1 cu2)]
    (= dist 1)))

;http://www.redblobgames.com/grids/hexagons/#rotation
(ann rotate-vector-60 [CubeVector Rotation -> CubeVector])
(defn rotate-vector-60
  [[x y z] rot]
  (case rot
    :cw  [(- y) (- z) (- x)]
    :ccw [(- z) (- x) (- y)]
    ))

(ann plus [CubePoint CubeVector -> CubePoint])
(defn plus
  [[x y z] [dx dy dz]]
  [(+ x dx) (+ y dy) (+ z dz)])
; (mapv - [3 2 1] [4 5 6]) also works but i had trouble getting the types right

(ann minus [CubePoint CubePoint -> CubeVector])
(defn minus
  [[x1 y1 z1] [x2 y2 z2]]
  [(- x1 x2) (- y1 y2) (- z1 z2)])

(ann gate-positions [AxialPoint AxialPoint -> '[AxialPoint AxialPoint]])
(defn gate-positions
  [apq bpq]
  (let [acube (axial->cube apq)
        bcube (axial->cube bpq)
        cubevec (minus bcube acube)
        gate1 (plus acube (rotate-vector-60 cubevec :cw))
        gate2 (plus acube (rotate-vector-60 cubevec :ccw)) ]
    [(cube->axial gate1) (cube->axial gate2)]))

(comment
  (= (gate-positions [0 1] [0 2]) [[-1 2] [1 1]])
  (= (gate-positions [0 4] [1 3]) [[1 4] [0 3]])
  )

; do i really have to do this myself?
(ann clojure.set/map-invert (All [a b] [(Map a b) -> (Map b a)]))

(ann find-direction-from-axial-points [AxialPoint AxialPoint -> (Option Direction)])
(defn find-direction-from-axial-points
  [from-pq to-pq]
  (get (clojure.set/map-invert DIR-VECTORS) (mapv - to-pq from-pq))
  )
(comment
  (find-direction-from-axial-points [2 7] [3 6])
  (find-direction-from-axial-points [2 7] [3 7])
  (find-direction-from-axial-points [2 7] [2 8])
  (find-direction-from-axial-points [2 7] [1 8])
  (find-direction-from-axial-points [2 7] [1 7])
  (find-direction-from-axial-points [2 7] [2 6])
  (find-direction-from-axial-points [2 7] [0 0])

  )

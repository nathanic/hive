(ns hive.gui
  (:use [hive.hex-grid])
  (:require [quil.core :as q]
            [quil.middleware :as m]))

; http://quil.info/api
; https://github.com/quil/quil/wiki/Functional-mode-(fun-mode)

; my goal right now is just to draw a grid and light up the hex your cursor is on

(def SKETCH-WIDTH 1024)
(def SKETCH-HEIGHT 800)

(def HEX-WIDTH 100) ; in pixels
(def HEX-HEIGHT (hex-height-from-width HEX-WIDTH))
(def HEX-SIZE (hex-size-from-width HEX-WIDTH))

(defn draw-hex-sides
  "Given an axial hex vector [p q], draw lines corresponding to particular sides of the
  hexagon represented as [:ne e :se :sw :w :nw] directions."
  [[p q] & dirs]
  (doseq [dir dirs]
    (let [[x y]  (axial->pixel HEX-SIZE [p q])
          w-2    (/ HEX-WIDTH 2)
          h-2    (/ HEX-HEIGHT 2)
          h-4    (/ HEX-HEIGHT 4) ]
      (apply q/line
             (case dir
               :ne [x         (- y h-2)
                    (+ x w-2) (- y h-4)] ;  *\
               :e  [(+ x w-2) (- y h-4)
                    (+ x w-2) (+ y h-4)] ;  *|
               :se [(+ x w-2) (+ y h-4)
                    x         (+ y h-2)] ;  */
               :sw [x         (+ y h-2)
                    (- x w-2) (+ y h-4)] ; \*
               :w  [(- x w-2) (- y h-4)
                    (- x w-2) (+ y h-4)] ; |*
               :nw [(- x w-2) (+ y h-2)
                    x         (- y h-4)] ; /*
               )))))

(defn hexes-per-column
  "decide how many hexes can fit on the vertical span of pixels"
  [cy]
  (/ cy HEX-HEIGHT))

(comment
  (axial-coords-fitting-sketch 400 400)
  (pixel->nearest-hex [351 0] HEX-WIDTH)
  (axial-round (pixel->axial HEX-WIDTH [261 0]))
  (axial->pixel 1 0 HEX-WIDTH)
  )

(defn zigzag
  "draw a horizontal zigzag of the given dimensions."
  [x0 y0 seg-w seg-h rise-or-fall]
  (loop [x x0]
    (if (= rise-or-fall :rise)
      (do
        (q/line x (+ y0 seg-h) (+ x seg-w) y0)
        (q/line (+ x seg-w) y0 (+ (* 2 seg-w) x) (+ y0 seg-h)))
      (do
        (q/line (+ x seg-w) (+ y0 seg-h) (+ (* 2 seg-w) x) y0)
        (q/line x y0  (+ x seg-w) (+ y0 seg-h))))
    (when (< x (q/width))
      (recur (+ x (* 2 seg-w))))))

(defn verticals [x0 y0]
  (loop [x x0]
    (q/line x y0 x (+ y0 HEX-SIZE))
    (when (< x (q/width))
      (recur (+ x HEX-WIDTH)))))

(defn draw-hex-grid-old [state]
  ; draw horizontal zigags
  ; then fill in vertical lines
  (let [win-w           (q/width)
        win-h           (q/height)
        hexes-per-row   (-> win-w (/ HEX-WIDTH) int)
        vertical-hexes  (/ win-h HEX-SIZE)
        zig-y-offset    (/ (- HEX-HEIGHT HEX-SIZE) 2)
        ]
    (q/stroke 20 20 20)
    (doseq [zig (range -1 vertical-hexes)]
      (zigzag 0 ; (if (odd? zig) 0 (/ HEX-WIDTH 2)),
              (+ zig-y-offset (* zig 3/4 HEX-HEIGHT))
              (/ HEX-WIDTH 2),
              zig-y-offset
              (if (odd? zig) :rise :fall)
              ))
    ; now fill in vertical lines
    (doseq [col (range hexes-per-row)]
      (if (odd? col)
        (verticals 0 (+ (/ HEX-HEIGHT 2) (* col 3/4 HEX-HEIGHT)))
        (verticals (/ HEX-WIDTH 2) (+ (/ HEX-HEIGHT 2) (* col 3/4 HEX-HEIGHT)))
        ))))

(defn draw-hex-grid [state]
  (let [win-w           (q/width)
        win-h           (q/height)]
    (doseq [pq (axial-coords-covering-rect HEX-SIZE win-w win-h)]
      (draw-hex-sides pq :ne :e :se :sw :w :nw))))


(defn setup []
  (q/frame-rate 1)
  ;; (q/color-mode :hsb)
  { })

(defn update [state]
  state)

(defn draw [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
  ;; (q/fill (:color state) 255 255)
  (q/stroke-weight 1)
  (draw-hex-grid state)
  ;; (q/stroke-weight 5)
  (loop [x 0]
    (q/stroke (q/color 255 0 0))
    (q/line x 200 (+ x HEX-WIDTH) 200)
    (q/stroke (q/color 0 0 255))
    (q/line (+ x HEX-WIDTH) 200 (+ x (* 2 HEX-WIDTH)) 200)
    (when (< x (q/width))
      (recur (+ x (* 2 HEX-WIDTH))))))

(comment
  (q/defsketch hello-quil
    :title "Hive Prototype"
    :size [SKETCH-WIDTH SKETCH-HEIGHT]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update is called on each iteration before draw is called.
    ; It updates sketch state.
    :update update
    :draw draw
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))

(comment
  (for [n (range 1 16)] (cond (zero? (mod n 15)) "FizzBuzz" (zero? (mod n 3)) "Fizz" (zero? (mod n 5)) "Buzz" :else n))
  )

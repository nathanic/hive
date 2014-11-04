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
  (doseq [dir (or dirs DIRS)]
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
               :nw [(- x w-2) (- y h-4)
                    x         (- y h-2)] ; /*
               ))
      (q/fill (q/color 10 10 10))
      (q/text (format "(%s,%s)" p q) x y)
      )))

(defn hexes-per-column
  "decide how many hexes can fit on the vertical span of pixels"
  [cy]
  (/ cy HEX-HEIGHT))


(defn draw-hex-grid [state]
  (let [win-w (q/width)
        win-h (q/height)]
    (doseq [pq (axial-coords-covering-rect HEX-SIZE win-w win-h)]
      (draw-hex-sides pq))))

(comment
  (count (set (axial-coords-covering-rect HEX-SIZE SKETCH-WIDTH SKETCH-HEIGHT)))
  (axial->pixel HEX-SIZE [0 1])
  (axial->pixel HEX-SIZE [1 1])
  (axial->pixel HEX-SIZE [2 0])
  )

(defn setup []
  (q/frame-rate 1)
  ;; (q/frame-rate 30)
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
  (q/stroke (q/color 0 0 255))
  (draw-hex-grid state)
  (let [x (q/mouse-x), y (q/mouse-y)]
    (q/stroke (q/color 255 0 0))
    (draw-hex-sides (pixel->nearest-hex HEX-SIZE [x y])))
  ;; (q/stroke-weight 5)
  #_(loop [x 0]
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

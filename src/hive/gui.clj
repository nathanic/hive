(ns hive.gui
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def SKETCH-WIDTH 1024)
(def SKETCH-HEIGHT 800)

(def HEX-WIDTH 100) ; in pixels
(def HEX-HEIGHT (/ (* 2 HEX-WIDTH) (Math/sqrt 3)))
(def HEX-SIDE-LEN (/ HEX-WIDTH (Math/sqrt 3)))

(comment
  (prn HEX-WIDTH)
  (prn HEX-HEIGHT)
  (/ (* (Math/sqrt 3) ) 2)
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
        (q/line (+ x seg-w) y0 (+ (* 2 seg-w) x) (+ y0 seg-h)) 
        (q/line x (+ y0 seg-h) (+ x seg-w) y0)))
    (when (< x (q/width))
      (recur (+ x (* 2 seg-w))))))

(defn verticals [x0 y0]
  (loop [x x0]
    (q/line x y0 x (+ y0 HEX-SIDE-LEN))
    (when (< x (q/width))
      (recur (+ x HEX-WIDTH)))))

(defn draw-hex-grid [state]
  ; draw horizontal zigags
  ; then fill in vertical lines
  (let [win-w           (q/width)
        win-h           (q/height)
        hexes-per-row   (-> win-w (/ HEX-WIDTH) int)
        vertical-hexes  (/ win-h HEX-SIDE-LEN)
        zig-y-offset    (/ (- HEX-HEIGHT HEX-SIDE-LEN) 2)
        ]
    (q/stroke 20 20 20)
    (doseq [zig (range vertical-hexes)]
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

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 1)
  ; Set color mode to HSB (HSV) instead of default RGB.
  ;; (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  { })

(defn update [state]
  ; Update sketch state by changing circle color and position.
  state)

(defn draw [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
  ;; (q/fill (:color state) 255 255)
  (q/stroke-weight 1)
  (draw-hex-grid state)
  (q/stroke-weight 5)
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

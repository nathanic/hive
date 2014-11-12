(ns hive.gui
  (:use [hive hex-grid game])
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.string :as string]))

; http://quil.info/api
; https://github.com/quil/quil/wiki/Functional-mode-(fun-mode)

; my goal right now is just to draw a grid and light up the hex your cursor is on

(def SKETCH-WIDTH 1024)
(def SKETCH-HEIGHT 800)

(def HEX-WIDTH 100) ; in pixels
(def HEX-HEIGHT (hex-height-from-width HEX-WIDTH))
(def HEX-SIZE (hex-size-from-width HEX-WIDTH))

(defn piece-at-position [state pq]
  (get (:pieces state) pq))

(defn insert-piece-at-position [state piece pq]
  (assoc-in state [:pieces pq] piece))

; TODO: inset a lil bit
(defn filled-hex [pq color]
  (q/fill color)
  (q/no-stroke)
  (let [[x y]  (axial->pixel HEX-SIZE pq)
        w-2    (- (/ HEX-WIDTH 2) 2)
        h-2    (- (/ HEX-HEIGHT 2) 2)
        h-4    (- (/ HEX-HEIGHT 4) 2) ]
    (q/quad
      x         (- y h-2)  ; north
      (- x w-2) (- y h-4)  ; northwest
      (- x w-2) (+ y h-4)  ; southwest
      x         (+ y h-2)) ; south
    (q/quad
      x         (- y h-2)  ; north
      (+ x w-2) (- y h-4)  ; northeast
      (+ x w-2) (+ y h-4)  ; southeast
      x         (+ y h-2)) ; south
    ))

(defn pixel-verticies-of-hex
  "calculates the PIXEL coordinates of the pointy-topped hexagon
  at the given axial hex coordinates, optionally inset by some number of pixels.
  results in a vector of [x y] pairs in the order [n, ne, se, s, sw, nw]"
  ([pq hex-size]
   (pixel-verticies-of-hex pq hex-size 0))
  ([pq hex-size inset]
   (let [[x y]  (axial->pixel hex-size pq) ; center coordinates
         h      (hex-height-from-size (- hex-size inset))
         h÷2    (/ h 2)
         h÷4    (/ h 4)
         w÷2    (/ (hex-width-from-size (- hex-size inset)) 2)
         ]
     [; n
      [ x                (- y h÷2)]
      ; ne
      [ (+ x w÷2)  (- y h÷4)]
      ; se
      [ (+ x w÷2)  (+ y h÷4)]
      ; s
      [ x                (+ y h÷2)]
      ; sw
      [ (- x w÷2)  (+ y h÷4)]
      ; nw
      [ (- x w÷2)  (- y h÷4)]
      ]))
  )

#_(defn pixel-verticies-of-hex
  "calculates the PIXEL coordinates of the pointy-topped hexagon
  at the given axial hex coordinates, optionally inset by some number of pixels.
  results in a vector of [x y] pairs in the order [n, ne, se, s, sw, nw]"
  ([pq hex-size]
   (pixel-verticies-of-hex pq hex-size 0))
  ([pq hex-size inset]
   (let [[x y]  (axial->pixel hex-size pq) ; center coordinates
         h      (hex-height-from-size hex-size)
         h÷2    (/ h 2)
         h÷4    (/ h 4)
         w÷2    (/ (hex-width-from-size hex-size) 2)
         -inset (- inset)
         ]
     [; n
      [ x                (- y h÷2 -inset)]
      ; ne
      [ (+ x w÷2 -inset)  (- y h÷4 -inset)]
      ; se
      [ (+ x w÷2 -inset)  (+ y h÷4 -inset)]
      ; s
      [ x                (+ y h÷2 -inset)]
      ; sw
      [ (- x w÷2 inset)  (+ y h÷4 -inset)]
      ; nw
      [ (- x w÷2 inset)  (- y h÷4 -inset)]
      ])))

(defn draw-filled-hex [pq inset color]
  (q/fill color)
  (q/no-stroke)
  (let [[[nx ny] [nex ney] [sex sey]
         [sx sy] [swx swy] [nwx nwy]]
        (pixel-verticies-of-hex pq HEX-SIZE inset)]
    (q/quad nx ny nex ney sex sey sx sy)
    (q/quad nx ny nwx nwy swx swy sx sy)))

(defn draw-hex-outline [pq inset thickness color]
  (q/stroke-weight thickness)
  (q/stroke color)
  (let [[n ne se s sw nw] (pixel-verticies-of-hex pq HEX-SIZE inset)]
    (q/line n ne)
    (q/line ne se)
    (q/line se s)
    (q/line s sw)
    (q/line sw nw)
    (q/line nw n)))

(def PLAYER->COLOR
  {:white [250 250 250]
   :black [10 10 10]
   })

(def SPECIES->COLOR
  {:ant [10 10 250]
   :beetle [250 10 250]
   :hopper [10 250 10]
   :ladybug [250 10 10]
   :mosquito [100 100 100]
   :pillbug [10 250 250]
   :queenbee [250 250 10]
   :spider [188 143 143]
   })


(def PIECE-INSET 5)

(comment
  (def piece :wG2)
  (apply q/color (get SPECIES->COLOR (piece-species piece) [0 0 0]))
  )

(defn render-piece [pq piece]
  (let [col (apply q/color (get SPECIES->COLOR (piece-species piece) [0 0 0]))]
    (draw-filled-hex pq PIECE-INSET (apply q/color (get PLAYER->COLOR (piece-player piece) [ 0 255 0 ])))
    (draw-filled-hex pq 30 col)))

(defn draw-hex-sides
  "Given an axial hex vector [p q], draw lines corresponding to particular sides of the
  hexagon represented as [:ne :e :se :sw :w :nw] directions."
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
  ;; (q/frame-rate 1)
  (q/frame-rate 30)
  ;; (q/color-mode :hsb)
  {:board {}})

(defn update [state]
  (let [mouse-xy [(q/mouse-x) (q/mouse-y)]
        mouse-hex (pixel->nearest-hex HEX-SIZE mouse-xy) ]
    (-> state
        (assoc :hovered mouse-hex))))


;; (ann decode-piece [Char -> Keyword])
(defn decode-piece [raw-key]
  (when (#{\a \b \g \l \m \p \q \s} (first (string/lower-case raw-key)))
    (let [upper? (Character/isUpperCase raw-key)
          color  (if upper? \b \w)]
      (keyword (str color (string/upper-case raw-key))))))

(comment
  (decode-piece \A)
  (decode-piece \a)
  (decode-piece \l)
  )

(defn on-key-typed [state evt]
  ; TODO: numerical indices and quantity limits on pieces based on current board state
  (println "key typed: " state evt)
  (assoc state :next-placement (decode-piece (:raw-key evt))))

(defn on-mouse-clicked [state  {:keys [x y button] :as evt}]
  (println "mouse clicked" state evt)
  (case button
    :left (if-let [piece (:next-placement state)]
            (let [pq (pixel->nearest-hex HEX-SIZE [x y])]
              (if (nil? (piece-at-position state pq))
                (insert-piece-at-position state piece pq)
                (assoc state :selected pq)))
            state)
    state))

(defn draw [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 190)
  ; Set circle color.
  ;; (q/fill (:color state) 255 255)
  (q/stroke-weight 1)
  (q/stroke (q/color 0 0 255))
  (draw-hex-grid state)
  (doseq [[coords piece] (:pieces state)]
    (render-piece coords piece))
  (if-let [selected-hex (:selected state)]
    (draw-hex-outline selected-hex 2 5 (q/color 255 10 10 240)))
  (if-let [mouse-hex (:hovered state)]
    (draw-filled-hex mouse-hex 1 (q/color 255 100 10 200))))


(defn go []
  (q/defsketch hello-quil
    :title "Hive Prototype"
    :size [SKETCH-WIDTH SKETCH-HEIGHT]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update is called on each iteration before draw is called.
    ; It updates sketch state.
    :update update
    :draw draw
    :key-typed on-key-typed
    :mouse-clicked on-mouse-clicked
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))

(comment
  (go)
  (for [n (range 1 16)] (cond (zero? (mod n 15)) "FizzBuzz" (zero? (mod n 3)) "Fizz" (zero? (mod n 5)) "Buzz" :else n))
  )

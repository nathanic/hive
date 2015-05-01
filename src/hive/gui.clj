(ns hive.gui
  ;; (:use [hive hex-grid game util])
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [hive.game :as game]
            [hive.game-server :as server]
            [hive.util :refer [dissoc-in]]
            ))

; http://quil.info/api
; https://github.com/quil/quil/wiki/Functional-mode-(fun-mode)

; TODO: more client-servery separation between this and the game module
; mock out some kind of (send-move!) type deal, and all state changes pass through that
; remove all referenes to hive.game in here
; maybe have a .cljx file with some accessors and helpers for this struct though

; need to enforce turns, and which
(def SKETCH-WIDTH 1024)
(def SKETCH-HEIGHT 800)

(def HEX-WIDTH 100) ; in pixels
(def HEX-HEIGHT (grid/hex-height-from-width HEX-WIDTH))
(def HEX-SIZE (grid/hex-size-from-width HEX-WIDTH))

(defn piece-at-position [state pq]
  (game/top-piece-at-pos (:board (:game state)) pq))

;(ann place-piece-at-position [State Piece Point -> State])
(defn place-piece-at-position [state piece pq]
  ; TODO: account for this request taking time
  ; and possibly failing
  ; need to simulate both
  ; maybe do a core async thing or something
  (if-let [g' (server/send-move (:id (:game state))
                                {:piece piece
                                 :position pq})]
    (assoc state
           :next-placement nil
           :game g')
    (do
      (println "failed to send move!!!")
      state)))


(defn deselect-piece [state]
  (dissoc state :selected :valid-moves))

(defn select-piece [state pq]
  (log/info "select piece" state pq)
  (if (= (:selected state) pq)
    (deselect-piece state)
    (let [{:keys [board possible-moves]} (:game state)]
      (assoc state
             :selected pq
             :highlight-cells (->> (game/top-piece-at-pos board pq)
                                possible-moves
                                (map :position))
             ;; :valid-moves (game/calculate-moves (:board state) pq)
             ))))

#_(defn remove-piece-at-position [state pq]
  (update-in state [:board] game/remove-top-piece-at-pos pq))

; TODO: inset a lil bit
(defn filled-hex [pq color]
  (q/fill color)
  (q/no-stroke)
  (let [[x y]  (grid/axial->pixel HEX-SIZE pq)
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
   (let [[x y]  (grid/axial->pixel hex-size pq) ; center coordinates
         h      (grid/hex-height-from-size (- hex-size inset))
         h÷2    (/ h 2)
         h÷4    (/ h 4)
         w÷2    (/ (grid/hex-width-from-size (- hex-size inset)) 2) ]
     [[ x          (- y h÷2)] ; n
      [ (+ x w÷2)  (- y h÷4)] ; ne
      [ (+ x w÷2)  (+ y h÷4)] ; se
      [ x          (+ y h÷2)] ; s
      [ (- x w÷2)  (+ y h÷4)] ; sw
      [ (- x w÷2)  (- y h÷4)] ; nw
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
   :grasshopper [10 250 10]
   :ladybug [250 10 10]
   :mosquito [100 100 100]
   :pillbug [10 250 250]
   :queen-bee [250 250 10]
   :spider [188 143 143]
   })


(def PIECE-INSET 5)

(comment
  (def piece :wG2)
  (apply q/color (get SPECIES->COLOR (game/piece->species piece) [0 0 0]))
  )

(defn render-piece [pq piece]
  (let [col (apply q/color (get SPECIES->COLOR (game/piece->species piece) [0 0 0]))]
    (draw-filled-hex pq PIECE-INSET (apply q/color (get PLAYER->COLOR (game/piece->player piece) [ 0 255 0 ])))
    (draw-filled-hex pq 30 col)))

(defn draw-hex-sides
  "Given an axial hex vector [p q], draw lines corresponding to particular sides of the
  hexagon represented as [:ne :e :se :sw :w :nw] directions."
  [[p q] & dirs]
  (doseq [dir (or dirs grid/DIRS)]
    (let [[x y]  (grid/axial->pixel HEX-SIZE [p q])
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
    (doseq [pq (grid/axial-coords-covering-rect HEX-SIZE win-w win-h)]
      (draw-hex-sides pq))))

(comment
  (count (set (grid/axial-coords-covering-rect HEX-SIZE SKETCH-WIDTH SKETCH-HEIGHT)))
  (grid/axial->pixel HEX-SIZE [0 1])
  (grid/axial->pixel HEX-SIZE [1 1])
  (grid/axial->pixel HEX-SIZE [2 0])
  )

(defn setup []
  ;; (q/frame-rate 1)
  (q/frame-rate 15)
  ;; (q/color-mode :hsb)
  {:game (server/create-game)
   })

(defn update [state]
  (let [mouse-xy [(q/mouse-x) (q/mouse-y)]
        mouse-hex (grid/pixel->nearest-hex HEX-SIZE mouse-xy) ]
    (-> state
        (assoc :hovered mouse-hex))))


;; (ann decode-piece [Char -> Keyword])
(defn decode-piece [raw-key]
  (when (#{\a \b \g \l \m \p \q \s} (first (string/lower-case raw-key)))
    (let [upper? (Character/isUpperCase raw-key)
          color  (if upper? \b \w)]
      (keyword (str color (string/upper-case raw-key))))))

(defn keypress->species [raw-key]
  (get game/LETTER->SPECIES (-> raw-key first string/upper-case)))

(comment
  (decode-piece \A)
  (decode-piece \a)
  (decode-piece \l)
  )

#_(defn valid-state? [state]
  ; each piece should appear either in :board or :unplaced,
  ; and no other things should be in those
  ; TODO: ensure no repeats of pieces
  (= (clojure.set/union (vals (:board state)) (:unplaced state))
     game/PIECES))

(defn choose-next-placement [state raw-key]
  ; choose from the unplaced pool
  (let [player-letter ({:white \w, :black \b} (:turn (:game state)))
        species-letter (#{\A \B \G \L \M \P \Q \S} (first (string/upper-case raw-key)))
        prefix (str player-letter species-letter)] 
    (if species-letter
      (-> (filter (fn [pc] (.startsWith (name pc) prefix))
                  (-> state :game :unplaced))
          sort
          first)
      nil)))

(comment
  (name (decode-piece \A))
  (choose-next-placement {:game {:unplaced #{:wQ :bQ :bA2 :bA3 :wS2 :wG1 :wG2 :wG3}}}
                         \p
                         ))

(defn on-key-typed [state evt]
  ; TODO: numerical indices and quantity limits on pieces based on current board state
  (println "key typed: " state evt)
  (if-let [next-placement (choose-next-placement state (:raw-key evt))]
    (assoc state :next-placement next-placement)))

; what all actions do we need?
; pick an unplaced piece
  ; need to make a sidebar for that
; drop an unplaced piece
; pick a placed piece to move
; place a piece (if holding)
  ; use different validation rules for unplaced and moving pieces

(defn on-mouse-clicked [state  {:keys [x y button] :as evt}]
  (println "mouse clicked" state evt)
  (case button
    :left (let [pq       (grid/pixel->nearest-hex HEX-SIZE [x y])
                target   (piece-at-position state pq)
                selected (:selected state)
                ]
            (log/info "left click pq" pq "target" target "selected" selected)
            (cond
              (and selected (nil? target))
              state ; TODO: move the piece

              :else
              (if target
                (select-piece state pq)
                (if-let [piece (:next-placement state)]
                  (place-piece-at-position state piece pq)
                  state)))
            )
    :right (let [pq     (grid/pixel->nearest-hex HEX-SIZE [x y])
                 target (piece-at-position state pq) ]
             #_(if target
               (remove-piece-at-position state pq)
               #_(assoc state :selected pq)
               #_(if-let [piece (:next-placement state)]
                   (place-piece-at-position state piece pq)
                   state)))

    state))

(defonce state* (atom nil))
(comment
  (prn state*)
  (def board (:board @state*))
  (prn board)
  )

(defn placable-pieces
  [state]
  (let [{:keys [unplaced turn], :as g} (:game state)]
    (filter
      (fn [piece] (= turn (game/piece->player piece)))
      unplaced)))

(comment
  (placable-pieces {:game {:turn :white
                           :unplaced #{:wQ :wS1 :bQ :bA2 :wA3}
                           }})
  (placable-pieces @state*)
  )

(defn draw [state]
  (let [{:keys [highlight-cells selected hovered game next-placement]} state
        {:keys [turn board]} game ]
    (q/background (if (= :white turn)
                    190
                    100))
    (q/stroke-weight 1)
    (q/stroke (q/color 0 0 255))
    (draw-hex-grid state)
    (doseq [[coords pieces] board
            piece pieces]
      (render-piece coords piece))
    (when selected
      (draw-hex-outline selected 2 5 (q/color 255 10 10 240)))
    (when hovered
      (draw-filled-hex hovered 1 (q/color 255 100 10 200)))
    (doseq [pq highlight-cells]
      (draw-filled-hex pq 1 (q/color 100 240 10 150)))

    ; filthy lame hax
    (when next-placement
      (render-piece [0 1] next-placement))
    (q/fill (q/color 20 20 00))
    (q/text (str "it is " (name (or turn "nobody")) "'s turn.  "
                 "choose piece to place: ["
                 (apply sorted-set
                        (map (comp second name game/piece->species)
                             (placable-pieces state)))
                 "]")
            20 20)))

(defn go []
  (q/defsketch hive-applet
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
    :middleware [m/fun-mode])
  (def state* (quil.applet/with-applet hive.gui/hive-applet (quil.core/state-atom)))
  )

(comment
  (go)
  (clojure.pprint/pprint @state*)
  (keys @server/games*)
  (swap! state* assoc :highlight-cells nil)

  )

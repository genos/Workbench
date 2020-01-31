(ns sketches.perlin-flow
  (:require [quil.core :as q]
            [quil.middleware :as middleware]))

(def body (.-body js/document))
(def w (.-clientWidth body))
(def h (.-clientHeight body))

(def palette
  {:name      "purple haze"
   :backgroud [10 10 10]
   :colors    [[32 0 40]
               [82 15 125]
               [99 53 126]
               [102 10 150]
               [132 26 200]
               [165 32 250]
               [196 106 251]]})

(defn particle
  "A particle map"
  [id]
  {:id        id
   :vx        1
   :vy        1
   :size      3
   :direction 0
   :x         (q/random w)
   :y         (q/random h)
   :color     (rand-nth (:colors palette))})

(def noise-zoom
  "Noise zoom level."
  0.005)

(defn direction
  "Next direction in [0, 2π] based on perlin noise at position (x, y) and id"
  [x y id]
  (* 2 Math/PI
    (+ (q/noise (* x noise-zoom) (* y noise-zoom))
      (* 0.2 (q/noise (* x noise-zoom) (* y noise-zoom) (* id noise-zoom))))))

(defn position
  "Calculate next position based on current, speed, and an upper-limit"
  [current speed upper-limit]
  (mod (+ current speed) upper-limit))

(defn velocity
  "Calculate next velocity based on current and added delta."
  [current delta]
  (/ (+ current delta) 2))

(defn sketch-setup
  "The inital state to use for the update-render loop"
  []
  (apply q/background (:backgroud palette))
  (map particle (range 0 2000)))

(defn sketch-update
  "Step the particle state"
  [particles]
  (map (fn [p]
         (assoc p
                :x         (position (:x p) (:vx p) w)
                :y         (position (:y p) (:vy p) h)
                :direction (direction (:x p) (:y p) (:id p))
                :vx        (velocity (:vx p) (Math/cos (:direction p)))
                :vy        (velocity (:vy p) (Math/sin (:direction p)))))
       particles))

(defn sketch-draw
  "Draw current state on canvas. Called on each itera fter sketch-update."
  [particles]
  (q/no-stroke)
  (doseq [p particles]
    (apply q/fill (conj (:color p) 10))
    (q/ellipse (:x p) (:y p) (:size p) (:size p))))

(defn create [canvas]
  (q/sketch
    :host canvas
    :size [w h]
    :draw #'sketch-draw
    :setup #'sketch-setup
    :update #'sketch-update
    :middleware [middleware/fun-mode]
    :settings (fn [] (q/random-seed 1729) (q/noise-seed 1729))))

(defonce sketch (create "sketch"))

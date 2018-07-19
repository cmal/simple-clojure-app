(ns simple-clojure-app.ants
  (:require [clojure.repl :refer [source doc]])
  (:import 
   [java.awt Color Graphics Dimension]
   [java.awt.image BufferedImage]
   [javax.swing JPanel JFrame]))

(def dim 80)
(def nants-sqrt 7)
(def food-places 35)
(def food-range 100)
(def pher-scale 20.0)
(def food-scale 30.0)
(def evap-rate 0.99)
(def animation-sleep-ms 100)
(def ant-sleep-ms 10)
(def evap-sleep-ms 1000)
(def running true)

(defstruct cell :food :pher)

(def world
  (apply vector
         (map (fn [_]
                (apply vector
                       (map (fn [_]
                              (ref (struct cell 0 0)))
                            (range dim))))
              (range dim))))


(defn place [[x y]]
  (-> world (nth x) (nth y)))

(defstruct ant :dir)

(defn create-ant
  "Create an ant at given location, returning an ant agent on the location."
  [location direction]
  (sync nil
        (let [the-place (place location)
              the-ant (struct ant direction)]
          (alter the-place assoc :ant the-ant)
          (agent location))))

(def home-offset (/ dim 4))
(def home-range (range home-offset (+ nants-sqrt home-offset)))

(defn setup
  "Places initial food and ants, returns seq of ant agents."
  []
  (sync nil
        (dotimes [i food-places]
          (let [p (place [(rand-int dim) (rand-int dim)])]
            (alter p assoc :food (rand-int food-range))))
        (doall
         (for [x home-range
               y home-range]
           (do
             (alter (place [x y]) assoc :home true)
             (create-ant [x y] (rand-int 8)))))))

(defn bound
  "Returns given n, wrapped into range 0-b"
  [b n]
  (let [n (rem n b)]
    (if (neg? n)
      (+ n b)
      n)))

(def direction-delta {0 [0 -1]
                      1 [1 -1]
                      2 [1 0]
                      3 [1 1]
                      4 [0 1]
                      5 [-1 1]
                      6 [-1 0]
                      7 [-1 -1]})

(defn delta-location
  "Returns the location one step in the given direction. Note the
  world is a torus."
  [[x y] direction]
  (let [[dx dy] (direction-delta (bound 8 direction))]
    [(bound dim (+ x dx)) (bound dim (+ y dy))]))

(defn turn
  "Turns the ant at the location by the given amount."
  [loc amt]
  {:pre [vector? loc]
   :post [vector?]}
  (dosync
   (let [p (place loc)
         ;; ant (:ant @p)
         ]
     (alter p update-in [:ant :dir] #(bound 8 (+ amt %)))
     #_(alter p assoc :ant (assoc ant :dir (bound 8 (+ (:dir ant) amt)))))
   loc))

(defn move
  "Moves the ant in the direction it is heading. Must be called in a
  transaction that has verified the way is clear."
  [startloc]
  {:pre [vector? startloc]
   :post [vector?]}
  (let [oldp (place startloc)
        ant (:ant @oldp)
        newloc (delta-location startloc (:dir ant))
        newp (place newloc)]
    ;; move the ant
    (alter newp assoc :ant ant)
    (alter oldp dissoc :ant)
    ;; leave pheromone trail
    (when (and (not (:home @oldp)) (:food @oldp))
      (alter oldp update :pher inc)
      #_(alter oldp assoc :pher (inc (:pher @oldp))))
    newloc))

(defn take-food
  "Takes one food from current location. Must be called in a
  transaction that has verified there is food available."
  [loc]
  {:pre [vector? loc]
   :post [vector?]}
  (let [p (place loc)
        ant (:ant @p)]
    (alter p assoc
           :food (dec (:food @p))
           :ant (assoc ant :food true))
    loc))

(defn drop-food
  "Drops food at current location. Must be called in a transaction
  that has verified the ant has food."
  [loc]
  {:pre [vector? loc]
   :post [vector?]}
  (let [p (place loc)
        ant (:ant @p)]
    (alter p assoc
           :food (inc (:food @p))
           :ant (dissoc ant :food))
    loc))

(defn rank-by
  "Returns a map of xs to their 1-based rank when sorted by keyfn."
  [keyfn xs]
  (let [sorted (sort-by (comp float keyfn) xs)]
    (reduce (fn [ret i]
              (assoc ret (nth sorted i) (inc i)))
            {} (range (count sorted)))))

(defn wrand
  "Given a vector of slice sizes, returns the index of a slice given a
  random spin of a roulettel wheel with compartments proportional to
  slices."
  [slices]
  (let [total (reduce + slices)
        r (rand total)]
    (loop [i 0
           sum 0]
      (if (< r (+ (slices i) sum))
        i
        (recur (inc i) (+ (slices i) sum))))))

(defn behave
  "The main function ofr the ant agent."
  [loc]
  {:pre [vector? loc]
   :post [vector?]}
  (let [p (place loc)
        ant (:ant @p)
        ahead (place (delta-location loc (:dir ant)))
        ahead-left (place (delta-location loc (dec (:dir ant))))
        ahead-right (place (delta-location loc (inc (:dir ant))))
        places [ahead ahead-left ahead-right]]
    ;; Old way of Java interop: (. Thread (sleep ant-sleep-ms))
    ;; New idiomatic way is,
    (Thread/sleep ant-sleep-ms)
    (dosync
     (when running
       (send-off *agent* #'behave))
     (if (:food ant)
       ;; Then take food home:
       (cond
         (:home @p) (-> loc drop-food (turn 4))
         (and (:home @ahead) (not (:ant @ahead))) (move loc)
         :else (let [ranks (merge-with
                            +
                            (rank-by (comp #(if (:home %) 1 0) deref) places)
                            (rank-by (comp :pher deref) places))]
                 (([move
                    #(turn % -1)
                    #(turn % 1)]
                   (wrand [(if (:ant @ahead) 0 (ranks ahead))
                           (ranks ahead-left)
                           (ranks ahead-right)]))
                  loc)))
       ;; No food, go foraging
       (cond
         (and (pos? (:food @p)) (not (:home @p))) (-> loc take-food (turn 4))
         (and (pos? (:food @ahead)) (not (:home @ahead)) (not (:ant @ahead))) (move loc)
         :else (let [ranks (merge-with
                            +
                            (rank-by (comp :food deref) places)
                            (rank-by (comp :pher deref) places))]
                 (([move #(turn % -1) #(turn % 1)]
                   (wrand [(if (:ant @ahead) 0 (ranks ahead))
                           (ranks ahead-left) (ranks ahead-right)]))
                  loc)))))))

(defn evaporate
  "Causes all the pheromones to evaporate a bit."
  []
  (dorun
   (for [x (range dim)
         y (range dim)]
     (dosync
      (let [p (place [x y])]
        (alter p update :pher #(* evap-rate %))
        #_(alter p assoc :pher (* evap-rate (:pher @p))))))))



;; ==== UI ==== 

;; pixels per world cell
(def scale 5)

(defn fill-cell [#^Graphics g x y c]
  (doto g
    (.setColor c)
    (.fillRect (* x scale) (* y scale) scale scale)))

(defn render-ant [ant #^Graphics g x y]
  (let [black (.getRGB (Color. 0 0 0 255))
        gray (.getRGB (Color. 100 100 100 255))
        red (.getRGB (Color. 255 0 0 255))
        [hx hy tx ty] ({0 [2 0 2 4] ; Up/North pointing
                        1 [4 0 0 4]
                        2 [4 2 0 2]
                        3 [4 4 0 0]
                        4 [2 4 2 0]
                        5 [0 4 4 0]
                        6 [0 2 4 2]
                        7 [0 0 4 4]}
                       (:dir ant))]
    (doto g
      (.setColor (if (:food ant)
                   (Color. 255 0 0 255)
                   (Color. 0 0 0 255)))
      (.drawLine (+ hx (* x scale)) (+ hy (* y scale))
                 (+ tx (* x scale)) (+ ty (* y scale))))))


(defn render-place [g p x y]
  (when (pos? (:pher p))
    (fill-cell g x y (Color. 0 255 0
                          (int (min 255 (* 255 (/ (:pher p) pher-scale)))))))
  (when (pos? (:food p))
    (fill-cell g x y (Color. 255 0 0
                          (int (min 255 (* 255 (/ (:food p) food-scale)))))))
  (when (:ant p)
    (render-ant (:ant p) g x y)))


(defn render [g]
  (let [v (dosync (apply vector (for [x (range dim)
                                      y (range dim)]
                                  @(place [x y]))))
        img (BufferedImage. (* scale dim) (* scale dim)
                 BufferedImage/TYPE_INT_ARGB)
        bg (.getGraphics img)]
    ;; First paint everything white, on the bg instance;
    (doto bg
      (.setColor Color/white)
      (.fillRect 0 0 (.getWidth img) (.getHeight img)))
    (dorun
     (for [x (range dim)
           y (range dim)]
       (render-place bg (v (+ (* x dim) y)) x y)))
    ;; Draw the home space of the ants:
    (doto bg
      (.setColor Color/blue)
      (.drawRect (* scale home-offset) (* scale home-offset)
                 (* scale nants-sqrt) (* scale nants-sqrt)))
    (.drawImage g img 0 0 nil)
    (.dispose bg))) ;; Finished using Graphics object, release it

(def panel
  (doto
      (proxy [JPanel] [] (paint [g] (render g)))
    (.setPreferredSize
     (Dimension.
      (* scale dim)
      (* scale dim)))))

(def frame
  (doto (new JFrame)
    (.add panel)
    .pack
    .show))

(def animator (agent nil))

(defn animation [x]
  (when running
    (send-off *agent* #'animation))
  (.repaint panel)
  (Thread/sleep animation-sleep-ms)
  nil)

(def evaporator (agent nil))

(defn evaporation [x]
  (when running
    (send-off *agent* #'evaporation))
  (evaporate)
  (Thread/sleep evap-sleep-ms)
  nil)

;; *agent*

(do
  ;; (load-file "./literate-ants.clj")
  (prn "Start...")
  (def ants (setup))
  (send-off animator animation)
  (dorun (map #(send-off % behave) ants))
  (send-off evaporator evaporation))

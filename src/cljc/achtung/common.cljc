(ns achtung.common
  "Namespace containing the game engine, game logic and game state. Does not render anything, just updates game state, and signals its updated state through a core async channel."
  (:require
   [taoensso.timbre :refer [debug info error]]
   [clojure.core.async :as async]))

(def pi 3.141592653589793)

(def pi2 (* 2 pi))

(def QUIT-KEY :q)

(def RESTART-KEY :r)

(defn random-direction
  []
  (* pi2 (Math/random)))

(defn random-start-point
  [[res-x res-y]]
  [(int (* (Math/random) res-x))
   (int (* (Math/random) res-y))])

(def speed-multiplier 1)
(def turning-speed-multiplier 1)

(defn initial-game-state
  [players resolution]
  {:game-over-players #{}
   ;;the degree where the head is pointing to. 0 is north, 90 is east, 180 south, 270 west
   :players           (zipmap (map :name players)
                              {:direction (random-direction)
                               :trail     [(random-start-point resolution)]})
   :view              :menu ;; or shows a :game, or a :score panel when game is over
   })

(defn create-clock
  "returns a chan that ticks 60 times per second, returning the number of frames that have passed"
  [kill-chan]
  (let [c (async/chan)]
    (async/go-loop [s 0]
      (let [timeout (async/timeout 16)]
        (async/alt!
          kill-chan
          ([_]
           (async/close! c))
          timeout
          ([_]
           (async/>! c ::tick)
           (recur (inc s))))))
    c))

(defmulti process-signal :type)

(defn round0
  "Round a double to an int"
  [d]
  (let [precision 0
        factor    (Math/pow 10 precision)]
    (int (/ (Math/round (* d factor)) factor))))

(defn progress-trail
  "`direction` is a radiant, ie. between 0 (east) and 2*pi.
  Trail is a vector of [x y] coords (vectors),where the last
  entry is the [x y] coord of the head of the snake."
  [current-trail direction]
  (assert (vector? current-trail) "Current-trail should be a vector")
  (let [[x y] (peek current-trail)
        dx    (* speed-multiplier (Math/cos direction))
        dy    (* speed-multiplier (Math/sin direction))
        x'    (round0 (+ x dx))
        y'    (round0 (+ y dy))]
    (conj current-trail [x' y'])))

(defmethod process-signal
  ::player-move
  [state [_ direction {:keys [name] :as player}]]
  (update-in state [:players name :direction]
             (fn [current-dir]
               ;;calc delta
               (+ current-dir
                  (if (= :left direction)
                    (* -1 turning-speed-multiplier)
                    turning-speed-multiplier)))))

(defmethod process-signal
  ::start
  [state [_ players resolution]]
  (->
   (initial-game-state players resolution)
   (assoc :view :game)))

(defmethod process-signal
  ::stop
  [state [_ players resolution]]
  (initial-game-state players resolution))

(defn update-trails
  [state]
  (update state :players
          (fn [m]
            (->> m
                 (map
                  (fn [[n {dir :direction :as r}]]
                    [n (update r :trail progress-trail dir)]))
                 (into {})))))
(defn crosses?
  [[[x1 y1]   [x2 y2]   :as segment1]
   [[xt1 yt1] [xt2 yt2] :as segment2]]
  (or
   (and (<= x1 ))
   ;;or check with reverse ordering of args
   (crosses? segment2 segment1)))

(defn trail-collides?
  "Collision of a head with a trail occurs when the line between
  the last two points of the head crosses any point of the trail"
  [head-segment [s1 s2 & rest]]
  (if (nil? s2)
    false
    (or
     (crosses? head-segment [s1 s2])
     (trail-collides? head-segment rest))))


(comment
  (trail-collides? [[0 0] [0 1]]
                   [[0 0] [0 1] [0 2] [0 3]])


  )

(defn collides?
  "Returns true if [x y] coord is the same as _any_ coord in any of the trails
   OR if it collides with the boundary of the screen."
  [[x y :as coord] trails [res-x res-y :as resolution]]
  (println  "coord" coord)
  (println "trails" trails)
  (println "reso " resolution)
  (or
   (> x res-x)
   (< x 0)
   (> y res-y)
   (< y 0)
   (some
    (partial trail-collides? coord)
    trails)))

(defn mark-colliding-players-as-game-over
  [{:keys [players] :as state} resolution]
  (let [trails            (map (fn [[n {:keys [trail]}]]
                                 trail)
                               players)
        game-over-players (->> players
                               (filter (fn [[name {:keys [trail]}]]
                                         (let [head (peek trail)]
                                           (collides? head trails resolution))))
                               keys
                               set)]
    (-> state
        (update :game-over-players
                (fn [ps]
                  (apply conj ps game-over-players)))
        (update :players
                (fn [ps]
                  (apply dissoc ps game-over-players))))))

(defmethod process-signal
  ::progress-players
  [state [_ resolution]]
  ;;updates the :trails of all players, based on their directions
  ;;calculates collisions of the remaining players
  ;;moves crashed players to the game over list
  ;;if it's game over, change :view
  (-> state
      update-trails
      (mark-colliding-players-as-game-over resolution)))

(defmethod process-signal
  :default
  [state _]
  (debug :process-signal :default)
  ;;noop
  state)

(def key-codes
  {82 "r"
   81 "q"
   90 "z"
   88 "x"
   78 "n"
   77 "m"})

(defn build-game-event
  "Returns a map with a key ::type signaling the event that happened,
   with addition context that happened."
  [{:keys [players key-event] :as cfg}]
  (debug :key-event key-event )
  (let [player-left  (->> players
                          (filter (fn [{left :left}]
                                    (= left key-event)))
                          first)
        player-right (->> players
                          (filter (fn [{right :right}]
                                    (= right key-event)))
                          first)
        stop         (= QUIT-KEY key-event)
        start        (= RESTART-KEY key-event)]
    (merge
     cfg
     (cond
       player-left  {::type      :player-move
                     ::direction :left
                     ::player    player-left}
       player-right {::type      :player-move
                     ::direction :right
                     ::player    player-right}
       start        {::type :start}
       stop         {::type :stop}))))

(defn game
  [{:keys [players resolution] :as cfg}] ;;vec of player configs
  (let [kill-chan     (async/chan)
        clock-chan    (create-clock kill-chan)
        key-chan      (async/chan) ;; all input from the keyboard is put on this channel
        render-chan   (async/chan)]
    (async/go-loop [game-state {}
                    signal-buffer []]
      (async/alt!
        kill-chan
        ([_]
         (do
           (async/close! clock-chan)
           (async/close! key-chan)))

        clock-chan
        ([_]
         ;;tick tock
         (when (seq signal-buffer)
           (debug signal-buffer))
         (let [game-state' (reduce process-signal game-state signal-buffer)]
           (async/>! render-chan game-state')
           (recur game-state' [])))

        key-chan
        ([e]
         (let [game-event (build-game-event (assoc cfg :key-event e))]
           (recur game-state (conj signal-buffer game-event))))))
    ;; return this, so that the caller can get a handle on the channels
    ;; to feed it with keyboard events
    ;; or kill the game
    ;; thus provide an input
    {:key-chan    key-chan
     :render-chan render-chan
     :kill-chan   kill-chan}))

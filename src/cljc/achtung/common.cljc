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

(def speed-multiplier 3)
(def turning-speed-multiplier 0.2)

(defn key-by
  [f m]
  (->> m
       (group-by f)
       (map (fn [[k rs]]
              [k (first rs)]))
       (into {})))

(declare progress-trail)

(defn initial-game-state
  [{:keys [players resolution]}]
  {:game-over-players #{}
   ;;the degree where the head is pointing to. 0 is north, 90 is east, 180 south, 270 west
   :players           (->> players
                           (map (fn [p]
                                  (let [rd           (random-direction)
                                        rp           (random-start-point resolution)
                                        head-segment (progress-trail [rp] rd)]
                                    (merge p
                                           {:direction rd
                                            :trail     head-segment}))))
                           (key-by :name))
   :view              :game})

(defn create-clock
  "returns a chan that ticks 60 times per second, returning the number of frames that have passed"
  [kill-chan]
  (let [c (async/chan)]
    (async/go-loop [s 0]
      (let [timeout (async/timeout 16)]
        (async/alt!
          kill-chan
          ([_]
           (debug ::kill)
           (async/close! c))
          timeout
          ([_]
           (async/>! c ::tick)
           (recur (inc s))))))
    c))

(defmulti process-signal
  (fn [state signal]
    (::type signal)))

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
  [state {::keys [direction player] :as o}]
  (update-in state [:players (:name player) :direction]
             (fn [current-direction]
               (+ current-direction
                  (if (= :left direction)
                    (* -1 turning-speed-multiplier)
                    turning-speed-multiplier)))))

(defmethod process-signal
  ::start
  [state cfg]
  (->
   (initial-game-state cfg)
   (assoc :view :game)))


(defmethod process-signal
  ::stop
  [{:keys [players resolution] :as state} cfg]
  (->
   (initial-game-state cfg)
   (assoc :view :menu)))

(defn update-trails
  [state]
  (update state
          :players
          (fn [players]
            (->> players
                 (map
                  (fn [[n {:keys [direction] :as player}]]
                    [n (update player :trail progress-trail direction)]))
                 (into {})))))

(defn orientation
  "Returns the orientation of three points (p, q, r) in a 2D plane.
  -1 -> counterclockwise
   1 -> clockwise
   0 -> collinear"
  [[px py] [qx qy] [rx ry]]
  (let [val (- (* (- qy py) (- rx qx))
               (* (- qx px) (- ry qy)))]
    (cond
      (> val 0) 1
      (< val 0) -1
      :else 0)))

(defn on-segment?
  "Checks if point r lies on the line segment pq."
  [[px py] [qx qy] [rx ry]]
  (and (<= (min px qx) rx (max px qx))
       (<= (min py qy) ry (max py qy))))

(defn crosses?
  "Checks if two line segments segment1 and segment2 intersect."
  [[[x1 y1] [x2 y2] :as segment1]
   [[xt1 yt1] [xt2 yt2] :as segment2]]
  (let [o1 (orientation [x1 y1] [x2 y2] [xt1 yt1])
        o2 (orientation [x1 y1] [x2 y2] [xt2 yt2])
        o3 (orientation [xt1 yt1] [xt2 yt2] [x1 y1])
        o4 (orientation [xt1 yt1] [xt2 yt2] [x2 y2])]
    (or
      ;; General case: orientations are different
      (and (not= o1 o2) (not= o3 o4))

      ;; Special cases: collinear points lying on segments
      (and (= o1 0) (on-segment? [x1 y1] [x2 y2] [xt1 yt1]))
      (and (= o2 0) (on-segment? [x1 y1] [x2 y2] [xt2 yt2]))
      (and (= o3 0) (on-segment? [xt1 yt1] [xt2 yt2] [x1 y1]))
      (and (= o4 0) (on-segment? [xt1 yt1] [xt2 yt2] [x2 y2])))))

(defn trail-collides?
  "Collision of a head with a trail occurs when the line between
  the last two points of the head crosses any point of the trail"
  [head-segment [s1 s2 & rest]]
  (if (nil? s2)
    false
    (or
     (crosses? head-segment [s1 s2])
     (trail-collides? head-segment rest))))

(defn collides?
  "Returns true if [x y] coord is the same as _any_ coord in any of the trails
   OR if it collides with the boundary of the screen."
  [[[x y :as head-coord] [x2 y2 :as neck-coord] :as head-segment] trails [res-x res-y :as resolution]]
  (or
   (> x res-x)
   (< x 0)
   (> y res-y)
   (< y 0)
   (some
    (partial trail-collides? head-segment)
    trails)))

(defn mark-colliding-players-as-game-over
  [{:keys [players resolution] :as state}]
  (let [trails            (map (fn [[n {:keys [trail]}]]
                                 trail)
                               players)
        game-over-players (->> players
                               (filter (fn [[name {:keys [trail]}]]
                                         (let [h1 (peek trail)
                                               h2 (drop-last trail)]
                                           (collides? [h2 h1] trails resolution))))
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
  [state _]
  ;;updates the :trails of all players, based on their directions
  ;;calculates collisions of the remaining players
  ;;moves crashed players to the game over list
  ;;if it's game over, change :view
  (try
    (-> state
        update-trails
        #_mark-colliding-players-as-game-over)
    (catch js/Error e
      (debug "Caught err: " state)
      state)))

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
       player-left  {::type      ::player-move
                     ::direction :left
                     ::player    player-left}
       player-right {::type      ::player-move
                     ::direction :right
                     ::player    player-right}
       start        {::type ::start}
       stop         {::type ::stop}))))

(defn game
  [{:keys [players resolution] :as cfg}] ;;vec of player configs
  (let [kill-chan   (async/chan)
        clock-chan  (create-clock kill-chan)
        key-chan    (async/chan) ;; all input from the keyboard is put on this channel
        render-chan (async/chan)]
    (async/go-loop [game-state (-> (initial-game-state cfg)
                                   (assoc :view :menu))
                    signal-buffer []]
      (async/alt!
        kill-chan
        ([_]
         (do
           (async/close! clock-chan)
           (async/close! key-chan)))

        clock-chan
        ([_]
         (let [buffer      (conj (vec signal-buffer) {::type ::progress-players})
               game-state' (reduce process-signal game-state buffer)]
           (async/>! render-chan game-state')
           (recur game-state' [])))

        key-chan
        ([e]
         (debug :key-chan e)
         (let [game-event (build-game-event (assoc cfg :key-event e))]
           (debug :key-chan e game-event)
           (recur game-state (conj signal-buffer game-event))))))
    ;; return this, so that the caller can get a handle on the channels
    ;; to feed it with keyboard events
    ;; or kill the game
    ;; thus provide an input
    {:key-chan    key-chan
     :render-chan render-chan
     :kill-chan   kill-chan}))

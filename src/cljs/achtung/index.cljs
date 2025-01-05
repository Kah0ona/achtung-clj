(ns ^:dev/always achtung.index
  (:require
   [achtung.common :as game]
   [goog.dom :as gdom]
   [re-frame.core :as rf]
   [reagent.core :as reagent]
   [reagent.dom :as reagent-dom]
   [clojure.core.async :as async]
   [taoensso.timbre :refer [debug info error]]))


(def resolution [1024 768])

(def players
  [{:name  "Player 1"
    :left  :z
    :right :x}
   {:name  "Player 2"
    :left  :n
    :right :m}])



;; create and start a game, from the common ns
;; provide a rendering function that is used to render the game state to HTML/SVG or whatever
;; also listen to user keyboard events, and put those events into the exposed keyboard-input chan
(defn bind-keypresses
  [f keys]
  (js/document.addEventListener
   "keydown"
   (fn [event]
     (debug :key-press
            (.-keyCode event)
            (get game/key-codes (.-keyCode event)))
     (when-let [kc (get game/key-codes (.-keyCode event))]
       (f (keyword kc))))))

(defn register-keyboard-events
  [players handler-fn]
  (->> players
       (mapcat (juxt :left :right))
       ;;quit and restart
       (concat ["q" "r"])
       (bind-keypresses handler-fn)))

(defn draw-line
  [canvas points color]
  (let [ctx (.getContext canvas "2d")]
    (.beginPath ctx)
    (let [[start-x start-y] (first points)]
      (.moveTo ctx start-x start-y)) ;; Move to the first point
    (doseq [[x y] (rest points)]
      (.lineTo ctx x y)) ;; Draw lines to the remaining points
    (set! (.-strokeStyle ctx) (name color))
    (.stroke ctx))) ;; Render the line

(defn render-game-on-canvas!
  [canvas state]
  (if canvas
    (let  [colors [:red :green :blue :yellow :magenta :orange]]
      (.clearRect (.getContext canvas "2d") 0 0 (.-width canvas) (.-height canvas))
      (->> state
           :players
           (map (comp :trail last))
           (map-indexed
            (fn [idx trail]
              (debug )
              (draw-line canvas trail (get colors idx))))
           doall))
    (debug :NO-CANVAS)))

(defn render!
  [canvas {:keys [view] :as state}]
  (when (= :game view)
    (render-game-on-canvas! canvas state)
    ;;noop else, in other cases we render a React component with scores / menu

    ))

(defn read-render-chan
  "Simply reads from the chan, gathers all other context, and calls render"
  [{:keys [render-chan] :as engine}]
  (assert render-chan)
  (async/go-loop []
    (let [canvas (gdom/getElement "canvas")
          game-state (async/<! render-chan)] ;; Read a value from the channel
      ;; render this for non-canvas components
      (rf/dispatch [::game game-state])
      (render! canvas game-state)
      (recur))))

(defn score-panel
  []
  [:div "scorepanel"])

(rf/reg-event-db
 ::engine
 (fn [db [_ engine]]
   (assoc db ::engine engine)))

(rf/reg-sub
 ::engine
 (fn [db _]
   (::engine db)))

(rf/reg-event-db
 ::game
 (fn [db [_ game]]
   (assoc db ::game game)))

(rf/reg-sub
 ::game
 (fn [db _]
   (::game db)))

(defn menu-panel
  [{:keys [key-chan] :as game-channels}]
  [:div.toolbar
   [:button
    {:on-click #(async/put! key-chan game/RESTART-KEY)} "Start (over)"]])

(defn start-engine!
  []
  (debug :start-engine!)
  (let [engine (game/game {:players    players
                           :resolution resolution})]
    (register-keyboard-events
     players
     (fn [e]
       (debug :key-event e)
       (async/put! (:key-chan engine) e)))
    (read-render-chan engine)
    (rf/dispatch [::engine engine])))

(defn ui
  [cfg]
  (let [game   (rf/subscribe [::game])
        engine (rf/subscribe [::engine])]
    (when-not @game
      (start-engine!))
    (fn [cfg]
      [:div
       (case (:view @game)
         :score [score-panel @game]
         :menu  [menu-panel @engine]
         :game  [:canvas#canvas
                 {:width  (first resolution)
                  :height (last resolution)}]
         [menu-panel @engine])])))

(def last-error (reagent/atom nil))

(defn error-boundary
  [component]
  (reagent/create-class
   {:component-did-catch
    (fn [this e errorInfo]
      (debug "error boundary component-did-catch hit")
      (error "msg: " (.-message e))
      (reset! last-error e))
    :reagent-render
    (fn [component]
      (debug "error boundary hit")
      component)}))

(defn ^:dev/after-load mount
  [el]
  (reagent-dom/render [error-boundary [ui cfg]] el))

(defn start-game
  []
  (debug "Achtung die Kurve started!")
  (mount (js/document.getElementById "app")))

(defn init []
  (start-game))

(comment

  @(rf/subscribe [::game])

  (def engine
    @(rf/subscribe [::engine]))

  (def key-chan (:key-chan engine))

  (async/put! (:kill-chan @engine) "r")

  (async/put! key-chan game/RESTART-KEY)

  key-chan

  (game/initial-game-state
   {:players players})

  )

(ns ^:dev/always achtung.index
  (:require
   [achtung.common :as game]
   [goog.dom :as gdom]
   [reagent.core :as reagent]
   [reagent.dom :as reagent-dom]
   [clojure.core.async :as async]
   [taoensso.timbre :refer [debug info error]]))

;; create and start a game, from the common ns
;; provide a rendering function that is used to render the game state to HTML/SVG or whatever
;; also listen to user keyboard events, and put those events into the exposed keyboard-input chan
(defn bind-keypresses
  [f keys]
  (let [key-codes (->> keys
                       (map (fn [key]
                              (-> key
                                  name
                                  (.charCodeAt 0)
                                  int)))
                       set)]
    (js/document.addEventListener
     "keydown"
     (fn [event]
       (when (get key-codes (.-keyCode event))
         (f (keyword (.-keyCode event))))))))

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
    (.stroke ctx))) ;; Render the line

(defn render!
  [canvas state]
  (let  [colors [:red :green :blue :yellow :magenta :orange]]
    (.clearRect (.getContext canvas "2d") 0 0 (.-width canvas) (.-height canvas))
    (->> state
         :players
         (map (comp :trail last))
         (map-indexed
          (fn [[idx trail]]
            (draw-line canvas trail (get colors idx))))
         doall)))

(defn read-render-chan
  "Simply reads from the chan, gathers all other context, and calls render"
  [{:keys [target render-chan] :as opts}]
  (assert render-chan)
  (let [canvas (gdom/getElement (or target "canvas"))]
    (async/go-loop []
      (let [game-state (async/<! render-chan)] ;; Read a value from the channel
        (render! canvas game-state)
        (recur)))))

(defn ui
  [{:keys [key-chan]}]
  [:button {:on-click #(async/>! key-chan game/RESTART-KEY)} "Start (over)"]
  [:canvas#canvas {:width "1024" :height "768"}])

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

(defn mount
  [game-channels el]
  (reagent-dom/render [error-boundary [ui game-channels]] el))

(def resolution [1024 768])

(def players
  [{:name  "Player 1"
    :left  :z
    :right :x}
   {:name  "Player 2"
    :left  :n
    :right :m}])

(defn start-game
  []
  (let [{:keys [kill-chan key-chan render-chan]
         :as game-channels}
        (game/game {:players players
                    :resolution resolution})
        key-handle-fn (fn [e]
                        (async/>! key-chan e))]
    (mount game-channels (js/document.getElementById "app"))
    (register-keyboard-events players key-handle-fn)
    (read-render-chan game-channels)))


(defn init []
  (js/console.log "Achtung app started!")
  (start-game))


(comment
  (def game
    (game/game {:players players
                :resolution resolution}))

  (:render-chan game)

 )

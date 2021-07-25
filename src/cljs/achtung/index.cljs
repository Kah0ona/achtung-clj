(ns achtung.index
  (:require [achtung.common :as game]
            [clojure.core.async :as async]))






;; create and start a game, from the common ns
;; provide a rendering function that is used to render the game state to HTML/SVG or whatever
;; also listen to user keyboard events, and put those events into the exposed keyboard-input chan


(defn register-keyboard-events
  [handler-fn]
  (.addEventListener js/document.body ""
                     handler-fn
                     )
  )


(defn start-game
  []

  (let [resolution [1024 768]
        players    [{:name  "Player 1"
                     :left  "z"
                     :right "x"}
                    {:name  "Player 2"
                     :left  ","
                     :right "."}]

        {:keys [kill-chan key-chan render-chan]}
        (game/game players resolution)
        key-handle-fn (fn [e]
                        ;;TODO if q is pressed, the game is quit/restarted

                        ;;TODO get the letter that is pressed,
                        ;; ie 'z'
                        (async/>!! key-chan e))]
    (register-keyboard-events key-handle-fn)


    ))

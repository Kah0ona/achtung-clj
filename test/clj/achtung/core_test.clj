(ns achtung.core-test
  (:require [clojure.test :as t]
            [achtung.core :as sut]))

;; y coord
;; (Math/sin (/ pi 2))
;; -------------------
;; x coord
;; (Math/cos (/ pi 2))

;;(def rd (random-direction))
;;(Math/sin rd) ;;y
;;(Math/cos rd) ;;x

(t/deftest test-goniometric-math
  (with-redefs [sut/speed-multiplier 1]
    (t/testing "testing extending the trail"
      (t/is (= [[0 0] [0 1]] (sut/progress-trail [[0 0]] (/ sut/pi 2))))
      (t/is (= [[0 0] [-1 0]] (sut/progress-trail [[0 0]] sut/pi)))
      (t/is (= [[0 0] [0 -1]] (sut/progress-trail [[0 0]] (* 1.5 sut/pi))))
      (t/is (= [[0 0] [1 0]] (sut/progress-trail [[0 0]] (* 2 sut/pi)))))))


(def out-of-bounds-head-1 [11 9])
(def out-of-bounds-head-2 [10 11])
(def out-of-bounds-head-3 [-1 11])
(def out-of-bounds-head-4 [-1 11])
(t/deftest test-mark-colliding-players-as-gameover
  (t/testing "test colliding moves key"
    (t/is
     (=
      {:game-over-players #{"a"}
       :players {;;"a" {:trail [[0 0]]}
                 "b" {:trail [[5 5]]}}}

      (sut/mark-colliding-players-as-game-over
       {:game-over-players  #{}
        :players  {"a" {:trail [out-of-bounds-head-1]}
                   "b" {:trail [[5 5]]} ;;valid trail
                   }
        }
       [10 10] ;;resolution
       )))

    )

  )

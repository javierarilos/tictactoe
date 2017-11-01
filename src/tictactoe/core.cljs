(ns tictactoe.core
    (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(println "This text is printed from src/tictactoe/core.cljs. Go ahead and edit it and see reloading in action.")
(println "This is updated in the browser each time code is saved...")

;; define your app data so that it doesn't get over-written on reload
(def board-size 3)
(defn new-board
  [n]
  (vec (repeat n (vec (repeat n "B")))))

(defonce app-state
  (atom
    {:text "Hello world!"
     :board (new-board board-size)
     :game-status :in-progress}))

(defn in-a-row
  [dx dy board player column row]
  (every? true?
          (for [i (range board-size)]
            (= player (get-in board
                              [(+ (* i dx) column)
                               (+ (* i dy) row)])))))

(def column-in-a-row (partial in-a-row 0 1))
(def diag-right-in-a-row (partial in-a-row 1 1))
(def diag-left-in-a-row (partial in-a-row -1 1))
(def row-in-a-row (partial in-a-row 1 0))

(defn is-winner? [board player]
  (or
    (diag-right-in-a-row board player 0 0)
    (diag-left-in-a-row board player 2 0)
    (some true?
      (for [i (range board-size)]
        (or
          (column-in-a-row board player i 0)
          (row-in-a-row board player 0 i))))))


(defn is-full?
  [board]
  (every? #{"P" "C"} (flatten board)))


(defn game-status
  [board]
  (cond
    (is-winner? board "P") :player-victory
    (is-winner? board "C") :computer-victory
    (is-full? board) :draw
    :else :in-progress
    ))

(defn check-game-status
  [state]
  (assoc state :game-status (game-status (:board state))))

(defn computer-move
  [board]
  (let [blank-spots (for [i (range board-size)
                          j (range board-size)
                          :when (= (get-in board [i j]) "B")]
                          [i j])
        random-blank (when (seq blank-spots)
                        (rand-nth blank-spots))
      ]
   (assoc-in board random-blank "C")))

 (defn blank
   [i j]
   [:rect {:width 0.9
         :height 0.9
         :fill "lightgray"
         :x i
         :y j
         :on-click (fn rect-click [e]
                     (when (= (:game-status @app-state) :in-progress)
                       (prn ">> game-status is in-progress")
                       (swap! app-state assoc-in [:board i j] "P")
                       (if (is-winner? (:board @app-state) "P")
                        (do
                          (swap! app-state assoc :game-status :player-victory)
                          (prn ">> PLAYER WON status=" (:game-status @app-state)))
                        (if (not (is-full? (:board @app-state)))
                          (do
                            (swap! app-state update-in [:board] computer-move)
                            (prn "not full... computer moved" (:board @app-state)))

                          (prn "this is full!!!!!"))
                       )
                       (swap! app-state check-game-status @app-state)
                       (prn ">> done movement status is " (:game-status @app-state))
                     )
                     (prn ">>> out of when status=" (:game-status @app-state))

                   )}])

 (defn circle
   [i j]
   [:circle
        {:r 0.35
         :stroke "red"
         :stroke-width 0.10
         :fill "none"
         :cx (+ 0.45 i)
         :cy (+ 0.45 j)}])

(defn cross
  [i j]
  [:g {:stroke "blue"
       :stroke-width 0.3
       :stroke-linecap "round"
       :transform (str "translate(" (+ 0.45 i) "," (+ 0.45 j) ") scale(0.35)")}
    [:line {:x1 -1 :y1 -1 :x2 1 :y2 1}]
    [:line {:x1 1 :y1 -1 :x2 -1 :y2 1}]])

(defn tictactoe []
  [:center
   [:h1 (:text @app-state)]
   (case (:game-status @app-state)
    :player-victory [:h2 "You won!!"]
    :computer-victory [:h2 "The machine won..."]
    :draw [:h2 "Draw. You must restart..."]
    [:h2 "Game in progres... your turn!"]
    )
  [:p
    [:button
      {:on-click
        (fn new-game-click [e]
          (swap! app-state assoc :board (new-board board-size))
          (swap! app-state assoc :game-status :in-progress)
          )}
      "New Game!"]]
   (into
     [:svg
      {:view-box (str "0 0 " board-size " " board-size)
       :width 500
       :height 500}
      (for [i (range board-size)
            j (range board-size)]
          (case (get-in @app-state [:board i j])
            "B" [blank i j]
            "P" [circle i j]
            "C" [cross i j]
            (blank i j))
          )
          (prn "= curr board:" (:board @app-state))
          ])])

(reagent/render-component [tictactoe]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  (swap! app-state assoc-in [:text] "Hi! from on-js-reload")
)

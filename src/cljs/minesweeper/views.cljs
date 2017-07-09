(ns minesweeper.views
  (:require [re-frame.core :as re-frame]
            [reagent.core :as reagent]))

(def number-of-rows 20)
(def number-of-cols 20)

(defn build-grid [default-value num-rows num-cols]
  (vec (repeat num-rows (vec (repeat num-cols default-value)))))

(def empty-grid (partial build-grid :_))

(defn flatten-grid [grid]
  (for [[i row] (map-indexed vector grid)
        [j val] (map-indexed vector row)]
    [i j val]))

(defn grid-size [grid]
  (let [[max-i max-j val] (last (flatten-grid grid))]
    [(inc max-i) (inc max-j)]))

(defn set-mines [grid num-mines]
  (let [num-rows (count grid)
        num-cols (count (get grid 0))]
    (loop [mine-count 0
           grid grid]
      (let [row (rand-int num-rows)
            col (rand-int num-cols)
            empty? (not= :X (get-in grid [row col]))]
        (if (< mine-count num-mines)
          (recur
            (if empty? (inc mine-count) mine-count)
            (if empty? (assoc-in grid [row col] :X) grid))
          grid)))))

(def mine-grid (set-mines (empty-grid number-of-rows number-of-cols) 40))

(def adjacent-indices
  (vec
    (for [x [-1 0 1]
        y [-1 0 1]
        :when (not (= x y 0))]
      [x y])))

(defn adjacent-cells [grid row col]
  (let [num-rows (count grid)
        num-cols (count (get grid 0))]
    (reduce
      (fn [cells [i j]]
        (let [x (+ i row)
              y (+ j col)]
          (if (and (< -1 x num-rows) (< -1 y num-cols))
            (conj cells [x y])
            cells)))
      []
      adjacent-indices)))


(defn hidden-adjacent-cells [grid row col]
  (let [num-rows (count grid)
        num-cols (count (get grid 0))]
    (reduce
      (fn [cells [i j]]
        (let [x (+ i row)
              y (+ j col)]
          (if (and (< -1 x num-rows) (< -1 y num-cols) (get-in grid [x y]))
            (conj cells [x y])
            cells)))
      []
      adjacent-indices)))


(defn count-adjacent-mines [grid row col]
  (let [num-rows (count grid)
        num-cols (count (get grid 0))]
    (reduce
      (fn [count cell]
        (if (= :X (get-in grid cell))
          (inc count)
          count))
      0
      (adjacent-cells grid row col))))

(defn display-counts [grid]
  (into []
    (map-indexed
      (fn [i row]
        (into []
          (map-indexed
            (fn [j val]
              (if (= :X val)
                val
                (keyword (str (count-adjacent-mines grid i j)))))
            row)))
      grid)))

(def minefield (display-counts mine-grid))

(def hidden-grid (build-grid true number-of-rows number-of-cols))

(defn number-to-text [n]
  (case n
    0 "zero"
    1 "one" 
    2 "two"
    3 "three"
    4 "four"
    5 "five"
    6 "six"
    7 "seven"
    8 "eight" ))

(def game-over (reagent/atom false))

(defn toggle-class [a k class1 class2]
  (if (= (@a k) class1)
    (swap! a assoc k class2)
    (swap! a assoc k class2)))

(defn get-hiddens [hiddens mines [row col]]
  (if (not= :0 (get-in mines [row col]))
    (assoc-in hiddens [row col] false)
    (reduce
      (fn [acc next]
        (get-hiddens acc mines next))
      (assoc-in hiddens [row col] false)
      (hidden-adjacent-cells hiddens row col))))

(defn add-flag [flags coord]
  (assoc-in flags coord true))

(defn cell [hiddens mines flags row col]
  (fn []
    (let [hidden (get-in @hiddens [row col])
          flagged (get-in @flags [row col])]
      (if hidden
        [:td.cell.hidden
         {:className (if flagged "flagged hidden" "hidden")
          :on-click #(swap! hiddens get-hiddens mines [row col])
          :onContextMenu #(do
                            (.preventDefault %)
                            (swap! flags add-flag [row col]))}]
        [:td.cell.visible
         (if (= :X (get-in mines [row col]))
           [:img {:src "images/bomb.png" :width "20px" :height "20px"}]
           [:span
            {:class (number-to-text (count-adjacent-mines mines row col))}
            (count-adjacent-mines mines row col)])]))))

(defn row [mines hiddens flags row]
  (fn []
    [:tr.row
     (for [j (range (count (get mines 0)))]
       ^{:key (str "cell" row j)} [cell hiddens mines flags row j])]))

(defn settings-panel []
  (fn []
    [:div.settings
     [:h3 "Settings"]
     [:p.label "Difficulty"]
     [:select
      [:option {:value :easy} "Easy"]
      [:option {:value :medium} "Medium"]
      [:option {:value :hard} "Hard"]
      [:option {:value :expert} "Expert"]]]))

(defn main-panel []
  (let [name (re-frame/subscribe [:name])
        minefield (reagent/atom minefield)
        hiddens (reagent/atom (build-grid true number-of-rows number-of-cols))
        flags (reagent/atom (build-grid false number-of-rows number-of-cols))]
    (fn []
      [:div
       [:h1 "Minesweeper"] 
       [:table.grid
        [:tbody
         (for [i (range (count @minefield))]
           ^{:key (str "row:" i)} [row @minefield hiddens flags i])]]
       [settings-panel]])))

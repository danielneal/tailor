(ns shadow-tailor.ui.grid
  (:require [hx.react :refer [defnc]]))

(defnc Grid [opts]
  (let [{:keys [x y w h dx dy]} opts
        y1 y
        y2 (+ y h)
        x1 x
        x2 (+ x w)]
    [:<>
     (doall (for [y (range y1 (inc y2) dy)]
              [:line {:x1 x1 :y1 y :x2 x2 :y2 y :stroke "white" :stroke-width 1}]))
     (doall (for [x (range x1 (inc x2) dx)]
              [:line {:x1 x :y1 y1 :x2 x :y2 y2 :stroke "white" :stroke-width 1}]))]))

(ns shadow-tailor.core
  (:require
   ["react" :as react]
   ["mathjs" :as math]
   [goog.dom :as gdom]
   [hx.react :as hx :refer [defnc]]
   [hx.hooks :refer [<-state <-ref <-effect]]
   [hx.hiccup :as hiccup]
   [shadow-tailor.matrix :as matrix]
   [shadow-tailor.state :refer [<-query handle-event]]
   [shadow-tailor.ui.circle :refer [Circle]]
   [shadow-tailor.ui.grid :refer [Grid]]
   [shadow-tailor.ui.project :refer [Project]]
   ["react-dom" :as react-dom]))

(defnc App
  []
  ;; use React Hooks for state management
  (let [coords (<-query {:circle/c0 {:x :u0
                                     :y :v0}
                         :circle/c1 {:x :u1
                                     :y :v1}
                         :circle/c2 {:x :u2
                                     :y :v2}
                         :circle/c3 {:x :u3
                                     :y :v3}})
        matrix (matrix/solve {:from {:x0 0, :y0 0
                                     :x1 100, :y1 0
                                     :x2 0, :y2 100
                                     :x3 100, :y3 100}
                              :to coords})]
    (println coords)
    [:<>
     [:svg {:style {:background-color "black"
                    :position "absolute"
                    :left 0 :top 0
                    :width "100vw"
                    :height "100vh"}}
      [Circle {:id :circle/c0 :x 0 :y 0 :r 20}]
      [Circle {:id :circle/c1 :x 100 :y 0 :r 20}]
      [Circle {:id :circle/c2 :x 0 :y 100 :r 20}]
      [Circle {:id :circle/c3 :x 100 :y 100 :r 20}]]
     [:div {:style {:position "absolute"
                    :left 0 :top 0
                    :pointer-events "none"}}
      [Project {:matrix matrix}
       [:svg
        [Grid {:x 0 :y 0 :w 100 :h 100 :dx 10 :dy 10}]]]]]))

(defn init
  []
  (react-dom/render (hx/f [App]) (gdom/getElement "app")))

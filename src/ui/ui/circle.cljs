(ns shadow-tailor.ui.circle
  (:require [shadow-tailor.state :refer [handle-event <-query dispatch! <-register]]
            [hx.react :refer [defnc]]
            [hx.hooks :refer [<-effect]]))

(defmethod handle-event :circle/update-position
  [db [_ opts]]
  (let [{:keys [id page-x page-y]} opts
        {:keys [x y offset]} (get db id)
        {offset-x :x, offset-y :y} offset]
    (update db id merge {:x (+ offset-x page-x)
                         :y (+ offset-y page-y)})))

(defmethod handle-event :circle/stop-dragging
  [db [_ opts]]
  (let [{:keys [id]} opts]
    (update db id dissoc :offset)))

(defmethod handle-event :circle/start-dragging
  [db [_ opts]]
  (let [{:keys [id page-x page-y]} opts
        {:keys [x y]} (get db id)]
    (update db id assoc :offset {:x (- x page-x)
                                 :y (- y page-y)})))

(defnc Circle
  [opts]
  (let [{:keys [id]} opts
        {:keys [circle]} (<-query {id :circle})
        handle-mouse-move (fn [e]
                            (let [page-x (.-pageX e)
                                  page-y (.-pageY e)]
                              (dispatch! [:circle/update-position {:id id
                                                                  :page-x (.-pageX e)
                                                                  :page-y (.-pageY e)}])))
        handle-mouse-up (fn []
                          (dispatch! [:circle/stop-dragging {:id id}])
                          (.removeEventListener js/document "mousemove" handle-mouse-move))
        handle-mouse-down (fn [e]
                            (let [page-x (.-pageX e)
                                  page-y (.-pageY e)]
                              (dispatch! [:circle/start-dragging {:id id
                                                                 :page-x (.-pageX e)
                                                                 :page-y (.-pageY e)}])
                              (.addEventListener js/document "mousemove" handle-mouse-move)
                              (.addEventListener js/document "mouseup" handle-mouse-up)))
        {:keys [x y r]} circle]
    (<-register opts)
    [:<>
     [:line {:x1 (- x r) :y1 y,:x2 (+ x r) :y2 y :stroke "white" :stroke-width 1}]
     [:line {:x1 x :y1 (- y r), :x2 x :y2 (+ y r) :stroke "white" :stroke-width 1}]
     [:circle {:cx x
               :cy y
               :r (* r 0.8)
               :fill "transparent"
               :stroke "white"
               :stroke-width 1
               :on-mouse-down handle-mouse-down}]]))

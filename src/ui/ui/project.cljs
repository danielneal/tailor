(ns shadow-tailor.ui.project
  (:require [hx.react :refer [defnc]]
            [goog.string :as gstring]
            [clojure.string :as string]))

(defn to-string
  [matrix]
  (let [[[a e i m]
         [b f j n]
         [c g k o]
         [d h l p]] matrix]
    (str "matrix3d(" (string/join "," [a b c d e f g h i j k l m n o p]))))

(defnc Project
  [opts]
  (let [{:keys [matrix origin children]} opts
        {:keys [x y]} origin]
    (if matrix
      [:div {:style {:transform (to-string matrix)
                   :transform-origin "0 0"}}
       [:<> children]]
      [:<> children])))

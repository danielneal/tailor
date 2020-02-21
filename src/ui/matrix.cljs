(ns shadow-tailor.matrix
  (:require ["mathjs" :as math]
            [clojure.string :as string]))

(defn solve
  "Forms the matrix to solve given the four corners of the unit square:
   [x y] = [0 0] [0 1] [1 0] [1 1]"
  [opts]
  (let [{:keys [from to]} opts
        {:keys [x0 y0 x1 y1 x2 y2 x3 y3]} from
        {:keys [u0 v0 u1 v1 u2 v2 u3 v3]} to

        A #js [#js [x0 y0 1 0 0 0 (- (* u0 x0)) (- (* u0 y0))]
               #js [0 0 0 x0 y0 1 (- (* v0 x0)) (- (* v0 y0))]
               #js [x1 y1 1 0 0 0 (- (* u1 x1)) (- (* u1 y1))]
               #js [0 0 0 x1 y1 1 (- (* v1 x1)) (- (* v1 y1))]
               #js [x2 y2 1 0 0 0 (- (* u2 x2)) (- (* u2 y2))]
               #js [0 0 0 x2 y2 1 (- (* v2 x2)) (- (* v2 y2))]
               #js [x3 y3 1 0 0 0 (- (* u3 x3)) (- (* u3 y3))]
               #js [0 0 0 x3 y3 1 (- (* v3 x3)) (- (* v3 y3))]]
        b #js [u0 v0 u1 v1 u2 v2 u3 v3]
        [h0 h1 h2 h3 h4 h5 h6 h7] (map first (math/lusolve A b))]
    [[h0 h1 0 h2]
     [h3 h4 0 h5]
     [0  0  1 0]
     [h6 h7 0 1]]))







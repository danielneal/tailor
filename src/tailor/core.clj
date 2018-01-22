(ns tailor.core
  (:require [clojure.string :as string]
            [hiccup.core :as hiccup]))

(def shirt
  {:pattern/name :shirt
   :pattern/pieces {[:shirt-yoke] {:normal 1
                                   :reverse 1}
                    [:cuff] {:normal 1
                             :reverse 1}
                    [:placket {:normal 1
                               :reverse 1}]
                    [:sleeve] {:normal 1
                               :reverse 1}
                    [:collar {:normal 1}]}
   :pattern/seams {}})

(def shirt-yoke
  {:pattern/name :shirt-yoke
   :pattern/variant 1})

(def cuff
  {:pattern/name :cuff
   :pattern/variant 1
   :pattern/pieces [{:pattern/name :cuff-}]})

(def sleeve
  {:pattern/name :sleeve
   :pattern/variant 1})

;; taking the pattern data
;; parameterising the pattern (ease)
;; default ease #unit/length [100 :mm]

;; #_(draw-pattern
;;  ;; pattern
;;  :shirt
;;  ;; measurements
;;  {:chest #unit/length [460 :mm]
;;   :waist #unit/length [400 :mm]}

;;  ;; pattern options
;;  {:ease #unit/length [50 :mm]
;;   :darts #{}}
;;  ;; generic options
;;  {:seam-allowance #unit/length [15 :mm]})


;; parameterising the pattern (seam allowance)
;; notching
(defmulti pattern
  (fn [piece measurements & [opts]] piece))

(defmethod pattern :cuff
  [_ measurements & [opts]]
  (let [{:keys [wrist-circumference]} measurements
        {:keys [wrist-ease cuff-length seam-allowance]
         :or {wrist-ease 20
              cuff-length 70
              seam-allowance 10}} opts
        h (+ wrist-circumference wrist-ease)
        v (+ cuff-length seam-allowance)
        s seam-allowance]
    {:cuts [[:line [0 0] [(+ h (* 2 s)) 0] [(+ h (* 2 s)) (+ v s)] [0 (+ v s)] [0 0]]]
     :folds [[:line [0 (+ (/ v 2) s)] [(+ h (* 2 s)) (+ (/ v 2) s)]]
             [:line [0 s] [(+ h (* 2 s)) s]]
             [:line [0 s] [(+ h (* 2 s)) s]]]
     :seams [[:line [s 0] [s (+ v s)] [(+ h s) (+ v s)] [(+ h s) 0]]]}))

;; should you add seam allowance around the outside
;; or include it in the pattern

(defn preview
  [x]
  (let [random-uuid (java.util.UUID/randomUUID)
        fname (str "/tmp/" random-uuid ".html")]
    (spit fname (hiccup/html x))
    (clojure.java.browse/browse-url fname)))

(comment
  (preview [:svg {:width 200
                  :height 200}
            [:path {:d "M50 50h100 v100h-100 v-100"
                    :stroke "black"
                    :fill "transparent"
                    :stroke-width 10}]])
  (preview [:svg {:width 200
                  :height 200}
            [:path {:d "M 10 10 l10 10 40 10 40 40 10 40 10 10"
                    :stroke "black"
                    :fill "transparent"
                    :stroke-width 1}]]))

(defn point-str [[x y]]
  (str x " " y " "))

(defmulti path (fn [[type & _]] type))

(defmethod path :line
  [[type & points]]
  (let [[origin & points] points]
    (str "M" (point-str origin)
         "L" (string/join (for [point points]
                            (point-str point))))))

(defn draw [pattern]
  (let [{:keys [cuts folds seams]} pattern]
    (into [:svg {:width 10000
                 :height 10000}]
          (concat (map (fn [cut]
                         [:path {:d (path cut)
                                 :fill "transparent"
                                 :stroke "black"}]) cuts)
                  (map (fn [fold]
                         [:path {:d (path fold)
                                 :fill "transparent"
                                 :stroke "black"
                                 :stroke-dasharray "10 5 1 5"}]) folds)
                  (map (fn [seam]
                         [:path {:d (path seam)
                                 :fill "transparent"
                                 :stroke "black"
                                 :stroke-dasharray "2 4"}]) seams)))))

(pattern :cuff {:wrist-circumference 500})
(preview (draw (pattern :cuff {:wrist-circumference 500} {:cuff-length 140})))

(ns tailor.core
  (:require [clojure.string :as string]
            [hiccup.core :as hiccup]))

(defn standard-measurements
  "Formula to calculate measurements from dress-size.
   Not accurate outside of the normal range for now. "
  [dress-size]
  (let [n (/ (- dress-size 38.0) 2.0M)]
    {:back-waist (+ 41.1M (* n 0.8M))
     :front-waist (+ 43.1M (* n 0.8M))
     :hip-depth  (+ 17.8M (* n 0.4M))
     :jacket-length (+ 66.1M (* n 1.3M))
     :dress-length (+ 95.0M (* n 2M))
     :skirt-length (+ 57.0M n)
     :crotch-length (+ 24.1M (* n 0.5M))
     :knee-length (+ 54.5M n)
     :bust-circumference (+ 80M (* n 4.0M))
     :waist-circumference (+ 60M (* n 4.0M))
     :hip-circumference (+ 86M (* n 4.0M))
     :back-width (+ 33.9M (* n 1.5M))
     :shoulder-width (+ 35.9M (* n 1.5M))
     :bust-height (+ 25.5M (* n 0.5M))
     :breast-distance (+ 16M n)}))

(defn expand-measurements
  "Check the measurements and return a warning if the measurement is outside of +/-
   1 dress size away from the standard measurement.
   Uses the standard measurement for the size for any measurements not provided"
  [measurements]
  (let [{:keys [dress-size]} measurements
        _ (assert (not (nil? dress-size)) "Dress size must be provided")
        dress-size-lower-bound (- dress-size 2)
        dress-size-upper-bound (+ dress-size 2)
        [measurements-lower-bound
         measurements-standard
         measurements-upper-bound] (mapv standard-measurements [dress-size-lower-bound
                                                                dress-size
                                                                dress-size-upper-bound])
         warnings (into [] (for [[k v] (dissoc measurements :dress-size)
                                 :let [lower (get measurements-lower-bound k)
                                       upper (get measurements-upper-bound k)]
                                 :when (and lower upper (not (<= lower v upper)))]
                             [:measurement-not-in-bounds {:measuremnt k
                                                          :value v
                                                          :bounds [lower upper]}]))]
    (cond-> (merge measurements-standard measurements)
      (seq warnings) (assoc :tailor/warnings warnings))))

(defn expand-measurements'
  "Check the measurements and return a warning if the measurement is outside of +/-
   1 dress size away from the standard measurement.
   Uses the standard measurement for the size for any measurements not provided"
  [measurements]
  (let [{:keys [dress-size]} measurements
        _ (assert (not (nil? dress-size)) "Dress size must be provided")
        dress-size-lower-bound (- dress-size 2)
        dress-size-upper-bound (+ dress-size 2)
        [measurements-lower-bound
         measurements-standard
         measurements-upper-bound] (mapv standard-measurements [dress-size-lower-bound
                                                                dress-size
                                                                dress-size-upper-bound])
         warnings (into [] (for [[k v] (dissoc measurements :dress-size)
                                 :let [lower (get measurements-lower-bound k)
                                       upper (get measurements-upper-bound k)]
                                 :when (and lower upper (not (<= lower v upper)))]
                             [:measurement-not-in-bounds {:measuremnt k
                                                          :value v
                                                          :bounds [lower upper]}]))]
    (cond-> (merge measurements measurements-standard)
      (seq warnings) (assoc :tailor/warnings warnings))))

(defn basic-dress
  "Basic dress measurements according to il modelismo"
  [measurements opts]
  (let [{:keys [ease-bust ease-waist ease-hip dart-waist arm-hole-depth]
         :or   {ease-bust 4
                ease-waist 4
                ease-hip 4
                dart-waist 3
                arm-hole-depth 22}} opts
        {:keys [back-waist
                dress-size
                front-waist
                back-width
                waist-circumference
                bust-circumference
                hip-circumference
                hip-depth
                bust-height
                breast-distance
                dress-length
                shoulder-width
                height]} measurements]
    {:back [[:a-b (+ (/ dress-size 24) 0.2)]
            [:a-b1 4.5]
            [:a-c arm-hole-depth]
            [:a-d back-waist]
            [:d-e hip-depth]
            [:a-g (/ dress-size 6)]
            [:a-h (/ back-width 2)]
            [:a-f1 dress-length]
            [:c-c1 (- (/ (+ bust-circumference ease-bust) 4) 1)]
            [:d-d1 (- (+ (/ (+ waist-circumference ease-waist) 4) dart-waist) 1)]
            [:e-e1 (- (/ (+ hip-circumference ease-hip) 4) 1)]
            [:f-f1 :as-explained]
            [:h-i :as-explained]
            [:h-l 4.5]
            [:b1-l1 (/ shoulder-width 2)]
            [:i-m :as-explained]]
     :front [[:a-c arm-hole-depth]
             [:a-d back-waist]
             [:d-a1 front-waist]
             [:a1-b (+ 1 (/ dress-size 6))]
             [:d-e hip-depth]
             [:a-f dress-length]
             [:a-g (/ dress-size 6)]
             [:a1-h (- (/ back-width 2) 1)]
             [:c-c2 (+ (/ (+ bust-circumference ease-bust) 4) 1)]
             [:d-d2 (+ (/ (+ waist-circumference ease-waist) 4) dart-waist 1)]
             [:e-e2 (+ (/ (+ hip-circumference ease-hip) 4) 1)]
             [:f-f2 :e-e2]
             [:h-i :as-explained]
             [:g-l :as-explained]
             [:a1-n bust-height]
             [:n-n1 (/ breast-distance 2)]
             [:g-g1 (+ (/ dress-size 10) 0.5)]
             [:g1-g2 :as-explained]
             [:g2-n1 :as-explained]
             [:n1-n2 :as-explained]
             [:g3-n1 :as-explained]
             [:g-l2 :as-explained]
             [:i-m 5]
             [:m-m1 2]
             [:m1-m2 :as-explained]]}))

(defn basic-sleeve
  "Basic sleeve according to il modelismo"
  [measurements opts]
  (let [{:keys [dress-size elbow-length sleeve-length]} measurements
        a-b (- (/ dress-size 2) 1)]
    {:sleeve [[:a-b a-b]
              [:a-c (/ a-b 2)]
              [:a-d (+ (/ dress-size 10) 0.5)]
              [:a-e 14]
              [:e-f :measure-from-front-bodice]
              [:e1-f1 :measure-from-front-bodice]
              [:e-m 5]
              [:m-n 1]
              [:e1-m1 5]
              [:d-d1 3]
              [:b1-b2 3.5]
              [:c-h elbow-length]
              [:c-i sleeve-length]
              [:i-l :as-directed]]}))

(def measurements
  {:jess {:bust-circumference 80
          :waist-circumference 63
          :hip-circumference 90
          :hip-depth 18.5
          :back-waist 41.5
          :shoulder-width 38.5
          :dress-size 40}
   :viktorija {:bust-circumference 86
               :waist-circumference 73
               :hip-circumference 105
               :back-waist 38
               :front-waist 41
               :shoulder-width 40
               :hip-depth 25
               :dress-length 100
               :breast-distance 19
               :bust-height 26
               :back-width 37
               :elbow-length 34
               :sleeve-length 60
               :dress-size 42}})

(defn round-to-nearest
  "Rounds n to the nearest [accuracy]"
  [n accuracy]
  (let [a' (/ accuracy 2)
        m (mod n a')
        q (int (quot n a'))]
    (cond
      (= 0 m) n
      (even? q) (* q a')
      :else (* (inc q) a'))))

(defn round-numbers
  "Rounds the numbers in a piece to the nearest [accuracy]"
  [piece & [{:keys [accuracy]
             :or {accuracy 0.25}}]]
  (mapv (fn [[k part]]
          [k (mapv (fn [[k length]]
                     [k (if (number? length)
                          (round-to-nearest length accuracy)
                          length)]) part)]) piece))

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
     :folds-mountain [[:line [0 v] [(+ h (* 2 s)) v]]]
     :folds-valley [[:line [0 (+ (/ v 2) s)] [(+ h (* 2 s)) (+ (/ v 2) s)]]]
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
  (let [{:keys [cuts folds-valley folds-mountain seams]} pattern]
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
                                 :stroke-dasharray "9 5 1 5"}]) folds-mountain)
                  (map (fn [fold]
                         [:path {:d (path fold)
                                 :fill "transparent"
                                 :stroke "black"
                                 :stroke-dasharray "5 5"}]) folds-valley)
                  (map (fn [seam]
                         [:path {:d (path seam)
                                 :fill "transparent"
                                 :stroke "black"
                                 :stroke-dasharray "20 5"}]) seams)))))




(find-roots (fn [x] (+ (Math/pow x 3) (* x 2) 39)))

(comment
  (let [m (expand-measurements (get measurements :jess))
        m2 (expand-measurements' (get measurements :jess))]
    (into {} (for [[k v] m]
               [k {:correct v
                   :used (get m2 k)}])))

  (round-numbers (basic-dress (expand-measurements (get measurements :viktorija)) {}) {:accuracy 0.25})
  (round-numbers (basic-sleeve (expand-measurements (get measurements :viktorija)) {}) {:accuracy 0.25})
  (pattern :cuff {:wrist-circumference 500})
  (preview (draw (pattern :cuff {:wrist-circumference 500} {:cuff-length 140}))))

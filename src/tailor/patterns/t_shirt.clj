(ns tailor.patterns.t-shirt
  (:require [tailor.patterns :as patterns :refer [pattern]]
            [tailor.measurements :as measurements]))


(defn up
  [point distance]
  (let [[x y] point]
    [x (- y distance)]))

(defn down
  [point distance]
  (let [[x y] point]
    [x (+ y distance)]))

(defn left
  [point distance]
  (let [[x y] point]
    [(- x distance) y]))

(defn right
  [point distance]
  (let [[x y] point]
    [(+ x distance) y]))

(defn length
  [a b]
  (let [[ax ay] a
        [bx by] b]
    (Math/sqrt (+ (Math/pow (- ax bx) 2)
                  (Math/pow (- ay by) 2)))))

(defn towards
  [a b distance]
  (let [[ax ay] a
        [bx by] b
        ab (length a b)
        fraction (/ distance ab)]
    [(+ ax (* fraction (- bx ax)))
     (+ ay (* fraction (- by ay)))]))

(defmethod pattern :il-modelismo/t-shirt
  [opts]
  (let [{:tailor/keys [measurements]} opts
        {:man/keys [size back-width back-waist hip-depth armhole-depth chest-circumference]} measurements
        a [100M 5M]
        b (-> a (down 1M))
        c (-> b (down armhole-depth))
        c1 (-> c (left (* 0.25M chest-circumference)))
        d (-> a (down back-waist))
        e (-> d (down hip-depth))
        e1 (-> e (left (* 0.25M chest-circumference)))
        g (-> a (left (.divide size 6M 2 BigDecimal/ROUND_HALF_UP)))
        g1 (-> g (up 1M))
        h (-> a (left (* 0.5 back-width)))
        i (-> c (left (* 0.5 back-width)))
        h1 (-> h (towards i (* 0.5 (length h i))))
        l (-> h1 (towards h (+ (* 0.5 (length h1 h)) 1M)))
        l1 (-> l (left 1M))]
    (clojure.pprint/pprint
     [:a a
      :b b
      :c c
      :c1 c1
      :d d
      :e e
      :e1 e1
      :g g
      :g1 g1
      :h h
      :i i
      :h1 h1
      :l l
      :l1 l1])
    [:M b
     :L e
     :L e1
     :L c1
     :Q (-> c1 (right 7M)) l1
     :L g1
     :Q (-> g1 (down 2M) (right 2M)) b]))

(comment
  (patterns/preview
   (pattern
    {:tailor/pattern :il-modelismo/t-shirt
     :tailor/measurements (measurements/standard-man-measurements nil)})))

(defn query
  ([db query-v]
   (query db query-v {:collected (transient {})
                      :depth 0}))
  ([db query-v opts]
   (let [{:keys [collected depth]} opts]
     (loop [db db
            [[k v :as entry] & more] (seq query-v)]
       (cond
         (map? v) (reduce-kv (fn [m k v]
                               (assoc! m v (get db k)))
                             collected
                             v)
         (keyword? v) (recur db more (assoc! collected v (get db k)) depth)
         (and (nil? entry) (zero? depth)) (persistent! collected)
         (nil? entry) collected)))))

(defn query
  ([db query-v]
   (query db query-v {:collected (transient {})
                      :depth 0}))
  ([db query-v opts]
   (let [{:keys [collected depth]} opts
         result (reduce-kv (fn [m k v]
                             (cond
                               (keyword? v) (assoc! collected v (get db k))
                               (map? v) (query (get db k) v (update opts :depth inc))))
                 collected
                 query-v)]
     (if (zero? depth)
       (persistent! result) result))))

(query {:a {:b 1
            :c 2
            :e 5
            :d {:z 3}}
        :b {:f 9}}
       {:a {:b :?x
            :c :?y
            :e :?j
            :d {:z :?z}}
        :b {:f :?f}})

{:?x 1, :?y 2, :?j 5, :?z 3, :?f 9}



(ns tailor.cone)

(defn find-roots
  [f target & [opts]]
  (let [{:keys [start stop step eps]
         :or {start -100 stop 100 step 0.0001 eps 0.001}} opts]
    (filter #(< (Math/abs (- (f %) target)) eps) (range start stop step))))

(find-roots (fn [x]
              (* x x))
            4)

(defn dart-equation
  [a b]
  (fn [x]
    (+ (* a (Math/pow x 3))
       (* a x))))

(ns tailor.measurements)

(defn standard-woman-measurements
  "Formula to calculate measurements from dress-size.
   Not accurate outside of the normal range for now. "
  [size]
  (let [n (/ (- size 38.0) 2.0M)]
    {:woman/size size
     :woman/back-waist (+ 41.1M (* n 0.8M))
     :woman/front-waist (+ 43.1M (* n 0.8M))
     :woman/hip-depth  (+ 17.8M (* n 0.4M))
     :woman/jacket-length (+ 66.1M (* n 1.3M))
     :woman/dress-length (+ 95.0M (* n 2M))
     :woman/skirt-length (+ 57.0M n)
     :woman/crotch-length (+ 24.1M (* n 0.5M))
     :woman/knee-length (+ 54.5M n)
     :woman/bust-circumference (+ 80M (* n 4.0M))
     :woman/waist-circumference (+ 60M (* n 4.0M))
     :woman/hip-circumference (+ 86M (* n 4.0M))
     :woman/back-width (+ 33.9M (* n 1.5M))
     :woman/shoulder-width (+ 35.9M (* n 1.5M))
     :woman/bust-height (+ 25.5M (* n 0.5M))
     :woman/breast-distance (+ 16M n)}))

(defn standard-man-measurements
  "Size = half chest"
  [size]
  {:man/size 44M
   :man/head-height 21.2M
   :man/back-waist 45.7M
   :man/armhole-depth 22.8M
   :man/hip-depth 19.2M
   :man/jacket-length 74M
   :man/coat-length 104M
   :man/crotch-length 23.1M
   :man/knee-length 58.2M
   :man/trouser-length 106M
   :man/elbow-length 34.8M
   :man/sleeve-length 61.5M
   :man/chest-circumference 88M
   :man/waist-circumference 80M
   :man/hip-circumference 90M
   :man/neck-circumference 41M
   :man/wrist-circumference 18M
   :man/back-width 38.2M})

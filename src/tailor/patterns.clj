(ns tailor.patterns
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [hiccup.core :as hiccup]))

(s/def :svg/coordinate-pair
  (s/tuple decimal? decimal?))

(s/def :svg/coordinate-pair-triplet
  (s/tuple :svg/coordinate-pair :svg/coordinate-pair :svg/coordinate-pair))

(s/def :svg/coordinate-pair-double
  (s/tuple :svg/coordinate-pair :svg/coordinate-pair))

(s/def :svg/coordinate
  decimal?)

(s/def :svg/flag #{0 1})

(s/def :svg/elliptical-arc-argument
  (s/cat :rx decimal?
         :ry decimal?
         :x-axis-rotation decimal?
         :large-arc? :svg/flag
         :sweep? :svg/flag
         :coordinates :svg/coordinate-pair))

(s/def :svg/moveto
  (s/cat :op (s/or :svg.moveto/relative #{:m}
                   :svg.moveto/absolute #{:M})
         :coordinates :svg/coordinate-pair))

(s/def :svg/closepath
  #{:Z})

(s/def :svg/lineto
  (s/cat :op (s/or :svg.lineto/relative #{:l}
                   :svg.lineto/absolute #{:L})
         :coordinates (s/+ :svg/coordinate-pair)))

(s/def :svg/horizontal-lineto
  (s/cat :op (s/or :svg.horizontal-lineto/relative #{:h}
                   :svg.horizontal-lineto/absolute #{:H})
         :coordinates (s/+ :svg/coordinate)))

(s/def :svg/vertical-lineto
  (s/cat :op (s/or :svg.vertical-lineto/relative #{:v}
                   :svg.vertical-lineto/absolute #{:V})
         :coordinates (s/+ :svg/coordinate)))

(s/def :svg/curveto
  (s/cat :op (s/or :svg.curveto/relative #{:c}
                   :svg.curveto/absolute #{:C})
         :coordinates (s/+ :svg/coordinate-pair-triplet)))

(s/def :svg/smooth-curveto
  (s/cat :op (s/or :svg.smooth-curveto/relative #{:s}
                   :svg.smooth-curveto/absolute #{:S})
         :coordinates (s/+ :svg/coordinate-pair-double)))

(s/def :svg/quadratic-bezier-curveto
  (s/cat :op (s/or :svg.quadratic-bezier-curveto/relative #{:q}
                   :svg.quadratic-bezier-curveto/absolute #{:Q})
         :coordinates (s/+ :svg/coordinate-pair-double)))

(s/def :svg/smooth-quadratic-bezier-curveto
  (s/cat :op (s/or :svg.quadratic-bezier-curveto/relative #{:t}
                   :svg.quadratic-bezier-curveto/absolute #{:T})
         :coordinates (s/+ :svg/coordinate-pair)))

(s/def :svg/elliptical-arc
  (s/cat :op (s/or :svg.quadratic-bezier-curveto/relative #{:t}
                   :svg.quadratic-bezier-curveto/absolute #{:T})
         :coordinates (s/+ :svg/elliptical-arc-argument)))

(s/def :svg/path
  (s/+ (s/alt :svg/moveto :svg/moveto
              :svg/closepath :svg/closepath
              :svg/lineto :svg/lineto
              :svg/vertical-lineto :svg/vertical-lineto
              :svg/horizontal-lineto :svg/horizontal-lineto
              :svg/curveto :svg/curveto
              :svg/smooth-curveto :svg/smooth-curveto
              :svg/quadratic-bezier-curveto :svg/quadratic-bezier-curveto
              :svg/smooth-quadratic-bezier-curveto :svg/smooth-quadratic-bezier-curveto
              :svg/elliptical-arc :svg/elliptical-arc)))

(defn path->str
  "Converts a vector representation of an svg path to a string"
  [path]
  (s/assert :svg/path path)
  (->> (flatten path)
       (map #(if (keyword? %) (name %) (str %)))
       (string/join " ")))

(defn preview-hiccup
  "Previews hiccup in the browser"
  [x]
  (let [random-uuid (java.util.UUID/randomUUID)
        fname (str "/tmp/" random-uuid ".html")]
    (spit fname (hiccup/html x))
    (clojure.java.browse/browse-url fname)))

(defn wrap-path
  "Wraps a path string in html hiccup with a black background"
  [s]
  [:html
   [:body {:style {:background-color "black"}}
    [:svg {:width "100%"
           :height "100%"
           :viewbox "80 0 100 100"}
     [:path {:d s
             :vector-effect "non-scaling-stroke"
             :stroke "white"
             :stroke-width 1
             :stroke-dasharray "4 2"
             :fill "transparent"}]]]])

(def preview (comp preview-hiccup wrap-path path->str))

(defmulti pattern :tailor/pattern)

#_(preview-path [:M [10M 10M]
                 :h [100M 0M]
                 :v [100M 0M]
                 :c [[50M 50M] [300M 20M] [300M 30M]]
                 :c [[0M 0M] [300M 20M] [-100M -100M]]])

(ns tailor.arduino
  (:require [serial.core :as sc]
            [serial.util :as su]
            [gloss.core :as g]
            [gloss.io :as i]))

(def arduino
  "tty.usbmodem1411")

(defonce port
  (sc/open arduino))

(def fr
  (g/compile-frame
   {:motor :int8,
    :acceleration :int16
    :speed :int16
    :move-to :int-16}))

(defn send-command [cmd]
  (let [{:keys [motor acceleration speed move-to]} cmd
        encoded (-> (i/encode fr {:motor 1
                                  :acceleration 200
                                  :speed 500
                                  :move-to 4000})
                    (first)
                    (.array))]
    (sc/write port encoded)))

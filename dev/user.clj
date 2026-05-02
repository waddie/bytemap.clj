(ns user
  #_{:clj-kondo/ignore [:unused-namespace :unused-referred-var]}
  (:require [build :as b]
            [bytemap.core :as bm]
            [bytemap.plot :as bp]
            [bytemap.util :as bu]
            [clojure.repl :refer [doc source]]
            [cognitect.test-runner.api :as test-runner]
            [malli.core :as m]
            [malli.dev :as md]
            [malli.dev.pretty :as mdp]
            [malli.instrument :as mi]
            [still.core :refer [snap!]])
  (:import (java.util Random)))

(comment
  (set! *warn-on-reflection* true)
  (set! *warn-on-reflection* false))

(md/start! {:report (mdp/reporter (mdp/-printer {:colors       false
                                                 :print-length 30
                                                 :print-level  2
                                                 :print-meta   false
                                                 :width        80}))})

(comment
  (mi/collect!)
  (m/function-schemas))

(comment
  (do (in-ns 'user) (time (test-runner/test nil))))

(comment
  (do (in-ns 'user) (time (b/jar-all nil)))
  (do (in-ns 'user) (time (b/uber-all nil))))

(snap!
 (str "\n"
      (bp/plot->string #(Math/sqrt (abs (- 1 (Math/pow % 2))))
                       [20 5]
                       1 Math/PI
                       :axis true))
 "
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⢀⣀⣀⣀⣀⣀⣇⣀⣀⣀⣀⣀⠀⠀⠀⠀
⠴⠾⠭⠭⠭⠤⠤⠤⠤⠤⡧⠤⠤⠤⠤⠬⠭⠭⠽⠦
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀")

(snap!
 (str "\n"
      (let [x-scale 1
            y-scale Math/PI]
        (-> (bm/new-canvas 20 5)
            (bp/plot #(Math/sqrt (abs (- 1 (Math/pow % 2))))
                     :axis true
                     :x-scale x-scale
                     :y-scale y-scale)
            bm/canvas->string)))
 "
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⢀⣀⣀⣀⣀⣀⣇⣀⣀⣀⣀⣀⠀⠀⠀⠀
⠴⠾⠭⠭⠭⠤⠤⠤⠤⠤⡧⠤⠤⠤⠤⠬⠭⠭⠽⠦
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀")

(let [random (Random. 1234)
      nums   (stream-seq! (.ints random 1000 0 11))]
  (println nums)
  (bp/plot-histogram nums))

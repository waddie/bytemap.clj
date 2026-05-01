(ns bytemap.plot-test
  "Tests for bytemap plotting functionality."
  (:require [bytemap.core :as bm]
            [clojure.test :refer [deftest testing]]
            [still.core :refer [snap!]]))

(deftest plot-test
  (testing "Plotting a function"
    (let [canvas (bm/new-canvas 10 5)]
      (snap! canvas
             {:height 5
              :pixels [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
              :width  10})
      (let [canvas (bm/plot canvas #(Math/sqrt (abs (- 1 (Math/pow % 2)))))]
        (snap! canvas
               {:height 5
                :pixels [0 128 52 22 26 95 18 166 64 0 176 1 0 0 0 71 0 0 8 70
                         46 36 36 36 36 103 36 36 36 53 0 0 0 0 0 71 0 0 0 0 0 0
                         0 0 0 71 0 0 0 0]
                :width  10})
        (snap! (str "\n" (bm/canvas->string canvas))
               "
⠀⢀⠴⠖⠚⡟⠒⢦⡀⠀
⢰⠁⠀⠀⠀⡇⠀⠀⠈⡆
⠮⠤⠤⠤⠤⡧⠤⠤⠤⠵
⠀⠀⠀⠀⠀⡇⠀⠀⠀⠀
⠀⠀⠀⠀⠀⡇⠀⠀⠀⠀")))))

(ns bytemap.plot
  (:require [bytemap.core :refer
             [bounds canvas->string draw-line draw-point new-canvas]]
            [bytemap.schema :as schema]
            [clojure.string :as s]))

(defn plot
  "Plots a mathematical function on a canvas.

  Arguments:
  - canvas: A canvas
  - f: Function to plot (takes a number, returns a number)

  Options:
  - :axis - Whether to draw x and y axes (default: true)
  - :x-scale: The range of x values (from -x-scale to +x-scale) (default: 1)
  - :y-scale: The range of y values (from -y-scale to +y-scale) (default: 1)

  The function is sampled at regular intervals across the canvas width,
  and consecutive points are connected with lines.

  Returns the new canvas."
  {:malli/schema [:function [:=> [:cat schema/Canvas fn?] schema/Canvas]
                  [:=> [:cat schema/Canvas fn? [:* :any]] schema/Canvas]]}
  [canvas f &
   {:keys [axis x-scale y-scale]
    :or   {axis    true
           x-scale 1
           y-scale 1}}]
  (let [[w h]   (bounds canvas)
        canvas  (if axis
                  ;; Draw axes
                  (let [canvas (reduce (fn [c i] (draw-point c [(/ w 2) i]))
                                       canvas
                                       (range h))]
                    (reduce (fn [c i] (draw-point c [i (/ h 2)]))
                            canvas
                            (range w)))
                  canvas)
        ;; Narrow y range slightly to avoid clipping extremes
        y-scale (* y-scale (/ (inc h) h))]
    ;; Sample function and draw lines between consecutive points
    (loop [i          0
           prev-point nil
           canvas     canvas]
      (if (>= i w)
        canvas
        (let [;; x spans -0.5 to 0.5 (inclusive)
              x      (- (/ i (dec w)) 0.5)
              y      (/ (f (* x 2 x-scale)) y-scale -2)
              p      [(* (+ x 0.5) (dec w)) (* (+ y 0.5) h)]
              canvas (if prev-point (draw-line canvas prev-point p) canvas)]
          (recur (inc i) p canvas))))))


(defn plot->string
  "Convenience function that plots a mathematical function and returns the string representation.

  Arguments:
  - f: Function to plot (takes a number, returns a number)
  - [w h]: schema/Canvas dimensions in pixels
  - x-scale: The range of x values (from -x-scale to +x-scale)
  - y-scale: The range of y values (from -y-scale to +y-scale)

  Options:
  - :axis - Whether to draw x and y axes (default: true)"
  {:malli/schema
   [:function [:=> [:cat fn? [:tuple :int :int] number? number?] :string]
    [:=> [:cat fn? [:tuple :int :int] number? number? [:* :any]] :string]]}
  [f [w h] x-scale y-scale &
   {:keys [axis]
    :or   {axis true}}]
  (-> (new-canvas w h)
      (plot f :x-scale x-scale :y-scale y-scale :axis axis)
      canvas->string))

(defn print-plot!
  "Convenience function that plots a mathematical function on a new canvas and prints it.

  Arguments:
  - f: Function to plot (takes a number, returns a number)
  - [w h]: schema/Canvas dimensions in pixels
  - x-scale: The range of x values (from -x-scale to +x-scale)
  - y-scale: The range of y values (from -y-scale to +y-scale)

  Options:
  - :axis - Whether to draw x and y axes (default: true)"
  {:malli/schema
   [:function [:=> [:cat fn? [:tuple :int :int] number? number?] :nil]
    [:=> [:cat fn? [:tuple :int :int] number? number? [:* :any]] :nil]]}
  [f [w h] x-scale y-scale &
   {:keys [axis]
    :or   {axis true}}]
  (let [s     (plot->string f [w h] x-scale y-scale :axis axis)
        lines (s/split s #"\n")]
    (doseq [line lines]
      (println line))
    nil))

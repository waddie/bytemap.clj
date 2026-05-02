(ns bytemap.core
  "Text-based canvas using braille characters.

  Bytemap creates bitmaps using Unicode braille characters, where each character
  represents a 2x4 grid of pixels. This enables reasonably high-resolution
  text-based graphics in terminal output."
  (:require [bytemap.schema :as schema]
            [bytemap.util :as util]
            [clojure.string :as s]
            [still.core :refer [snap!]]))

;; Malli Schemas

;; Constants
(def ^:private braille-offset 0x2800)

;; Core Functions

(defn braille
  "Converts a byte (0-255) to a braille Unicode character.

  Each bit in the byte corresponds to one of the 8 dots in a braille character.
  The byte is added to the braille Unicode offset (0x2800) to get the final character."
  {:malli/schema [:function [:=> [:cat schema/ByteValue] :string]]}
  [byte-val]
  (str (char (+ braille-offset byte-val))))

(snap! (braille 0) "⠀")
(snap! (braille 64) "⡀")
(snap! (braille 255) "⣿")

(defn bit-of-subpixel
  "Maps a subpixel coordinate [x y] to its corresponding bit position (0-7).

  The mapping follows the braille standard layout:
  - x is 0-1 (left or right column)
  - y is 0-3 (top to bottom)

  The bit layout is:
    [0,0]=0  [1,0]=3
    [0,1]=1  [1,1]=4
    [0,2]=2  [1,2]=5
    [0,3]=6  [1,3]=7"
  {:malli/schema [:function [:=> [:cat schema/Subpixel] schema/Bit]]}
  [[x y]]
  (if (= y 3) (+ 6 x) (+ (* 3 x) y)))

(snap! (bit-of-subpixel [0 0]) 0)
(snap! (bit-of-subpixel [1 3]) 7)

(defn set-subpixel
  "Sets or clears a specific subpixel in a byte value.

  Returns a new byte with the specified subpixel bit set or cleared."
  {:malli/schema [:function
                  [:=> [:cat schema/ByteValue schema/Subpixel :any]
                   schema/ByteValue]]}
  [num subpixel value]
  (util/set-bit num (bit-of-subpixel subpixel) value))

(snap! (set-subpixel 0 [0 0] true) 1)
(snap! (set-subpixel 255 [0 0] false) 254)

(defn new-canvas
  "Creates a new canvas with the specified width and height in 'pixels'.

  Each pixel is a braille character representing a 2x4 grid of subpixels.
  So, a 10x5 canvas has dimensions of 20x20 in subpixel coordinates."
  {:malli/schema [:function [:=> [:cat :int :int] schema/Canvas]]}
  [width height]
  {:height height
   :pixels (vec (repeat (* width height) 0))
   :width  width})

(snap! (new-canvas 10 5)
       {:height 5
        :pixels [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
        :width  10})

(defn bounds
  "Returns the canvas dimensions in subpixels [width height], where each pixel is 2x4 subpixels."
  {:malli/schema [:function [:=> [:cat schema/Canvas] [:tuple :int :int]]]}
  [{:keys [width height]}]
  [(* 2 width) (* 4 height)])

(snap! (bounds (new-canvas 10 5)) [20 20])

(defn draw-point
  "Draws a point at subpixel coordinates [x y] on the canvas.

  Returns a new canvas with the point drawn. Coordinates are rounded to
  nearest integer. Points outside canvas bounds are silently ignored.

  The optional value parameter determines whether to set (true) or clear (false)
  the point. Defaults to true."
  {:malli/schema [:function
                  [:=> [:cat schema/Canvas schema/Point] schema/Canvas]
                  [:=> [:cat schema/Canvas schema/Point :any] schema/Canvas]]}
  ([canvas point] (draw-point canvas point true))
  ([canvas [x y] value]
   (let [{:keys [width height _]} canvas
         x       (Math/round (double x))
         y       (Math/round (double y))
         pixel-x (util/idiv x 2)
         pixel-y (util/idiv y 4)]
     (if (or (< pixel-x 0) (< pixel-y 0) (>= pixel-x width) (>= pixel-y height))
       canvas ; out of bounds, return unchanged
       (let [pixel-ix (+ (* pixel-y width) pixel-x)
             subpixel [(mod x 2) (mod y 4)]]
         (update
          canvas
          :pixels
          (fn [pixels]
            (update pixels pixel-ix #(set-subpixel % subpixel value)))))))))

(snap! (-> (new-canvas 4 2)
           (draw-point [4 4]))
       {:height 2
        :pixels [0 0 0 0 0 0 1 0]
        :width  4})

(defn canvas->string
  "Converts a canvas to a string representation using braille characters.

  Returns a multi-line string where each line represents one row of the canvas."
  {:malli/schema [:function [:=> [:cat schema/Canvas] :string]]}
  [{:keys [width height pixels]}]
  (apply str
         (for [y (range height)]
           (str (apply str
                       (for [x    (range width)
                             :let [i (+ (* y width) x)]]
                         (braille (nth pixels i))))
                (when (< y (dec height)) "\n")))))


(-> (new-canvas 5 3)
    (draw-point [5 6])
    (canvas->string))
(defn print-canvas!
  "Prints a canvas to stdout using braille characters.

  Outputs line-by-line to avoid buffer boundary issues with multibyte characters."
  {:malli/schema [:function [:=> [:cat schema/Canvas] :nil]]}
  [canvas]
  (let [s     (canvas->string canvas)
        lines (s/split s #"\n")]
    (doseq [line lines]
      (println line)))
  nil)

;; Vector operations

(defn ^:private span
  "Calculates the span between two points along an axis (0=x, 1=y)."
  [axis p0 p1]
  (- (nth p1 axis) (nth p0 axis)))

(defn ^:private sign
  "Returns the sign of a number: -1, 0, or 1."
  [x]
  (cond (< x 0) -1
        (> x 0) 1
        :else 0))

(defn ^:private make-vec2
  "Constructs a 2D vector from major/minor axis values.

  major-axis is 0 for x, 1 for y."
  [major-axis major minor]
  (case major-axis
    0 [major minor]
    1 [minor major]))

(defn draw-line
  "Draws a line from start point to end point using Bresenham’s algorithm.

  Returns a new canvas with the line drawn. Both start and end are subpixel
  coordinates."
  {:malli/schema [:function
                  [:=> [:cat schema/Canvas schema/Point schema/Point]
                   schema/Canvas]]}
  [canvas start end]
  (let [x-axis      0
        y-axis      1
        x-span      (span x-axis start end)
        y-span      (span y-axis start end)
        ;; Determine major and minor axes
        [major-axis minor-axis] (if (< (Math/abs y-span) (Math/abs x-span))
                                  [x-axis y-axis]
                                  [y-axis x-axis])
        ;; Ensure we draw from lower to higher major coordinate
        [start end] (if (< (nth start major-axis) (nth end major-axis))
                      [start end]
                      [end start])
        minor-step  (sign (- (nth end minor-axis) (nth start minor-axis)))
        run         (- (nth end major-axis) (nth start major-axis))
        rise        (Math/abs (- (nth end minor-axis) (nth start minor-axis)))]
    (loop [canvas canvas
           major  (nth start major-axis)
           minor  (nth start minor-axis)
           err    (- (* 2 rise) run)]
      (if (> major (nth end major-axis))
        canvas
        (let [canvas      (draw-point canvas (make-vec2 major-axis major minor))
              [minor err] (if (> err 0)
                            [(+ minor minor-step) (- err (* 2 run))]
                            [minor err])]
          (recur canvas (inc major) minor (+ err (* 2 rise))))))))

(snap! (-> (new-canvas 10 5)
           (draw-line [0 0] [20 20])
           (draw-line [0 20] [20 0]))
       {:height 5
        :pixels [17 132 0 0 0 0 0 0 128 20 0 0 17 132 0 0 128 20 1 0 0 0 0 0 145
                 148 1 0 0 0 0 0 128 20 1 0 17 132 0 0 128 20 1 0 0 0 0 0 17
                 132]
        :width  10})

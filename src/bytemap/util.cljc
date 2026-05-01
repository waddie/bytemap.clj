(ns bytemap.util
  "Low-level utility functions for bit manipulation and numeric operations."
  (:require [bytemap.schema :as schema]
            [still.core :refer [snap!]]))

(defn set-bit
  "Sets or clears bit i in num."
  {:malli/schema [:function [:=> [:cat :int schema/Bit :any] :int]]}
  [num i value]
  (if value
    (bit-or num (bit-shift-left 1 i))
    (bit-and num (bit-not (bit-shift-left 1 i)))))

(snap! (set-bit 0 3 true) 8)
(snap! (set-bit 15 0 false) 14)

(defn idiv
  "Returns the floor of a divided by b."
  {:malli/schema [:function [:=> [:cat :int :int] :int]]}
  [a b]
  #?(:clj (long (Math/floor (/ a b)))
     :cljs (js/Math.floor (/ a b))))

(snap! (idiv 7 2) 3)
(snap! (idiv -7 2) -4)

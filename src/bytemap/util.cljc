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

(defn format-double
  "Format a floating point number."
  {:malli/schema [:function [:=> [:cat number?] :string]]}
  [x]
  #?(:clj (format "%1$9.6g" x)
     :cljs x))

(snap! (idiv 7 2) 3)
(snap! (idiv -7 2) -4)

(defn calculate-mean
  {:malli/schema [:function [:=> [:cat [:seqable number?]] :double]
                  [:=> [:cat [:seqable number?] number?] :double]]}
  [xs]
  (double (/ (reduce + xs) (count xs))))

(defn calculate-std-dev
  {:malli/schema [:function [:=> [:cat [:seqable number?]] :double]
                  [:=> [:cat [:seqable number?] number?] :double]]}
  [xs & rest]
  (let [m (or (first rest) (calculate-mean xs))]
    (->> xs
         (map #(- % m))
         (map #(* % %))
         (reduce +)
         (/ (count xs))
         Math/sqrt)))

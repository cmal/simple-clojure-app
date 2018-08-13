(ns simple-clojure-app.hackerrank
  (:require [clojure.repl :refer [source doc]])
  (:use [clojure.test]
        [clojure.pprint]))

(defn r-insertion-sort [arr]
  ;; insertion sort for java array
  (loop [j 1]
    (let [key (aget arr j)
          i   (dec j)]
      (loop [i i]
        (if (and (>= i 0) (> (aget arr i) key))
          (do
            (aset arr (inc i) (aget arr i))
            (recur (dec i)))
          (aset arr (inc i) key)))
      (if (= j (dec (count arr)))
        arr
        (recur (inc j))))))

(defn insertion-sort [v]
  ;; insertion sort for clojure seq
  (loop [j 1
         v v]
    (let [new-v (let [curr  (get v j)
                      split (split-with (partial > curr) (take (inc j) v))]
                  (vec (concat (first split)
                               [curr]
                               (drop-last (second split))
                               (drop (inc j) v))))]
      (if (= j (dec (count v)))
        new-v
        (recur (inc j) new-v)))))


(defn find-max-crossing-subarray [arr low mid high]
  ;; page 71 CLRS v3 2010
  (let [left-sum Double/MIN_VALUE
        sum 0]
    (loop [i mid]

      ;; TODO
      )
    ))

(defn find-maximum-subarray [arr low high]
  (if (= high low)
    [low high (aget arr low)]
    (let [mid (int (/ (+ low high) 2))

          [left-low left-high left-sum]
          (find-maximum-subarray arr low mid)

          [right-low right-high right-sum]
          (find-maximum-subarray arr (inc mid) high)

          [cross-low cross-high cross-sum]
          (find-maximum-subarray arr low mid high)]

      (cond

        (and (>= left-sum right-sum) (>= left-sum cross-sum))
        [left-low left-high left-sum]

        (and (>= right-sum left-sum) (>= right-sum cross-sum))
        [left-low right-high right-sum]

        :else
        [cross-low cross-high cross-sum]))))

(defn r-swap [arr i j]
  (let [tmp (aget arr i)]
    (aset arr i (aget arr j))
    (aset arr j tmp)))

(defn r-shuffle [arr]
  (let [len (count arr)]
    (loop [i 0]
      (if (= i len)
        arr
        (do
          (r-swap arr i (rand-int len))
          (recur (inc i)))))))

(defn swap
  [v i j]
  (assoc v i (v i) j (v j)))

;; clojure has its own shuffle implementation
;; clojure.core/shuffle


;; clojure has its own random-sample implementation

;; heap-sort

(defprotocol MaxHeapProtocol
  (max-heapify [this len i])
  (build-max-heap [this])
  (max-heap-sort [this])
  (max-heap-insert [this x]) ;; conj
  (heap-extract-max [this]) ;; pop return max
  (heap-extract-max-remanents [this]) ;; pop return remanents
  (heap-maximum [this]) ;; peek
  (heap-increase-key [this k v]))

(defn parent [i]
  (quot i 2))

(defn left [i]
  (* 2 i))

(defn right [i]
  (inc (left i)))

(deftype MaxHeap [^{:volatile-mutable true} arr]
  MaxHeapProtocol
  (max-heapify [this len i]
    (let [l (left i)
          r (right i)
          largest (if (and (< l len)
                           (> (aget arr l) (aget arr i)))
                    l i)
          largest (if (and (< r len)
                           (> (aget arr r) (aget arr largest)))
                    r largest)]
      (when (not= i largest)
        (r-swap arr i largest)
        (pprint arr)
        (max-heapify this len largest))))
  (build-max-heap [this]
    (let [len (count arr)]
      (loop [i (parent len)]
       (when-not (neg? i)
         (max-heapify this len i)
         (recur (dec i))))))
  (max-heap-sort [this]
    (build-max-heap this)
    (let [len (count arr)]
     (loop [i (dec len)]
       (when-not (= i 1)
         (r-swap arr 0 i)
         ;; decrease heap-size
         (max-heapify this (dec len) 0)
         (recur (dec i))))))
  (max-heap-insert [this x]
    (prn "max-heap-insert")
    (let [index (count arr)]
      (set! arr (to-array (conj (vec arr) Double/NEGATIVE_INFINITY)))
      (heap-increase-key this index x)
      (vec arr)))
  (heap-extract-max-remanents [this]
    (let [len (count arr)
          max-key (first arr)]
      (aset arr 0 (aget arr (dec len)))
      (max-heapify this (dec len) 0)
      (prn "heap balanced")
      (vec arr)))
  (heap-extract-max [this]
    (let [len (count arr)
          max-key (force (first arr))]
      (aset arr 0 (aget arr (dec len)))
      (max-heapify this (dec len) 0)
      (prn "heap balanced")
      max-key))
  (heap-maximum [this]
    (first arr))
  (heap-increase-key [this k v]
    (prn "heap-increase-key")
    (aset arr k v)
    (loop [i k]
      (prn "loop: i = " i)
      (when (and (not (zero? i)) (< (aget arr (parent i))
                                (aget arr i)))
        (r-swap arr i (parent i))
        (recur (parent i)))))

  java.util.List
  (toArray [this] arr)

  clojure.lang.Seqable
  (seq [t]
    (seq arr))

  clojure.lang.IPersistentStack
  (peek [this] (heap-maximum this))
  (pop [this] (heap-extract-max-remanents this))
  (cons [this x]
    (max-heap-insert this x)))


(def heap-arr
  (to-array
   [16 4 10 14 7 9 3 2 8 1]))

(do
  (def heap-arr
    (to-array
     [1 2 3 4 7 8 9 10 14 16]))
  (def heap (MaxHeap. heap-arr))
  (build-max-heap heap)
  (pprint (.toArray heap))
  (prn "peek: " (peek heap))
  (pop heap)
  (conj heap 16))

(ns simple-clojure-app.hackerrank
  (:require [clojure.repl :refer [source doc]]
            [clojure.edn :as edn])
  (:use [clojure.test]
        [clojure.pprint]
        [clojure.reflect]))

(defn r-insertion-sort [arr]
  ;; insertion sort for java array
  (when-not (empty? arr)
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
          (recur (inc j)))))))

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

(defn insert-to-sorted! [sorted val]
  ;; sorted must be asc
  ;; return a java array
  (let [[v1 v2] (split-with (partial > val) sorted)]
    (to-array (concat v1 [val] v2))))

(defn remove-from-sorted! [sorted val]
  (let [[v1 v2] (split-with (partial > val) sorted)]
    (to-array (concat v1 (rest v2)))))


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

(defprotocol MinHeapProtocol
  (min-heapify [this len i])
  (build-min-heap [this])
  (min-heap-sort [this])
  (min-heap-insert [this x])
  (heap-extract-min [this])
  (heap-extract-min-remanents [this])
  (heap-minimum [this])
  (heap-decrease-key [this k v]))

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
        #_(pprint arr)
        (max-heapify this len largest))))
  (build-max-heap [this]
    (let [len (count arr)]
      (loop [i (parent (dec len))]
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
         (max-heapify this (dec i) 0)
         (recur (dec i))))))
  (max-heap-insert [this x]
    (let [index (count arr)]
      (set! arr (to-array (conj (vec arr) Double/NEGATIVE_INFINITY)))
      (heap-increase-key this index x)
      (vec arr)))
  (heap-extract-max-remanents [this]
    (let [len (count arr)
          max-key (first arr)
          raised-key (aget arr (dec len))]
      (aset arr 0 raised-key)
      (set! arr (to-array (drop-last arr)))
      (max-heapify this (dec len) 0)
      (vec arr)))
  (heap-extract-max [this]
    (let [len (count arr)
          max-key (force (first arr))]
      (aset arr 0 (aget arr (dec len)))
      (max-heapify this (dec len) 0)
      max-key))
  (heap-maximum [this]
    (first arr))
  (heap-increase-key [this k v]
    (aset arr k v)
    (loop [i k]
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
  (count [this] (count this))
  (peek [this] (heap-maximum this))
  (pop [this] (heap-extract-max-remanents this))
  (cons [this x]
    (max-heap-insert this x)))

(deftype MinHeap [^{:volatile-mutable true} arr]
  MinHeapProtocol
  (min-heapify [this len i]
    (let [l (left i)
          r (right i)
          smallest (if (and (< l len)
                           (< (aget arr l) (aget arr i)))
                    l i)
          smallest (if (and (< r len)
                            (< (aget arr r) (aget arr smallest)))
                    r smallest)]
      (when (not= i smallest)
        (r-swap arr i smallest)
        #_(pprint arr)
        (min-heapify this len smallest))))
  (build-min-heap [this]
    (let [len (count arr)]
      (loop [i (parent (dec len))]
       (when-not (neg? i)
         (min-heapify this len i)
         (recur (dec i))))))
  (min-heap-sort [this]
    (build-min-heap this)
    (let [len (count arr)]
     (loop [i (dec len)]
       (when-not (= i 1)
         (r-swap arr 0 i)
         ;; decrease heap-size
         (min-heapify this (dec i) 0)
         (recur (dec i))))
     (set! arr (to-array (reverse arr)))))
  (min-heap-insert [this x]
    (let [index (count arr)]
      (set! arr (to-array (conj (vec arr) Double/POSITIVE_INFINITY)))
      (heap-decrease-key this index x)
      (vec arr)))
  (heap-extract-min-remanents [this]
    (let [len (count arr)
          min-key (first arr)
          raised-key (aget arr (dec len))]
      (aset arr 0 raised-key)
      (set! arr (to-array (drop-last arr)))
      (min-heapify this (dec len) 0)
      (vec arr)))
  (heap-extract-min [this]
    (let [len (count arr)
          min-key (force (first arr))]
      (aset arr 0 (aget arr (dec len)))
      (min-heapify this (dec len) 0)
      min-key))
  (heap-minimum [this]
    (first arr))
  (heap-decrease-key [this k v]
    (aset arr k v)
    (loop [i k]
      (when (and (not (zero? i)) (< (aget arr i)
                                    (aget arr (parent i))))
        (r-swap arr i (parent i))
        (recur (parent i)))))

  java.util.List
  (toArray [this] arr)

  clojure.lang.Seqable
  (seq [t]
    (seq arr))

  clojure.lang.IPersistentStack
  (count [this] (count this))
  (peek [this] (heap-minimum this))
  (pop [this] (heap-extract-min-remanents this))
  (cons [this x]
    (min-heap-insert this x)))


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


(do
  (def heap-arr
    (to-array
     (reverse [1 2 3 4 7 8 9 10 14 16])))
  (def heap (MinHeap. heap-arr))
  (build-min-heap heap)
  (pprint (.toArray heap))
  (prn "peek: " (peek heap))
  (pop heap)
  (conj heap 16))


(defn move-to! [heap1 heap2]
  ;; move the root element of heap1 to heap2
  ;; and balanced both heap
  ;; NOTE this will change heap1 and heap2
  (let [item (heap-extract-max heap1)]
    (conj heap2 item)))

(defn find-median [v]
  (let [min-heap (MinHeap. (to-array [])) ;; hold bigger objects
        max-heap (MaxHeap. (to-array [])) ;; hold smaller objects
        ]
   (loop [index 0
          v v]
     (when-not (empty? v)
      (let [item (first v)]
        (if (even? index)
          (if (or (empty? max-heap)
                  (< (peek max-heap) item))
            (conj min-heap item)
            (do
              (conj min-heap (peek max-heap))
              (pop max-heap)
              (conj max-heap item)))
          (if (or (empty? min-heap)
                  (> (peek min-heap) item))
            (conj max-heap item)
            (do
              (conj max-heap (peek min-heap))
              (pop min-heap)
              (conj min-heap item)))))
      (recur (inc index)
             (rest v))))
   (cond
     (> (count min-heap) (count max-heap)) (peek min-heap)
     (> (count max-heap) (count min-heap)) (peek max-heap)
     :else                                 (/ (+ (peek min-heap) (peek max-heap)) 2))))


;; the above is wrong
;; ==== ANOTHER implementation ====

(defn parent [i]
  (if (zero? i)
    -1
    (quot (dec i) 2)))

(defn left [i]
  (inc (* 2 i)))

(defn right [i]
  (* 2 (inc i)))

(defprotocol HeapProtocol
  (heapify [this len i])
  (build-heap [this])
  (heap-sort [this])
  (heap-insert [this x])
  (heap-extract-top [this])
  (heap-extract-top-remanents [this])
  (heap-top [this])
  (heap-sink-key [this k v]))

(deftype Heap [^{:volatile-mutable true} arr cmp]
  HeapProtocol
  (heapify [this len i]
    (let [l          (left i)
          r          (right i)
          min-or-max (if (and (< l len)
                              (cmp (aget arr l) (aget arr i)))
                       l i)
          min-or-max (if (and (< r len)
                              (cmp (aget arr r) (aget arr min-or-max)))
                       r min-or-max)]
      #_(prn "comparing: " (and (< l len) (aget arr l)) (and (< r len) (aget arr r)) (aget arr i) ", winner: " (aget arr min-or-max))
      #_(pprint arr)
      (when (not= i min-or-max)
        #_(prn "swap: ", i , min-or-max)
        (r-swap arr i min-or-max)
        (heapify this len min-or-max))))
  (build-heap [this]
    (let [len (count arr)]
      (loop [i (parent (dec len))]
        #_(prn "i: " i)
        (when-not (neg? i)
          (heapify this len i)
          (recur (dec i))))))
  (heap-sort [this]
    (build-heap this)
    (let [len (count arr)]
      (loop [i (dec len)]
        (when-not (zero? i)
          (r-swap arr 0 i)
          (heapify this i 0)
          (recur (dec i))))))
  (heap-insert [this x]
    (let [index (count arr)]
      ;; 0 on the following line
      ;; is the same effect as Infinity
      ;; because the heap-sink-key method
      ;; will set the index to x and regardless
      ;; of its value
      (set! arr (to-array (conj (vec arr) 0)))
      (heap-sink-key this index x)
      (vec arr)))
  (heap-extract-top-remanents [this]
    (let [len        (count arr)]
      (aset arr 0 (aget arr (dec len)))
      (set! arr (to-array (drop-last arr)))
      (heapify this (dec len) 0)
      (vec arr)))
  (heap-extract-top [this]
    (let [len     (count arr)
          top-key (force (first arr))]
      (aset arr 0 (aget arr (dec len)))
      (heapify this (dec len) 0)
      top-key))
  (heap-top [this]
    (first arr))
  (heap-sink-key [this k v]
    (aset arr k v)
    (loop [i k]
      (when (and (not (zero? i))
                 (cmp (aget arr i)
                      (aget arr (parent i))))
        (r-swap arr i (parent i))
        (recur (parent i)))))

  java.util.List
  (toArray [this] arr)

  clojure.lang.Seqable
  (seq [t]
    (seq arr))

  clojure.lang.IPersistentStack
  (count [this] (count this))
  (peek [this] (heap-top this))
  (pop [this] (heap-extract-top-remanents this))
  (cons [this x]
    (heap-insert this x)))


(defn move-to! [heap1 heap2]
  ;; move the root element of heap1 to heap2
  ;; and balanced both heap
  ;; NOTE this will change heap1 and heap2
  (let [item (heap-extract-top heap1)]
    (conj heap2 item)))

(defn median [v]
  (let [min-heap (Heap. (to-array []) <) ;; hold bigger objects
        max-heap (Heap. (to-array []) >) ;; hold smaller objects
        ]
    (loop [index 0
           v     v]
      (when-not (empty? v)
        (let [item (first v)]
          (if (even? index)
            (if (or (empty? max-heap)
                    (< (peek max-heap) item))
              (conj min-heap item)
              (do
                (conj min-heap (peek max-heap))
                (pop max-heap)
                (conj max-heap item)))
            (if (or (empty? min-heap)
                    (> (peek min-heap) item))
              (conj max-heap item)
              (do
                (conj max-heap (peek min-heap))
                (pop min-heap)
                (conj min-heap item)))))
        (recur (inc index)
               (rest v))))
    (cond
      (> (count min-heap)
         (count max-heap)) (peek min-heap)
      (> (count max-heap)
         (count min-heap)) (peek max-heap)
      :else                (/ (+ (peek min-heap) (peek max-heap)) 2))))

(comment

  ;; test case
 (do
   (def heap-arr
     (to-array
      (reverse [1 2 3 4 7 8 9 10 14 16])))
   (def heap (Heap. heap-arr <))
   (build-heap heap)
   (prn heap)
   (prn "peek: " (peek heap))
   ;; (pop heap)
   ;; (conj heap 16)
   )

 (heap-sort heap)


 heap

 (heap-extract-top-remanents heap)

 (heapify heap (dec (count heap)) 0)

 (do
   (def heap-arr
     (to-array
      [1 2 3 4 7 8 9 10 14 16]))
   (def heap (Heap. heap-arr >))
   (build-heap heap)
   (prn heap)
   (prn "peek: " (peek heap))
   (pop heap)
   (conj heap 16))


 (heap-sort heap))


(def arr (edn/read-string (slurp "/Users/yuzhao/Downloads/hackerrank-median.edn")))

(median arr)


(def moving-arr (to-array (range 10)))

(r-insertion-sort moving-arr)

(defn sorted-median [sorted]
  (let [len   (count sorted)
        index (quot len 2)]
    (if (even? len)
      (/ (+ (aget sorted index)
            (aget sorted (dec index)))
         2)
      (aget sorted index))))

(defn update-median [val-remove val-insert]
  (alter-var-root moving-arr (remove-from-sorted! moving-arr val-remove))
  (alter-var-root moving-arr (insert-to-sorted! moving-arr val-insert))
  (sorted-median moving-arr))


(def moving-v (vec (range 10)))

(def sorted (atom (vec (sort (range 10)))))

(defn sorted-median [sorted]
  (let [len (count sorted)
        index (quot len 2)]
    (if (even? len)
      (/ (+ (get sorted index)
            (get sorted (dec index)))
         2)
      (get sorted index))))

(defn insert-to-sorted! [sorted val]
  ;; sorted must be asc
  ;; return a java array
  (let [[v1 v2] (split-with (partial > val) @sorted)]
    (reset! sorted (vec (concat v1 [val] v2)))))

(defn remove-from-sorted! [sorted val]
  (let [[v1 v2] (split-with (partial > val) @sorted)]
    (reset! sorted (vec (concat v1 (rest v2))))))

(defn update-median [sorted val-remove val-insert]
  (remove-from-sorted! sorted val-remove)
  (insert-to-sorted! sorted val-insert)
  (sorted-median @sorted))

(time
 (let [n 200000
       d 10000
       expenditure (edn/read-string
                    (slurp
                     "/Users/yuzhao/Downloads/hackerrank-median.edn"))]
   (defn activityNotifications []
     (let [sorted (atom (vec (sort (take (dec d) expenditure))))
           infi Double/NEGATIVE_INFINITY
           _      (insert-to-sorted! sorted infi)]
       (loop [index 0
              times 0]
         (if (= index (- n d))
           times
           (recur (inc index)
                  (let [curr (+ index d)]
                    (if (>=
                         (get expenditure curr)
                         (* 2 (update-median
                               sorted
                               (if (zero? index)
                                 infi
                                 (get expenditure (dec index)))
                               (get expenditure (dec curr)))))
                      (inc times)
                      times)))))))
   (prn "Answer: " (activityNotifications))))




;; couting sort

(time
 (let [n 200000
       d 10000
       expenditure (edn/read-string
                    (slurp
                     "/Users/yuzhao/Downloads/hackerrank-median.edn"))
       n 9
       d 5
       expenditure [2 3 4 2 3 6 8 4 5]
       
       ;; n 5
       ;; d 4
       ;; expenditure [1 2 3 4 4]

       ;; n 5
       ;; d 3
       ;; expenditure [10 20 30 40 50]
       counter (atom (vec (repeat 201 0)))
       update-counter (fn update-counter [remove insert]
                        (let [mid (quot (inc d) 2)
                              median (if (even? d)
                                       (/
                                        (loop [sum 0
                                               index 0
                                               i  0
                                               j  0]
                                          (if (and (not (zero? i)) (not (zero? j)))
                                            (+ i j)
                                            (let [sum (+ sum (get @counter index))]
                                              (recur
                                               sum
                                               (inc index)
                                               (if (zero? i)
                                                 (if (>= mid sum)
                                                   i index)
                                                 i)
                                               (if (zero? j)
                                                 (if (> mid sum)
                                                   j index)
                                                 j)))))
                                        2)
                                       (loop [sum 0
                                              index 0]
                                         (let [sum (+ sum (get @counter index))]
                                           (if (> mid sum)
                                             (recur
                                              sum
                                              (inc index))
                                             index))))]
                          (prn "median: " median)
                          (swap! counter assoc remove (dec (get @counter remove)))
                          (swap! counter assoc insert (inc (get @counter insert)))
                          median))]

   (defn activityNotifications []
     (let [first-d (take d expenditure)]
       (loop [l first-d]
         (when-not (empty? l)
           (swap! counter assoc (first l) (inc (get @counter (first l))))
           (recur (rest l))))
       (loop [index 0
              times 0]
         (if (= index (- n d))
           times
           (recur (inc index)
                  (let [curr (+ index d)
                        median (update-counter
                                  (get expenditure index)
                                  (get expenditure curr))]
                    (if (>= (get expenditure curr)
                            (* 2 median))
                      (inc times)
                      times)))))))
   (prn "Answer: " (activityNotifications))))


;; === merge sort ====

(defn merge* [arr p q r]
  #_(prn "merge " p q r)
  (let [n1    (- q p -1)
        n2    (- r q)
        left  (java.util.Arrays/copyOfRange arr p (inc q))
        right (java.util.Arrays/copyOfRange arr (inc q) (inc r))]
    (loop [i 0
           j 0
           k p]
      #_(prn "loop" i j k " n1 " n1 " n2 " n2)
      (when (<= k r)
        (cond

          (and (= i n1)
               (= j n2))
          nil

          (= i n1)
          (do
            #_(prn "aset " k (aget right j))
            (aset arr k (aget right j))
            (recur i (inc j) (inc k)))

          (= j n2)
          (do
            #_(prn "aset " k (aget left i) (count left) (type left))
            (aset arr k (aget left i))
            (recur (inc i) j (inc k)))

          :else
          (let [left-i  (aget left i)
                right-j (aget right j)]
            #_(prn "left right" left-i right-j)
            (if (<= left-i right-j)
              (do
                (aset arr k left-i)
                (recur (inc i) j (inc k)))
              (do
                (aset arr k right-j)
                (recur i (inc j) (inc k))))))))))

(defn merge-sort [arr p r]
  ;; initial p = 0, and
  ;; r = (count arr)
  #_(prn "merge sort " p r)
  (when (< p r)
    (let [q (quot (+ p r) 2)]
      (merge-sort arr p q)
      (merge-sort arr (inc q) r)
      (merge* arr p q r))))



(do
  (def arr (to-array [7, 9, 6, 15, 16, 18, 13, 1, 10, 12, 4, 11, 8, 3, 14, 2, 17, 19, 5, 0]))
  (merge-sort arr 0 19))

(defn copy-of-range [arr beg n]
  (let [new-arr (java.lang.reflect.Array/newInstance (type (first arr)) n)]
    (System/arraycopy arr beg new-arr 0 n)
    new-arr))

(defn merge-inversions [arr p q r]
  (let [n1    (- q p -1)
        n2    (- r q)
        left  (copy-of-range arr p n1)
        right (copy-of-range arr (inc q) n2)]
    (loop [i 0 j 0 k p cnt 0]
      (cond

          (and (= i n1)
               (= j n2))
          cnt

          (= i n1)
          (do
            (aset arr k (aget right j))
            (recur i (inc j) (inc k) cnt))

          (= j n2)
          (do
            (aset arr k (aget left i))
            (recur (inc i) j (inc k) cnt))

          :else
          (let [left-i  (aget left i)
                right-j (aget right j)]
            (if (<= left-i right-j)
              (do
                (aset arr k left-i)
                (recur (inc i) j (inc k) cnt))
              (do
                (aset arr k right-j)
                (recur i (inc j) (inc k) (+ cnt (- n1 i))))))))))

(defn merge-sort-inversions [arr p r]
  ;; initial p = 0, and
  ;; r = (count arr)
  (if (< p r)
    (let [q (quot (+ p r) 2)]
      (+
       (merge-sort-inversions arr p q)
       (merge-sort-inversions arr (inc q) r)
       (merge-inversions arr p q r)))
    0))

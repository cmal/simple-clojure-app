(ns simple-clojure-app.simple
  (:require [clojure.repl :refer [doc source]]))

(defn rec-step [[x & xs]]
  (if x
    [x (rec-step xs)]
    []))

(rec-step [1 2 3])
(rec-step '(1 2 3))


(def very-lazy
  (-> (iterate #(do (print \.) (inc %)) 1)
      rest rest rest))

(def less-lazy
  (-> (iterate #(do (print \.) (inc %)) 1)
      next next next))

(println (first very-lazy))
(println (first less-lazy))

(clojure-version)

(defn lz-rec-step [s]
  (lazy-seq
   (if (seq s)
     [(first s) (lz-rec-step (rest s))]
     [])))

(lz-rec-step [1 2 3 4])

(class (lz-rec-step [1 2 3 4]))
(type (lz-rec-step [1 2 3 4]))

(class (lz-rec-step [1]))

(dorun (lz-rec-step (range 200000)))
(rec-step (range 200000))


(defn simple-range [i limit]
  (lazy-seq
   (when (< i limit)
     (cons i (simple-range (inc i) limit)))))

;; clojure.lang.LazySeq
(class (simple-range 0 9))
;; clojure.lang.Cons
(class (next (simple-range 0 9)))
;; clojure.lang.LazySeq
(class (rest (simple-range 0 9)))
(dorun (simple-range 0 9))
(dorun (simple-range 0 200000))
;; clojure.lang.Cons
(class (next (simple-range 0 9)))

(let [r (range 1e9)]
  (first r)
  (last r))

(let [r (range 1e9)]
  (last r)
  (first r))

(defn triangle [n]
  (/ (* n (+ n 1)) 2))

(triangle 10)

(map triangle (range 1 11))

(def tri-nums
  (map triangle (iterate inc 1)))

(class tri-nums)
(take 10 tri-nums)

(= (map triangle (range 1 10000001))
   (take 10000000 tri-nums))

(take 10 (filter even? tri-nums))

(nth tri-nums 999)

(take 10 (map / tri-nums))

(double (reduce + (take 10000 (map / tri-nums))))

(take 2 (drop-while #(< % 10000) tri-nums))

;; delay and force

(defn defer-expensive [cheap expensive]
  (if-let [good-enough (force cheap)]
    good-enough
    (force expensive)))

(defer-expensive
  (delay :cheap)
  (delay (do (Thread/sleep 1000) :expensive)))

(defer-expensive
  (delay false)
  (delay (do (Thread/sleep 1000) :expensive)))

(delay? (delay false))

@(delay false)

(delay false)


(defn inf-triangles [n]
  {:head (triangle n)
   :tail (delay (inf-triangles (inc n)))})

(defn head [l]
  (:head l))

(defn tail [l]
  (force (:tail l)))

(def tri-nums
  (inf-triangles 1))

(class tri-nums)

(first tri-nums)

(head tri-nums)
(head (tail tri-nums))
(-> tri-nums tail tail head)

(defn taker [n l]
  (loop [t n
         src l
         ret []]
    (if (zero? t)
      ret
      (recur (dec t)
             (tail src)
             (conj ret (head src))))))

(defn nthr [l n]
  (if (zero? n)
    (head l)
    (recur (tail l) (dec n))))

(taker 10 tri-nums)

(nthr tri-nums 99)


;; lazy quick-sort

(defn rand-ints [n]
  (take n (repeatedly #(rand-int n))))

(rand-ints 10)

(def counter (atom 0))

(defn sort-parts [work]
  (lazy-seq
   (loop [[part & parts] work]
     (if-let [[pivot & xs] (seq part)]
       (let [smaller? #(< % pivot)
             new-work (list*
                       (filter smaller? xs)
                       pivot
                       (remove smaller? xs)
                       parts)]
         (recur new-work))
       (when-let [[x & parts] parts]
         (cons x (sort-parts parts)))))))
;;        ^^^^               ^^^^^
;;        每次都得到第一个元素
;;                           并保证剩余部分的第一个元素是seqable

(defn my-sort-parts [v]
  (lazy-seq
   (loop [[pivot & xs]])))

(defn qsort [xs]
  (sort-parts (list xs)))

(qsort [2 1 4 3])

(qsort (rand-ints 6))

(nth (qsort (rand-ints 10000)) 9999)

(reset! counter 0)

(do
  (reset! counter 0)
  (dotimes [_ 100]
    (println (take 1 (qsort (rand-ints 100000))))))

(println @counter) ;; 2 million

(do
  (reset! counter 0)
  (dotimes [_ 100]
    (println (take 10 (qsort (rand-ints 100000))))))

(println @counter) ;; 20 million

(when-let [[x y] [nil nil]] 1)
(when-let [[x y] nil] 1)

(list* [1 2 3] 4 [5])

(take 10 (qsort (rand-ints 1E6)))

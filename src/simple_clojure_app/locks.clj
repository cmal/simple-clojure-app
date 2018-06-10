(ns simple-clojure-app.locks
  (:refer-clojure :exclude [aget aset count seq])
  (:require
   [clojure.core :as clj]
   [simple-clojure-app.concurrency :refer [dothreads!]]
   )
  (:use [clojure.repl]))

;; locks

(defprotocol SafeArray
  (aset [this i f])
  (aget [this i])
  (count [this])
  (seq [this]))

(defn make-dumb-array [t sz]
  (let [a (make-array t sz)]
    (reify
      SafeArray
      (count [_] (clj/count a))
      (seq [_] (clj/seq a))
      (aget [_ i] (clj/aget a i))
      (aset [this i f]
        (clj/aset a i (f (aget this i)))))))

(defn pummel [a]
  (dothreads! #(dotimes [i (count a)] (aset a i inc))
              :threads 100))


(def D (make-dumb-array Integer/TYPE 8))

(pummel D)

(println (seq D)) ;; wrong!

;; safe mutation through locking
(defn make-safe-array [t sz]
  (let [a (make-array t sz)]
    (reify
      SafeArray
      (count [_] (clj/count a))
      (seq [_] (clj/seq a))
      (aget [_ i]
        (locking a
          (clj/aget a i)))
      (aset [this i f]
        (locking a
          (clj/aset a i (f (aget this i))))))))

(doc make-array)

(def A (make-safe-array Integer/TYPE 8))
(pummel A)
(println (seq A))

(doc locking)

;; using Java's explicit locks

;; lock striping
(defn lock-i [target-index num-locks]
  (mod target-index num-locks))

(import 'java.util.concurrent.locks.ReentrantLock)

(defn make-smart-array [t sz]
  (let [a (make-array t sz)
        Lsz (/ sz 2)
        ;; create locks
        L (into-array (take Lsz
                            (repeatedly #(ReentrantLock.))))]
    (reify
      SafeArray
      (count [_] (clj/count a))
      (seq [_] (clj/seq a))
      (aget [_ i]
        (let [lk (clj/aget L (lock-i (inc i) Lsz))]
          (.lock lk)
          (try
            (clj/aget a i)
            (finally (.unlock lk)))))
      (aset [this i f]
        (let [lk (clj/aget L (lock-i (inc i) Lsz))]
          (.lock lk)
          (try
            (clj/aset a i (f (aget this i)))
            (finally (.unlock lk))))))))

(def S (make-smart-array Integer/TYPE 8))

(pummel S)

(seq S)

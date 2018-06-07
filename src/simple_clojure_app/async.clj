(ns simple-clojure-app.async
  (:require [clojure.core.async :as async :refer :all]))

(chan)

(chan 10)

(let [c (chan)]
  (close! c))

(let [c (chan 10)]
  ;; blocking put
  (>!! c "hello")
  (assert (= "hello" (<!! c)))
  (close! c))

(let [c (chan)]
  ;; if not use thread, will block
  (thread (>!! c "hello"))
  (assert (= "hello" (<!! c)))
  (close! c))


(let [c (chan)]
  (go (>! c "hello"))
  (assert (= "hello" (<!! (go (<! c))))) ;; (<!! c) is also right
  (close! c))

(let [c1 (chan)
      c2 (chan)]
  (thread (while true
            (let [[v ch] (alts!! [c1 c2])]
              (println "Read" v "from" ch))))
  (>!! c1 "hi")
  (>!! c2 "there"))

(let [c1 (chan)
      c2 (chan)]
  (thread (while true
            (let [[v1 ch1] (alts!! [c1])
                  [v2 ch2] (alts!! [c2])]
              (println "Read" v1 "from" ch1)
              (println "Read" v2 "from" ch2))))
  (>!! c1 "hi")
  (>!! c2 "there"))

(let [c1 (chan)
      c2 (chan)]
  (go (while true
        (let [[v ch] (alts! [c1 c2])]
          (println "Read" v "from" ch))))
  (go (>! c1 "hi"))
  (go (>! c2 "there")))

(let [n 1000
      cs (repeatedly n chan)
      begin (System/currentTimeMillis)]
  (doseq [c cs] (go (>! c "hi")))
  (dotimes [i n]
    (let [[v c] (alts!! cs)]
      (assert (= "hi" v))))
  (println "Read" n "msgs in" (- (System/currentTimeMillis) begin) "ms"))

(let [t (timeout 100)
      begin (System/currentTimeMillis)]
  (println (<!! t))
  (println "Waited" (- (System/currentTimeMillis) begin)))

(let [c (chan)
      begin (System/currentTimeMillis)]
  (alts!! [c (timeout 100)])
  (println "Gave up after" (- (System/currentTimeMillis) begin)))


(let [c1 (chan)
      c2 (chan)]
  (go
    (while true
      (let [[v ch] (alts! [c1 c2])]
        (println "Read" v "from" ch))))
  (go (>! c1 "hi"))
  (go (>! c2 "there")))

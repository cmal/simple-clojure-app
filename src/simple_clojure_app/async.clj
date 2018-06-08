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

;; drop newest
(chan (dropping-buffer 3))

(let [c (chan (dropping-buffer 1))]
  ;; blocking put
  (>!! c "helloHelloworld")
  (assert (= "helloHelloworld" (<!! c)))
  (close! c))

;; drop oldest
(chan (sliding-buffer 3))

;; pub sub
(def input-chan (chan))

(def our-pub (pub input-chan :msg-type))

our-pub
(def an-ch1 (chan 1))
(take! an-ch1
       (fn [x]
         (println "Clojure callback value " x)))

(put! an-ch1 "XYZ")
(put! an-ch1 "XYZ")
(put! an-ch1 "ABC")

(>!! input-chan {:msg-type :greeting :text "hello"})

(def output-chan (chan))

(sub our-pub :greeting output-chan)

(>!! input-chan {:msg-type :greeting :text "hi"})

(go-loop []
  (let [{:keys [text]} (<! output-chan)]
    (println text)
    (recur)))

;; double subs
(let [c (chan)]
  (sub our-pub :greeting c)
  (go-loop []
    (let [{:keys [msg-type text]} (<! c)]
      (println msg-type text)
      (recur))))


;; sliding-buffer dropping-buffer

(def c (chan (sliding-buffer 1)))

(>!! c "A")
(>!! c "B")
(>!! c "SDAFDF")

(go-loop []
  (println "Sliding buffer Received: " (<! c))
  (recur))

(def c1 (chan (dropping-buffer 1)))
(>!! c1 "A")
(>!! c1 "B")
(>!! c1 "C")
(go (println "Dropping buffer Received: " (<! c1)))

(def loser-chan (chan))

(sub our-pub :loser loser-chan)

;; hanging a go block inside
(>!! input-chan {:msg-type :loser :text "I won't be accepted"})

(def pub-central
  (let [topic-fn #(case (:msg-type %)
                    :db-change :acid
                    :http-request :stateless)
        buf-fn #(case %
                  :stateless (sliding-buffer 10)
                  :acid (dropping-buffer 1000))]
    (pub REQUEST-SOURCE topic-fn buf-fn)))


;; go block best practices
(def c (chan 1))
(def c (chan (sliding-buffer 1)))

;; instead of using
(go (>! c 42))
;; use
(put! c 42)

(go-loop []
  (println "c Received: " (<! c))
  (recur))


;; The go macro stops translating at function creation boundaries.
(go (let [my-fn (fn []
                  (prn (<! c)))]
      (my-fn)))
;; Assert failed: <! used not in (go ...) block


;; the followings will not work
(go (map <! some-chan))
(go (for [c [c]]
      (println ">>>" (<! c))))


; This works just fine
(go (doseq [c [c]]
      (println "<<<" (<! c))))


(clojure.core/map str [1 2 3])

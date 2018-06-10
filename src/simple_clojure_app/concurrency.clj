(ns simple-clojure-app.concurrency
  (:require
   [simple-clojure-app.reflect :refer [pprint-class]]
   [simple-clojure-app.functional :refer [neighbors]])
  (:use [clojure.pprint]
        [clojure.repl])
  (:import java.util.concurrent.Executors))

(def thread-pool
  (Executors/newFixedThreadPool
   (+ 2 (.availableProcessors (Runtime/getRuntime)))))

(defn dothreads!
  [f & {thread-count :threads
        exec-count :times
        :or {thread-count 1 exec-count 1}}]
  (dotimes [t thread-count]
    (.submit thread-pool
             #(dotimes [_ exec-count] (f)))))

;; (clojure.lang.Reflector/invokeConstructor Runtime)

(pprint-class Thread)

;; (pprint-class Executors) ;; FIXME

(dothreads!
 #(.print System/out "hi ") :threads 2 :times 2)

(def initial-board
  [[:- :k :-]
   [:- :- :-]
   [:- :K :-]])

(defn board-map [f board]
  (vec (map #(vec (for [s %] (f s)))
            board)))

(defn reset-board!
  "Resets the board state.  Generally these types of functions are a
  bad idea, but matters of page count force our hand."
  []
  (def board (board-map ref initial-board))
  (def to-move (ref [[:K [2 1]] [:k [0 1]]]))
  (def num-moves (ref 0)))

(def king-moves
  (partial neighbors
           [[-1 -1] [-1 0]
            [-1 1] [0 -1]
            [0 1] [1 -1]
            [1 0] [1 1]]
           3))

(defn good-move?
  [to enemy-sq]
  (when (not= to enemy-sq)
    to))

#_(defn print-board
  []
  (pprint
   (vec (map #(vec (for [s %] (deref s)))
             board))))

(defn choose-move
  "Randomly choose a legal move"
  [[[mover mpos] [_ enemy-pos]]]
  [mover (some #(good-move? % enemy-pos)
               (shuffle (king-moves mpos)))])

(reset-board!)

(take 5 (repeatedly #(choose-move @to-move)))
#_(print-board)

(defn place [form to] to)

(defn move-piece [[piece dest] [[_ src] _]]
  (alter (get-in board dest) place piece)
  (alter (get-in board src) place :-)
  (alter num-moves inc))

(defn update-to-move [move]
  (alter to-move #(vector (second %) move)))

;; a wrong version
(defn make-move []
  (let [move (choose-move @to-move)]
    (dosync (move-piece move @to-move))
    (dosync (update-to-move move))))

(reset-board!)

(make-move)

(pprint (board-map deref board))
;; for now everythings is OK

(dothreads! make-move :threads 100 :times 100)

(pprint (board-map deref board)) ;; wrong board!



(io! (.println System/out "Haikeeba!"))

(dosync (io! (.println System/out "Haikeeba!")))
;; java.lang.IllegalStateException:   I/O in transaction

(defn make-move-v2 []
  (dosync
   (let [move (choose-move @to-move)]
     (move-piece move @to-move)
     (update-to-move move))))

(reset-board!)
(make-move)

(pprint (board-map deref board))

@num-moves

(dothreads! make-move-v2 :threads 100 :times 100)

(pprint (board-map #(dosync (deref %)) board))

@to-move

@num-moves

;; commute
(defn move-piece [[piece dest] [[_ src] _]]
  (commute (get-in board dest) place piece)
  (commute (get-in board src) place :-)
  (commute num-moves inc))

(reset-board!)

(dothreads! make-move-v2 :threads 100 :times 100)

@to-move

(defn update-to-move [move]
  (commute to-move #(vector (second %) move)))

(dothreads! make-move-v2 :threads 100 :times 100)

@to-move

(dosync ref-set to-move '[[:K [2 1]] [:k [0 1]]])

;; history
(defn stress-ref [r]
  (let [slow-tries (atom 0)]
    (future
      (dosync
       (swap! slow-tries inc)
       (Thread/sleep 200)
       @r)
      (println (format "r is : %s, history: %d, after: %d tries"
                       @r (.getHistoryCount r) @slow-tries)))
    (dotimes [i 500]
      (Thread/sleep 10)
      (dosync (alter r inc)))
    (println :done)
    :done))

(stress-ref (ref 0))

(stress-ref (ref 0 :max-history 30))

(stress-ref (ref 0 :min-history 15 :max-history 30))


;; agents

(def joy (agent []))

(send joy conj "First edition")

@joy

(defn slow-conj [coll item]
  (Thread/sleep 10000)
  (conj coll item))

(send joy slow-conj "Second edition")

@joy

(def log-agent (agent 0))

(defn do-log [msg-id message]
  (println msg-id ":" message)
  (inc msg-id))

(defn do-step [channel message]
  (Thread/sleep 1)
  (send-off log-agent do-log (str channel message)))

(defn three-step [channel]
  (do-step channel " ready to begin (step 0)")
  (do-step channel " warming up (step 1)")
  (do-step channel " really getting going now (step 2)")
  (do-step channel " done! (step 3)"))

(defn all-together-now []
  (dothreads! #(three-step "alpha"))
  (dothreads! #(three-step "beta"))
  (dothreads! #(three-step "omega")))

(all-together-now)

@log-agent

(do-step "important: " "this must go out")

(await log-agent)

(send log-agent (fn [_] 1000))

(do-step "epsilon " "near miss")

(defn exercise-agents [send-fn]
  (let [agents (map #(agent %) (range 10))]
    (doseq [a agents]
      (send-fn a (fn [_] (Thread/sleep 1000))))
    (doseq [a agents]
      (await a))))

(time (exercise-agents send-off))

(time (exercise-agents send))

;; fail mode by default
(send log-agent (fn [] 2000)) ;; error arg number

@log-agent

(agent-error log-agent)

(send log-agent (fn [_] 3000))

@log-agent

(restart-agent log-agent 2500 :clear-actions true)

(send-off log-agent do-log "The agent, it lives!")

(restart-agent log-agent 2500 :clear-actions true) ;; does not need a restart

;; continue mode
(defn handle-log-error [the-agent the-err]
  (println "An action sent to the log-agent threw " the-err))

(set-error-handler! log-agent handle-log-error)

;; change error mode to continue
(set-error-mode! log-agent :continue)

;; incorrect
(send log-agent (fn [x] (/ x 0)))

;; also incorrect
(send log-agent (fn [] 0))

(send-off log-agent do-log "Stayin' alive, stayin' alive...")

@log-agent

;; atoms

(def ^:dynamic *time* (atom 0))
(defn tick [] (swap! *time* inc))

(dothreads! tick :threads 1000 :times 100)

@*time*

;; using atoms in transaction

(defn manipulable-memoize [function]
  (let [cache (atom {})]
    (with-meta
      (fn [& args]
        (or (second (find @cache args))
            (let [ret (apply function args)]
              (swap! cache assoc args ret)
              ret)))
      {:cache cache})))

(def slowly (fn [x] (Thread/sleep 1000) x))

(time [(slowly 9) (slowly 9)])

(def sometimes-slowly (manipulable-memoize slowly))

(time [(sometimes-slowly 108) (sometimes-slowly 108)])

(meta sometimes-slowly)

(let [cache (:cache (meta sometimes-slowly))]
  (swap! cache dissoc '(108)))

(meta sometimes-slowly)

(time [(sometimes-slowly 108) (sometimes-slowly 108)])

;; see locks.clj

;; ...

;; vars and dynamic binding

*read-eval*

(var *read-eval*)

#'*read-eval*

(defn print-read-eval []
  (println "*read-eval* is currently" *read-eval*))

(defn binding-play []
  (print-read-eval)
  (binding [*read-eval* false]
    (print-read-eval))
  (print-read-eval))

(binding-play)

;; use defn, defmacro, defonce, defmulti
;; to create a named var

(def favorite-color :green)

favorite-color

(var favorite-color)

(defn test-x []
  (println (resolve 'x)
           (bound? #'x)
           (thread-bound? #'x)))

(do
  (def x)
  (test-x))

(do
  (def x 5)
  (test-x))

(do
  (def ^:dynamic x)
  (binding [x 7]
    (test-x)))

(with-local-vars [y 9]
  (println (resolve 'y)
           #_(bound? #'y) ;; error
           #_(thread-bound? #'y) ;; error
           ))



(doc def)

(def x 42)
{:outer-var-value x
 :with-locals (with-local-vars [x 9]
                {:local-var       x
                 :local-var-value (var-get x)})}


*math-context*

(with-precision 4
  (/ 1M 3))

(/ 1M 3)

(with-precision 4
  (map (fn [x] (/ x 3) (range 1M 4M))))

(with-precision 4
  (doall (map (fn [x] (/ x 3)) (range 1M 4M))))

(with-precision 4
  (map (bound-fn [x] (/ x 3)) (range 1M 4M)))

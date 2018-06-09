(ns simple-clojure-app.functional
  (:require [clojure.repl :refer [doc source]]
            [clojure.test :as t])
  (:use [clojure.pprint]
        [clojure.reflect]))

(map [:chthon :phthor :beowulf :grendel] #{0 3})

(= (fn []) (fn []))

(class (fn []))

analogy

(def fifth (comp first rest rest rest rest))
(fifth [1 2 3 4 5])

(defn fnth [n]
  (apply comp
         (cons first
               (take (dec n) (repeat rest)))))

(fnth 5)

((fnth 5) '[a b c d e])

(map (comp
      keyword
      #(.toLowerCase %)
      name)
     '(a B C))

((partial + 5) 100 200)
(#(apply + 5 %&) 100 200)
(apply + 5 '(100 200))

summation

;; complement
(let [truthiness (fn [v] v)]
  [((complement truthiness) true)
   ((complement truthiness) 42)
   ((complement truthiness) false)
   ((complement truthiness) nil)])

(= ((comp not empty?) [])
   ((complement empty?) []))


(class comp)
(class (fn []))

(defn join
  {:test (fn []
           (assert
            (= (join "," [1 2 3]) "1,3,3")))}
  [sep s]
  (apply str (interpose sep s)))

;; (use '[clojure.test :as t])
(test #'join) ;; Assert failed: (= (join "," [1 2 3]) "1,3,3")

(join "," "safdf")

(defn ^:private ^:dynamic sum [nums]
  (apply + nums))

(defn ^{:private true :dynamic true} sum [nums]
  (apply + nums))

(defn sum {:private true :dynamic true} [nums]
  (apply + nums))

(sum [1 2 3])

;; for `:dynamic`
(def foo 1) ;; root binding
(binding [foo 1] foo)
;; java.lang.IllegalStateException: Can't dynamically bind non-dynamic var: simple-clojure-app.functional/foo

(def ^:dynamic bar 1)
(binding [bar 2] bar)
bar ;; => 1
(defn print-bar []
  (println "bar:" bar))

(print-bar) ;; bar: 1
(binding [bar 2] (print-bar)) ;; bar: 2
(let [bar 2] (print-bar)) ;; bar: 1

(.start (Thread. (print-bar))) ;; bar: 1

(binding [bar 2]
  (.start (Thread. (print-bar)))) ;; bar: 2

(let [bar 2]
  (.start (Thread. (print-bar)))) ;; bar: 1

;; change bar

(let [bar 2] (set! bar 3)) ;; java.lang.IllegalArgumentException

(binding [bar 2]
  (print-bar) ;; bar: 2
  (set! bar 3)
  (print-bar)) ;; bar: 3

;; binding

(defn ^:dynamic fib [n]
  (loop [ n n r 1]
    (if (= n 1)
      r
      (recur (dec n) (* n r)))))

(defn call-fibs [a b]
  (+ (fib a) (fib b)))

(call-fibs 3 3)

(binding [fib (memoize fib)] 
  (call-fibs 9 10))

(def members
  (:members (reflect (Thread.))))

;; (pprint (map class members))

(print-table [:name :type :flags] (sort-by :name members))

(print-table members)


(class (Thread.))

(defn pprint-class [class-name & args]
  (print-table
   (for [meth (-> (clojure.lang.Reflector/invokeConstructor class-name (into-array Object args))
                  .getClass
                  .getMethods)]
     (->> meth
          ((juxt
            #(.getName %)
            #(vec (map (fn [t] (.getName t))
                       (.getParameterTypes %)))
            #(.getReturnType %)
            #(.getModifiers %)
            #(vec (map (fn [t] (.getName t))
                       (.getExceptionTypes %)))))
          (zipmap [:name :params :returnType :modifiers :exceptions])))))

(pprint-class Thread)

(sort-by second [[:a 73] [:c 13] [:b 21]])

(def plays
  [{:band "Burial", :plays 979, :loved 9}
   {:band "Eno", :plays 2333, :loved 15}
   {:band "Bill Evans", :plays 979, :loved 9}
   {:band "Magma", :plays 2665, :loved 31}])

(def sort-by-loved-ratio (partial sort-by #(/ (:plays %) (:loved %))))

(print-table (sort-by-loved-ratio plays))

(print-table (sort-by (columns [:plays :loved :band]) plays))

(defn columns [column-names]
  (fn [row]
    (vec (map row column-names))))

(columns [:plays :loved :band])

((columns [:plays :loved :band])
 {:band "burial"
  :plays 979
  :loved 9})

(vec (map (plays 0) [:plays :loved :band]))

(sort [[2 1] [1 2]])

tertiary

(defn keys-apply [f ks m]
  (let [only (select-keys m ks)]
    (zipmap (keys only)
            (map f (vals only)))))

(keys-apply #(.toUpperCase %) #{:band} (plays 0))

(defn manip-map [f ks m]
  (merge m (keys-apply f ks m)))

(manip-map #(.toUpperCase %) #{:band} (plays 0))

(manip-map #(int (/ % 2)) #{:plays :loved} (plays 0))

dissolves

(defn mega-love! [ks]
  ;; because plays could change, this is not pure function
  (map (partial manip-map #(int (* % 1000)) ks) plays))

(print-table (mega-love! [:loved :plays]))

(defn slope
  [& {:keys [p1 p2] :or {p1 [0 0] p2 [1 1]}}]
  (float (/ (- (p2 1) (p1 1))
            (- (p2 0) (p1 0)))))

(slope :p1 [4 15] :p2 [3 21])
(slope :p2 [2 1])
(slope)

(defn slope [p1 p2]
  {:pre  [(not= p1 p2) (vector? p1) (vector? p2)]
   :post [(float? %)]}
  (/ (- (p2 1) (p1 1))
     (- (p2 0) (p1 0))))

(slope [10 10] [10 10])
(slope [10 1] '(1 20))
(slope [10 1] [1 20])
(slope [10.0 1] [1 20])

(defn put-things [m]
  (into m {:meat "beef" :veggie "broccoli"}))

(put-things {})

(defn vegan-constrains [f m]
  {:pre  [(:veggie m)]
   :post [(:veggie %) (nil? (:meat %))]}
  (f m))

(vegan-constrains put-things {:veggie "carrot"})

(defn balanced-diet [f m]
  {:post [(:meat %) (:veggie %)]}
  (f m))

(balanced-diet put-things {})

(defn finicky [f m]
  {:post [(= (:meat %) (:meat m))]}
  (f m))

(finicky put-things {:meat "chicken"})


;; closures

(def times-two
  (let [x 2]
    (fn [y] (* y x))))

(times-two 5)

(def add-and-get
  (let [ai (java.util.concurrent.atomic.AtomicInteger.)]
    (fn [y] (.addAndGet ai y))))

(add-and-get 2)
(add-and-get 2)
(add-and-get 7)

(defn times-n [n]
  (let [x n]
    (fn [y] (* y x))))

(times-n 4)
(def times-four (times-n 4))

(times-four 10)

(defn times-n [n]
  (fn [y] (* y n)))
((times-n 4) 10)

(defn divisible [denom]
  (fn [num]
    (zero? (rem num denom))))

((divisible 3) 6)
((divisible 3) 7)

(filter even? (range 10))
(filter (divisible 4) (range 10))
(defn filter-divisible [denom s]
  (filter (fn [num] (zero? (rem num denom))) s))

(filter-divisible 4 (range 10))

(defn filter-divisible [denom s]
  (filter #(zero? (rem % denom)) s))

(filter-divisible 5 (range 20))


;; bot
(def bearings
  [{:x 0 :y 1} ;; north
   {:x 1 :y 0} ;; east
   {:x 0 :y -1} ;; source
   {:x -1 :y 0} ;; west
   ])

(defn forward [x y bearing-num]
  [(+ x (:x (bearings bearing-num)))
   (+ y (:y (bearings bearing-num)))])

(forward 5 5 0)
(forward 5 5 1)
(forward 5 5 2)

(defn bot [x y bearing-num]
  {:coords  [x y]
   :bearing ([:north :east :source :west] bearing-num)
   :forward (fn [] (bot
                    (+ x (:x (bearings bearing-num)))
                    (+ y (:y (bearings bearing-num)))
                    bearing-num))})

(:coords (bot 5 5 0))
(:bearing (bot 5 5 0))

(:coords ((:forward (bot 5 5 0))))

(defn bot [x y bearing-num]
  {:coords     [x y]
   :bearing    ([:north :east :south :west] bearing-num)
   :forward    (fn [] (bot
                       (+ x (:x (bearings bearing-num)))
                       (+ y (:y (bearings bearing-num)))
                       bearing-num))
   :turn-right (fn [] (bot x y (mod (+ 1 bearing-num) 4)))
   :turn-left  (fn [] (bot x y (mod (- 1 bearing-num) 4)))})

(:bearing ((:forward ((:forward ((:turn-right (bot 5 5 0))))))))
(:coords ((:forward ((:forward ((:turn-right (bot 5 5 0))))))))

(defn mirror-bot [x y bearing-num]
  {:coords     [x y]
   :bearing    ([:north :east :source :west] bearing-num)
   :forward    (fn [] (mirror-bot (- x (:x (bearings bearing-num)))))
   :turn-right (fn [] (mirror-bot x y (mod (- 1 bearing-num) 4)))
   :turn-left  (fn [] (mirror-bot x y (mod (+ 1 bearing-num) 4)))})



(defn pow [base exp]
  (if (zero? exp)
    1
    (* base (pow base (dec exp)))))

(pow 2 10)
(pow 1.01 925)
(pow 2 10000)

(defn pow [base exp]
  (letfn [(kapow [base exp acc]
            (if (zero? exp)
              acc
              (recur base (dec exp) (* base acc))))]
    (kapow base exp 1)))

(pow 2N 10000)

;; see units.clj

;; TCO

(defn gcd [x y]
  (cond
    (> x y) (gcd (- x y) y)
    (< x y) (gcd x (- y x))
    :else   x))

(defn gcd [x y]
  (cond
    (> x y) (recur (- x y) y)
    (< x y) (recur x (- y x))
    :else   x))

;; FSA
(defn elevator [commands]
  (letfn
      [(ff-open [[_ & r]]
         "When the elevator is open on the 1st floor
          it can either close or be done."
         #(case _
            :close (ff-closed r)
            :done  true
            false))
       (ff-closed [[_ & r]]
         "When the elevator is closed on the 1st floor
          it can either open or go up."
         #(case _
            :open (ff-open r)
            :up   (sf-closed r)
            false))
       (sf-closed [[_ & r]]
         "When the elevator is closed on the 2nd floor
          it can either go down or open."
         #(case _
            :down (ff-closed r)
            :open (sf-open r)
            false))
       (sf-open [[_ & r]]
         "When the elevator is open on the 2nd floor
          it can either close or be done"
         #(case _
            :close (sf-closed r)
            :done  true
            false))]
    (trampoline ff-open commands)))

(elevator [:close :open :close :up :open :open :done])
(elevator [:close :open :close :up :open :done])
(elevator (cycle [:close :open]))


;; CPS
(defn fac-cps [n k]
  (letfn [(cont [v] (k (* v n)))]
    (if (zero? n)
      (k 1)
      (recur (dec n) cont))))

(defn fac [n]
  (fac-cps n identity))

(fac 10)

;; 1. Accept 2. Return 3. Continuation
(defn mk-cps [accept? kend kont]
  (fn [n]
    ((fn [n k]
       (let [cont (fn [v]
                    (k ((partial kont v) n)))]
         (if (accept? n)
           (k 1)
           (recur (dec n) cont))))
     n kend)))

(def fac
  (mk-cps zero?
          identity
          #(* %1 %2)))

(fac 10)

(def tri
  (mk-cps #(== 1 %)
          identity
          #(+ %1 %2)))

(tri 10)


;; A* algorithm

(def world
  [[1 1 1 1 1]
   [999 999 999 999 1]
   [1 1 1 1 1]
   [1 999 999 999 999]
   [1 1 1 1 1]])

(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]]
                        size yx))
  ([deltas size yx]
   (filter (fn [new-yx]
             (every? #(< -1 % size) new-yx))
           (map #(vec (map + yx %))
                deltas))))

(neighbors 5 [0 0])

(defn estimate-cost [step-cost-est size y x]
  (* step-cost-est
     (- (+ size size) y x 2)))

(estimate-cost 900 5 0 0)
(estimate-cost 900 5 4 4)

(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost
     (or (:cost cheapest-nbr) 0)))

(path-cost 900 {:cost 1})

(defn total-cost [newcost step-cost-est size y x]
  (+ newcost
     (estimate-cost step-cost-est size y x)))

(estimate-cost 900 5 0 0)
(total-cost 0 900 5 0 0)
(estimate-cost 900 5 3 4)
(total-cost 1000 900 5 3 4)
(total-cost (path-cost 900 {:cost 1}) 900 5 3 4)

(defn astar [start-yx step-est cell-costs]
  (let [size (count cell-costs)]
    (loop [steps     0
           routes    (vec (repeat size (vec (repeat size nil))))
           work-todo (sorted-set [0 start-yx])]
      (if (empty? work-todo)
        [(peek (peek routes)) :steps steps]
        (let [[_ yx :as work-item] (first work-todo)
              rest-work-todo       (disj work-todo work-item)
              nbr-yxs              (neighbors size yx)
              cheapest-nbr         (min-by :cost
                                           (keep #(get-in routes %)
                                                 nbr-yxs))
              newcost              (path-cost (get-in cell-costs yx)
                                              cheapest-nbr)
              oldcost              (:cost (get-in routes yx))]
          (if (and oldcost (>= newcost oldcost))
            (recur (inn steps) routes rest-work-todo)
            (recur (inc steps)
                   (assoc-in routes yx
                             {:cost newcost
                              :yxs  (conj (:yxs cheapest-nbr [])
                                          yx)})
                   (into rest-work-todo
                         (map
                          (fn [w]
                            (let [[y x] w]
                              [(total-cost newcost step-est size y x) w]))
                          nbr-yxs)))))))))

(astar [0 0]
       900
       world)

(astar [0 0]
       900
       [[1 1 1 2 1]
        [1 1 1 999 1]
        [1 1 1 999 1]
        [1 1 1 1 1]])

(astar [0 0]
       900
       [[1 1 1 2 1]
        [1 1 1 999 1]
        [1 1 1 999 1]
        [1 1 1 666 1]])

(astar [0 0]
       900
       [[1 1 1 2 1]
        [1 1 1 999 1]
        [1 1 1 999 1]
        [1 1 1 666 1]])


;; juxt
(def each-math (juxt + * - /))
(each-math 2 3)
((juxt take drop) 3 (range 9))

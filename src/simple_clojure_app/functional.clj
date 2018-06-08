(ns simple-clojure-app.functional
  (:require [clojure.repl :refer [doc source]]))

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



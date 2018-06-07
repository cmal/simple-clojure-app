(ns simple-clojure-app.scalars)

(def clueless 9)

(class clueless)

(class (+ clueless 90000000000000000))

(class (+ clueless 900000000000000000000000000))

(class (+ clueless 9.0))

(+ Long/MAX_VALUE Long/MAX_VALUE)

Long/MAX_VALUE

(Long/MAX_VALUE)

(unchecked-add (Long/MAX_VALUE) (Long/MAX_VALUE))

;;(float 0.0000000000000000000000000000000000000000000001)

;;(+ 1.0E-430M 1)


;;(clojure.string/join (repeat 30000 "1234"))

(let [approx-interval (/ 209715 2097152)
      actual-interval (/ 1 10)
      hours (* 3600 100 10)
      actual-total (double (* hours actual-interval))
      approx-total (double (* hours approx-interval))]
  (- actual-total approx-total))

(= (+ 0.1M 0.1M 0.1M 0.1 0.1M 0.1M 0.1M 0.1M 0.1M 0.1M)
   (+ 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1))
(+ 0.1M 0.1M 0.1M 0.1M 0.1M 0.1M 0.1M 0.1M 0.1M 0.1M)

1.0E-430000000M
1.0E-4300000000M

(contains? {:a 1} :a)

(rest (into-array [1 2 3]))

(count (aclone (into-array [1 2 3])))

(defn len [x]
  (.length x))

(defn len2 [^String x]
  (.length x))

(def a-string
  (clojure.string/join
   (repeat 1000 "sdkfjksd")))

(time (len a-string))
(time (len2 a-string))

(def a 1.0e50)
(def b -1.0e50)
(def c 17.0e00)

(+ a b c)
(+ (+ a b) c)

(def ra (rationalize 1.0e50))
(def rb (rationalize -1.0e50))
(def rc (rationalize 17.0e00))

(+ ra rb rc)

(ratio? ra)
(rational? ra)
(ratio? 2/4)

(numerator 2/4)
(denominator 2/4)

::not-in-ns

(ns another)
:user/in-another

(defn do-blowfish [directive]
  (case directive
    :another/blowfish (println "feed the fish")
    :crypto/blowfish (println "encode the message")
    :blowfish (println "not sure what to do")))

(ns crypto)
(another/do-blowfish :blowfish)
(another/do-blowfish :crypto/blowfish)
(another/do-blowfish :another/blowfish)
(another/do-blowfish ::blowfish)

'goat

(identical? 'goat 'goat)

(type 'goat)

(meta 'goat)

(let [x (with-meta 'goat {:ornery true})
      y (with-meta 'goat {:ornery false})]
  [(= x y)
   (identical? x y)
   (meta x)
   (meta y)])

(= 'goat 'goat)

(name 'goat)
(let [x 'goat
      y x]
  (identical? x y))

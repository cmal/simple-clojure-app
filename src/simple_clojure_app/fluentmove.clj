(ns simple-clojure-app.fluentmove)

(defn build-move [& pieces]
  (apply hash-map pieces))

(build-move :from "e7" :to "e8" :promotion \Q)

(defrecord Move [from to castle? promotion]
  Object
  (toString [this]
    (str "Move " (:from this)
         " to " (:to this)
         (if (:castle? this) " castel"
             (if-let [p (:promotion this)]
               (str " promote to " p)
               "")))))

(str (Move. "e2" "e4" nil nil))

(.println System/out (Move. "e7" "e8" nil \Q))

(defn build-move [& {:keys [from to castle? promotion]}]
  {:pre [from to]}
  (Move. from to castle? promotion))

(str (build-move :from "e2" :to "e4"))

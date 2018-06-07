(ns simple-clojure-app.core
  (:require [clojure.tools.nrepl.server :refer [start-server]])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (let [port 17890]
    (start-server :port port)
    (println (str "nrepl server started at port: " port "!")))
)



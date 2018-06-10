(ns simple-clojure-app.reflect
  (:require
   [clojure.pprint :refer [print-table]]))

;; class-name must be defined in clojure?
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

(ns simple-clojure-app.demo
  (:require [clojure.string :as str])
  (:use [clojure.repl]))


;; seq
(defn my-map [f coll]
  (reduce (fn [res cur]
            (conj res (f cur)))
          []
          coll))


;; file number
;; get-all-files: get all files and dirs
;; file-stat: {:filetype .. :filename ..}
;; get-line-count: get common file line count
(defn get-all-files [dir]
  ["a-dir" "a-file"])

(defn file-stat [file]
  {:filetype "dir"
   :filename "a-dir"})

(defn get-line-count [file]
  1)

(defn sum-of-java-line-number [dir]
  (let [files (get-all-files dir)]
    (reduce +
     (for [file files]
       (let [{:keys [filetype filename]} (file-stat file)]
         (case filetype
           "dir"  (sum-of-java-line-number filename)
           "file" (if (str/ends-with? filename ".java")
                    (get-line-count filename)
                    0)))))))
(doc reduce)

(source file-seq)

(defrecord TreeNode [val l r]
  FIXO
  (fixo-push [t v]
    (if (< v val)
      (TreeNode. val (fixo-push l v) r)
      (TreeNode. val l (fixo-push r v))))
  (fixo-peek [t]
    (if l
      (fixo-peek l)
      val))
  (fixo-pop [t]
    (if l
      (TreeNode. val (fixo-pop l) r)
      r))
  (rotate [t]
    (when t
      (TreeNode. val (rotate r) (rotate l)))))

#()
#{}


(def cnt (atom 0))


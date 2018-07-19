(ns simple-clojure-app.dijkstra)

(def ^:private inf Double/POSITIVE_INFINITY)

(defn update-costs
  "Returns costs updated with any shorter paths found to curr's
  unvisisted neighbors by using curr's shortest path"
  [g costs unvisited curr]
  (let [curr-cost (get costs curr)]
    (reduce-kv
      (fn [c nbr nbr-cost]
        (if (unvisited nbr)
          (update-in c [nbr] min (+ curr-cost nbr-cost))
          c))
      costs
      (get g curr))))


;; this algorithm returns costs, do not return paths.
(defn dijkstra
  "Returns a `map` of `nodes` to minimum `cost` from src using
  Dijkstra algorithm.  Graph is a map of nodes to map of neighboring
  nodes and associated cost.  Optionally, specify destination node to
  return once cost is known"
  ([g src]
    (dijkstra g src nil))
  ([g src dst]
    (loop [costs (assoc (zipmap (keys g) (repeat inf)) src 0)
           curr src
           unvisited (disj (apply hash-set (keys g)) src)]
      (cond
       (= curr dst)
       (select-keys costs [dst])

       (or (empty? unvisited) (= inf (get costs curr)))
       costs

       :else
       (let [next-costs (update-costs g costs unvisited curr)
             next-node (apply min-key next-costs unvisited)]
         (recur next-costs next-node (disj unvisited next-node)))))))

(def demo-graph {:red    {:green 10, :blue   5, :orange 8},
                 :green  {:red 10,   :blue   3},
                 :blue   {:green 3,  :red    5, :purple 7},
                 :purple {:blue 7,   :orange 2},
                 :orange {:purple 2, :red    2}})

(dijkstra demo-graph :red)
;; => {:green 8, :blue 5, :purple 10, :red 0, :orange 8}

(dijkstra demo-graph :red :purple)
;; => {:purple 10}



;; another version TODO read NOTE: first read the 3rd version follows
;; result is shortest path and min cost

(use 'clojure.contrib.def)

(declare dijkstra build-path add-rdist update-rdists take-minnode)

(defn shortest-path
  ([net root nodedst children distance]
     " return [path dist]"
     " net is the graph "
     " root the source node "
     " nodedst the destination "
     " children a function returning the children for a node "
     " distance a function returning the distance between two nodes "
     (let [preds (dijkstra net root nodedst children distance)
	   path (build-path preds root nodedst)]
       (when path
	 [path (second (preds nodedst))])))

  ([net root nodedst children]
     (shortest-path net root nodedst children (constantly 1))))

(defn- dijkstra [net root nodedst children distance]
  (loop [rdists (sorted-map 0 {root root})
	 minnode root
	 preds {root [root 0]}
	 dist 0]
    ; (printf "minnode = %s preds = %s rdists = %s\n\n\n" minnode preds rdists)
    (if (empty? rdists)
      preds
      (let [[nminnode ndist nrdists npreds] (take-minnode rdists preds)
	    [nnrdists nnpreds] (update-rdists nrdists
					      npreds
					      net
					      nminnode
					      ndist
					      children distance)]
	(recur nnrdists nminnode nnpreds ndist)))))

(defn- build-path [preds root nodedst]
  "reverse walk on preds to reconstruct the shortest path"
  (loop [[pred dist] (preds nodedst) path (list nodedst)]
      (when pred
        (if (= pred root)
          (cons root path)
          (recur (preds pred) (cons pred path))))))

(defn- add-rdist
  ([rdists node pred dist]
  "add a known rdist (rdist = distance to the root)"
  (assoc rdists dist (assoc (rdists dist) node pred)))

  ([rdists node pred dist prevdist]
     (let [nrdists (add-rdist rdists node pred dist)
	   minnodes (rdists prevdist)
	   nminnodes (dissoc minnodes node)]
       (if (empty? nminnodes)
	 (dissoc nrdists prevdist)
	 (assoc nrdists prevdist nminnodes)))))

(defn- update-rdists [rdists preds net node dist children distance]
  "return [rdists preds] updated"
  (reduce (fn [acc x]
            (let [curdist (+ dist (distance net node x))
                  prevdist (second (preds x))
                  nrdists (first acc)
                  npreds (second acc)]
              (if (nil? prevdist)
                [(add-rdist nrdists x node curdist) (assoc npreds x [node curdist])]
                (if (< curdist prevdist)
                  [(add-rdist nrdists x node curdist prevdist)
                   (assoc npreds x [node curdist])]
                  [nrdists npreds]))))
          [rdists preds]
          (children net node)))

(defn- take-minnode [rdists preds]
  "return a vector [minnode dist rdists preds]"
  (let [ [dist minnodes] (first rdists)
         [minnode pred] (first minnodes)
         others (rest minnodes)]
    [minnode
     dist
     (if (empty? others)
        (dissoc rdists dist)
        (assoc rdists dist others))
     (assoc preds minnode [pred dist]) ]))


(comment

;;
;; Example (based on the french wikipedia)
;; http://fr.wikipedia.org/wiki/Algorithme_de_Dijkstra
;;

(def net {:A {:B 85, :C 217, :E 173},
	  :B {:F 80},
	  :C {:G 186 :H 103},
	  :D {},
	  :E {:J 502},
	  :F {:I 250}
	  :G {},
	  :H {:D 183 :J 167}
	  :I {:J 84},
	  :J {}
	  })


(defn children [net node]
  (keys (net node)))

(defn distance [net nodesrc nodedst]
  ((net nodesrc) nodedst))

;(defn nodes [net]
;  (apply hash-set (keys net)))

(let [pathinfo (shortest-path net :A :J children distance)]
  (printf "path = %s\n" pathinfo)) ;; [(:A :C :H :J) 487]

;; with all distances = 1
(let [pathinfo (shortest-path net :A :J children)]
  (printf "path = %s\n" pathinfo)) ;; [(:A :E :J) 2]

)



;; yet another version TODO READ

(ns other.dijkstra
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure.test              :refer [is deftest]]))

(def infinity Double/POSITIVE_INFINITY)
(def all-nodes (comp set flatten (juxt keys #(map keys (vals %)))))

(defn dijkstra-step [graph to-visit]
  (let [[current {:keys [path distance]}] (peek to-visit)
        update-neighbours (fn [distances [node edge-length]]
                            (update distances node
                                    (comp first #(sort-by :distance [%1 %2]))
                                    {:distance (+ distance edge-length)
                                     :path (conj (or path []) current)}))
        new-distances (reduce update-neighbours
                              to-visit
                              (select-keys (graph current) (keys to-visit)))]
    (dissoc new-distances current)))

(defn shortest-path [graph start end]
  (->> (-> (into (priority-map-keyfn :distance)
                 (map vector (all-nodes graph) (repeat {:distance infinity})))
           (assoc start {:distance 0}))
       (iterate (partial dijkstra-step graph))
       (some (fn [to-visit]
               (when (or (= end (-> to-visit peek first))
                         (every? #(= infinity (:distance %)) (vals to-visit)))
                 (let [end-node (to-visit end)]
                   [(when-let [path (:path end-node)]
                      (conj path end))
                    (or (:distance end-node) infinity)]))))))

(def g1 {:a {:b 1}})

(def g2 {:a {:b 2
             :c 3
             :d 9}
         :b {:d 8}
         :c {:d 5}})

(def g3 {:a {:b 1}
         :c {:d 1}})

(def g4 {:a {:b 2
             :c 3}
         :b {:d 1}
         :c {:d 1}
         :d {:e 1}})

(deftest simple-graph
  (is (= [[:a :b] 1] (shortest-path g1 :a :b))))

(deftest more-complex-graph
  (is (= [[:a :c :d] 8] (shortest-path g2 :a :d))))

(deftest longer-graph
  (is (= [[:a :b :d :e] 4] (shortest-path g4 :a :e))))

(deftest unreachable-nodes-are-infinite
  (is (= [nil infinity] (shortest-path g3 :a :d)))
  (is (= [nil infinity] (shortest-path g2 :b :c)))
  (is (= [nil infinity] (shortest-path g2 :c :b))))

(deftest unknown-start-end-is-infinite
  (is (= [nil infinity] (shortest-path g1 :a :foo)))
  (is (= [nil infinity] (shortest-path g1 :foo :a))))

#_(defn shortest-pathv02 [graph start end]
    (loop [to-visit (-> (into (priority-map)
                              (zipmap (all-nodes graph)
                                      (repeat Double/POSITIVE_INFINITY)))
                        (assoc start 0))]
      (if (or (= end (-> to-visit peek first))
              (every? #(= Double/POSITIVE_INFINITY %) (vals to-visit)))
        (to-visit end)
        (let [[current base-distance] (peek to-visit)
              new-distances (reduce (fn [distances-accum [k prev-distance]]
                                      (update distances-accum k
                                              min (+ base-distance prev-distance)))
                                    to-visit
                                    (select-keys (graph current) (keys to-visit)))]
          (recur (dissoc new-distances current))))))


#_(defn shortest-pathv01 [graph start end]
    (loop [distances (-> (into (priority-map)
                               (zipmap (all-nodes graph)
                                       (repeat Double/POSITIVE_INFINITY)))
                         (assoc start 0))
           visited #{}]
      (let [to-visit (apply dissoc distances visited)]
        (if (or (visited end)
                (every? #(= Double/POSITIVE_INFINITY %) (vals to-visit)))
          (distances end)
          (let [[current base-distance] (peek to-visit)
                new-distances (reduce (fn [distances-accum [k prev-distance]]
                                        (update distances-accum k
                                                min (+ base-distance prev-distance)))
                                      distances
                                      (graph current))]
            (recur new-distances (conj visited current)))))))



;; TODO read the 4th version
;; see https://github.com/xpe/clj-dijkstra

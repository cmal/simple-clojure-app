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



;; another version
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
       (if (nil? path)
	 nil
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
      (if (nil? pred)
	nil
        (if (= pred root)
          (cons root path)
          (recur (preds pred) (cons pred path))))))

(defn- add-rdist
  ([rdists node pred dist]
  "add a known rdist (rdist = distance to the root)"
  (if-let [nodes (rdists dist)]
    (assoc rdists dist (assoc nodes node pred))
    (assoc rdists dist {node pred})))

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

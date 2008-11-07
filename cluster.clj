;; Clustering algorithms in Clojure
;;
;; Perform a hierarchical cluster with: hcluster
;;   (hcluster vectors)
;;   vectors - a sequence of vectors
;;
;; Perform k-means clustering with: kcluster
;;   (kcluster vectors number-of-clusters range-start range-end)
;;   vectors - a sequence of vectors
;;   

(ns cluster
    (:use cluster.internal))

(defn kcluster
  "Performs k-means clustering.
    :vectors - a sequence of vectors
    :how-many - how many clusters to find
    :start - lower limit of numbers in vectors
    :end - upper limit of numbers in vectors

  (kcluster [[1 2] [3 4] [5 6]] 2 0 6)"
  ([vectors how-many start end]
     (kcluster vectors (random-vectors how-many (count (nth vectors 0)) start end)))
  ([vectors nodes]
     (let [clusters
           (loop [index 0
                  clusters (vec (replicate (count nodes) []))]
             (if (= (count vectors) index)
               clusters
               (let [vector (nth vectors index)
                     [sim closest-node] (closest-vector vector nodes)]
                 (recur 
                  (inc index)
                  (assoc clusters 
                    closest-node 
                    (conj (nth clusters closest-node) index))))))
           new-nodes (map (fn [cluster] (average-vectors (map #(nth vectors %) cluster))) clusters)]
       (if (= new-nodes nodes)
         [clusters new-nodes]
         (kcluster vectors new-nodes)))))


(defn hcluster 
  "Performs hierarchical clustering.
    :nodes - a sequence of maps of the form:
      { :vec [1 2 3] }
   The return value will be a tree of Maps of the form:
     { :vec [] :left { :vec ... } :right { :vec ... } }"
  [nodes]
  (if (< (count nodes) 2)
    nodes
    (let [vectors (map #(get % :vec) nodes)
          [closest-pair cls] (closest-vectors vectors)
          [left-idx right-idx] closest-pair
          new-nodes (without nodes left-idx right-idx)
          left-node (nth nodes left-idx)
          right-node (nth nodes right-idx)]
      (hcluster (conj 
                 new-nodes
                 {:left left-node
                  :right right-node 
                  :vec (average-vectors 
                        [(get left-node :vec) 
                         (get right-node :vec)])})))))



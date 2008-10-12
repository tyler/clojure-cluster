;; Internal functions used by clojure.cluster

(ns cluster.internal)

(defn mean [v1] (/ (reduce + v1) (count v1)))
(defn sqrt [n] (. Math sqrt n))
(defmacro square [n] 
  `(let [n# ~n] (* n# n#)))

(defn sum [v] (reduce + v))

(defn pearson [v1 v2]
  (let [v1mean (mean v1)
        v2mean (mean v2)]
    (/ (sum (map
             #(* (- %1 v1mean) (- %2 v2mean))
             v1 v2))
       (sqrt (* (sum (map #(square (- % v1mean)) v1))
                (sum (map #(square (- % v2mean)) v2)))))))

(defn compact [v]
  (loop [v1 v out '()]
    (if (= 0 (count v1))
      out
      (recur (rest v1) 
             (if (nil? (first v1))
               out 
               `(~@out ~(first v1)))))))
    

(defn average-vectors [vectors]
  (loop [rests vectors out '()]
    (let [firsts (compact (map first rests))]
      (if (= 0 (count rests))
        out
        (recur (compact (map rest rests)) `(~@out ~(mean firsts)))))))

(defmacro grtr [n1 n2] `(if (> ~n1 ~n2) ~n1 ~n2))

(defn closest-vector
  ([target others] (closest-vector target others 0))
  ([target others n]
   (let [current (first others)
         sim (pearson target current)
         others (rest others)]
     (if (= 0 (count others))
       [sim, n]
       (let [[other-sim, other-n] (closest-vector target others (inc n))]
         (if (> sim other-sim) [sim,n] [other-sim,other-n]))))))
      
(defn n-in-cycle [cyclen start i]
  (let [absi (+ start i 1)]
    (if (>= absi cyclen)
      (n-in-cycle cyclen start (- absi cyclen))
      absi)))

(defn closest-vectors [vs]
  (let [vcyc (cycle vs) vlen (count vs)]
    (loop [ptr 0 pair [0 0] c -1.0]
      (if (= ptr vlen)
        [pair c]
        (let [[cp cn] (closest-vector 
                       (nth vs ptr) 
                       (take (dec vlen) (nthrest vcyc (inc ptr))))
              closer? (if (> cp c) true nil)]
          (recur (inc ptr)
                 (if closer? [ptr (n-in-cycle vlen ptr cn)] pair)
                 (if closer? cp c)))))))

(defn include? [v i]
  (some #(= i %) v))

(defn without [v & more]
  (loop [cv v ov '() ptr 0]
    (if (= 0 (count cv))
      ov
      (recur (rest cv)
             (if (not (include? more ptr))
               `(~@ov ~(first cv))
               ov)
             (inc ptr)))))


(defn random-vector [length range-start range-end]
  (loop [index 0 vector []]
    (if (= index length)
      vector
      (recur (inc index)
             (cons (+ range-start 
                      (rand (- range-end range-start))) 
                   vector)))))

(defn random-vectors [how-many length range-start range-end]
  (loop [how-many-left how-many
         vectors []]
    (if (= 0 how-many-left)
      vectors
      (recur (dec how-many-left)
             (cons (random-vector length range-start range-end) vectors)))))

(defn list-of [how-many value]
  (loop [how-many-left how-many
         list []]
    (if (= 0 how-many-left)
      list
      (recur (dec how-many-left) (conj list value)))))



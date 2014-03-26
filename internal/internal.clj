;; Internal functions used by clojure.cluster

(ns cluster.internal)

(defn mean [v1] (/ (reduce + v1) (count v1)))
(defn sqrt [n] (. Math sqrt n))
(defmacro square [n] 
  `(let [n# ~n] (* n# n#)))

(defn sum [v] (reduce + v))



(defn pearson [x y]
  (let [n (count x)
        o (/ 1 n)
        sum-y (sum y)
        sum-x (sum x)
        prod-of-sqrts (* (sqrt (- (sum (map #(square %) x))
                                  (* o (square sum-x)))) 
                         (sqrt (- (sum (map #(square %) y))
                                  (* o (square sum-y)))))]
    (if (= 0.0 prod-of-sqrts)
      nil
      (/ (- (sum (map #(* %1 %2) x y))
            (* o sum-x sum-y)) 
         prod-of-sqrts))))



(defn compact [v]
  (filter #(not (nil? %)) v))
    

(defn average-vectors [vectors]
  (loop [rests vectors out '()]
    (let [firsts (compact (map first rests))]
      (if (= 0 (count firsts))
        out
        (recur (compact (map rest rests)) `(~@out ~(mean firsts)))))))

(defmacro grtr [n1 n2] `(if (> ~n1 ~n2) ~n1 ~n2))

(defn closest-vector
  ([target others] (closest-vector target others 0))
  ([target others n]
   (let [current (first others)
         sim (or (pearson target current) 0.0)
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





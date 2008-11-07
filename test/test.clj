;; Tests for clojure.cluster

(ns cluster.test
    (:use unit-test)
    (:use cluster)
    (:use cluster.internal))


;; hcluster tests

(deftest test-hcluster-empty-nodes []
  (assert-equal [] (hcluster [])))

(deftest test-hcluster-single-node []
  (assert-equal [{:vec []}]
                (hcluster [{:vec []}])))

(deftest test-hcluster-two-nodes []
  (assert-equal '({:left {:vec [1 2]}
                   :right {:vec [2 3]}
                   :vec [3/2 5/2]})
                (hcluster [{:vec [1 2]}
                           {:vec [2 3]}])))

(deftest test-hcluster-three-nodes []
  (assert-equal '({:left {:left {:vec [1 2]} 
                          :right {:vec [4 8]}
                          :vec (5/2 5)}
                   :right {:vec [2 3]}
                   :vec (9/4 4)})
                (hcluster [{:vec [1 2]}
                           {:vec [2 3]}
                           {:vec [4 8]}])))


;; kcluster tests

(deftest test-kcluster-two-nodes []
  (let [[[[x] [y]] nodes] (kcluster [[1 2] [2 1]] 2 1 2)]
    (assert-true (and (or (= x 1) (= x 0))
                      (or (= y 1) (= y 0))
                      (= (+ x y) 1)))))


;; internal tests
    
(deftest test-mean []
  (assert-equal (mean [0]) 0)
  (assert-equal (mean [1]) 1)
  (assert-equal (mean [1 2]) 1.5))

(deftest test-sqrt []
  (assert-equal (sqrt 4) 2)
  (assert-equal (sqrt 9) 3)
  (assert-equal (sqrt 2.25) 1.5))

(deftest test-square []
  (assert-equal (square 3) 9)
  (assert-equal (square 0) 0))

(deftest test-sum []
  (assert-equal (sum [1 2 3]) 6))

(deftest test-pearson []
  (assert-equal 0.9999999999999998 (pearson [1 2] [1 2]))
  (assert-equal -0.9999999999999998 (pearson [1 2] [2 1]))
  (assert-equal 0.0 (pearson [1 2 3] [1 2 1])))

(deftest test-compact []
  (assert-equal [1 2 3] (compact [nil 1 nil 2 nil 3 nil nil])))

(deftest test-average-vectors []
  (assert-equal [2 2 2] (average-vectors [[1 3 1] [3 1 3] [2 2 2]])))

(deftest test-grtr []
  (assert-equal 2 (grtr 1 2))
  (assert-equal 2 (grtr 2 1)))

(deftest test-closest-vector []
  (assert-equal [0.9999999999999998, 0] (closest-vector [1 2 3] [[1 2 3] [1 2 1]]))
  (assert-equal [0.9999999999999998, 1] (closest-vector [1 2 3] [[1 2 1] [1 2 3]]))
  (assert-equal [0.0, 1] (closest-vector [1 2 3] [[3 2 1] [1 2 1]])))

(deftest test-closest-vectors []
  (assert-equal [[0 1] 0.9999999999999998] (closest-vectors [[1 2 3] [1 2 3] [1 2 1]]))
  (assert-equal [[0 2] 0.9999999999999998] (closest-vectors [[1 2 3] [1 2 1] [1 2 3]]))
  (assert-equal [[1 2] 0.9999999999999998] (closest-vectors [[1 2 1] [1 2 3] [1 2 3]])))

(deftest test-include? []
  (assert-true (include? [1 2 3] 1))
  (assert-nil (include? [1 2 3] 4)))

(deftest test-without []
  (assert-equal [1 2 4] (without [1 2 3 4] 2))
  (assert-equal [1 4] (without [1 2 3 4] 2 1)))

(deftest test-random-vector []
  (let [r (random-vector 5 0 1)]
    (assert-equal 5 (count r))
    (assert-equal true (every? #(< % 1) r))
    (assert-equal true (every? #(> % 0) r))))

(deftest test-random-vectors []
  (let [rs (random-vectors 4 5 0 1)]
    (assert-equal 4 (count rs))
    (assert-true (every? #(= 5 (count %)) rs))))

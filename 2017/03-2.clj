;; https://www.reddit.com/r/adventofcode/comments/7h7ufl/2017_day_3_solutions/dqowiyb/
(defn spiral-steps
  ([] (partition 2 (interleave
                    (take-while (constantly true) (spiral-steps 1))
                    (cycle [:right :up :left :down]))))

  ([n] (lazy-seq (cons n (cons n (spiral-steps (inc n)))))))

(def normalized-spiral-steps
  (mapcat (fn [[distance direction]] (repeat distance direction))
          (spiral-steps)))

(defn next-location [i j direction]
  (case direction
    :up [(inc i) j]
    :down [(dec i) j]
    :left [i (dec j)]
    :right [i (inc j)]))

(defn neighbor-locations [i j]
  [[i (inc j)]
   [(inc i) (inc j)]
   [(inc i) j]
   [(inc i) (dec j)]
   [i (dec j)]
   [(dec i) (dec j)]
   [(dec i) j]
   [(dec i) (inc j)]])

(defn matrix
  ([steps]
   (lazy-seq (cons 1 (matrix (next-location 0 0 (first steps))
                             (rest steps)
                             {[0 0] 1}))))

  ([[i j] steps m]
   (let [direction (first steps)
         n (reduce + (map #(m % 0) (neighbor-locations i j)))]
     (lazy-seq (cons n
                     (matrix (next-location i j direction)
                             (rest steps)
                             (assoc m [i j] n)))))))

(first (drop-while (partial >= 325489) (matrix normalized-spiral-steps)))

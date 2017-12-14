(def input [10 3 15 10 5 15 5 15 9 2 5 8 5 2 3 6])

(defn index-of-max [coll]
  (.indexOf coll (apply max coll)))

(defn f [coll]
  (into []
        (let [i (index-of-max coll)
              v (coll i)
              quot (quot v (count coll))
              mod (mod v (count coll))
              ks (map #(clojure.core/mod % (count coll))
                      (map inc (range i (+ i mod))))]
          (-> coll
              (assoc i 0)

              (as-> coll
                (mapv (partial + quot) coll)
                (reduce #(update-in %1 [%2] inc) coll ks))))))

(loop [input input
       m #{}
       n 0]
  (if (m input)
    n

    (let [new-input (f input)]
      (recur new-input (conj m input) (inc n)))))

;; Tortoise and Hare
;; https://en.wikipedia.org/wiki/Cycle_detection#Floyd.27s_Tortoise_and_Hare
(loop [tortoise (f input)
       hare (f tortoise)
       n 0
       phase 0]
  (case phase
    0 (if (= tortoise hare)
        (recur input hare n (inc phase))
        (recur (f tortoise) (f (f hare)) 0 phase))

    1 (if (= tortoise hare)
        (recur tortoise (f tortoise) (inc n) (inc phase))
        (recur (f tortoise) (f hare) (inc n) phase))

    2 (if (= tortoise hare)
        n
        (recur tortoise (f hare) (inc n) phase))))

;; Brent's algorithm
;; https://en.wikipedia.org/wiki/Cycle_detection#Brent's_algorithm
(loop [tortoise input
       hare (f tortoise)
       steps 1
       step-limit 2
       phase 0]
  (case phase
    0 (if (= tortoise hare)
        (recur input
               (nth (iterate f input) steps)
               steps
               step-limit
               (inc phase))

        (if (= steps step-limit)
          (recur hare (f hare) 1 (.pow (biginteger step-limit) 2) phase)
          (recur tortoise (f hare) (inc steps) step-limit phase)))

    1 (if (= tortoise hare)
        steps
        (recur (f tortoise) (f hare) (inc steps) step-limit phase))))

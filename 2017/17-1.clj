(def input 380)

(defn insert [v i e]
  (into [] (concat (subvec v 0 i) [e] (subvec v i))))

(loop [coll [0]
       i 0
       n 1
       step input]
  (let [new-i (inc (mod (+ i step) (count coll)))
        new-coll (insert coll new-i n)]
    (if (= n (int 2017))
      (new-coll (inc new-i))
      (recur new-coll new-i (inc n) step))))

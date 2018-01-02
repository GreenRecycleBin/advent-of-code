(def input 380)

(loop [result -1
       count 1
       i 0
       n 1
       step input]
  (let [new-i (inc (mod (+ i step) count))]
    (if (= n (int 50e6))
      result
      (recur (if (= new-i 1) n result) (inc count) new-i (inc n) step))))

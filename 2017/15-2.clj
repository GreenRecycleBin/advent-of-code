(def mask 0xFFFF)

(defn f [factor]
  #(mod (* factor %1) 2147483647))

(loop [ga (filter #(zero? (mod % 4)) (drop 1 (iterate (f 16807) 679)))
       gb (filter #(zero? (mod % 8)) (drop 1 (iterate (f 48271) 771)))
       i 0
       n 0]
  (if (= i (long 5e6))
    n

    (let [a (first ga)
          b (first gb)
          same-16-lsb-bits (= (bit-and a mask) (bit-and b mask))]
      (recur (rest ga) (rest gb) (inc i) (if same-16-lsb-bits (inc n) n)))))

(def input "ugkiagan")

(defn char->byte [input]
  (map int input))

(defn f [coll start length]
  (let [end (mod (+ start length) (count coll))]
    (vec
     (if (<= start end)
       (concat (subvec coll 0 start)
               (reverse (subvec coll start end))
               (subvec coll end))

       (let [reversed (vec
                       (reverse
                        (concat (subvec coll start (count coll))
                                (subvec coll 0 end))))
             i (- (count coll) start)]
         (concat (subvec reversed i)
                 (subvec coll end start)
                 (subvec reversed 0 i)))))))

(defn compute-knot-hash [input]
  (-> input
      char->byte
      (concat [17 31 73 47 23])

      (as-> input
          (loop [coll (vec (range 256))
                 start 0
                 skip 0
                 original-input input
                 input input
                 n 0]
            (if (= n 64)
              coll

              (if (seq input)
                (let [length (first input)]
                  (recur (f coll start length)
                         (mod (+ start length skip) (count coll))
                         (inc skip)
                         original-input
                         (rest input)
                         n))

                (recur coll
                       start
                       skip
                       original-input
                       original-input
                       (inc n)))))

        (->> input
             (partition 16)
             (map (partial apply bit-xor))))))

(->> (range 128)
     (map (partial str input "-"))
     (map (partial compute-knot-hash))
     (mapcat (fn [knot-hash] (map #(Integer/bitCount %) knot-hash)))
     (reduce +))

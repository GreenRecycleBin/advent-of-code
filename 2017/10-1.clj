(def input [225 171 131 2 35 5 0 13 1 246 54 97 255 98 254 110])

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

(loop [coll (vec (range 256))
       start 0
       skip 0
       input input]
  (if (seq input)
    (let [length (first input)]
      (recur (f coll start length)
             (mod (+ start length skip) (count coll))
             (inc skip)
             (rest input)))

    (* (first coll) (second coll))))

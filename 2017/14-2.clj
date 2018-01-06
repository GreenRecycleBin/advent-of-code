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

(defn big-endian->little-endian [byte]
  (let [binary-string (Integer/toBinaryString byte)
        space-padded-binary-string (format "%8s" binary-string)
        zero-padded-binary-string (clojure.string/replace space-padded-binary-string \space \0)]
    (Short/parseShort (clojure.string/reverse zero-padded-binary-string) 2)))

(defn bytes->bitset [bytes]
  (java.util.BitSet/valueOf (byte-array (map big-endian->little-endian bytes))))

(defn index->row-column [bitsets index]
  (let [n (count bitsets)]
    [(quot index n) (mod index n)]))

(defn row-column->index [bitsets row column]
  (let [n (count bitsets)]
    (+ (* row n) column)))

(defn neighbors [bitsets index]
  (let [[row column] (index->row-column bitsets index)]
    (when (.get (bitsets row) column)
      (let [directions [[0 1] [1 0] [0 -1] [-1 0]]]
        (->> directions
             (map (partial mapv + [row column]))

             (filter (fn [[row column]]
                       (and (< -1 row (count bitsets))
                            (< -1 column (count bitsets))
                            (.get (bitsets row) column))))

             (map (fn [[row column]]
                    (row-column->index bitsets row column))))))))

(defn build-bitsets [input]
  (->> (range 128)
       (map (partial str input "-"))
       (map (partial compute-knot-hash))
       (mapv bytes->bitset)))

(defn build-adjacency-map [bitsets]
  (let [n (count bitsets)]
    (reduce (fn [m index]
              (let [[row column] (index->row-column bitsets index)]
                (cond-> m
                  (.get (bitsets row) column)
                  (assoc index (neighbors bitsets index)))))

            {}
            (range (int (Math/pow n 2))))))

(def bitsets (build-bitsets input))
(def adjacency-map (build-adjacency-map bitsets))

(defn dfs [index]
  (loop [open (conj clojure.lang.PersistentQueue/EMPTY index)
         visited #{index}
         adjacency-map adjacency-map]
    (if (seq open)
      (let [index (peek open)
            neighbors (->> (adjacency-map index)
                           (remove visited))]
        (recur (into (pop open) neighbors)
               (conj visited index)
               adjacency-map))

      visited)))

(loop [indices (keys adjacency-map)
       visited #{}
       connected-components-count 0]
  (if (seq indices)
    (let [index (first indices)]
      (if (some #{index} visited)
        (recur (rest indices) visited connected-components-count)

        (recur (rest indices)
               (into visited (dfs index))
               (inc connected-components-count))))

    connected-components-count))

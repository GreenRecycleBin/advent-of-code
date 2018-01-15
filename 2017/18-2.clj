(def input
  "set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 952
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19")

(defn parse-int-or-return-as-is [s]
  (try
    (Integer/parseInt s)
    (catch NumberFormatException _ s)))

(defn value [register-or-number m]
  (let [register-or-number (parse-int-or-return-as-is register-or-number)]
    (if (int? register-or-number)
      register-or-number
      (or (m register-or-number) 0))))

(def matches
  (->> input
       (re-seq #"(snd|rcv|set|add|mul|mod|jgz) (-?\d+|[a-z])(?: )?(-?\d+|[a-z])?")
       (map (partial drop 1))
       (mapv vec)))

(loop [ms [{"p" 0} {"p" 1}]
       states [:ok :ok]
       is [0 0]
       queues [clojure.lang.PersistentQueue/EMPTY clojure.lang.PersistentQueue/EMPTY]
       sent-counts [0 0]
       id 0]
  (let [m (ms id)
        i (is id)
        [command v1 v2] (matches i)
        other-id (- 1 id)]
    (if (< -1 i (count matches))
      (case command
        "snd" (recur ms states
                     (update-in is [id] inc)
                     (update-in queues [other-id] #(conj % (value v1 m)))
                     (update-in sent-counts [id] inc)
                     id)

        "set" (recur (update-in ms [id] #(assoc % v1 (value v2 m)))
                     states
                     (update-in is [id] inc)
                     queues sent-counts id)

        "add" (recur (update-in ms [id] #(update-in % [v1] (fn [old] (+ (or old 0) (value v2 m)))))
                     states
                     (update-in is [id] inc)
                     queues sent-counts id)

        "mul" (recur (update-in ms [id] #(update-in % [v1] (fn [old] (* (or old 0) (value v2 m)))))
                     states
                     (update-in is [id] inc)
                     queues sent-counts id)

        "mod" (recur (update-in ms [id] #(update-in % [v1] (fn [old] (mod (or old 0) (value v2 m)))))
                     states
                     (update-in is [id] inc)
                     queues sent-counts id)

        "rcv" (let [queue (queues id)]
                (if (seq queue)
                  (let [v (peek queue)]
                    (recur (update-in ms [id] #(assoc % v1 v))
                           (assoc states id :ok)
                           (update-in is [id] inc)
                           (update-in queues [id] pop)
                           sent-counts
                           id))

                  (case (states other-id)
                    :done (sent-counts 1)

                    :receiving (let [queue (queues other-id)]
                                 (if (seq queue)
                                   (recur ms states is queues sent-counts other-id)
                                   (sent-counts 1)))

                    (recur ms
                           (assoc states id :receiving)
                           is queues sent-counts other-id))))

        "jgz" (if (pos? (value v1 m))
                (recur ms states
                       (update-in is [id] #(+ % (value v2 m)))
                       queues sent-counts id)

                (recur ms states
                       (update-in is [id] inc)
                       queues sent-counts id)))

      (if (= (states other-id) :done)
        (sent-counts 1)

        (recur ms
               (assoc states id :done)
               is queues sent-counts other-id)))))

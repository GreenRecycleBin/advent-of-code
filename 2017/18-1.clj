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

(loop [recovered-frequency nil
       m {}
       i 0
       matches (->> input
                    (re-seq #"(snd|rcv|set|add|mul|mod|jgz) (-?\d+|[a-z])(?: )?(-?\d+|[a-z])?")
                    (map (partial drop 1))
                    (mapv vec))]
  (if (or (neg? i) (>= i (count matches)))
    recovered-frequency

    (let [[command v1 v2] (matches i)]
      (case command
        "snd" (recur (value v1 m) m (inc i) matches)
        "set" (recur recovered-frequency (assoc m v1 (value v2 m)) (inc i) matches)
        "add" (recur recovered-frequency (update-in m [v1] #(+ (or % 0) (value v2 m))) (inc i) matches)
        "mul" (recur recovered-frequency (update-in m [v1] #(* (or % 0) (value v2 m))) (inc i) matches)
        "mod" (recur recovered-frequency (update-in m [v1] #(mod (or % 0) (value v2 m))) (inc i) matches)

        "rcv" (if (not (zero? (value v1 m)))
                recovered-frequency
                (recur recovered-frequency m (inc i) matches))

        "jgz" (if (pos? (value v1 m))
                (recur recovered-frequency m (+ i (value v2 m)) matches)
                (recur recovered-frequency m (inc i) matches))))))

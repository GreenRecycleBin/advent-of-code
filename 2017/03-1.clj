;; https://www.reddit.com/r/adventofcode/comments/7h7ufl/2017_day_3_solutions/dqoxrb7/
;; https://www.reddit.com/r/adventofcode/comments/7h7ufl/2017_day_3_solutions/dqp12nl/
(defn manhattan-distance [n]
  (let [axis-to-center-distance
        (int (/ (Math/floor (Math/ceil (Math/sqrt n))) 2))

        cycle-offset
        (mod (- n (int (Math/pow (- (* 2 axis-to-center-distance) 1) 2)))
             (* 2 axis-to-center-distance))]
    (+ axis-to-center-distance
       (Math/abs (- cycle-offset axis-to-center-distance)))))

(manhattan-distance 325489)

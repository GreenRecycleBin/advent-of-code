(def input
  [:n :nw :nw :nw :sw :sw :sw :ne :s :s :nw :s :s :ne :se :s :s :nw :s :se :se
   :se :se :ne :se :ne :ne :ne :ne :ne :ne :ne :ne :sw :se :ne :sw :n :s :ne :ne
   :sw :n :ne :sw :nw :n :n :ne :ne :ne :ne :n :se :n :n :n :n :n :n :n :n :n :n
   :nw :n :n :sw :nw :n :nw :s :nw :n :n :s :nw :ne :n :se :nw :nw :nw :nw :se :s
   :s :s :s :s :sw :nw :n :nw :nw :se :nw :nw :nw :nw :nw :nw :sw :nw :nw :nw :se
   :nw :nw :nw :nw :nw :s :nw :sw :nw :nw :sw :sw :sw :n :s :nw :sw :sw :nw :nw
   :nw :s :n :n :sw :sw :nw :nw :sw :sw :sw :n :nw :sw :sw :sw :sw :sw :sw :sw
   :sw :n :sw :sw :sw :sw :sw :sw :nw :ne :sw :s :sw :sw :sw :sw :sw :sw :s :sw
   :sw :sw :sw :sw :sw :sw :sw :sw :s :sw :s :s :sw :sw :s :s :s :se :nw :sw :s
   :s :se :sw :nw :s :se :s :sw :sw :sw :s :sw :s :s :s :s :s :s :s :s :s :sw :s
   :s :ne :s :s :ne :s :nw :sw :s :s :s :nw :s :se :s :ne :s :s :s :s :s :s :s :s
   :s :s :s :s :n :s :s :ne :nw :s :s :s :se :se :s :s :nw :se :s :se :s :s :s :s
   :sw :sw :s :s :s :se :n :se :s :ne :se :s :se :nw :s :ne :se :se :se :se :se
   :s :s :se :ne :s :nw :se :s :se :sw :se :se :se :se :se :s :s :n :ne :se :s
   :se :n :nw :s :se :se :s :s :s :se :se :se :s :sw :se :se :se :sw :se :se :s
   :se :se :se :se :se :se :ne :se :se :ne :s :se :n :se :se :se :se :se :ne :se
   :n :se :ne :se :ne :se :ne :ne :nw :se :ne :s :ne :n :se :sw :se :se :nw :se
   :ne :se :se :s :ne :n :se :se :se :s :ne :se :se :ne :se :se :se :ne :ne :sw
   :ne :se :s :ne :nw :nw :ne :n :ne :se :nw :s :ne :se :ne :ne :se :ne :n :ne
   :ne :ne :ne :ne :se :ne :sw :ne :sw :ne :ne :ne :ne :se :ne :ne :n :ne :ne :ne
   :se :n :se :ne :se :nw :se :ne :ne :ne :ne :ne :se :s :nw :ne :ne :ne :ne :ne
   :ne :ne :se :ne :n :ne :ne :ne :se :ne :s :ne :ne :ne :sw :ne :ne :ne :ne :ne
   :ne :ne :ne :se :ne :ne :n :sw :nw :se :ne :ne :sw :ne :nw :ne :ne :ne :n :s
   :ne :ne :ne :n :ne :ne :ne :se :n :ne :se :se :s :ne :ne :nw :ne :ne :sw :ne
   :se :n :ne :ne :s :n :ne :ne :ne :n :ne :ne :n :nw :ne :ne :ne :n :ne :n :s
   :ne :se :s :ne :ne :ne :ne :ne :nw :s :ne :sw :n :ne :n :ne :ne :nw :se :n :ne
   :ne :ne :n :n :ne :n :ne :ne :ne :n :nw :ne :n :ne :n :ne :n :ne :ne :ne :nw
   :n :s :ne :ne :sw :n :ne :n :ne :s :sw :ne :ne :ne :ne :ne :ne :n :n :ne :ne
   :n :s :n :ne :nw :ne :n :ne :n :s :ne :ne :ne :n :n :s :se :nw :n :ne :n :ne
   :n :n :n :s :n :s :n :n :ne :n :n :n :n :ne :n :n :n :n :sw :n :n :n :n :n :n
   :sw :nw :sw :n :n :n :n :n :n :ne :n :s :nw :n :nw :n :ne :n :n :n :n :s :n :n
   :n :n :n :n :n :n :n :sw :s :n :ne :n :n :n :n :s :sw :n :ne :n :n :n :n :n :n
   :n :se :s :n :n :sw :n :se :n :n :n :n :n :n :n :s :s :sw :n :n :n :n :n :n
   :sw :nw :n :n :sw :s :nw :sw :se :n :n :n :n :s :n :nw :nw :nw :n :sw :nw :n
   :sw :s :sw :nw :n :n :n :ne :n :nw :nw :n :n :n :n :s :nw :se :n :n :se :n :nw
   :nw :n :n :n :s :n :s :nw :n :nw :nw :nw :nw :nw :sw :nw :ne :n :nw :n :n :n
   :nw :se :sw :n :n :s :ne :n :nw :nw :nw :nw :n :n :nw :se :n :n :n :se :n :nw
   :n :sw :se :nw :n :n :nw :nw :n :nw :n :nw :nw :n :n :nw :n :n :nw :n :n :n :n
   :n :n :n :n :nw :nw :nw :nw :sw :n :n :nw :nw :n :se :s :n :nw :n :nw :nw :n
   :n :nw :n :n :n :nw :nw :nw :n :nw :se :nw :n :n :nw :n :n :n :n :n :nw :n :se
   :s :ne :n :nw :ne :nw :nw :nw :n :nw :nw :n :se :n :nw :se :s :n :ne :n :nw
   :se :s :n :n :ne :n :nw :se :nw :nw :n :nw :nw :nw :nw :nw :nw :nw :nw :nw :nw
   :nw :nw :n :n :nw :nw :n :nw :n :nw :nw :nw :n :ne :nw :nw :nw :n :nw :nw :sw
   :n :nw :sw :nw :nw :n :nw :nw :nw :n :n :nw :nw :s :nw :sw :nw :se :nw :nw :nw
   :nw :s :nw :n :nw :nw :n :nw :nw :nw :nw :n :ne :nw :nw :nw :nw :nw :nw :sw
   :nw :s :sw :nw :nw :nw :nw :nw :nw :nw :nw :ne :nw :nw :sw :nw :nw :nw :nw :nw
   :nw :s :nw :s :nw :nw :nw :nw :ne :nw :s :nw :nw :nw :nw :nw :sw :nw :nw :s
   :nw :sw :se :sw :nw :nw :s :n :nw :nw :nw :s :nw :nw :nw :nw :se :nw :ne :nw
   :nw :n :nw :nw :nw :sw :sw :n :nw :nw :sw :nw :sw :nw :nw :nw :n :nw :nw :sw
   :nw :s :sw :se :nw :nw :nw :nw :nw :nw :nw :sw :nw :nw :ne :sw :nw :nw :n :nw
   :sw :sw :se :nw :nw :nw :n :nw :nw :nw :nw :s :se :ne :sw :nw :nw :nw :nw :nw
   :nw :nw :nw :nw :sw :sw :n :se :s :s :nw :n :sw :s :s :sw :sw :s :nw :nw :sw
   :sw :nw :sw :nw :nw :nw :n :sw :sw :s :nw :ne :ne :sw :n :se :nw :sw :sw :nw
   :sw :nw :ne :nw :nw :sw :nw :nw :n :nw :se :sw :nw :nw :se :sw :sw :se :sw :nw
   :se :nw :nw :nw :nw :nw :nw :sw :nw :sw :sw :sw :n :sw :sw :nw :nw :sw :sw :nw
   :sw :nw :nw :nw :nw :ne :se :nw :nw :sw :nw :sw :nw :sw :nw :nw :nw :sw :nw
   :sw :se :sw :nw :sw :n :ne :sw :sw :sw :sw :se :nw :se :sw :sw :nw :sw :sw :nw
   :sw :nw :sw :sw :sw :sw :sw :sw :nw :sw :sw :sw :sw :nw :sw :sw :nw :nw :sw
   :se :sw :nw :sw :sw :sw :nw :s :ne :n :s :n :sw :sw :sw :sw :se :sw :nw :nw
   :se :nw :sw :nw :se :sw :sw :nw :sw :ne :sw :s :n :se :se :sw :s :s :sw :sw
   :nw :s :se :sw :sw :sw :sw :s :nw :n :nw :sw :sw :sw :nw :sw :sw :s :n :sw :nw
   :nw :nw :se :sw :sw :nw :se :sw :sw :sw :sw :sw :nw :n :sw :nw :ne :sw :se :sw
   :nw :n :sw :se :sw :sw :se :nw :s :sw :sw :nw :nw :sw :ne :n :se :sw :se :sw
   :nw :sw :sw :sw :sw :sw :nw :se :sw :sw :ne :sw :sw :se :ne :sw :sw :n :sw :nw
   :sw :n :sw :sw :ne :sw :sw :nw :sw :sw :sw :sw :sw :sw :nw :s :sw :sw :sw :sw
   :se :sw :sw :sw :sw :sw :sw :sw :sw :nw :sw :sw :sw :se :sw :sw :n :sw :s :sw
   :sw :n :se :sw :sw :ne :n :se :sw :sw :sw :sw :sw :sw :nw :sw :s :sw :se :nw
   :s :sw :sw :sw :sw :sw :sw :sw :sw :sw :se :se :sw :sw :sw :sw :sw :sw :sw :sw
   :sw :n :sw :sw :sw :sw :sw :n :sw :s :s :sw :se :sw :ne :sw :s :sw :sw :sw :sw
   :ne :se :sw :sw :sw :sw :sw :sw :sw :sw :sw :sw :sw :s :s :se :se :s :sw :sw
   :nw :sw :s :sw :sw :nw :s :se :nw :sw :sw :sw :s :sw :sw :sw :s :sw :ne :sw
   :sw :se :sw :sw :sw :sw :s :sw :n :sw :sw :nw :s :s :s :sw :se :sw :sw :sw :sw
   :sw :s :sw :nw :se :se :nw :sw :sw :n :ne :sw :s :sw :se :s :sw :sw :sw :sw
   :sw :sw :se :se :s :sw :sw :sw :sw :s :sw :s :nw :sw :sw :s :sw :s :sw :sw :s
   :s :sw :sw :s :ne :sw :sw :sw :s :s :s :sw :nw :nw :s :sw :sw :ne :s :sw :sw
   :sw :s :se :s :s :sw :sw :sw :se :sw :sw :nw :sw :s :sw :sw :n :sw :sw :sw :sw
   :s :s :s :s :s :ne :se :sw :s :n :sw :se :se :s :sw :sw :s :s :s :s :sw :s :s
   :s :s :sw :sw :s :s :s :sw :s :sw :n :s :nw :nw :sw :sw :sw :sw :s :se :sw :sw
   :s :sw :s :s :sw :s :sw :sw :sw :sw :s :se :se :n :ne :ne :s :sw :sw :sw :s
   :sw :nw :nw :n :nw :s :sw :sw :ne :s :sw :sw :n :s :s :s :s :se :s :sw :se :sw
   :s :sw :s :nw :se :sw :s :sw :sw :sw :s :n :sw :ne :se :s :s :n :s :s :s :sw
   :s :sw :sw :sw :s :nw :ne :n :s :s :sw :ne :s :sw :s :s :sw :sw :sw :s :s :nw
   :s :nw :s :s :sw :ne :s :s :s :ne :s :sw :s :se :s :s :s :s :s :s :s :s :s :ne
   :sw :n :s :s :s :s :s :ne :s :sw :s :s :n :s :s :s :s :s :s :s :s :s :s :s :s
   :sw :n :s :se :ne :s :s :s :s :sw :s :nw :s :s :s :s :s :s :s :se :s :sw :s
   :sw :n :s :sw :s :sw :sw :s :sw :s :ne :sw :s :se :s :sw :s :s :sw :ne :nw :s
   :s :sw :sw :s :s :s :se :s :s :s :n :sw :sw :ne :ne :sw :nw :s :s :sw :s :s
   :sw :s :s :s :s :nw :sw :s :s :s :s :sw :s :s :s :sw :s :s :s :sw :s :s :s :s
   :s :s :s :s :s :s :s :s :sw :s :s :se :s :s :n :s :ne :s :s :se :s :s :s :s :s
   :s :s :ne :s :s :s :s :n :s :s :s :n :s :n :s :sw :s :n :sw :nw :s :s :ne :s
   :s :s :s :s :s :n :s :nw :s :sw :s :s :s :s :s :s :s :n :s :s :s :s :s :se :s
   :s :s :s :s :n :s :s :ne :s :s :s :s :n :s :ne :s :s :s :nw :se :ne :se :se :s
   :ne :nw :s :s :n :s :s :s :s :n :s :s :s :s :se :s :sw :se :ne :s :s :s :n :n
   :s :se :se :ne :s :s :s :s :s :se :s :s :s :s :nw :s :se :s :s :s :s :s :s :se
   :s :s :s :sw :n :s :sw :s :s :s :se :s :s :s :s :s :s :s :s :s :s :nw :s :s
   :ne :s :s :sw :s :ne :s :s :ne :s :sw :n :s :nw :s :se :se :s :s :s :sw :nw :s
   :s :s :s :s :ne :s :se :s :s :s :s :s :s :s :s :se :n :s :sw :s :s :ne :se :s
   :s :se :se :se :s :se :nw :s :s :s :se :nw :sw :s :s :s :ne :s :s :se :s :ne
   :s :s :s :nw :n :se :s :s :se :s :s :se :se :s :s :s :s :nw :s :s :s :nw :s :s
   :n :se :nw :s :s :sw :se :s :n :n :s :sw :s :s :nw :s :n :nw :nw :s :se :s :s
   :se :s :s :ne :s :s :s :sw :sw :s :se :s :s :se :se :s :nw :s :se :s :s :nw :s
   :n :se :ne :sw :se :s :s :s :s :ne :se :n :se :s :se :s :s :nw :s :se :s :ne
   :s :s :se :s :se :n :sw :sw :s :nw :s :se :s :se :nw :se :s :se :s :sw :n :s
   :se :se :s :s :ne :s :n :s :se :se :ne :s :s :s :s :se :se :se :se :sw :s :se
   :sw :s :s :s :s :se :se :s :nw :s :se :se :s :ne :s :se :s :s :s :s :s :s :se
   :se :s :se :se :se :se :se :ne :nw :s :n :ne :s :se :s :s :s :nw :se :se :nw
   :s :se :nw :s :se :se :s :s :se :se :se :sw :se :s :se :ne :sw :s :se :s :s :s
   :s :s :s :s :nw :n :ne :se :sw :s :se :s :se :s :se :n :se :s :se :se :se :se
   :se :s :se :ne :se :s :se :n :s :s :ne :s :s :se :s :s :s :s :sw :se :s :se
   :sw :sw :s :sw :nw :se :nw :se :s :s :ne :se :s :s :s :s :s :se :s :se :se :nw
   :se :nw :s :sw :s :n :sw :se :se :s :se :nw :se :sw :s :se :se :nw :s :s :s :s
   :sw :nw :n :se :s :sw :se :se :s :se :ne :s :s :se :s :se :s :ne :se :s :se
   :se :se :se :se :s :se :se :se :s :se :s :se :s :s :s :s :s :se :nw :se :nw
   :se :se :s :se :se :se :s :sw :se :se :se :se :sw :se :se :se :s :n :s :sw :se
   :n :se :s :se :se :se :s :nw :s :se :se :nw :se :se :se :s :se :se :se :se :se
   :nw :sw :se :s :s :sw :se :se :nw :s :se :s :s :se :ne :se :nw :se :s :se :se
   :s :se :se :se :se :sw :n :se :ne :se :se :s :se :se :s :se :s :se :se :s :se
   :s :se :n :s :se :s :s :s :se :se :se :se :se :n :se :se :se :se :ne :s :sw
   :se :se :se :n :se :se :se :s :se :se :se :se :n :ne :se :se :se :se :se :se
   :se :se :se :se :s :sw :se :se :se :se :se :se :se :se :s :se :se :se :se :se
   :s :se :se :se :se :se :se :se :se :se :nw :s :se :s :se :se :se :se :se :s :s
   :ne :se :se :se :ne :se :se :se :se :se :se :s :se :se :se :se :se :se :se :se
   :se :s :se :se :se :s :s :s :s :se :ne :sw :se :se :se :se :se :s :ne :se :se
   :se :se :s :sw :se :se :sw :se :s :sw :se :nw :se :se :s :se :se :nw :s :s :se
   :s :se :se :se :nw :se :ne :se :se :n :se :se :se :se :n :ne :se :se :sw :se
   :sw :se :se :se :se :se :se :se :n :nw :se :s :sw :n :ne :se :se :se :se :n
   :sw :se :se :se :se :se :n :se :nw :se :nw :sw :se :n :se :ne :se :se :se :se
   :se :se :sw :n :se :se :nw :se :se :s :se :se :se :se :ne :se :se :s :se :se
   :se :se :se :nw :n :se :se :se :s :se :se :nw :ne :nw :se :se :se :se :se :se
   :se :se :se :se :se :se :s :ne :ne :se :se :nw :se :se :se :se :se :se :nw :se
   :sw :se :sw :se :se :nw :se :se :se :se :ne :se :se :se :se :se :nw :nw :se
   :se :se :se :se :se :se :se :se :se :nw :se :se :se :se :se :se :nw :se :se
   :se :se :se :se :nw :se :se :se :se :se :se :se :se :se :se :s :se :se :se :ne
   :s :sw :s :se :ne :se :ne :se :se :se :se :ne :se :se :nw :se :se :se :se :se
   :se :se :se :n :s :se :nw :se :se :n :nw :sw :s :se :se :se :sw :se :se :ne
   :se :s :nw :se :se :n :se :se :sw :ne :se :se :se :ne :se :se :se :se :se :nw
   :se :se :s :se :ne :se :se :se :nw :sw :se :n :se :ne :s :ne :nw :n :ne :se
   :ne :se :ne :se :se :ne :se :sw :se :se :n :s :se :nw :ne :se :se :ne :se :ne
   :se :se :nw :se :se :s :ne :ne :se :se :se :se :se :se :se :se :se :ne :se :se
   :ne :se :se :se :nw :nw :se :se :se :n :se :se :s :ne :se :se :se :s :se :ne
   :se :ne :se :se :se :se :ne :se :n :se :se :se :ne :n :se :se :s :se :se :nw
   :se :se :sw :se :se :ne :se :se :se :se :se :se :s :ne :sw :ne :se :se :se :se
   :nw :se :se :ne :se :s :se :se :se :ne :ne :nw :se :se :se :s :sw :se :ne :sw
   :se :n :ne :se :sw :se :ne :se :se :ne :se :s :se :ne :se :se :se :ne :sw :ne
   :se :se :se :ne :ne :ne :ne :se :se :se :se :se :se :sw :se :ne :se :sw :ne
   :se :se :se :se :se :se :ne :se :sw :se :ne :n :ne :se :se :se :ne :se :se :se
   :ne :se :n :ne :nw :se :se :ne :ne :ne :ne :nw :se :ne :se :nw :se :se :se :ne
   :se :se :se :se :ne :ne :ne :nw :se :se :sw :se :ne :se :nw :n :s :s :se :ne
   :ne :sw :ne :se :ne :ne :ne :sw :se :se :se :nw :nw :ne :se :se :s :se :ne :sw
   :se :se :se :se :ne :se :ne :se :ne :se :se :se :se :ne :se :ne :se :se :ne
   :ne :se :n :sw :se :sw :se :sw :ne :ne :se :nw :se :sw :ne :se :nw :ne :se :ne
   :se :se :se :se :se :ne :ne :ne :ne :se :ne :nw :se :sw :se :nw :ne :se :se
   :ne :se :se :ne :se :se :se :nw :ne :ne :se :ne :se :sw :se :se :nw :ne :ne
   :ne :n :n :nw :nw :se :ne :ne :se :n :ne :se :ne :se :n :se :se :se :ne :ne
   :se :se :nw :se :se :se :se :se :ne :ne :se :n :se :ne :se :ne :nw :se :se :se
   :n :ne :ne :se :se :se :se :se :ne :n :ne :ne :ne :n :se :se :se :se :se :ne
   :ne :se :ne :se :ne :nw :se :se :ne :ne :ne :ne :ne :se :se :ne :se :ne :ne
   :ne :ne :sw :ne :se :ne :se :nw :ne :ne :se :ne :ne :se :se :ne :se :sw :ne
   :ne :se :ne :se :s :ne :ne :se :ne :ne :sw :se :se :s :nw :se :n :s :s :ne :se
   :se :ne :se :se :ne :s :se :se :se :nw :ne :se :ne :se :ne :ne :se :se :se :se
   :ne :sw :ne :se :s :ne :ne :ne :ne :n :se :sw :s :se :ne :se :se :ne :ne :se
   :se :nw :ne :ne :ne :sw :se :ne :ne :ne :ne :se :se :se :se :ne :se :se :se
   :se :sw :s :se :ne :ne :ne :ne :ne :se :ne :ne :ne :se :ne :ne :sw :nw :nw :n
   :ne :se :se :s :se :s :n :ne :se :se :se :ne :se :ne :sw :nw :nw :ne :ne :se
   :ne :ne :ne :ne :ne :se :ne :n :ne :se :se :se :se :se :ne :ne :se :se :ne :n
   :n :ne :se :ne :se :nw :se :ne :se :ne :ne :se :ne :se :ne :se :se :nw :se :nw
   :ne :ne :ne :ne :s :s :ne :ne :se :ne :se :ne :ne :n :sw :ne :ne :s :nw :ne
   :ne :se :ne :nw :ne :se :s :se :ne :sw :ne :sw :se :ne :n :nw :ne :nw :ne :se
   :n :se :ne :se :ne :ne :se :ne :se :ne :n :se :se :ne :ne :sw :se :ne :ne :ne
   :ne :ne :ne :nw :ne :se :se :se :ne :se :ne :s :se :se :ne :sw :se :s :ne :n
   :ne :se :ne :se :se :ne :ne :ne :nw :sw :se :ne :se :ne :ne :se :n :s :ne :n
   :ne :ne :se :ne :ne :se :ne :ne :ne :ne :ne :ne :ne :ne :ne :s :nw :ne :ne :ne
   :ne :n :ne :ne :ne :se :ne :ne :sw :ne :ne :se :se :se :ne :ne :nw :ne :ne :ne
   :se :n :se :sw :ne :se :se :ne :nw :n :se :ne :s :ne :n :ne :ne :se :ne :ne
   :se :ne :ne :ne :n :ne :n :sw :sw :ne :n :se :ne :se :n :s :ne :ne :ne :sw :ne
   :ne :ne :ne :ne :ne :nw :ne :nw :se :ne :sw :ne :ne :ne :ne :n :ne :n :ne :ne
   :ne :ne :ne :ne :n :ne :ne :ne :ne :ne :ne :ne :ne :n :se :ne :ne :ne :sw :ne
   :ne :se :ne :ne :se :n :ne :ne :s :se :nw :ne :ne :ne :s :se :sw :ne :ne :ne
   :ne :n :s :se :se :ne :ne :ne :ne :ne :ne :s :sw :sw :ne :ne :ne :ne :ne :sw
   :ne :n :ne :ne :sw :ne :n :ne :ne :ne :ne :s :ne :ne :ne :ne :ne :ne :ne :ne
   :ne :ne :sw :se :ne :ne :ne :ne :ne :sw :ne :ne :ne :ne :ne :ne :ne :ne :ne
   :ne :ne :ne :sw :ne :se :sw :ne :ne :ne :ne :ne :ne :ne :ne :n :ne :ne :nw :ne
   :ne :ne :n :ne :nw :ne :ne :ne :ne :ne :ne :sw :ne :ne :ne :n :sw :s :nw :nw
   :sw :s :nw :ne :ne :sw :ne :ne :se :ne :ne :ne :s :ne :sw :nw :ne :ne :s :ne
   :ne :ne :ne :se :ne :ne :sw :ne :ne :ne :ne :ne :ne :ne :ne :ne :ne :ne :ne
   :ne :ne :ne :ne :ne :ne :ne :ne :n :ne :se :ne :sw :se :ne :ne :nw :ne :ne :s
   :ne :ne :ne :nw :ne :nw :ne :ne :n :ne :ne :ne :ne :s :ne :ne :ne :se :n :ne
   :ne :ne :ne :s :se :ne :nw :ne :se :n :nw :ne :ne :ne :ne :ne :ne :n :ne :se
   :ne :ne :n :ne :ne :ne :ne :s :ne :ne :ne :n :n :ne :n :ne :ne :ne :ne :ne :n
   :nw :ne :ne :ne :se :ne :ne :ne :s :se :ne :ne :ne :ne :ne :ne :ne :ne :n :ne
   :sw :ne :ne :ne :nw :nw :ne :ne :nw :ne :ne :ne :n :ne :ne :ne :n :ne :ne :ne
   :ne :se :se :n :ne :ne :ne :n :ne :ne :ne :s :se :ne :ne :ne :n :ne :ne :ne
   :nw :ne :ne :ne :nw :ne :ne :ne :ne :ne :se :ne :ne :ne :n :ne :ne :nw :ne :ne
   :ne :n :ne :nw :ne :ne :ne :se :ne :ne :ne :ne :ne :ne :ne :ne :ne :ne :ne :ne
   :ne :ne :nw :ne :ne :ne :ne :se :ne :se :ne :ne :ne :ne :ne :ne :sw :ne :ne
   :ne :ne :n :n :se :ne :n :ne :ne :n :ne :ne :ne :nw :ne :ne :ne :n :ne :s :ne
   :n :n :ne :ne :ne :ne :n :ne :sw :ne :ne :ne :nw :ne :ne :ne :se :s :nw :ne
   :ne :ne :ne :n :ne :ne :ne :ne :ne :n :nw :ne :ne :ne :ne :ne :ne :ne :ne :n
   :ne :n :ne :ne :ne :ne :nw :n :ne :ne :ne :n :n :n :se :ne :sw :ne :nw :ne :ne
   :ne :n :ne :ne :ne :ne :sw :n :ne :ne :se :ne :nw :s :ne :ne :ne :s :n :ne :ne
   :ne :ne :se :ne :ne :sw :ne :ne :n :ne :ne :ne :ne :ne :n :n :nw :n :ne :ne :n
   :s :sw :n :ne :ne :ne :n :n :ne :ne :ne :sw :ne :ne :ne :nw :ne :ne :n :ne :ne
   :n :se :ne :ne :n :se :n :ne :ne :ne :ne :ne :ne :ne :ne :ne :ne :ne :ne :ne
   :se :n :ne :ne :nw :s :n :n :n :ne :n :n :ne :n :ne :n :ne :ne :ne :nw :s :ne
   :ne :ne :ne :ne :se :nw :ne :ne :s :se :n :n :ne :ne :ne :ne :n :ne :ne :ne :n
   :n :ne :ne :ne :nw :ne :ne :ne :n :ne :n :ne :sw :ne :ne :n :ne :ne :ne :n :n
   :n :n :n :sw :ne :nw :n :se :s :ne :ne :ne :s :ne :sw :n :ne :se :ne :ne :ne
   :ne :ne :se :n :n :ne :ne :n :n :n :ne :ne :n :n :ne :s :ne :ne :se :ne :se :n
   :sw :ne :ne :ne :s :ne :ne :sw :nw :ne :se :n :ne :n :ne :n :n :ne :n :s :ne
   :n :s :ne :ne :ne :n :n :ne :n :sw :sw :ne :n :ne :ne :sw :n :n :n :ne :ne :se
   :ne :ne :n :ne :ne :ne :sw :ne :nw :sw :ne :s :ne :ne :ne :s :ne :sw :ne :n
   :ne :n :n :ne :nw :ne :s :n :n :ne :nw :ne :n :ne :ne :ne :ne :ne :se :nw :ne
   :sw :ne :ne :ne :sw :n :n :ne :ne :ne :ne :ne :nw :ne :ne :nw :ne :sw :ne :ne
   :sw :ne :ne :ne :n :ne :se :ne :n :n :se :n :s :n :n :ne :ne :ne :ne :ne :ne
   :ne :n :n :ne :ne :se :ne :ne :n :ne :nw :ne :n :se :nw :ne :ne :n :ne :n :nw
   :ne :ne :n :ne :n :n :ne :n :n :n :ne :ne :ne :ne :ne :n :n :nw :ne :ne :ne
   :nw :ne :ne :ne :ne :n :sw :ne :ne :n :n :ne :n :se :n :ne :n :n :nw :ne :ne
   :n :sw :ne :ne :ne :sw :ne :ne :n :s :n :nw :ne :s :ne :n :n :n :ne :s :ne :s
   :ne :sw :ne :se :ne :n :ne :n :n :n :ne :n :ne :n :ne :ne :ne :se :ne :ne :n
   :n :n :ne :n :n :ne :ne :ne :ne :sw :ne :ne :n :n :ne :n :n :n :ne :ne :ne :nw
   :sw :sw :n :ne :ne :n :ne :n :ne :sw :ne :n :n :se :n :n :ne :ne :sw :n :n :n
   :s :ne :n :ne :sw :ne :n :ne :ne :ne :ne :ne :ne :ne :n :n :n :se :ne :nw :n
   :ne :n :ne :sw :nw :n :se :ne :ne :n :n :s :n :n :n :ne :sw :ne :ne :se :n :n
   :ne :n :ne :n :ne :ne :ne :n :nw :n :n :n :nw :n :n :n :n :nw :ne :n :s :sw
   :ne :n :n :ne :n :nw :n :ne :s :sw :n :n :se :n :n :n :se :n :se :ne :se :n :n
   :n :n :s :ne :n :n :n :ne :ne :nw :n :n :sw :ne :n :n :n :n :ne :n :n :n :ne
   :ne :ne :n :ne :sw :ne :n :n :ne :n :n :n :ne :s :nw :n :ne :nw :n :n :ne :s
   :n :ne :ne :ne :ne :ne :n :ne :n :n :nw :ne :n :n :ne :n :n :n :se :sw :n :ne
   :s :n :n :ne :s :ne :n :n :sw :n :n :n :n :se :n :n :n :ne :n :n :se :ne :ne
   :sw :n :ne :n :n :ne :n :ne :n :s :s :n :s :n :n :n :n :ne :n :n :sw :nw :n :n
   :sw :ne :n :s :n :n :n :n :nw :n :n :ne :ne :ne :sw :ne :nw :n :ne :n :ne :n
   :n :n :nw :ne :nw :n :ne :sw :n :sw :s :ne :n :n :n :ne :n :s :n :n :n :n :ne
   :n :n :ne :se :se :n :ne :n :n :ne :n :ne :ne :ne :s :n :n :ne :ne :n :n :sw
   :ne :n :n :n :n :n :ne :ne :n :n :ne :n :n :n :n :ne :n :ne :n :n :ne :nw :n
   :sw :sw :nw :n :se :sw :ne :n :ne :n :ne :ne :s :n :ne :s :ne :s :ne :n :n :n
   :n :ne :n :ne :nw :n :n :n :n :n :ne :n :ne :ne :nw :sw :s :se :n :n :s :n :n
   :n :n :ne :n :ne :n :n :sw :ne :ne :n :n :n :ne :ne :n :n :n :n :nw :n :ne :s
   :n :ne :ne :n :sw :se :n :ne :ne :ne :s :ne :n :n :ne :n :nw :nw :ne :n :n :nw
   :n :n :ne :s :n :nw :n :n :n :nw :n :n :sw :n :n :n :s :n :n :n :n :ne :se :n
   :n :n :ne :nw :n :n :n :ne :n :ne :n :n :n :n :n :n :n :n :n :n :ne :n :n :ne
   :ne :n :n :s :n :s :n :n :n :nw :ne :n :n :ne :n :n :ne :n :n :n :ne :n :n :n
   :n :n :n :n :ne :n :nw :n :ne :n :ne :n :n :n :n :sw :ne :ne :ne :n :s :ne :ne
   :n :ne :n :n :n :n :n :n :n :n :s :n :ne :s :n :n :ne :ne :ne :ne :nw :s :n :n
   :n :ne :sw :n :n :s :n :n :n :n :ne :n :n :s :ne :n :n :n :se :nw :ne :ne :ne
   :n :ne :n :ne :n :n :n :se :n :s :n :ne :se :n :ne :n :ne :n :n :ne :n :nw :ne
   :n :ne :nw :n :n :s :sw :se :n :n :n :ne :nw :n :n :n :n :se :n :ne :ne :n :nw
   :n :n :nw :n :ne :n :n :n :ne :sw :n :n :se :n :n :sw :ne :nw :n :n :n :n :n
   :n :ne :n :n :n :n :ne :ne :ne :s :n :n :ne :sw :n :n :n :se :n :ne :sw :sw
   :sw :n :n :n :sw :n :ne :n :sw :n :ne :n :se :ne :n :n :n :n :n :n :n :n :s :n
   :n :sw :ne :n :n :n :se :se :n :n :n :s :n :n :n :sw :n :n :n :nw :sw :n :n :n
   :nw :n :ne :ne :n :se :n :n :n :n :se :ne :sw :n :n :sw :n :sw :n :s :ne :n :n
   :s :se :n :n :se :n :n :n :ne :sw :n :nw :n :ne :n :n :n :n :se :n :s :n :n :n
   :se :n :n :n :nw :n :n :n :n :n :n :nw :n :n :n :n :sw :s :s :n :n :n :se :ne
   :n :n :ne :ne :ne :n :n :n :n :n :sw :n :ne :n :n :n :n :n :se :n :n :n :nw
   :se :n :nw :n :ne :n :n :n :s :n :n :n :n :n :n :n :n :n :n :ne :n :ne :n :n
   :n :ne :n :n :n :s :n :n :n :n :n :n :n :n :n :n :n :n :n :ne :s :n :n :n :ne
   :n :n :n :sw :n :ne :s :n :n :n :n :sw :n :n :n :n :n :se :n :n :n :n :n :n :n
   :n :n :n :n :n :n :n :n :n :n :n :se :sw :n :n :ne :se :s :se :s :s :s :s :s
   :sw :sw :sw :nw :nw :sw :nw :se :ne :nw :nw :sw :nw :nw :n :nw :nw :nw :nw :nw
   :nw :nw :nw :nw :nw :se :n :ne :n :n :sw :ne :n :n :n :sw :n :ne :s :n :ne :ne
   :ne :ne :n :ne :n :n :se :ne :ne :ne :se :ne :ne :ne :ne :ne :ne :sw :ne :ne
   :se :ne :ne :ne :ne :ne :se :ne :se :se :se :ne :ne :ne :se :ne :se :ne :se
   :ne :se :se :n :se :se :n :se :se :nw :se :se :se :se :se :s :se :se :se :s
   :se :se :se :se :s :s :nw :s :se :s :se :se :s :s :se :sw :s :se :nw :s :s :ne
   :n :sw :se :s :se :s :se :s :s :s :s :s :ne :s :s :sw :s :s :s :s :s :s :n :s
   :s :s :sw :s :s :s :sw :sw :nw :sw :s :sw :n :s :s :s :s :sw :sw :s :n :s :sw
   :ne :sw :sw :s :s :sw :s :s :s :s :s :ne :nw :sw :sw :s :s :sw :sw :sw :sw :sw
   :sw :n :sw :s :sw :sw :sw :s :se :n :sw :s :s :n :sw :sw :sw :sw :sw :sw :sw
   :se :sw :sw :sw :sw :sw :sw :sw :sw :sw :nw :nw :sw :sw :sw :sw :se :sw :se
   :sw :sw :sw :sw :sw :sw :sw :sw :sw :ne :s :sw :nw :nw :sw :sw :sw :nw :n :sw
   :sw :nw :nw :se :sw :sw :sw :nw :sw :sw :sw :sw :n :sw :nw :nw :sw :ne :sw :nw
   :sw :nw :sw :sw :nw :s :nw :n :nw :nw :nw :nw :nw :nw :sw :s :n :se :nw :nw
   :nw :nw :ne :nw :n :nw :nw :sw :n :ne :nw :nw :nw :nw :nw :nw :nw :nw :nw :nw
   :nw :s :nw :nw :sw :n :se :sw :sw :ne :nw :nw :nw :nw :ne :nw :nw :nw :sw :nw
   :ne :nw :nw :nw :ne :nw :ne :n :n :nw :ne :nw :nw :nw :nw :nw :n :nw :nw :nw
   :nw :n :nw :s :se :nw :nw :nw :nw :n :n :sw :n :nw :nw :nw :nw :n :nw :n :sw
   :nw :nw :n :nw :se :nw :nw :n :n :n :n :nw :n :sw :n :n :nw :sw :sw :nw :nw :s
   :nw :nw :nw :sw :n :n :nw :nw :n :n :n :s :nw :se :n :n :n :nw :nw :nw :n :n
   :n :n :nw :n :ne :n :n :sw :se :n :nw :s :s :n :se :n :s :n :ne :nw :n :n :n
   :ne :n :n :nw :nw :ne :n :nw :nw :se :n :n :nw :n :n :n :n :n :ne :n :n :n :n
   :se :n :n :n :n :ne :n :sw :n :n :n :n :n :ne :n :se :n :n :n :nw :n :ne :n :n
   :ne :n :nw :n :s :n :n :ne :n :n :n :n :ne :s :n :n :se :n :n :se :n :n :sw
   :ne :s :n :nw :s :n :ne :nw :n :n :ne :n :n :n :se :n :n :n :n :s :se :n :n
   :ne :s :se :ne :n :ne :ne :n :ne :ne :ne :ne :ne :se :ne :n :n :ne :ne :ne :ne
   :n :sw :n :n :sw :ne :ne :ne :nw :nw :ne :ne :se :sw :n :n :ne :n :n :ne :n :n
   :ne :n :n :ne :n :ne :n :nw :ne :se :ne :sw :se :s :n :n :ne :ne :n :ne :nw :n
   :ne :n :ne :ne :ne :ne :ne :ne :ne :ne :se :sw :ne :se :ne :ne :n :ne :ne :ne
   :s :ne :ne :ne :ne :ne :n :se :ne :n :ne :ne :sw :se :ne :ne :ne :ne :ne :ne
   :ne :ne :n :ne :ne :s :ne :ne :sw :nw :n :ne :ne :ne :n :s :n :se :ne :ne :sw
   :se :ne :ne :ne :s :ne :se :ne :ne :ne :ne :ne :n :ne :ne :ne :ne :ne :sw :ne
   :ne :ne :sw :ne :se :ne :ne :ne :nw :ne :ne :se :ne :ne :ne :ne :ne :s :ne :ne
   :ne :ne :ne :se :se :ne :ne :ne :sw :ne :n :ne :ne :ne :ne :s :nw :ne :ne :ne
   :ne :ne :ne :ne :se :ne :se :ne :ne :ne :se :se :ne :ne :sw :ne :ne :s :se :ne
   :n :ne :s :s :ne :s :nw :se :ne :ne :sw :ne :ne :se :ne :ne :ne :ne :se :ne
   :ne :sw :se :ne :ne :ne :n :sw :ne :ne :se :se :se :se :s :nw :se :ne :se :nw
   :se :ne :ne :se :ne :ne :ne :ne :sw :ne :ne :n :ne :se :sw :ne :ne :se :se :se
   :nw :ne :ne :ne :ne :se :se :sw :ne :se :se :se :ne :se :nw :ne :ne :ne :ne
   :nw :ne :se :se :ne :se :se :se :n :ne :ne :se :n :se :ne :se :se :se :se :se
   :se :ne :s :se :ne :se :se :ne :sw :ne :se :n :ne :se :ne :se :se :ne :se :ne
   :ne :se :nw :se :se :se :ne :se :ne :se :se :se :ne :se :se :ne :se :se :sw
   :se :se :ne :se :se :se :se :se :se :se :nw :s :ne :ne :ne :se :sw :se :se :ne
   :se :n :se :se :se :se :se :se :s :se :se :se :se :ne :s :se :se :ne :se :se
   :se :sw :se :se :n :ne :ne :se :ne :n :se :se :ne :s :se :se :nw :se :n :se
   :ne :se :se :ne :sw :se :se :se :nw :se :se :ne :ne :se :nw :se :se :se :se
   :se :se :se :se :se :se :se :se :se :n :sw :se :se :se :n :se :se :se :sw :se
   :sw :se :s :nw :se :se :se :se :sw :se :se :se :se :se :se :se :ne :se :se :se
   :se :nw :n :sw :se :ne :se :se :se :sw :se :se :se :sw :n :se :se :sw :se :se
   :s :se :se :se :s :se :se :ne :s :se :se :se :se :s :se :s :se :se :s :se :ne
   :se :se :se :se :se :ne :se :se :se :ne :s :se :s :ne :n :nw :se :sw :se :se
   :s :se :se :s :se :s :nw :s :s :s :ne :s :se :se :se :n :s :s :se :se :se :ne
   :sw :se :se :se :se :se :s :s :s :se :se :ne :se :ne :se :nw :se :se :se :ne
   :s :se :se :se :nw :se :se :s :se :s :s :s :nw :ne :se :s :se :s :sw :nw :se
   :s :ne :se :n :se :s :se :ne :se :s :s :se :s :se :s :s :ne :se :se :se :se :s
   :ne :se :s :se :s :s :s :s :se :se :se :s :s :s :se :se :sw :s :se :se :s :se
   :se :s :sw :sw :s :s :nw :s :s :ne :s :ne :s :s :n :se :s :s :sw :s :n :n :s
   :s :ne :se :s :s :s :ne :s :s :se :s :sw :se :n :se :s :ne :n :s :s :se :s :sw
   :s :nw :se :n :n :s :s :s :s :n :s :s :sw :s :se :s :se :s :se :se :s :s :sw
   :s :se :s :ne :s :s :s :s :s :sw :nw :s :s :nw :s :s :s :se :s :se :s :n :se
   :s :s :se :s :ne :s :n :s :s :s :s :se :s :n :s :s :s :s :s :s :s :s :s :nw :s
   :se :nw :se :sw :se :se :s :se :s :s :s :s :s :ne :nw :n :s :s :s :n :s :s :nw
   :s :s :s :s :s :s :s :n :se :ne :s :se :s :s :se :s :s :ne :s :s :s :s :s :s
   :sw :nw :se :se :s :s :s :s :sw :s :s :s :nw :se :s :se :sw :s :s :s :nw :sw
   :s :ne :s :s :s :nw :s :s :s :s :s :s :s :s :s :s :s :s :s :ne :s :s :s :s :ne
   :s :n :se :s :s :s :s :s :n :s :nw :s :s :s :sw :s :s :s :s :s :sw :s :s :s
   :sw :s :s :s :ne :sw :s :s :ne :s :ne :s :s :s :s :s :s :s :s :nw :ne :nw :s
   :se :s :s :sw :s :n :s :sw :s :sw :s :se :sw :sw :sw :s :s :s :nw :sw :s :s :s
   :ne :s :se :s :sw :sw :s :sw :s :sw :s :s :s :s :s :s :sw :se :s :s :sw :sw :s
   :s :s :s :sw :s :s :se :nw :sw :s :nw :s :s :s :sw :se :s :s :sw :sw :n :sw :s
   :sw :s :s :sw :s :s :s :sw :se :s :s :s :s :se :s :s :s :s :s :s :s :s :s :s
   :ne :s :s :s :s :s :nw :sw :s :s :sw :sw :s :nw :s :s :s :sw :se :nw :ne :s :n
   :s :se :s :s :s :s :nw :s :s :s :ne :s :sw :s :s :ne :sw :s :ne :sw :nw :sw :s
   :s :s :s :s :sw :s :sw :sw :nw :s :n :s :sw :s :s :sw :s :n :sw :sw :s :s :s
   :s :s :nw :nw :s :sw :s :nw :sw :nw :s :s :sw :sw :sw :nw :s :nw :n :sw :sw
   :sw :sw :n :sw :s :sw :sw :sw :s :sw :sw :sw :s :s :ne :ne :sw :sw :s :se :s
   :s :nw :s :n :s :s :s :n :s :s :s :s :se :ne :se :nw :ne :sw :sw :s :n :n :s
   :s :s :ne :s :sw :sw :sw :sw :s :sw :s :sw :sw :s :sw :sw :nw :s :sw :sw :s
   :nw :sw :s :n :s :s :s :se :sw :s :s :s :s :s :s :sw :sw :sw :sw :nw :sw :sw
   :ne :nw :nw :n :s :sw :sw :sw :ne :s :s :sw :s :s :sw :sw :se :sw :sw :sw :n
   :n :s :sw :se :s :sw :ne :sw :se :s :sw :sw :s :s :s :sw :s :sw :s :sw :s :s
   :s :s :sw :sw :s :sw :s :sw :sw :sw :s :s :sw :s :s :sw :s :sw :ne :s :sw :s
   :sw :sw :s :s :sw :sw :n :n :s :sw :s :sw :s :s :sw :sw :s :s :sw :sw :sw :sw
   :sw :n :s :sw :sw :sw :sw :s :sw :se :sw :s :s :n :s :s :sw :sw :ne :sw :se
   :sw :sw :s :s :sw :sw :s :sw :s :sw :sw :nw :sw :s :nw :n :sw :n :se :sw :sw
   :sw :se :ne :sw :sw :sw :sw :n :sw :sw :sw :sw :n :sw :sw :sw :s :sw :se :sw
   :nw :sw :sw :se :sw :sw :sw :s :sw :sw :sw :sw :ne :se :sw :s :sw :s :s :nw
   :sw :n :se :s :s :sw :sw :s :s :sw :sw :sw :nw :sw :sw :sw :s :n :se :sw :sw
   :sw :sw :n :sw :sw :sw :sw :sw :sw :sw :sw :s :sw :sw :s :sw :sw :sw :sw :sw
   :sw :ne :sw :sw :sw :sw :sw :sw :sw :sw :sw :s :sw :sw :sw :sw :sw :sw :n :sw
   :n :sw :sw :sw :sw :sw :sw :sw :sw :sw :s :sw :sw :n :sw :sw :sw :sw :sw :sw
   :sw :sw :se :s :sw :ne :sw :ne :sw :sw :sw :sw :sw :sw :sw :sw :nw :sw :sw :sw
   :n :nw :s :sw :sw :sw :sw :sw :n :n :sw :sw :sw :sw :se :sw :sw :sw :sw :sw
   :sw :sw :sw :nw :sw :sw :sw :nw :sw :sw :sw :sw :sw :sw :sw :sw :n :sw :sw :sw
   :s :sw :sw :ne :sw :nw :sw :sw :sw :sw :sw :s :s :sw :nw :sw :sw :sw :se :sw
   :s :sw :s :se :sw :sw :s :sw :ne :sw :sw :sw :sw :nw :sw :n :sw :sw :s :nw :nw
   :nw :sw :se :nw :ne :sw :sw :sw :n :sw :se :sw :sw :sw :sw :sw :sw :se :nw :sw
   :nw :nw :sw :sw :n :sw :sw :s :nw :sw :sw :sw :nw :sw :s :sw :s :sw :sw :sw
   :se :sw :sw :se :se :sw :sw :nw :sw :nw :sw :sw :sw :nw :n :ne :sw :nw :ne :sw
   :ne :sw :nw :ne :sw :sw :ne :nw :sw :nw :ne :sw :sw :sw :sw :s :sw :n :nw :nw
   :nw :nw :nw :nw :s :s :sw :nw :sw :sw :sw :nw :sw :nw :sw :nw :sw :nw :sw :sw
   :nw :nw :nw :sw :nw :sw :sw :nw :sw :sw :sw :sw :sw :sw :sw :sw :se :sw :sw
   :sw :n :s :ne :sw :nw :nw :sw :sw :ne :sw :sw :nw :nw :sw :nw :nw :sw :se :sw
   :s :sw :n :sw :sw :nw :ne :sw :sw :se :nw :sw :sw :nw :nw :sw :sw :nw :nw :sw
   :sw :nw :ne :sw :sw :ne :sw :sw :nw :ne :nw :sw :sw :nw :sw :s :ne :nw :nw :n
   :sw :nw :nw :sw :nw :sw :sw :ne :sw :sw :sw :sw :se :sw :nw :sw :sw :nw :nw
   :sw :sw :nw :n :sw :n :nw :nw :nw :nw :s :se :nw :sw :nw :sw :sw :nw :sw :nw
   :sw :n :nw :sw :nw :sw :sw :sw :sw :nw :n :sw :sw :sw :nw :sw :sw :s :nw :nw
   :nw :nw :sw :nw :sw :sw :nw :sw :nw :sw :nw :nw :nw :sw :sw :s :ne :sw :nw :sw
   :nw :n :nw :sw :nw :nw :nw :nw :nw :ne :n :sw :nw :se :sw :sw :nw :sw :nw :sw
   :sw :sw :sw :sw :nw :n :s :ne :ne :sw :sw :sw :nw :nw :sw :se :sw :nw :ne :nw
   :sw :s :nw :nw :sw :ne :nw :se :ne :sw :s :sw :nw :nw :s :nw :sw :sw :sw :sw
   :nw :nw :nw :nw :sw :sw :s :sw :nw :sw :ne :nw :sw :se :sw :nw :ne :nw :nw :nw
   :nw :nw :sw :nw :nw :nw :nw :sw :se :nw :ne :nw :nw :n :sw :sw :sw :se :sw :nw
   :nw :nw :n :nw :nw :nw :nw :nw :nw :nw :nw :ne :ne :nw :ne :sw :sw :nw :n :se
   :sw :nw :nw :sw :sw :nw :nw :nw :sw :nw :nw :se :n :sw :sw :sw :sw :s :nw :sw
   :nw :n :sw :sw :sw :sw :n :sw :nw :se :nw :s :sw :nw :nw :nw :nw :s :nw :sw
   :sw :nw :sw :n :s :nw :s :nw :sw :s :nw :sw :s :n :nw :sw :nw :nw :nw :nw :s
   :sw :nw :nw :nw :sw :sw :ne :se :sw :sw :nw :nw :nw :nw :sw :sw :sw :nw :sw :n
   :nw :nw :nw :nw :sw :nw :nw :s :nw :nw :sw :sw :nw :nw :sw :nw :nw :nw :nw :nw
   :nw :se :nw :nw :s :nw :nw :s :nw :n :nw :nw :nw :n :n :nw :nw :nw :nw :nw :nw
   :nw :n :ne :nw :nw :nw :sw :nw :se :sw :nw :sw :nw :sw :n :nw :nw :sw :nw :nw
   :s :nw :nw :sw :sw :nw :nw :nw :nw :nw :s :sw :nw :se :nw :nw :nw :ne :se :nw
   :s :nw :sw :nw :sw :ne :nw :nw :sw :nw :sw :n :nw :nw :nw :s :s :nw :nw :nw
   :nw :nw :nw :nw :nw :nw :se :nw :s :nw :nw :nw :nw :nw :nw :nw :se :nw :nw :nw
   :nw :nw :nw :nw :se :sw :se :nw :nw :se :se :nw :n :nw :nw :se :nw :nw :n :nw
   :s :nw :nw :nw :sw :nw :nw :nw :nw :sw :nw :nw :nw :ne :nw :nw :nw :nw :nw :nw
   :nw :nw :se :nw :s :s :nw :nw :nw :nw :nw :nw :nw :nw :s :nw :ne :nw :nw :nw
   :se :nw :nw :ne :nw :nw :se :nw :se :nw :n :n :nw :ne :sw :nw :nw :nw :nw :nw
   :nw :nw :s :nw :nw :nw :nw :nw :nw :nw :se :nw :nw :nw :nw :nw :nw :nw :nw :nw
   :nw :nw :nw :nw :nw :nw :nw :nw :nw :nw :nw :nw :nw :nw :nw :nw :nw :nw :nw
   :nw :ne :nw :nw :nw])

(defprotocol Neighbor
  (neighbor [this direction])
  (distance [this other]))

;; https://www.redblobgames.com/grids/hexagons/#coordinates-cube
(defrecord Hexagon [x y z])

(extend-type Hexagon
  Neighbor
  ;; https://www.redblobgames.com/grids/hexagons/#neighbors
  (neighbor [{:keys [x y z]} direction]
            (let [directions {:se [1 -1 0]
                              :ne [1 0 -1]
                              :n [0 1 -1]
                              :nw [-1 1 0]
                              :sw [-1 0 1]
                              :s [0 -1 1]}]
              (apply ->Hexagon (map + [x y z] (direction directions)))))

  ;; https://www.redblobgames.com/grids/hexagons/#distances
  (distance [{:keys [x y z]} {other-x :x other-y :y other-z :z}]
    (max (Math/abs (- x other-x)) (Math/abs (- y other-y)) (Math/abs (- z other-z)))))

(def initial-hexagon (->Hexagon 0 0 0))

(loop [hexagon initial-hexagon
       input input]
  (if (seq input)
    (let [direction (first input)]
      (recur (neighbor hexagon direction) (rest input)))

    (distance initial-hexagon hexagon)))

(def files (->> ($ ls) (filter |(s-suffix? ".mkv" $))))
(def ans
  (reduce
   (fn [ac file]
     (let [line (->> ($ mediainfo $file)
                     (find |(s-prefix? "Duration" $))) # Duration : XX min XX sec
           [min sec] (-< (->> line (s->/ ":") (snd) (s/ " ") (map s->n))
                         (get 1)
                         (get 3))]
       (+ ac (* 60 min) sec))) 0 files))
(pp (/ ans 3600))


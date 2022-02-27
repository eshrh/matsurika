(def changes (-> (file<- "../changes.org")
                 (s>: "Renaming")))

(def pattern '(* "-" :s (<- :S*) :s "::" :s (<- :S*) :s))

(def matches (->> (peg>!* pattern changes)
                  (sort-by |(- (length (first $))))))

(defun main [& args]
  (let [file-path (last args)]
    (file-> file-path
            (reduce (fn [acc el] (s/>* (first el) (last el) acc))
                    (file<- file-path) matches))))

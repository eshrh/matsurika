(def changes-file (-> (file<- "../changes.org")
                      (s>: "Renaming")))

(def patt '{:main (* "-" :s (<- :S*) :s "::" :s (<- :S*) :s)})

(def matches (->> (peg>!* patt changes-file)
                  (sort-by |(- (length (first $))))))

(defun main [& args]
  (file-> (last args)
          (reduce (fn [acc el] (s/>* (first el) (last el) acc))
                  (file-get file) matches)))

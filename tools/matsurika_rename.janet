(def changes-file (-> (file-get "../changes.org")
                      (s>: "Renaming")))

(def patt '{:old (* "-" :s (<- :S*) :s)
            :new (* "::" :s (<- :S*) :s)
            :main (* :old :new)})

(def matches (->> (peg>!* patt changes-file)
                  (sort-by (fn [match]
                             (- (length (first match)))))))

(defun transform [file]
  (def contents (file-get file))
  (file-dump file 
             (reduce (fn [acc el]
                       (s/>* (first el) (last el) acc))
                     contents matches)))

(defun main [& args]
  (transform (last args)))
(defun doc-formatter [doc &opt fmt]
  (let [docstring (get doc :doc)
        sourcemap (get doc :source-map)
        macro (get doc :macro)]
    (def composed-doc
      (s/>* "\"" "\\\""
            (string
             (if macro
               "MACRO"
               "FUNCTION")
             "\n"
             docstring
             (if sourcemap
               (string 
                "\n"
                "Defined in " (fst sourcemap) " -- "
                (snd sourcemap) " : "(last sourcemap))))))
    (if fmt
      (doc-format composed-doc)
      composed-doc)))

(def docs
  (->> (pairs root-env)
       (map (fn [pair]
              (array
               (string (fst pair))
               (doc-formatter (snd pair)))))))


(def qt "\"")

(file->
 "docs.txt"
 (s+
  "(setq mts-docs '("
  (->> (pairs root-env)
       (map (fn [[name doc]]
              (s+ "("
                  qt (string name) qt
                  " . "
                  qt (doc-formatter doc) qt ")")))
       *(s-join nl))
  "))"))

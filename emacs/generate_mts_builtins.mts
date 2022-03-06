
(defun doc-formatter [doc]
  (let [docstring (get doc :doc)
        sourcemap (get doc :source-map)
        macro (get doc :macro)]
    (s/>* "\"" "\\\""
          (s+
           (if macro "MACRO" "FUNCTION")
           nl docstring
           (if sourcemap
             (s+
              nl
              "Defined in " (fst sourcemap) " -- "
              (snd sourcemap) " : "(last sourcemap)))))))

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
       *(s-join nl)) "))"))

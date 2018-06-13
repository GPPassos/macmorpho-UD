;;;; Produces files in exact-matches/suffixed-tag

(defparameter conllu-dir (nth 1 sb-ext:*posix-argv*))
(defparameter output-dir (nth 2 sb-ext:*posix-argv*))

(dolist (file (directory conllu-dir))
  (with-open-file (stream (format nil
                                  "~a/~a-tag.txt"
                                  output-dir
                                  (pathname-name file))
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (sent (cl-conllu:read-conllu file))
      (conllu.converters.tags:write-sentence-tag-suffix-to-stream
       sent
       :stream stream)
      (format stream "~%~%"))))

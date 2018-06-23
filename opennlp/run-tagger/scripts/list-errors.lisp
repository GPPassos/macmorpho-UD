;;;; Auxiliary script used to produce the list of errros.

;;; A wrong token should occur only once.

;;; Input:
;;; - tagged files
;;; - original files

;;; Output:
;;; - a JSON file with errors
;;;   each wrong token occurs only once, with a list of all cases in which it was tagged wrong

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-conllu)
  (ql:quickload :jsown))

;; (in-package :cl-conllu)

;; (defvar *tagged-files*)
(defvar *original-files*)
(defvar *original-tagged-map* '()
  "A plist of ( original-file . (list of tagged files)) Each tagged file should correspond to an original (gold, reference) file. Thus for each original file there is a set of tagged files.")


(defun get-tagged-sentences (original-file original-tagged-map &key (fix-postags? nil) (number-of-sentences))
  "Produces a plist ( tagger-name . tagged-sentences ) for each element in ORIGINAL-TAGGED-MAP corresponding to ORIGINAL-FILE."
  (let ((tagged-files
         (rest (assoc (pathname-name original-file)
                      original-tagged-map
                      :test #'equal))))
    (mapcar
     #'(lambda (tagged-file)
         (let ((tagged-sentences
                (read-file tagged-file :fix-postags? fix-postags?)))
           (if number-of-sentences
               (assert (equal (length tagged-sentences)
                              number-of-sentences)))
           (cons (pathname-name tagged-file)
                 tagged-sentences)))
     tagged-files)))

(defun fix-postags (list-of-sentences &key (switch-labels t))
  "Input:
   - List of sentences
   Output:
   - List of sentences

   Tagged files have some problems that this function fixes.
   1. Some are tagged with more than one tag (TAG1+TAG2), because for OpenNLP it was necessary to use a tag dictionary in order to limit the possible tags (a tag dictionary is a word dictionary with possible tags). 
      Thus we simply select the first tag.
   2. In training files, some tags were wrong:
      - CONJ (instead of CCONJ)
      - NIL (which is always PROPN as far as we have verified)"
  (mapc
   #'(lambda (sentence)
       (mapc
	#'(lambda (token)
	    (setf (cl-conllu:token-upostag token)
		  (ppcre:regex-replace-all
		   "\\+.*" (cl-conllu:token-upostag token)
		   ""))
	    (if switch-labels
		(cond
		  ((equal (cl-conllu:token-upostag token)     
			  "CONJ")
		   (setf (cl-conllu:token-upostag token)
			 "CCONJ"))
		  ((equal (cl-conllu:token-upostag token)
			  "NIL")
		   (setf (cl-conllu:token-upostag token)
			 "PROPN")))))
	(cl-conllu:sentence-tokens sentence)))
   list-of-sentences))

(defun list-to-1d-array (a-list)
  (assert (listp a-list))
  (let ((array (make-array (length a-list)
                           :initial-contents a-list)))
    (assert (typep array 'simple-array))
    array))

(defun read-file (file &key (fix-postags? nil))
  (list-to-1d-array
   (funcall
    (if fix-postags?
        #'fix-postags
        #'identity)
    (funcall
     (if (equal (pathname-type file)
                "conllu")
         #'cl-conllu:read-conllu
         #'conllu.converters.tags:read-file-tag-suffix)
     file))))

(defclass token-error-entry ()
  ((sentence-text
    :accessor entry-sentence-text
    :initarg :sentence-text)
   (original-tag
    :accessor entry-original-tag
    :initarg :original-tag)
   (predicted-tags
    :accessor entry-predicted-tags
    :initarg :predicted-tags) ; alist (tagger . predicted-tag)
   (sentence-id
    :accessor entry-sentence-id
    :initarg :sentence-id)
   (token-id
    :accessor entry-token-id
    :initarg :token-id)))

(defun get-tagged-sentence (token tagged-sentences sentence-index)
  "Find in TAGGED-SENTENCES the sentence corresponding to the
sentence where TOKEN is."
  (let ((output-sentence
         ;; (find (cl-conllu:sentence->text (cl-conllu:token-sentence token))
         ;;       tagged-sentences
         ;;       :key #'cl-conllu:sentence->text
         ;;       :test #'equal)
         (aref tagged-sentences sentence-index)))
    (if (null output-sentence)
        (error "Tagged sentence corresponding to token ~a was not found!"
               token)
        output-sentence)))

(defun get-wrong-predicted-tag (original-token tagged-sentence tagged-name)
  "Find in TAGGED-SENTENCE the token corresponding to ORIGINAL-TOKEN
and, if its tag is different than the original one, returns a pair 
'( TAGGED-NAME . predicted-tag )'.
Otherwise, returns NIL."
  (let ((tagged-token
         (find (cl-conllu:token-id original-token)
               (cl-conllu:sentence-tokens tagged-sentence)
               :key #'cl-conllu:token-id
               :test #'equal)))
    (if (null tagged-token)
        (error "Tagged token corresponding to token ~a was not found!"
               original-token))
    (if (conllu.evaluate::token-diff original-token tagged-token
                                     :fields '(cl-conllu:upostag))
        (cons
         tagged-name
         (cl-conllu:token-upostag tagged-token)))))

(defun get-wrong-predicted-tags (original-token plist-tagged-sentences sentence-index)
  "Returns the alist ('tagger-scenario' . predicted-tag) for every tagger that predicted the tag wrong."
  (remove
   nil
   (mapcar
    #'(lambda (pair)
        (let ((tagged-name (car pair))
              (tagged-sentences (cdr pair)))
          (get-wrong-predicted-tag
           original-token
           (get-tagged-sentence original-token tagged-sentences
                                sentence-index)
           tagged-name)))
    plist-tagged-sentences)))

(defun pretty-sentence-text (sentence token)
  "Returns the sentence text indicating the wrong token."
  (cl-conllu:sentence->text sentence
                            :ignore-mtokens t
                            :special-format-test
                            #'(lambda (tk)
                                (equal (cl-conllu:token-id token)
                                       (cl-conllu:token-id tk)))
                            :special-format-function
                            #'(lambda (string)
                                (format nil "*~a*" (string-upcase string)))))

(defun produce-error-entries (original-sentences plist-tagged-sentences)
  ;; We want a single original list of sentences, even if it has multiple tagged versions (corresponding to different used taggers).
  ;; Therefore this functions receives an original list with all tagged list corresponding to it
  ;; PLIST-TAGGED-SENTENCES is a plist ( tagger-name . tagged-sentences )
  (let ((token-error-entries '()))
    (dotimes (sentence-index (length original-sentences) token-error-entries)
      (let ((sentence (aref original-sentences sentence-index)))
        (dolist (token (cl-conllu:sentence-tokens sentence))
          (let ((wrong-predicted-tags
                 (get-wrong-predicted-tags
                  token
                  plist-tagged-sentences
                  sentence-index))) ; non-nil if there is any error
            (when wrong-predicted-tags
              (let ((new-error-entry
                     (make-instance 'token-error-entry
                                    :sentence-text (pretty-sentence-text sentence token)
                                    :original-tag (cl-conllu:token-upostag token)
                                    :predicted-tags wrong-predicted-tags
                                    :sentence-id (cl-conllu:sentence-id sentence)
                                    :token-id (cl-conllu:token-id token))))
                (push new-error-entry token-error-entries)))))))))

(defmethod jsown:to-json ((token-error-entry token-error-entry))
  (jsown:to-json
   (jsown:new-js
     ("sentence-text" (entry-sentence-text token-error-entry))
     ("original-tag" (entry-original-tag token-error-entry))
     ("predicted-tags" (mapcar
                        #'(lambda (predicted-tag-pair)
                            (let ((tagged-name (car predicted-tag-pair))
                                  (upostag (cdr predicted-tag-pair)))
                              (jsown:new-js
                                (tagged-name upostag))))
                        (entry-predicted-tags token-error-entry)))
     ("sentence-id" (entry-sentence-id token-error-entry))
     ("token-id" (entry-token-id token-error-entry)))))

(defun write-json (error-entries output-file)
  (with-open-file (stream output-file :direction :output
                          :if-exists :supersede)
    (princ (jsown:to-json error-entries) stream)))

(defun run ()
  (let ((*original-files*
         '("/home/gppassos/Documentos/nlp-general/macmorpho-UD/opennlp/run-tagger/original-data/macmorpho-v1-test-keep-pcp.conllu"
           "/home/gppassos/Documentos/nlp-general/macmorpho-UD/opennlp/run-tagger/original-data/macmorpho-v1-test-remove-pcp.conllu"
           "/home/gppassos/Documentos/nlp-general/macmorpho-UD/opennlp/run-tagger/original-data/macmorpho-v1-test-mm-revisto.conllu"
           "/home/gppassos/Documentos/nlp-general/macmorpho-UD/opennlp/run-tagger/original-data/pt-ud-test.conllu"))
        
        (*original-tagged-map*
         
         `(("macmorpho-v1-test-keep-pcp"
            . ,(directory "/home/gppassos/Documentos/nlp-general/macmorpho-UD/opennlp/run-tagger/tagged/pt-keep-pcp-*.tagged"))
           
           ("macmorpho-v1-test-remove-pcp"
            . ,(remove-if
                #'(lambda (x)
                    (ppcre:scan "complete"
                                (pathname-name x)))
                (directory "/home/gppassos/Documentos/nlp-general/macmorpho-UD/opennlp/run-tagger/tagged/pt-remove-pcp-*.tagged")))
           
           ("macmorpho-v1-test-mm-revisto"
            . ,(directory "/home/gppassos/Documentos/nlp-general/macmorpho-UD/opennlp/run-tagger/tagged/pt-mm-revisto-*.tagged"))
           
           ("pt-ud-test"
            . ,(append
                (directory "/home/gppassos/Documentos/nlp-general/macmorpho-UD/opennlp/run-tagger/tagged/pt-bosque-*.tagged")
                (directory "pt-remove-pcp-*-complete*.tagged")))))
        (output-dir "/home/gppassos/Documentos/nlp-general/macmorpho-UD/opennlp/run-tagger/errors-listed"))
    
    (dolist (original-file *original-files*)
      (gc)
      (let ((fix-postags
             (if (member (pathname-name original-file)
                         '("pt-ud-test.conllu" "macmorpho-v1-test-mm-revisto.conllu"))
                 nil
                 t)))
        (let* ((original-sentences (read-file original-file
                                              :fix-postags? fix-postags))
               (plist-tagged-sentences
                (get-tagged-sentences original-file *original-tagged-map*
                                      :fix-postags? fix-postags
                                      :number-of-sentences (length original-sentences))))
          (write-json (produce-error-entries original-sentences plist-tagged-sentences)
                      (format nil "~a/~a.json" output-dir (pathname-name original-file))))))))


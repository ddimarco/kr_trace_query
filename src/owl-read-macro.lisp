(in-package :kr-trace-query)

(defparameter *owl-namespace-shortcuts* (make-hash-table :test 'equalp))

(defun register-owl-namespace (short full)
  (setf (gethash short *owl-namespace-shortcuts*) full))
(defun get-owl-namespace (short)
  (gethash short *owl-namespace-shortcuts*))

(defun shorten-owl-id (full)
  "converts a fully namespaced owl id to a shortened one. Should be used only for display."
  (let ((res
         (loop for key being the hash-keys of *owl-namespace-shortcuts*
            using (hash-value value)
            for val-len = (length value)
            when (and (> (length full) val-len)
                      (string= (subseq full 0 val-len)
                               value))
            return (concatenate 'string key ":" (subseq full (1+ val-len))))))
    (if res
        res
        full)))

(defun owl-symbol-reader (stream sub-char numarg)
  "Defines a read macro for shortcut representation of OWL identifiers. E.g.: #\"knowrob:Thing\""
  (declare (ignore sub-char numarg))
  (let ((string
         (coerce
          (loop for c = (read-char stream)
             until (char= c #\")
             collect c)
          'string)))
    (unless (position #\: string)
      (error "no ':' in owl identifier ~s, so no namespace can be found." string))
    (let ((namespace-short (subseq string 0 (position #\: string)))
          (identifier (subseq string (1+ (position #\: string)))))
      (let ((namespace-resolved (get-owl-namespace namespace-short)))
        (when (null namespace-resolved)
          (error "could not resolve owl namespace ~s in ~s!" namespace-short string))
        (concatenate 'string namespace-resolved "#" identifier)))))

(eval-when (:load-toplevel)
  (register-owl-namespace "knowrob" "http://ias.cs.tum.edu/kb/knowrob.owl")
  (register-owl-namespace "w3" "http://www.w3.org/2002/07/owl")
  (register-owl-namespace "cram_log" "http://ias.cs.tum.edu/kb/cram_log.owl")
  (register-owl-namespace "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns")
  (register-owl-namespace "rdfs" "http://www.w3.org/2000/01/rdf-schema")
  (register-owl-namespace "owl" "http://www.w3.org/2002/07/owl")
  (register-owl-namespace "xsd" "http://www.w3.org/2001/XMLSchema")
  (register-owl-namespace "ias_semantic_map" "http://ias.cs.tum.edu/kb/ias_semantic_map.owl")
  (register-owl-namespace "srdl2comp" "http://ias.cs.tum.edu/kb/srdl2-comp.owl")
  (register-owl-namespace "srdl2cap" "http://ias.cs.tum.edu/kb/srdl2-cap.owl")
  (register-owl-namespace "pr2" "http://ias.cs.tum.edu/kb/PR2.owl" )
  (set-dispatch-macro-character #\# #\" #'owl-symbol-reader))

;; (defun test-namespace-reader ()
;;   (format t "namespace: ~a~%" #"knowrob:Action"))

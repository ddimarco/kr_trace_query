(in-package :kr-trace-query)

(defun extraction-main (input output)
  (extract-from-log input)
  (cl-prada::write-learn-file output *prada-experiences*)
  (sb-ext:quit))

(crs:def-fact-group sem-map-stuff (cl-semantic-map-utils:semantic-map-name)
  ;; defines the IAI kitchen map as the semantic map to use
  (crs:<- (cl-semantic-map-utils:semantic-map-name
           "http://ias.cs.tum.edu/kb/ias_semantic_map.owl#SemanticEnvironmentMap_PM580j")))

(defun load-experiment (exp)
  (json-prolog:prolog `("load_experiment" ,(format nil "/home/marcodl/roslogs/~a/cram_log.owl" exp))))

(defun init-mongo-db ()
  (cl-mongo:db.use "roslog"))

(eval-when (:load-toplevel)
  (init-mongo-db))

(defun init-kr (&optional (exp "neu"))
  (roslisp:start-ros-node "kr_trace_query")
  (json-prolog:prolog '("register_ros_package" "iai_maps"))
  (load-experiment exp)
  (re-pl-utils:load-local-owl-file "iai_maps" "owl" "room")
  (re-pl-utils:load-local-owl-file "mod_srdl" "owl" "PR2"))

(defparameter *experiment* nil)
(defparameter *prada-experiences* nil)

(defun store-experiences (file)
  (cl-store:store *prada-experiences* file))

(defun extract-from-log (log)
  (roslisp:start-ros-node "kr_trace_query")
  (json-prolog:prolog '("register_ros_package" "iai_maps"))
  (re-pl-utils:load-local-owl-file "iai_maps" "owl" "room")
  (re-pl-utils:load-local-owl-file "mod_srdl" "owl" "PR2")
  (json-prolog:prolog `("load_experiment" ,log))
  (format t "loaded experiment data, starting extraction...~%")
  (extract-everything)
  (cl-prada::write-prada-files-learning
   *prada-experiences*
   (prada-symbol-defs-from-learn-data *prada-experiences*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-all-actions ()
  ;; for now, consider performactiondesignator as actions
  ;; the designator itself should be linked in the parent
  (all-owl-instances-of #"knowrob:PerformOnProcessModule"))

(defun shorten-uri (uri)
  (subseq uri (1+ (position #\# uri))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun assert-single (asrt)
  (assert (= (length asrt) 1))
  (car asrt))

(defun assert-max-single (asrt)
  (assert (<= (length asrt) 1))
  (car asrt))

(defun assert-single-recursive (asrt)
  (assert (= (length asrt) 1))
  (let ((head (car asrt)))
    (if (listp head)
        (assert-single head)
        head)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun timesteps-in-task (task-id)
  (destructuring-bind (start . end)
      (task-interval task-id)
    (timesteps-between start end)))

(defun sort-tasks (lst)
  (let ((start-end-map (make-hash-table :test #'equal)))
    (loop for tsk in lst
         for se = (task-interval tsk)
         do
         (when (> (length se) 1)
           (roslisp:ros-warn () "more than one start-end pair found for ~a: ~a" tsk se)
           (setf se (list (car se))))
         (setf (gethash tsk start-end-map) (car se)))
    (setf lst (sort lst (lambda (tsk1 tsk2)
                          (< (car (gethash tsk1 start-end-map))
                             (car (gethash tsk2 start-end-map))))))
    lst))

(defun lispify-mongo-doc (doc &key (pkg *package*))
  (check-type doc cl-mongo:document)
  (loop for key in (cl-mongo:get-keys doc)
       for val = (cl-mongo:get-element key doc)
     collect
       (cons (intern (string-upcase key) pkg)
             (typecase val
                 (cl-mongo:document
                  (lispify-mongo-doc val))
                 (list
                  (mapcar #'lispify-mongo-doc val))
                 (t val)))))

(defun mongo-get-designator (id &key (collection "logged_designators"))
  (let ((id (if (find #\# id)
                (shorten-uri id)
                id)))
    (let ((result (cl-mongo:docs
                   (cl-mongo:iter
                    (cl-mongo:db.find collection (cl-mongo:kv "designator._id" id) :limit 0)))))
      (assert (= (length result) 1) (result) "designator list is /= 1: ~a" result)
      (if result
          (values (lispify-mongo-doc (car (cl-mongo:get-element "designator" result)))
                  (cl-mongo:bson-time-to-ut (cl-mongo:get-element "__recorded" (car result)))
                  (cl-mongo:get-element "__topic" (car result)))
          nil))))

(defun mongo-desig-type (m-desig &key (pkg *package*))
  (let ((type-str (cdr (assoc '_designator_type m-desig))))
    (if type-str
        (intern (string-upcase type-str) pkg))))

(defun mongo-action-desig-type (m-desig &key (pkg *package*))
  (let ((type-str (cdr (assoc 'type m-desig))))
    (if type-str
        (intern (string-upcase type-str) pkg))))

;; actions: combination of "to" and "type" in action-designators
(defun mongo-desig->action-predicate-name (m-desig &key (pkg *package*))
  (let ((type (cdr (assoc 'type m-desig)))
        (to (cdr (assoc 'to m-desig))))
    (intern (if (and type to)
                (format nil "~a-~a" to type)
                (format nil "~a" (or type to)))
            pkg)))

(defun get-action-designator-for-perform-pm (owlid)
  "Find a designator (which can then be read from mongodb) for the given
 performonprocessmodule designator. Searches upwards the subAction predicate
 until a performactiondesignator instance is found."
  (labels ((find-action-upwards (owlid &optional (depth 0))
             (let ((parent
                    ;; should be only one parent at max
                    (assert-single-recursive
                     (mapcar #'car
                             (re-pl-utils:owl-has-query :predicate #"knowrob:subAction"
                                                        :object owlid)))))
               (if (re-pl-utils:is-individual-of parent #"knowrob:PerformActionDesignator")
                   parent
                   (find-action-upwards parent (1+ depth))))))
    (let* ((performactiondesig (find-action-upwards owlid))
           (desig-id
            (assert-single-recursive (re-pl-utils:owl-has-query :subject performactiondesig
                                                                :predicate #"knowrob:designator"))))
      desig-id)))

(defun owl-search-upwards (owlid &key (child-relation #"knowrob:subAction")
                                  (predicate
                                   (lambda (id)
                                     (re-pl-utils:is-individual-of id #"knowrob:ArmMovement"))))
  (labels ((find-upwards (owlid &optional (depth 0))
             (let ((next
                    (assert-single-recursive
                     (mapcar #'car
                             (re-pl-utils:owl-has-query :predicate child-relation
                                                        :object owlid)))))
               (if (funcall predicate next)
                   next
                   (find-upwards next (1+ depth))))))
    (find-upwards owlid)))

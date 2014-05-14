(in-package :kr-trace-query)

;; (defun load-pick-trace ()
;;   (re-pl-utils:load-owl-file "/home/marcodl/bremen_ws/src/data/packaging/pick.owl"))
;; (defun load-pick-and-place-trace ()
;;   ;; (re-pl-utils:load-owl-file "/home/marcodl/roslogs/exp-2014-02-25_12-54-56/pick-and-place.owl")
;;   (json-prolog:prolog `("load_experiment" "/home/marcodl/roslogs/exp-2014-02-25_12-54-56/pick-and-place.owl")))

(defun load-experiment (&optional (exp "first-part-demo"))
  (json-prolog:prolog `("load_experiment" ,(format nil "/home/marcodl/roslogs/~a/cram_log.owl" exp))))

(defun init-mongo-db ()
  (cl-mongo:db.use "roslog"))

(eval-when (:load-toplevel)
  (init-mongo-db))

;;(defparameter *map-name* #"ias_semantic_map:SemanticEnvironmentMap_PM580j")
;; cl-semantic-map-util:get-semantic-map to get map
(crs:def-fact-group sem-map-stuff (cl-semantic-map-utils:semantic-map-name)
  (crs:<- (cl-semantic-map-utils:semantic-map-name
           "http://ias.cs.tum.edu/kb/ias_semantic_map.owl#SemanticEnvironmentMap_PM580j")))

(defun init-kr ()
  (roslisp:start-ros-node "kr_trace_query")
  (json-prolog:prolog '("register_ros_package" "iai_maps"))
  (load-experiment)
  (re-pl-utils:load-local-owl-file "iai_maps" "owl" "room"))

(defun all-action-classes ()
  (cut:with-vars-strictly-bound (?l)
      (car (json-prolog:prolog-simple-1 "setof(C, T^task_class(T, C), L)"))
    (re-pl-utils:pl-tree->string ?l)))

(defun task-of-class (task-class)
 (re-pl-utils:pl-query (?t)
     `("task_class" ?t ,task-class)
   (re-pl-utils:pl-tree->string ?t)))

(defun failures-with-task-class ()
  (cut:force-ll (re-pl-utils:pl-query (?e ?c) '(and ("task_class" ?t ?c)
                                                ("failure_task" ?e ?t))
                    (cons
                     (re-pl-utils:pl-tree->string ?c)
                     (re-pl-utils:pl-tree->string ?e)))))

(defmacro simple-pl-query (query)
  `(cut:force-ll (json-prolog:prolog ,query :package ,*package*)))

(defun all-owl-instances-of (cls)
  (cut:with-vars-strictly-bound (?l)
      (car (json-prolog:prolog-simple-1
            (format nil "setof(I, owl_individual_of(I, '~a'), L)" cls)))
    (re-pl-utils:pl-tree->string ?l)))

(defun all-objects ()
  (all-owl-instances-of "http://ias.cs.tum.edu/kb/knowrob.owl#HumanScaleObject"))

(defun all-time-steps ()
  (all-owl-instances-of "http://ias.cs.tum.edu/kb/knowrob.owl#TimePoint"))

(defun all-perceptions ()
 (cut:force-ll
  (json-prolog:prolog '(and
                        ("owl_individual_of" ?b "http://ias.cs.tum.edu/kb/knowrob.owl#VisualPerception")
                        ("returned_value" ?b ?o)))))

(defun all-designators ()
  (all-owl-instances-of "http://ias.cs.tum.edu/kb/knowrob.owl#CRAMDesignator"))

(defun all-tasks ()
  (mapcar #'(lambda (bdg)
              (cut:with-vars-strictly-bound (?t) bdg
                (re-pl-utils:pl-tree->string ?t)))
          (cut:force-ll (json-prolog:prolog '("task" ?t)))))

(defun start-end-task (tsk)
  (re-pl-utils:pl-query (?start ?end)
      `(and ("task_start" ,tsk ?start) ("task_end" ,tsk ?end))
    (cons ?start ?end)))

(defun sort-tasks (lst)
  (let ((start-end-map (make-hash-table :test #'equal)))
    (loop for tsk in lst
         for se = (cut:force-ll (start-end-task tsk))
         do
         (when (> (length se) 1)
           (roslisp:ros-warn () "more than one start-end pair found for ~a: ~a" tsk se)
           (setf se (list (car se))))
         (setf (gethash tsk start-end-map) (car se)))
    (setf lst (sort lst (lambda (tsk1 tsk2)
                          (< (car (gethash tsk1 start-end-map))
                             (car (gethash tsk2 start-end-map))))))
    lst))


;; (defun get-all-obj-pose-beliefs (int-time-lst)
;;   (remove nil
;;           (mapcar #'(lambda (time) (cut:force-ll
;;                                     (re-pl-utils:pl-query (?o ?l) `("belief_at" ("loc" ?o ?l) ,time)
;;                                       (list time ?o ?l))))
;;                   int-time-lst)))

;; (defun get-all-loc-changes (int-time-lst)
;;   (remove nil
;;           (mapcar #'(lambda (time) (cut:force-ll
;;                                     (re-pl-utils:pl-query (?o) `("occurs" ("loc_change" ?o) ,time)
;;                                       (list time ?o))))
;;                   int-time-lst)))

(defun shorten-uri (uri)
  (subseq uri (1+ (position #\# uri))))

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
      (assert (= (length result) 1) (result) "designator list is > 1: ~a" result)
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

(defun owl-action-designator-p (owlid)
  (eq (mongo-desig-type (mongo-get-designator (shorten-uri owlid)))
                                 'action))

(defun owl-get-all-action-designators ()
  (let ((action-desig-ids
         (remove-if (complement #'owl-action-designator-p)
                    (all-designators))))
    action-desig-ids))

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

(defun get-all-mongo-desigs (&key (type nil))
  (let ((all
         (mapcar #'mongo-get-designator (mapcar #'shorten-uri (all-designators)))))
    (cond
      (type
       (remove-if (lambda (desig)
                    (not (eq (mongo-desig-type desig) type)))
                  all))
      (t all))))

;; get recursively all subactions
(defun get-subactions (id &key (recursive t))
  (check-type id string)
  (let ((subactions
         (mapcar #'car (re-pl-utils:owl-has-query
                        :subject id
                        :predicate #"knowrob:subAction"))))
    (if recursive
        (append
         subactions
         (loop for sa in subactions append
              (get-subactions sa)))
        subactions)))

(defun get-all-actions ()
  ;; for now, consider performactiondesignator as actions
  ;; the designator itself should be linked in the parent
  (all-owl-instances-of #"knowrob:PerformOnProcessModule"))






;;(db.use "roslog")
;;(pp (db.find "logged_designators" :all :selector (kv "__recorded" 1) :limit 0))
;;(bson-time-to-ut (get-element "__recorded" (caadr (db.find "logged_designators" :all :selector (kv "__recorded" 1) :limit 1))))
;; sort tasks time-wise and subtask-wise
;; when are actions triggered?

;; symbols from current package
;; (do-all-symbols (s)
;;   (when (eq (symbol-package s) *package*)
;;     (print s)))
;; (car (db.find "cities" (kv (kv "city" "ATLANTA") (kv "state" "GA"))  :limit 0))
;; (pp (db.find "logged_designators" (kv "designator.TYPE" "CONTAINER") :limit 0))

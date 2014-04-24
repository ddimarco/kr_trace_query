(in-package :kr-trace-query)

(defun load-pick-trace ()
  (re-pl-utils:load-owl-file "/home/marcodl/bremen_ws/src/data/packaging/pick.owl"))

(defun init-mongo-db ()
  (cl-mongo:db.use "roslog"))

(eval-when (:load-toplevel)
  (init-mongo-db))

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

(defun timepoint-id->time (tp-id)
  (let ((str (subseq tp-id (1+ (position #\_ tp-id :from-end t)))))
    (parse-integer str)))

(defun sort-times (timepoints)
  (let ((timepoints (sort timepoints
                          #'<
                          :key (lambda (tp) (timepoint-id->time tp)))))
    timepoints))

(defun leads-to-failure (owlid)
  )

(defun get-all-obj-pose-beliefs (int-time-lst)
  (remove nil
          (mapcar #'(lambda (time) (cut:force-ll
                                    (re-pl-utils:pl-query (?o ?l) `("belief_at" ("loc" ?o ?l) ,time)
                                      (list time ?o ?l))))
                  int-time-lst)))

(defun get-all-loc-changes (int-time-lst)
  (remove nil
          (mapcar #'(lambda (time) (cut:force-ll
                                    (re-pl-utils:pl-query (?o) `("occurs" ("loc_change" ?o) ,time)
                                      (list time ?o))))
                  int-time-lst)))

(defun shorten-uri (uri)
  (subseq uri (1+ (position #\# uri))))
(defun tasks->graphviz (int-time-lst)
  (with-open-file (str "/tmp/bla.dot" :direction :output :if-exists :supersede)
    (format str "digraph G {~%")
    (format str " subgraph cluster_time_points {~%")
    (format str "order=LR; node [style=filled];~%")
    (loop for tp-uri in int-time-lst
       for tp = (timepoint-id->time tp-uri)
       do
         (format str "~a " tp))
    (format str ";~% }~%")

    (loop for tp-uri in int-time-lst
       for tp = (timepoint-id->time tp-uri)
         with prev = nil
       do
         (when prev
           (format str "~a -> ~a;~%" prev tp))
         (setf prev tp))

    (loop for tp-uri in int-time-lst
       for tp = (timepoint-id->time tp-uri)
       for start-tasks = (cut:force-ll
                          (re-pl-utils:pl-query (?t)
                              `("task_start" ?t ,tp) (re-pl-utils:prolog->string  ?t)))
       for end-tasks = (cut:force-ll
                        (re-pl-utils:pl-query (?t)
                            `("task_end" ?t ,tp) (re-pl-utils:prolog->string ?t)))
       do
         (dolist (et end-tasks)
           (format str "  ~a -> ~a;~%" (shorten-uri et) tp))
         (dolist (st start-tasks)
           (format str "  ~a -> ~a;~%" tp (shorten-uri st))))
    (format str "}~%")))

(defun lispify-mongo-doc (doc &key (pkg *package*))
  (check-type doc cl-mongo:document)
  (loop for key in (cl-mongo:get-keys doc)
       for val = (cl-mongo:get-element key doc)
     collect
       (cons (intern (string-upcase key) pkg)
             (if (typep val 'cl-mongo:document)
                 (lispify-mongo-doc val)
                 val))))

(defun mongo-get-designator (id &key (collection "logged_designators"))
  (let ((id (if (find #\# id)
                (shorten-uri id)
                id)))
    (let ((result (cl-mongo:docs
                   (cl-mongo:db.find collection
                                     (cl-mongo:kv "designator.__ID" id) :limit 0))))
      (assert (<= (length result) 1))
      (if result
          (values (lispify-mongo-doc (car (cl-mongo:get-element "designator" result)))
                  (cl-mongo:bson-time-to-ut (cl-mongo:get-element "__recorded" (car result)))
                  (cl-mongo:get-element "__topic" (car result)))
          nil))))

(defun mongo-desig-type (m-desig &key (pkg *package*))
  (let ((type-str (cdr (assoc '_designator_type m-desig))))
    (if type-str
        (intern (string-upcase type-str) pkg))))

(defparameter *action-predicate-params*
  '((perceive (obj))
    (follow-trajectory (pose))
    (navigation (goal))
    (carry-trajectory (obj))
    (grasp-trajectory (obj))
    (lift-trajectory (obj))
   )
  )

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

;; for now: consider only performactiondesignator elements as actions
(defun action-desig->relational (owl-id)
  (let* ((mongo-desig (mongo-get-designator owl-id))
         (predicate (mongo-desig->action-predicate-name mongo-desig))
         (param-symbols (cadr (assoc predicate *action-predicate-params*)))
         (param-instances (make-hash-table)))
    (let ((params
           (loop for ps in param-symbols
                for ps-instance = (gensym "INSTANCE")
                for ps-sexp = (assoc ps mongo-desig)
                do
                (setf (gethash ps-instance param-instances) ps-sexp)
              collect
              ps-instance)))
      (values
       `(,predicate
         ,params)
       (alexandria:hash-table-alist param-instances)))))

(defun assert-single (asrt)
  (assert (= (length asrt) 1))
  (car asrt))

(defun get-action-desig-outcome (owl-id)
  (let ((all-times (sort-times (all-time-steps)))
        (end-time (assert-single (find-action-desig-time owl-id :end t))))
    (let ((tpos (position end-time all-times :test #'equal)))
      (setf all-times (subseq all-times tpos)))

    (loop for time in all-times
       for starting-actions = (remove-if
                               (complement #'owl-action-designator-p)
                               (apply #'append
                                (re-pl-utils:owl-has-query :predicate #"knowrob:startTime"
                                                           :object time)))
         )
    )
)

(defun find-action-desig-time (owl-id &key (end t))
  ;; action designators usually don't have children
  (let ((end-time
         (if end
             (re-pl-utils:owl-has-query :subject owl-id :predicate
                                        #"knowrob:endTime")
             (re-pl-utils:owl-has-query :subject owl-id :predicate
                                        #"knowrob:startTime"))))
    (if end-time
        (mapcar #'car end-time)
        (let ((parents (re-pl-utils:owl-has-query :predicate #"knowrob:designator" :object owl-id)))
          (remove-duplicates
           (loop for p in parents
              append (find-action-desig-time (car p)))
           :test #'equal)
          ))))

(defun timesteps-after (timestep)
  (let ((all-times (sort-times (all-time-steps))))
    (subseq all-times (1+ (position timestep all-times :test #'equal)))))

;; toplevel: cram_log:CRAMAchieve_AOXX3iUI
(defun task-context (owl-id)
  (let ((task-contexts
         (re-pl-utils:owl-has-query :subject owl-id :predicate #"knowrob:taskContext")))
    (loop for x in task-contexts
         for (lit (tp_ type ctx)) = (car x)
         collect ctx)))
;; a "task" is a subclass of knowrob:CRAMEvent
;; (defun trace-task->relational (task-owl-id)
;;   )

(defun get-all-mongo-desigs (&key (type nil))
  (let ((all
         (mapcar #'mongo-get-designator (mapcar #'shorten-uri (all-designators)))))
    (cond
      (type
       (remove-if (lambda (desig)
                    (not (eq (mongo-desig-type desig) type)))
                  all))
      (t all))))

;; outcome for a designator
;; "designator_MVn6wQqyNdQc3d": grasp
;; (defun get-action-desig-outcome (desig-id)
;;   ;; (let ((failures (re-pl-utils:owl-has-query
;;   ;;                  :subject desig-id
;;   ;;                  :predicate #"knowrob:eventFailure")))
;;   ;;   failures
;;   ;;   )
;;   )

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

(defun mongo-desig->predicate-list (desig &key (filter-properties nil))
  (let* ((desig-type (cdr (assoc '_designator_type desig)))
         (desig-identifier (gensym
                            (if (null desig-type)
                                "DESIG"
                                (string-upcase desig-type))))
         (filter-properties (append filter-properties '(_designator_type))))
    (values
     (append
      `((,(intern (if (null desig-type)
               "DESIGNATOR"
               (string-upcase desig-type)))
          ,desig-identifier))
      (loop for desig-prop-pair in desig
         for prop = (car desig-prop-pair)
         for prop-val = (cdr desig-prop-pair)
         unless (member prop filter-properties)
         append
           (if (listp prop-val)
               (multiple-value-bind (pred-lst new-desig-id)
                   (mongo-desig->predicate-list prop-val :filter-properties filter-properties)
                 (append `((,prop ,desig-identifier ,new-desig-id))
                         pred-lst))
               (list (list prop desig-identifier prop-val)))))
     desig-identifier)))

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

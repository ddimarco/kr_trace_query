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


(defun owl-action-designator-p (owlid)
  (eq (mongo-desig-type (mongo-get-designator (shorten-uri owlid)))
                                 'action))

(defun owl-get-all-action-designators ()
  (let ((action-desig-ids
         (remove-if (complement #'owl-action-designator-p)
                    (all-designators))))
    action-desig-ids))

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


(defun all-perceptions ()
 (cut:force-ll
  (json-prolog:prolog '(and
                        ("owl_individual_of" ?b "http://ias.cs.tum.edu/kb/knowrob.owl#VisualPerception")
                        ("returned_value" ?b ?o)))))

(defun all-objects ()
  (all-owl-instances-of "http://ias.cs.tum.edu/kb/knowrob.owl#HumanScaleObject"))

(defun all-designators ()
  (all-owl-instances-of "http://ias.cs.tum.edu/kb/knowrob.owl#CRAMDesignator"))

(defun all-tasks ()
  (mapcar #'(lambda (bdg)
              (cut:with-vars-strictly-bound (?t) bdg
                (re-pl-utils:pl-tree->string ?t)))
          (cut:force-ll (json-prolog:prolog '("task" ?t)))))


;; (mapcar #'lispify-mongo-doc (cl-mongo:docs (tf-mongo-query (cl-mongo:date-time 0 0 0 19 2 2014) (cl-mongo:date-time 0 0 12 19 2 2014) "/map" "/odom_combined")))

;; (cl-mongo:pp (cl-mongo:db.find "tf" (cl-mongo:$em "transforms" (cl-mongo:kv "header.frame_id" "/map"))))
;; db.tf.find({__recorded: {$gt: ISODate("2014-02-19T08:00:00"), $lte: ISODate("2014-02-19T08:42:38.278Z")}}).count()
;; all localizations
;; db.tf.find({transforms: {$elemMatch: {"header.frame_id": child_frame_id: '/odom_combined'}}})

;; all entries recorded before a given time
;; (cl-mongo:db.find "tf" (cl-mongo:$< "__recorded" (cl-mongo:date-time 0 43 8 19 2 2014)))


;; (cl-mongo:pp
;;  (cl-mongo:db.find "tf"
;;                    (cl-mongo:$em "transforms"
;;                                  (cl-mongo:kv
;;                                   (cl-mongo:kv "child_frame_id" "/odom_combined")
;;                                   (cl-mongo:$< "header.stamp" (cl-mongo:date-time 30 3 9 21 2 2014))))
;;                    :limit 0))

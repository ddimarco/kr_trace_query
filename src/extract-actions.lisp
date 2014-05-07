(in-package :ktq)


;; assign create parameters for relational actions from designators via pattern matching
;; TODO: convert to real designator and use CPL's predicates
;; TODO: add timestamps to actions?
(crs:def-fact-group desig->predicates (mongo-desig-prop extract-relational)
  (crs:<- (mongo-desig-prop ?d (?prop ?val))
    (crs:lisp-pred listp ?d)
    (crs:lisp-fun assoc ?prop ?d ?pair)
    (crs:lisp-fun cdr ?pair ?pair-car )
    (crs:equal ?pair-car ?val))

  ;; extract parameters for navigation
  (crs:<- (extract-relational ?mongo-desig ?action)
    (mongo-desig-prop ?mongo-desig (type "NAVIGATION"))
    (mongo-desig-prop ?mongo-desig (goal ?goal))

    ;; for "to see" navs
    (mongo-desig-prop ?goal (to "SEE"))
    ;; for objects
    (mongo-desig-prop ?goal (obj ?obj))
    ;; of a specific type
    ;; (mongo-desig-prop ?obj (type ?type))
    ;; (crs:equal ?params (see ?type ))

    ;; TODO: should operate on type
    (mongo-desig-prop ?obj (name ?nm))
    (crs:lisp-pred identity ?nm)
    (crs:equal ?action (navigate-to-see ?nm)))

  (crs:<- (extract-relational ?mongo-desig ?action)
    (mongo-desig-prop ?mongo-desig (type "NAVIGATION"))
    (mongo-desig-prop ?mongo-desig (goal ?goal))

    ;; for "to see" navs
    (mongo-desig-prop ?goal (to "REACH"))
    ;; for objects
    (mongo-desig-prop ?goal (obj ?obj))
    ;; of a specific type
    ;; (mongo-desig-prop ?obj (type ?type))
    ;; (crs:equal ?params (see ?type ))

    ;; TODO: should operate on type
    (mongo-desig-prop ?obj (name ?nm))
    (crs:lisp-pred identity ?nm)
    (crs:equal ?action (navigate-to-see ?nm)))

  ;; navigation with only cartesian goals
  (crs:<- (extract-relational ?mongo-desig ?action)
    (mongo-desig-prop ?mongo-desig (type "NAVIGATION"))
    (mongo-desig-prop ?mongo-desig (goal ?goal))

    (mongo-desig-prop ?goal (pose ?pose))
    (mongo-desig-prop ?pose (pose ?p2))
    (mongo-desig-prop ?p2 (position ?position))
    (mongo-desig-prop ?position (x ?x))
    (mongo-desig-prop ?position (y ?x))
    (mongo-desig-prop ?position (z ?x))

    (crs:lisp-pred identity ?x)
    (crs:lisp-pred identity ?y)
    (crs:lisp-pred identity ?z)

    (crs:format "----------------------------~%")
    ;; TODO: orientation
    (crs:equal ?action (navigate (?x ?y ?z))))

  ;; follow trajectory
;;   (crs:<- (extract-relational ?mongo-desig ?action)
;;     (mongo-desig-prop ?mongo-desig (type "TRAJECTORY"))
;;     (mongo-desig-prop ?mongo-desig (to "FOLLOW"))
;;     (crs:equal ?action nil)
;;     ;; problem: only fixed poses in follow-trajectory?
;;     ;; how to get to the object?
;; ;;    (mongo-desig-prop ?mongo-desig ())
;;     )

  (crs:<- (extract-relational ?mongo-desig ?action)
    (mongo-desig-prop ?mongo-desig (type "TRAJECTORY"))
    (mongo-desig-prop ?mongo-desig (to "PUT-DOWN"))
    (mongo-desig-prop ?mongo-desig (obj ?obj))
    (mongo-desig-prop ?obj (name ?obj-name))
    (crs:equal ?action (put-down ?obj-name)))

  (crs:<- (extract-relational ?mongo-desig ?action)
    (mongo-desig-prop ?mongo-desig (type "TRAJECTORY"))
    (mongo-desig-prop ?mongo-desig (to "LIFT"))
    (mongo-desig-prop ?mongo-desig (obj ?obj))
    (mongo-desig-prop ?obj (name ?obj-name))
    (crs:equal ?action (lift ?obj-name)))

  (crs:<- (extract-relational ?mongo-desig ?action)
    (mongo-desig-prop ?mongo-desig (type "TRAJECTORY"))
    (mongo-desig-prop ?mongo-desig (to "GRASP"))
    (mongo-desig-prop ?mongo-desig (obj ?obj))
    (mongo-desig-prop ?obj (name ?obj-name))
    (crs:equal ?action (grasp ?obj-name)))

  (crs:<- (extract-relational ?mongo-desig ?action)
    (mongo-desig-prop ?mongo-desig (type "TRAJECTORY"))
    (mongo-desig-prop ?mongo-desig (to "PARK"))
    (crs:equal ?action (park-arm)))

  (crs:<- (extract-relational ?mongo-desig ?action)
    (mongo-desig-prop ?mongo-desig (type "TRAJECTORY"))
    (mongo-desig-prop ?mongo-desig (to "CARRY"))
    (mongo-desig-prop ?mongo-desig (obj ?obj))
    (mongo-desig-prop ?obj (name ?obj-name))
    (crs:equal ?action (carry-object ?obj-name)))

  (crs:<- (extract-relational ?mongo-desig ?action)
    (mongo-desig-prop ?mongo-desig (to "PERCEIVE"))
    (mongo-desig-prop ?mongo-desig (obj ?obj))

    (mongo-desig-prop ?obj (name ?obj-name))
    (crs:equal ?action (perceive ?obj-name)))


  ;; actions without object names
  ;; TODO

  )


(defun owl-desig->relational (owlid)
  "Convert a action designator owl id into a relational action."
  (let ((res
         (mapcar #'(lambda (bdg)
                     (cut:with-vars-strictly-bound (?action) bdg
                       ?action))
                 (cut:force-ll
                  (crs:prolog `(extract-relational
                                ,(mongo-get-designator owlid) ?action))))))
    (car res)))

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

;;;; get world state
;; need to get *all* objects and their state at the action start time
;; also take the information from the action designators themselves?

;; first: get time interval of an action
(defun get-action-designator-time-interval (owlid)
  (let ((start-time (assert-single-recursive (re-pl-utils:owl-has-query :subject owlid
                                                              :predicate #"knowrob:startTime")))
        (end-time (assert-single-recursive (re-pl-utils:owl-has-query :subject owlid
                                                            :predicate #"knowrob:endTime"))))
    (cons start-time end-time)))


;; FIXME: some timestamps in designators seem off -> expected for static ones (timestamp=0)

;; TODO: we might want a cache, similar to the cl-tf one for faster lookups
;; (defun get-transform (from to timestamp)
;;   (let ((doc (car (last
;;                    (cl-mongo:docs
;;                     (cl-mongo:db.sort "tf"
;;                                       (cl-mongo:$em "transforms"
;;                                                     (cl-mongo:kv
;;                                                      (cl-mongo:kv
;;                                                       (cl-mongo:kv "child_frame_id" to)
;;                                                       (cl-mongo:kv "header.frame_id" from))
;;                                                      (cl-mongo:$<= "header.stamp" timestamp)))
;;                                       :field "header.stamp"
;;                                       :limit 0))))))
;;     ;; check if last timestamp is bigger than 1 hour
;;     (if (null doc)
;;         nil
;;         (let ((stamp-local (bson->local (assert-single (cl-mongo:get-element "transforms.header.stamp"
;;                                                                              doc))))
;;               (min-time-ok (local-time:timestamp- (bson->local timestamp) 1 :hour)))
;;           ;; (assert (local-time:timestamp> stamp-local min-time-ok))
;;           (when (local-time:timestamp> stamp-local min-time-ok)
;;             (error
;;              (format nil
;;                      "time difference between requested and latest found transform is too big: ~a"
;;                      stamp-local)))
;;           (cl-transforms:make-transform
;;            (cl-transforms:make-3d-vector
;;             (assert-single (cl-mongo:get-element "transforms.transform.translation.x" doc))
;;             (assert-single (cl-mongo:get-element "transforms.transform.translation.y" doc))
;;             (assert-single (cl-mongo:get-element "transforms.transform.translation.z" doc)))
;;            (cl-transforms:make-quaternion
;;             (assert-single (cl-mongo:get-element "transforms.transform.rotation.x" doc))
;;             (assert-single (cl-mongo:get-element "transforms.transform.rotation.y" doc))
;;             (assert-single (cl-mongo:get-element "transforms.transform.rotation.z" doc))
;;             (assert-single (cl-mongo:get-element "transforms.transform.rotation.w" doc))))))))

;; (defun tf-mongo-query (start-time end-time frame child-frame)
;;   (cl-mongo:db.find "tf"
;;                     (cl-mongo:$em "transforms"
;;                                   (cl-mongo:kv
;;                                    ;; frames
;;                                    (cl-mongo:kv
;;                                     (cl-mongo:kv "child_frame_id" child-frame)
;;                                     (cl-mongo:kv "header.frame_id" frame))
;;                                    ;; time interval
;;                                    (cl-mongo:kv
;;                                     (cl-mongo:$> "header.stamp" start-time)
;;                                     (cl-mongo:$<= "header.stamp" end-time))))
;;                     :limit 0))

;; need the robot state: arms, base, head?
;; (defun robot-state-between (start end)
;;   (let ((start-loc (local->bson-time (owl-time->local start)))
;;         (end-loc (local->bson-time (owl-time->local end))))
;;     (tf-mongo-query start-loc end-loc "/map" "/odom_combined")
;;     ))




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

(in-package :ktq)

;; assign create parameters for relational actions from designators via pattern matching
;; TODO: convert to real designator and use CPL's predicates
;; TODO: add timestamps to actions?
;; TODO: knowrob:putdownlocation for putdown actions
(crs:def-fact-group desig->predicates (mongo-desig-prop extract-relational arm-movement-details)
  (crs:<- (mongo-desig-prop ?d (?prop ?val))
    (crs:lisp-pred listp ?d)
    (crs:lisp-fun assoc ?prop ?d ?pair)
    (crs:lisp-fun cdr ?pair ?pair-car )
    (crs:equal ?pair-car ?val))

  ;; TODO: sometimes multiple arm movements
  (crs:<- (arm-movement-details ?popmid ?details)
    (crs:lisp-fun arm-movements-for-popm ?popmid ?movementids)
    (crs:member ?mid ?movementids)
    (crs:lisp-fun linked-designator ?mid #"knowrob:voluntaryMovementDetails" ?details-id)
    (crs:lisp-fun mongo-get-designator ?details-id ?details))

  (crs:<- (arm-used ?popmid ?arm)
    (crs:findall ?details (arm-movement-details ?popmid ?details) ?all-details-lazy)
    (crs:lisp-fun cut:force-ll ?all-details-lazy ?all-details)
    (crs:lisp-fun length ?all-details ?len)

    (crs:or
     (crs:-> (crs:> ?len 1)
             (crs:equal ?arm both))
     (crs:-> (crs:< ?len 1)
             (crs:equal ?arm none))
     (crs:-> (crs:and (crs:== ?len 1)
                      (crs:lisp-fun car ?all-details ?d1)
                      (mongo-desig-prop ?d1 (PLANNING-GROUP "left_arm")))
             (crs:equal ?arm left))
          (crs:-> (crs:and (crs:== ?len 1)
                      (crs:lisp-fun car ?all-details ?d1)
                      (mongo-desig-prop ?d1 (PLANNING-GROUP "right_arm")))
             (crs:equal ?arm right))))

  (crs:<- (extract-at-object ?obj ?direction ?object-parameter)
    (crs:or
     (crs:-> (crs:and
              (mongo-desig-prop ?obj (name ?nm))
              (crs:lisp-pred identity ?nm))
             (crs:and
              (crs:equal ?object-parameter ?nm)
              (crs:equal ?direction center)))
     (crs:-> (crs:and
              (mongo-desig-prop ?obj (at ?where))
              (mongo-desig-prop ?where (on ?on-obj))
              (mongo-desig-prop ?where (name ?on-obj-name-str1))
              (crs:lisp-fun string-upcase ?on-obj-name-str1 ?on-obj-name-str)
              (crs:lisp-pred identity ?on-obj-name-str)
              (crs:lisp-fun intern ?on-obj-name-str ?on-obj-name))
             (crs:and
              (crs:equal ?object-parameter ?on-obj-name)
              (crs:equal ?direction on))))
    )

  ;; extract parameters for navigation
  (crs:<- (extract-relational ?mongo-desig ?popm-id ?action)
    (mongo-desig-prop ?mongo-desig (type "NAVIGATION"))
    (mongo-desig-prop ?mongo-desig (goal ?goal))
    ;; for "to see" navs
    (mongo-desig-prop ?goal (to "SEE"))
    ;; for objects
    (mongo-desig-prop ?goal (obj ?obj))

    (extract-at-object ?obj ?dir ?object-parameter)

    (crs:equal ?action (navigate-to-see ?dir
                        ?object-parameter)))

  (crs:<- (extract-relational ?mongo-desig ?popm-id ?action)
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
    (crs:equal ?action (navigate-to-reach ?nm)))

  ;; navigation with only cartesian goals
  ;; (crs:<- (extract-relational ?mongo-desig ?popm-id ?action)
  ;;   (mongo-desig-prop ?mongo-desig (type "NAVIGATION"))
  ;;   (mongo-desig-prop ?mongo-desig (goal ?goal))

  ;;   ;; (mongo-desig-prop ?goal (pose ?pose))
  ;;   ;; (mongo-desig-prop ?pose (pose ?p2))
  ;;   ;; (mongo-desig-prop ?p2 (position ?position))
  ;;   ;; (mongo-desig-prop ?position (x ?x))
  ;;   ;; (mongo-desig-prop ?position (y ?x))
  ;;   ;; (mongo-desig-prop ?position (z ?x))

  ;;   ;; (crs:lisp-pred identity ?x)
  ;;   ;; (crs:lisp-pred identity ?y)
  ;;   ;; (crs:lisp-pred identity ?z)

  ;;   (crs:format "~a~%" ?goal)
  ;;   ;; TODO: orientation
  ;;   (crs:equal ?action (navigate somewhere
  ;;                       ;; (?x ?y ?z)
  ;;                                )))

  ;; follow trajectory
;;   (crs:<- (extract-relational ?mongo-desig ?action)
;;     (mongo-desig-prop ?mongo-desig (type "TRAJECTORY"))
;;     (mongo-desig-prop ?mongo-desig (to "FOLLOW"))
;;     (crs:equal ?action nil)
;;     ;; problem: only fixed poses in follow-trajectory?
;;     ;; how to get to the object?
;; ;;    (mongo-desig-prop ?mongo-desig ())
;;     )

  (crs:<- (extract-relational ?mongo-desig ?popm-id ?action)
    (mongo-desig-prop ?mongo-desig (type "TRAJECTORY"))
    (mongo-desig-prop ?mongo-desig (to "PUT-DOWN"))
    (mongo-desig-prop ?mongo-desig (obj ?obj))
    (mongo-desig-prop ?obj (name ?obj-name))

    (arm-used ?popm-id ?arm)
    (crs:equal ?action (put-down ?obj-name ?arm)))

  (crs:<- (extract-relational ?mongo-desig ?popm-id ?action)
    (mongo-desig-prop ?mongo-desig (type "TRAJECTORY"))
    (mongo-desig-prop ?mongo-desig (to "LIFT"))
    (mongo-desig-prop ?mongo-desig (obj ?obj))
    (mongo-desig-prop ?obj (name ?obj-name))
    (arm-used ?popm-id ?arm)
    (crs:equal ?action (lift ?obj-name ?arm)))

  (crs:<- (extract-relational ?mongo-desig ?popm-id ?action)
    (mongo-desig-prop ?mongo-desig (type "TRAJECTORY"))
    (mongo-desig-prop ?mongo-desig (to "GRASP"))
    (mongo-desig-prop ?mongo-desig (obj ?obj))
    (mongo-desig-prop ?obj (name ?obj-name))
    (arm-used ?popm-id ?arm)
    (crs:equal ?action (grasp ?obj-name ?arm)))

  ;; FIXME: is always :none
  (crs:<- (extract-relational ?mongo-desig ?popm-id ?action)
    (mongo-desig-prop ?mongo-desig (type "TRAJECTORY"))
    (mongo-desig-prop ?mongo-desig (to "PARK"))
    (arm-used ?popm-id ?arm)
    (crs:equal ?action (park-arm ?arm)))

  (crs:<- (extract-relational ?mongo-desig ?popm-id ?action)
    (mongo-desig-prop ?mongo-desig (type "TRAJECTORY"))
    (mongo-desig-prop ?mongo-desig (to "CARRY"))
    (mongo-desig-prop ?mongo-desig (obj ?obj))
    (mongo-desig-prop ?obj (name ?obj-name))
    (arm-used ?popm-id ?arm)
    (crs:equal ?action (carry-object ?obj-name ?arm)))

  (crs:<- (extract-relational ?mongo-desig ?popm-id ?action)
    (mongo-desig-prop ?mongo-desig (to "PERCEIVE"))
    (mongo-desig-prop ?mongo-desig (obj ?obj))

    (mongo-desig-prop ?obj (name ?obj-name))
    ;; FIXME: what if no object name?
    (crs:lisp-pred identity ?obj-name)
    (crs:equal ?action (perceive ?obj-name)))

  ;; FIXME: actions without object names
  )

;; grasp: "http://ias.cs.tum.edu/kb/cram_log.owl#PerformOnProcessModule_6tqgTSvV"

(defun owl-desig->relational (popm-id)
  "Convert a action designator owl id into a relational action."
  (car (mapcar #'(lambda (bdg)
               (cut:with-vars-strictly-bound (?action) bdg
                 ;; HACK: convert object ids to symbols
                 (mapcar (lambda (x) (if (stringp x)
                                         (intern x)
                                         x))
                         ?action)))
           (cut:force-ll
            (crs:prolog `(extract-relational
                          ,(mongo-get-designator (get-action-designator-for-perform-pm popm-id))
                          ,popm-id
                          ?action))))))

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

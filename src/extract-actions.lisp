(in-package :ktq)

(defun linked-designator (task-id owl-predicate)
  (assert-single-recursive
   (re-pl-utils:owl-has-query :subject task-id :predicate owl-predicate)))

(defun arm-movements-for-popm (owlid)
  (assert-max-single
   (cut:force-ll (re-pl-utils:pl-query (?l)
                     `("setof" ?t (and ("subtask_all" ,owlid ?t)
                                       ("owl_individual_of" ?t #"knowrob:ArmMovement"))
                               ?l)
                   (re-pl-utils:pl-tree->string ?l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
              (crs:equal ?direction on)))))

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

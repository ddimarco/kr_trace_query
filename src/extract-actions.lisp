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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (crs:<- (create-pose-object ?desig ?object-name ?timestamp ?object-assertions)
    (crs:lisp-fun gensym "LOCATION" ?object-name)
    (crs:format "creating new location object~%")
    (mongo-desig-has-quantitative-at ?object-name ?timestamp ?desig ?object-loc-assertions)
    (crs:lisp-fun append ((location ?object-name)) ?object-loc-assertions ?object-assertions))

  (crs:<- (object-name ?desig ?name ?timestamp ?additional-assertions)
    ;; use the referenced object name in the designator
    (mongo-desig-prop ?desig (obj ?obj))
    (mongo-desig-prop ?obj (name ?name))
    (crs:lisp-pred identity ?name)
    (crs:equal ?additional-assertions nil))

  ;; (crs:<- (object-name ?desig ?name ?timestamp ?additional-assertions)
  ;;   ;; create a new location designator
  ;;   (crs:and (mongo-desig-prop ?desig (obj ?obj)) (crs:lisp-pred null ?obj))
  ;;   (create-pose-object ?desig ?name ?timestamp ?additional-assertions))

    ;; TODO: also for object types?
  ;; (crs:<- (object-name ?desig ?name ?additional-assertions)
  ;;   )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (crs:<- (->symbol ?str ?sym)
    (crs:lisp-fun string-upcase ?str ?str-upper)
    (crs:lisp-fun intern ?str-upper ?sym))

  (crs:<- (extract-at-object ?obj ?direction ?object-parameter ?additional)
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
              (mongo-desig-prop ?where (name ?on-obj-name-str))
              (crs:lisp-pred identity ?on-obj-name-str)
              (->symbol ?on-obj-name-str ?obj-name))
             (crs:and
              (crs:equal ?direction on)
              (crs:equal ?object-parameter ?obj-name))))
    ;; no additional assertions
    (crs:equal ?additional nil))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (crs:<- (extract-relational ?mongo-desig ?popm-id  ?timestamp ?action ?additional)
    (mongo-desig-prop ?mongo-desig (to "PERCEIVE"))
    (mongo-desig-prop ?mongo-desig (obj ?obj))

    (extract-at-object ?obj ?dir ?obj-parameter ?additional)

    (crs:equal ?action (perceive ?dir ?obj-parameter)))

  ;; extract parameters for navigation
  (crs:<- (extract-relational ?mongo-desig ?popm-id  ?timestamp ?action ?additional)
    (mongo-desig-prop ?mongo-desig (type "NAVIGATION"))
    (mongo-desig-prop ?mongo-desig (goal ?goal))
    (mongo-desig-prop ?goal (to "SEE"))
    (mongo-desig-prop ?goal (obj ?obj))
    (extract-at-object ?obj ?dir ?object-parameter ?additional)
    (crs:equal ?action (navigate-to-see ?dir ?object-parameter)))

  (crs:<- (extract-relational ?mongo-desig ?popm-id ?timestamp ?action ?additional)
    (mongo-desig-prop ?mongo-desig (type "NAVIGATION"))
    (mongo-desig-prop ?mongo-desig (goal ?goal))
    (mongo-desig-prop ?goal (to "REACH"))
    (mongo-desig-prop ?goal (obj ?obj))
    (extract-at-object ?obj ?dir ?object-parameter ?additional)
    (crs:equal ?action (navigate-to-reach ?dir ?object-parameter)))

  ;; navigation with only cartesian goals
  (crs:<- (extract-relational ?mongo-desig ?popm-id ?timestamp ?action ?additional)
    (mongo-desig-prop ?mongo-desig (type "NAVIGATION"))
    (mongo-desig-prop ?mongo-desig (goal ?goal))
    (crs:and (mongo-desig-prop ?goal (to ?to)) (crs:lisp-pred null ?to))
    (object-name ?goal ?goal-name ?timestamp ?additional)
    (crs:equal ?action (navigate ?goal-name)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (crs:<- (split-manipulation-action ?arm-used ?popm-id ?results)
  ;;   (crs:or (crs:equal ?arm-used left) (crs:equal ?arm-used right)
  ;;           (crs:equal ?arm-used none)))
  ;; (crs:<- (split-manipulation-action ?arm-used ?popm-id ?results)
  ;;   (crs:equal ?arm-used both)
  ;;   (crs:format "both arms used in ~a...~%" ?popm-id))

   ;; follow trajectory: basically "move-head"
  (crs:<- (extract-relational ?mongo-desig ?popm-id ?timestamp ?action ?additional)
    (mongo-desig-prop ?mongo-desig (type "TRAJECTORY"))
    (mongo-desig-prop ?mongo-desig (to "FOLLOW"))
    ;; (crs:format "~a~%" ?mongo-desig)
    (object-name ?mongo-desig ?name ?timestamp ?additional)
    (crs:equal (follow-trajectory ?name) ?action))

  (crs:<- (extract-relational ?mongo-desig ?popm-id ?timestamp ?action ?additional)
    (mongo-desig-prop ?mongo-desig (type "TRAJECTORY"))
    (mongo-desig-prop ?mongo-desig (to "PUT-DOWN"))
    (mongo-desig-prop ?mongo-desig (obj ?obj))
    (mongo-desig-prop ?obj (name ?obj-name))
    (crs:equal ?additional nil)
    (arm-used ?popm-id ?arm)
    ;;(split-manipulation-action ?arm-used ?popm-id ?additional)
    (crs:equal ?action (put-down ?obj-name ;; ?arm
                                 )))

  (crs:<- (extract-relational ?mongo-desig ?popm-id ?timestamp ?action ?additional)
    (mongo-desig-prop ?mongo-desig (type "TRAJECTORY"))
    (mongo-desig-prop ?mongo-desig (to "LIFT"))
    (mongo-desig-prop ?mongo-desig (obj ?obj))
    (mongo-desig-prop ?obj (name ?obj-name))
    (arm-used ?popm-id ?arm)
    ;; (split-manipulation-action ?arm-used ?popm-id ?additional)
    (crs:equal ?additional nil)
    (crs:equal ?action (lift ?obj-name ;; ?arm
                             )))

  (crs:<- (extract-relational ?mongo-desig ?popm-id ?timestamp ?action ?additional)
    (mongo-desig-prop ?mongo-desig (type "TRAJECTORY"))
    (mongo-desig-prop ?mongo-desig (to "GRASP"))
    (mongo-desig-prop ?mongo-desig (obj ?obj))
    (mongo-desig-prop ?obj (name ?obj-name))
    (arm-used ?popm-id ?arm)
    (crs:equal ?additional nil)
    (crs:equal ?action (grasp ?obj-name ;; ?arm
                              )))

  ;; FIXME: arm is always :none
  ;; always null duration
  ;; (crs:<- (extract-relational ?mongo-desig ?popm-id ?timestamp ?action ?additional)
  ;;   (mongo-desig-prop ?mongo-desig (type "TRAJECTORY"))
  ;;   (mongo-desig-prop ?mongo-desig (to "PARK"))
  ;;   ;; (arm-used ?popm-id ?arm)
  ;;   (crs:equal ?additional nil)
  ;;   (crs:equal ?action (park-arms pr2)))

  (crs:<- (extract-relational ?mongo-desig ?popm-id ?timestamp ?action ?additional)
    (mongo-desig-prop ?mongo-desig (type "TRAJECTORY"))
    (mongo-desig-prop ?mongo-desig (to "CARRY"))
    (mongo-desig-prop ?mongo-desig (obj ?obj))
    (mongo-desig-prop ?obj (name ?obj-name))
    (arm-used ?popm-id ?arm)
    (crs:equal ?additional nil)
    (crs:equal ?action (carry-object ?obj-name ;; ?arm
                                     )))

  ;; FIXME: actions without object names:
  ;; (defparameter *unusable-actions*
  ;;      (loop for action in (get-all-actions)
  ;;         for rel = (owl-desig->relational action)
  ;;         when (null rel)
  ;;           collect action))
  )

;; grasp: "http://ias.cs.tum.edu/kb/cram_log.owl#PerformOnProcessModule_6tqgTSvV"
(defun owl-desig->relational (popm-id timestamp)
  "Convert a action designator owl id into a relational action."
  (let ((bdg (assert-max-single
              (cut:force-ll
               (crs:prolog
                `(extract-relational
                  ,(mongo-get-designator
                    (get-action-designator-for-perform-pm popm-id))
                  ,popm-id
                  ,timestamp
                  ?action
                  ?additional))))))
    (if bdg
        (cut:with-vars-bound (?action ?additional)
            bdg
          (values (mapcar (lambda (id) (if (stringp id)
                                           (intern id)
                                           id))
                          ?action)
                  ?additional))
        (values nil nil))))

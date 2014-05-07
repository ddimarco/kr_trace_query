(in-package :ktq)

;; q: task(T), task_goal(T, '(OBJECT-PLACED-AT ?OBJ ?LOC)'), task_start(T, S), task_end(T, E), add_trajectory('/base_link', S, E, 1.0).
;; text: PR2's path during a pick-and-place sequence

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lookup-mongo-transform (from to owl-time)
  (if (string= from to)
      (cl-transforms:make-identity-transform)
      (let ((poses (cut:force-ll
                    (re-pl-utils:pl-query (?l)
                        `("mng_lookup_transform" ,from ,to ,owl-time ?l)
                      (cl-transforms:pose->transform
                       (re-pl-utils:pl-matrix->pose ?l))))))
        (assert-single poses))))

;; FIXME: odom_combined <-> base_link
(defun robot-pose (owl-time)
  (cl-transforms:transform->pose
   (lookup-mongo-transform "/map"
                           ;; "/odom_combined"
                           "/base_link"
                           owl-time)))

(defun robot-gripper-pose (side owl-time)
  (cl-transforms:transform->pose
   (lookup-mongo-transform "/map"
                           (ecase side
                             (:left "/l_gripper_palm_link")
                             (:right "/r_gripper_palm_link"))
                           owl-time)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pick-and-place-path ()
  (json-prolog:prolog '(and
                        ("task" ?t)
                        ("task_goal" ?t "(OBJECT-PLACED-AT ?OBJ ?LOC)")
                        ("task_start" ?t ?s)
                        ("task_end" ?t ?e))))

;; (defun all-objects ()
;;   (json-prolog:prolog-simple "findall([Obj, E],((task_end(T,E), task_class(T,knowrob:'UIMAPerception'), rdf_has(T, knowrob:'perceptionResult', Obj))),_Objs),!, member([O, E], _Objs), get_belief_by_designator(O, L)"))

(defun all-objects-in-interval (timesteps)
  ;; get all perception designators
  (re-pl-utils:pl-query (;; ?t
                         ?od ;; ?ts
                             )
      `(and ("task_class" ?t ,#"knowrob:UIMAPerception")
            ("rdf_has" ?t ,#"knowrob:perceptionResult" ?od)
            ("task_start" ?t ?ts)
            ("member" ?ts ',timesteps)
            ("task_end" ?t ?te)
            ("member" ?te ',timesteps))
    (re-pl-utils:pl-tree->string ?od)))

(defun task-interval (task-id)
  (assert-single
   (cut:force-ll
    (re-pl-utils:pl-query (?s ?e)
        `(and ("task_start" ,task-id ?s)
              ("task_end" ,task-id ?e))
      (cons (re-pl-utils:pl-tree->string ?s)
            (re-pl-utils:pl-tree->string ?e))))))

(defun timesteps-between (start end)
  (let ((timesteps (sort (all-time-steps) #'string<))
        (int-start (timepoint-id->time start))
        (int-end (timepoint-id->time end)))
    (loop for ts in timesteps
       for int-ts = (timepoint-id->time ts)
         when (and (>= int-ts int-start)
                   (<= int-ts int-end))
         collect ts)))

(defun timesteps-in-task (task-id)
  (destructuring-bind (start . end)
      (task-interval task-id)
    (timesteps-between start end)))

(defun plot-robot-poses (&optional (timesteps (all-time-steps)))
  (with-open-file (plot-file "/tmp/robot-poses.dat" :direction :output :if-exists :supersede)
    (loop for ts in timesteps
       for pose = (robot-pose ts)
       for pos = (if pose
                     (cl-transforms:origin pose)
                     nil)
       when pos do
         (format plot-file "~a ~a ~a~%" (cl-transforms:x pos)
                 (cl-transforms:y pos)
                 (shorten-uri ts)))))

(defun all-sem-objects-in-room (room-id)
  (alexandria:flatten
   (cut:force-ll (re-pl-utils:pl-query (?o) `("map_root_objects" ,room-id ?o)
                   (re-pl-utils:pl-tree->string ?o)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun above-of (p1 p2)
  (with-slots ((o1 cl-transforms:origin)) p1
    (with-slots ((o2 cl-transforms:origin)) p2
      (- (cl-transforms:z o1) (cl-transforms:z o2)))))

(defun below-of (p1 p2)
  (with-slots ((o1 cl-transforms:origin)) p1
    (with-slots ((o2 cl-transforms:origin)) p2
      (- (cl-transforms:z o2) (cl-transforms:z o1)))))

(defun left-of (p1 p2)
  (with-slots ((o1 cl-transforms:origin)) p1
    (with-slots ((o2 cl-transforms:origin)) p2
      (- (cl-transforms:y o2) (cl-transforms:y o1)))))

(defun right-of (p1 p2)
  (with-slots ((o1 cl-transforms:origin)) p1
    (with-slots ((o2 cl-transforms:origin)) p2
      (- (cl-transforms:y o1) (cl-transforms:y o2)))))

(defun get-minimal-distance-relation (pose map-parts)
  ;; assuming everything in map frame
  (let ((rel-distances
         (loop for mp in map-parts
            for mp-pose = (cl-semantic-map-utils:pose mp)
            for left = (left-of pose mp-pose)
            for above = (above-of pose mp-pose)
            collect
              (list
               (if (> left 0)
                   (cons :left left)
                   (cons :right (- left)))
               (if (> above 0)
                   (cons :above above)
                   (cons :below (- above)))
               mp))))
    ;; just take the one with the minimal distance
    (loop for ((lr-dir . lr-dist) (tb-dir . tb-dist) mp) in rel-distances
       for dist = (cl-transforms:v-dist (cl-transforms:origin pose)
                                        (cl-transforms:origin (cl-semantic-map-utils:pose mp)))
       with min-lr = (list :left 1000 nil)
       with min-tb = (list :below 1000 nil)
       for i from 0
       do
         (when (< dist (cadr min-lr))
           (setf (car min-lr) lr-dir
                 (cadr min-lr) dist
                 (caddr min-lr) mp))
         (when (< dist (cadr min-tb))
           (setf (car min-tb) tb-dir
                 (cadr min-tb) dist
                 (caddr min-tb) mp))
       finally (return (list min-lr min-tb)))))

;; TODO: would probably be better to use a viewpoint from the robot instead of from the map origin
(defun discretize-pose (pose semantic-map &key name)
  (destructuring-bind ((lr-dir lr-dist lr-obj) (tb-dir tb-dist tb-obj))
      (get-minimal-distance-relation pose (cl-semantic-map-utils:semantic-map-parts semantic-map))
    (list
     (remove nil
             (list
              (if (eq :left lr-dir)
                  'left-of
                  'right-of)
              (if name
                  name)
              (->relational-id lr-obj)))

     (remove nil
             (list
              (if (eq :above tb-dir)
                  'above-of
                  'below-of)
              (if name
                  name)
              (->relational-id tb-obj))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *relational-id->obj* (make-hash-table))
(defgeneric ->relational-id (object))

(defmethod ->relational-id :around (object)
  (let ((resulting-id (call-next-method object)))
    (setf (gethash resulting-id *relational-id->obj*) object)
    resulting-id))

(defmethod ->relational-id ((object cl-semantic-map-utils:semantic-map-geom))
  (intern (string-upcase (cl-semantic-map-utils:name object)) *package*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: robot-at, gripper-at predicates, parking positions
(defun robot-world-state-at (owl-time semantic-map)
  (let ((rpose (discretize-pose (robot-pose owl-time) semantic-map :name 'pr2))
        (lgripper (discretize-pose (robot-gripper-pose :left owl-time) semantic-map :name 'lgripper))
        (rgripper (discretize-pose (robot-gripper-pose :right owl-time) semantic-map :name 'rgripper)))
    (append
     '((robot pr2)
       (gripper-left lgripper)
       (gripper-right rgripper))
     rpose lgripper rgripper)))

;; (unique-objects (cut:force-ll (all-objects-in-interval (timesteps-in-task "http://ias.cs.tum.edu/kb/cram_log.owl#CRAMAction_xPY9fAsI"))))
(defun unique-objects (desig-id-lst &key (prop 'name))
  "takes a list of object designator owl ids, removes duplicates (i.e. where prop is the same)."
  (remove-duplicates desig-id-lst
                     :test (lambda (a b)
                             (let ((m1 (mongo-get-designator a))
                                   (m2 (mongo-get-designator b)))
                               (let ((n1 (cdr (assoc prop m1)))
                                     (n2 (cdr (assoc prop m2))))
                                 (equal n1 n2))))))

(defclass manip-object ()
  ;; TODO: handles etc?
  ((name :initarg :name)
   (type :initarg :type)
   ;; (shape :initarg shape)
   ;; (color :initarg color)
   ))

(defun make-object-from-owlid (desig-id)
  (let ((mng (mongo-get-designator desig-id)))
    (make-instance 'manip-object
                   :name (cdr (assoc 'name mng))
                   :type (cdr (assoc 'type mng)))))

(defun unique-manip-objects-from-interval (timestamps)
  (mapcar #'make-object-from-owlid
          (unique-objects (cut:force-ll (all-objects-in-interval timestamps)))))

;; TODO: how to identify object designators? -> for now, use name (should use the designator chain)
;; TODO: when to use pose from perceptions vs tf?
;; TODO: equated designators -> same object

(defun perception-desigs-of-object-type (type)
  (json-prolog:prolog `(and ("task_class" ?t ,#"knowrob:UIMAPerception")
                            ("rdf_has" ?t ,#"knowrob:perceptionResult" ?od)
                            ("mng_designator_props" ?od "TYPE" ,type))))

(defun mng-latest-obj-name-perception (name before &key after)
  (car (mapcar #'lispify-mongo-doc
           (cl-mongo:docs
            (cl-mongo:iter
             (cl-mongo:db.sort "logged_designators"
                               (cl-mongo:kv
                                (cl-mongo:kv
                                 (cl-mongo:kv "designator._designator_type" "object")
                                 (cl-mongo:kv "designator.NAME" name))
                                (cl-mongo:kv
                                 (cl-mongo:$<= "__recorded" (ensure-bson-time before))
                                 (cl-mongo:$>= "__recorded" (if after
                                                                (ensure-bson-time after)
                                                                (cl-mongo:date-time 0 0 0 1 1 1900)))))
                               :field "__recorded"
                               :asc nil
                               :limit 0))))))

(defun mng-extract-at (mng-desig)
  "extracts the pose from a designator retrieved from mongodb."
  (assert-single
   (mapcar (lambda (bdg)
             (cut:with-vars-bound (?spec) bdg
               ?spec))
           (cut:force-ll (crs:prolog `(mongo-desig-extract-at ,mng-desig ?spec))))))

(crs:def-fact-group mng-desig->qualitative-pos (mongo-desig-extract-at mongo-desig-pose)
  (crs:<- (mongo-desig-extract-at ?desig ?res)
    (mongo-desig-prop ?desig (designator ?d))
    (mongo-desig-prop ?d (at ?spec))
    (mongo-desig-prop ?spec (_designator_type "LOCATION"))
    (mongo-desig-prop ?d (name ?obj-name-str))
    (crs:lisp-fun intern ?obj-name-str ?obj-name)

    (crs:or
     (crs:-> (mongo-desig-has-qualitative-at ?obj-name ?spec ?res1)
             (crs:equal ?res1 ?res))

     (crs:-> (mongo-desig-has-quantitative-at ?obj-name ?spec ?res2)
             (crs:equal ?res2 ?res))))

  (crs:<- (mongo-desig-has-qualitative-at ?obj-name ?desig ?res)
    (mongo-desig-prop ?desig (on ?sth))
    (mongo-desig-prop ?desig (name ?nme))
    (crs:lisp-pred identity ?nme)
    (crs:lisp-pred identity ?sth)
    (crs:equal ?res (on ?obj-name ?nme)))

  (crs:<- (mongo-desig-pose ?desig ?resulting-pose)
    (mongo-desig-prop ?desig (pose ?pose))
    (mongo-desig-prop ?pose (pose ?pose2))
    (mongo-desig-prop ?pose2 (position ?position))
    (crs:equal ?position ((x . ?x) (y . ?y) (z . ?z)))
    (mongo-desig-prop ?pose2 (orientation ?orientation))
    (crs:equal ?orientation ((x . ?ox) (y . ?oy) (z . ?oz) (w . ?ow)))

    (crs:lisp-fun cl-transforms:make-3d-vector ?x ?y ?z ?origin)
    (crs:lisp-fun cl-transforms:make-quaternion ?ox ?oy ?oz ?ow ?quaternion)
    (crs:lisp-fun cl-transforms:make-pose ?origin ?quaternion ?resulting-pose))

  (crs:<- (mongo-desig-has-quantitative-at ?obj-name ?desig ?res)
    (mongo-desig-pose ?desig ?obj-pose)

    (crs:lisp-fun cl-semantic-map-utils:get-semantic-map ?semmap)
    (crs:lisp-fun discretize-pose ?obj-pose ?semmap :name ?obj-name ?res))

  ;; to extract a pose from an object designator
  ;; TODO: unify with above stuff
  (crs:<- (mongo-obj-desig->pose ?desig ?pose)
    (mongo-desig-prop ?desig (designator ?d))
    (mongo-desig-prop ?d (at ?spec))
    (mongo-desig-prop ?spec (_designator_type "LOCATION"))
    (mongo-desig-pose ?spec ?pose)))

(defun mongo-obj-desig->pose (mng-desig)
  (when mng-desig
    (cut:with-vars-bound (?p)
        (assert-single
         (cut:force-ll (crs:prolog `(mongo-obj-desig->pose ,mng-desig ?p))))
      ?p)))

(defun pose-reachable-p (side pose owl-time)
  "`pose' has to be in the /map frame."
  (let (;; (map->bl (lookup-mongo-transform  "/map"
        ;;                                    ;; "/torso_lift_link"
        ;;                                    "/base_link"
        ;;                                    owl-time))
        ;; (bl->tll (lookup-mongo-transform "/base_link" "/torso_lift_link" owl-time))
        (map->tll (lookup-mongo-transform  "/torso_lift_link" "/map" owl-time))
        )
    ;; let ((poses (loop for i below 8
    ;;                for rotation = (cl-transforms:yaw (* i (/ (* 2 pi) 8)))
    ;;                collect
    ;;                  (cl-transforms:copy-pose
    ;;                   ;;                       cl-transforms:transform-pose bl->tll
    ;;                   (cl-transforms:transform-pose
    ;;                    map->bl
    ;;                    (cl-transforms:copy-pose pose :orientation rotation))

    ;;                   ;; :orientation rotation
    ;;                   ))))
    ;; (format t "~a~%" poses)

    ;; (roslisp:publish
    ;;  *vis-publisher*
    ;;  (make-marker-array
    ;;   (loop for p in poses
    ;;        for id from 420
    ;;        collect
    ;;        (make-marker p id '(1 0 0) :arrow))))

    ;; (loop for p in poses
    ;;      for r = (pr2-reachability-costmap:pose-reachability
    ;;               (pr2-reachability-costmap:get-reachability-map side)
    ;;               p)
    ;;    do
    ;;      (format t "reachability: ~a~%" p)
    ;;      )
    (pr2-reachability-costmap:pose-reachability
     (pr2-reachability-costmap:get-reachability-map side)

     (cl-transforms:transform-pose map->tll pose)
     )

    ;;(cl-transforms:transform-pose map->tll pose)


    ;; (some (lambda (pose)
    ;;         (pr2-reachability-costmap:pose-reachable-p
    ;;          (pr2-reachability-costmap:get-reachability-map side)
    ;;          pose
    ;;          ;; TODO: grasp orientation/offset?
    ;;          :use-closest-orientation t))
    ;;       poses)


    ;; (pr2-reachability-costmap:pose-reachable-p
    ;;  (pr2-reachability-costmap:get-reachability-map side)

    ;;  ;; TODO: grasp orientation/offset?
    ;;  ;; (cl-transforms:copy-pose
    ;;  ;;  (cl-transforms:transform-pose map->tll pose)
    ;;  ;;  :orientation (cl-transforms:make-identity-rotation))
    ;;  :use-closest-orientation t)
    ))

;; FIXME: timestamp is "before" -> problem with outcomes?
(defun manip-object-world-state (timestamp unique-manip-obj &key after)
  (mng-extract-at (mng-latest-obj-name-perception (slot-value unique-manip-obj 'name) timestamp
                                                  :after after)))

(defun all-manip-objects-world-state (timestamp unique-objects &key after)
  (append
   ;; type declarations
   (loop for obj in unique-objects
      collect (list (intern (slot-value obj 'type))
                    (intern (slot-value obj 'name))))
   ;; TODO: use only relevant objects
   (loop for semmap-obj in (cl-semantic-map-utils:semantic-map-parts
                            (cl-semantic-map-utils:get-semantic-map))
      for name = (cl-semantic-map-utils:name semmap-obj)
      for type = (cl-semantic-map-utils:obj-type semmap-obj)
      collect (list (intern (string-upcase type))
                    (intern (string-upcase name))))
   ;; relational positions
   (loop for obj in unique-objects
      append
        (manip-object-world-state timestamp obj :after after))
   ;; reachability of unique-objects
   ;; grasp spatula0:
   ;; "http://ias.cs.tum.edu/kb/cram_log.owl#PerformOnProcessModule_1IVtZaIv"
   (loop for obj in unique-objects
      for obj-name = (slot-value obj 'name)
      for obj-pose = (mongo-obj-desig->pose
                      (mng-latest-obj-name-perception obj-name timestamp))
      for in-reach-left = (if obj-pose
                              (pose-reachable-p :left obj-pose timestamp))
      for in-reach-right = (if obj-pose
                               (pose-reachable-p :right obj-pose timestamp))
        do
        (format t "~a: ~a~%" obj-name in-reach-left)
      ;;   when in-reach-left
      ;; append (list 'reachable-left obj-name)

        )
   ))


;; (all-manip-objects-world-state "http://ias.cs.tum.edu/kb/cram_log.owl#timepoint_1396513242" (unique-manip-objects-from-interval (timesteps-in-task "http://ias.cs.tum.edu/kb/cram_log.owl#CRAMAction_xPY9fAsI")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: should be possible to use all
(defun usable-actions ()
  "only keep actions which can be converted to a relational form"
  (remove-if (lambda (actionid)
               (not (identity
                 (owl-desig->relational (get-action-designator-for-perform-pm actionid)))))
             (get-all-actions)))

(defun world-state-before-action (popm-id unique-manip-objects)
  (destructuring-bind (start . end)
      (task-interval popm-id)
    (append
     (all-manip-objects-world-state start unique-manip-objects)
     (robot-world-state-at start (cl-semantic-map-utils:get-semantic-map)))))

(defun world-state-after-action (popm-id unique-manip-objects)
  (destructuring-bind (start . end)
      (task-interval popm-id)
    (append
     ;; TODO: timestamp +1?
     (all-manip-objects-world-state end unique-manip-objects)
     (robot-world-state-at end (cl-semantic-map-utils:get-semantic-map)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: add reachable predicate -> pr2_reachability_costmap


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; q:
;; findall([Obj, E],
;;  ((task_end(T,E), task_class(T,knowrob:'UIMAPerception'), rdf_has(T, knowrob:'perceptionResult', Obj))),
;;  Objs),!, member([Obj, E], Objs), belief_at(loc(Obj,Loc), E),
;;  add_object_as_semantic_instance(Obj, Loc, E, SemanticMapInstance),
;;  add_object_with_children(SemanticMapInstance).
;; text: Where were the objects perceived during the execution?

;; q:
;; findall(E,((task_end(T,E), task_goal(T,'PUTDOWN'))), Es),!, member(E, Es),
;;  mng_robot_pose_at_time(pr2:'PR2Robot1','/map',E,Pose),
;;  add_robot_as_basic_semantic_instance(Pose, E, SemanticMapInstance),
;; add_object_with_children(SemanticMapInstance),
;; highlight_object(SemanticMapInstance).
;; text:
;; What were the robot poses during all PUT-DOWN Actions?

;; q:
;; successful_instances_of_given_goal('PUTDOWN', Tasks),
;;  member(T, Tasks), task_end(T,End), !,
;;  mng_robot_pose_at_time(pr2:'PR2Robot1','/map',End,Pose),
;;  add_object_with_children(pr2:'PR2Robot1',End).
;; text:
;;  Where was the robot localized at the time of a successful putdown?

;; q:
;; task_start(T,S), task_end(T,E), task_goal(T,'PUTDOWN'),
;;  arm_used_for_manipulation(T, Link), add_trajectory(Link, S, E, 0.1).
;; text:
;; What is the trajectory of the arm during a PUT-DOWN Action?

;; q:
;; task_start(T,S), task_end(T,E),
;; task_goal(T,'REPLACEABLE-FUNCTION-DEMO-PART-POURING'), publish_designator(T),
;; add_trajectory('/r_wrist_roll_link', S, E, 0.1),
;; !, mng_robot_pose_at_time(pr2:'PR2Robot1','/map',S,Pose),
;; add_object_with_children(pr2:'PR2Robot1',S).
;; text:
;; What is the trajectory of the arm during the pouring Action?

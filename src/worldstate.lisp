(in-package :kr-trace-query)

(defparameter *use-moved-predicates* nil
  "Whether to include base-moved, grippers-moved, etc. predicates.
Useful for debugging, less for plannieng.")

(defparameter *use-relational-poses* nil
  "Whether to describe positions using directional relations, such as right-of, in-front-of, etc.")

;; poses for the wrist_roll_link frames to be considered "carrying"
(defparameter *carry-pose-left*
  (cl-transforms:make-pose
   (tf:make-3d-vector 0.3 0.5 1.3)
   (tf:euler->quaternion :ax 0)))
(defparameter *carry-pose-right*
  (cl-transforms:make-pose
   (tf:make-3d-vector 0.3 -0.5 1.3)
   (tf:euler->quaternion :ax 0)))

(defparameter *pose-epsilon* 0.1)

;; assumption: distance between left & right gripper tip is less than this value -> closed
(defparameter *gripper-open-distance* 0.1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to speed pose lookups up a little
(defparameter *transform-cache* (make-hash-table :test #'equal))

(defun lookup-mongo-transform (from to owl-time)
  (let ((key (list from to owl-time)))
    (when (null (gethash key *transform-cache*))
      (setf (gethash key *transform-cache*)
            (if (string= from to)
                (cl-transforms:make-identity-transform)
                (let ((poses (cut:force-ll
                              (re-pl-utils:pl-query (?l)
                                  `("mng_lookup_transform" ,from ,to ,owl-time ?l)
                                (cl-transforms:pose->transform
                                 (re-pl-utils:pl-matrix->pose ?l))))))
                  (assert-single poses)))))
    (gethash key *transform-cache*)))

(defun robot-pose (owl-time)
  (cl-transforms:transform->pose
   (lookup-mongo-transform "/map" "/base_link" owl-time)))

(defun robot-gripper-pose (side owl-time)
  (cl-transforms:transform->pose
   (lookup-mongo-transform "/map"
                           (ecase side
                             (:left "/l_gripper_palm_link")
                             (:right "/r_gripper_palm_link"))
                           owl-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun axis-relation (axis-function p1 p2)
  (with-slots ((o1 cl-transforms:origin)) p1
    (with-slots ((o2 cl-transforms:origin)) p2
      (- (funcall axis-function o2) (funcall axis-function o1)))))

(defun get-minimal-distance-relation (pose timestamp map-parts)
  ;; assuming everything in map frame
  (let ((pose (map->robot-frame pose timestamp)))
    (let ((rel-distances
           (loop for mp in map-parts
              for mp-pose = (map->robot-frame (cl-semantic-map-utils:pose mp) timestamp)
              for left = (axis-relation #'cl-transforms:y pose mp-pose)
              for below = (axis-relation #'cl-transforms:z pose mp-pose)
              for behind = (axis-relation #'cl-transforms:x pose mp-pose)
              collect
                (list
                 (if (> left 0)
                     (cons 'left-of left)
                     (cons 'right-of (- left)))
                 (if (> below 0)
                     (cons 'below-of below)
                     (cons 'above-of (- below)))
                 (if (> behind 0)
                     (cons 'behind-of behind)
                     (cons 'in-front-of (- behind)))
                 mp))))
      ;; just take the one with the minimal distance
      (loop for ((lr-dir . lr-dist) (tb-dir . tb-dist) (fb-dir . fb-dist) mp) in rel-distances
         for dist = (cl-transforms:v-dist (cl-transforms:origin pose)
                                          (cl-transforms:origin (cl-semantic-map-utils:pose mp)))
         with min-lr = (list :left 1000 nil)
         with min-tb = (list :below 1000 nil)
         with min-fb = (list :behind 1000 nil)
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
           (when (< dist (cadr min-fb))
             (setf (car min-fb) fb-dir
                   (cadr min-fb) dist
                   (caddr min-fb) mp))
         finally (return (list min-lr min-tb min-fb))))))

(defparameter *relational-id->obj* (make-hash-table))
(defgeneric ->relational-id (object))

(defmethod ->relational-id :around (object)
  (let ((resulting-id (call-next-method object)))
    (setf (gethash resulting-id *relational-id->obj*) object)
    resulting-id))

(defmethod ->relational-id ((object cl-semantic-map-utils:semantic-map-geom))
  (intern (string-upcase (cl-semantic-map-utils:name object)) *package*))

(defun discretize-pose (pose semantic-map time &key name)
  (flet ((make-relation (dir obj)
           (list dir name (->relational-id obj))))
    (if *use-relational-poses*
     (destructuring-bind ((lr-dir lr-dist lr-obj) (tb-dir tb-dist tb-obj) (fb-dir fb-dist fb-obj))
         (get-minimal-distance-relation pose time (cl-semantic-map-utils:semantic-map-parts semantic-map))
       (list
        (make-relation lr-dir lr-obj)
        (make-relation tb-dir tb-obj)
        (make-relation fb-dir fb-obj))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
          (unique-objects
           (mapcar #'car (cut:force-ll (all-objects-in-interval timestamps))))))

;; TODO: how to identify object designators? -> for now, use name (should use the designator chain)
;; TODO: when to use pose from perceptions vs tf?
;; TODO: equated designators -> same object

;; (defun perception-desigs-of-object-type (type)
;;   (json-prolog:prolog `(and ("task_class" ?t ,#"knowrob:UIMAPerception")
;;                             ("rdf_has" ?t ,#"knowrob:perceptionResult" ?od)
;;                             ("mng_designator_props" ?od "TYPE" ,type))))

;; FIXME: discrepancy between this one and mng-latest-designator
(defun all-objects-in-interval (timesteps)
  ;; get all perception designators
  (re-pl-utils:pl-query (?t ?od)
      `(and ("task_class" ?t ,#"knowrob:UIMAPerception")
            ("rdf_has" ?t ,#"knowrob:perceptionResult" ?od)
            ("task_start" ?t ?ts)
            ("member" ?ts ',timesteps)
            ("task_end" ?t ?te)
            ("member" ?te ',timesteps))
    (cons (re-pl-utils:pl-tree->string ?od)
          (re-pl-utils:prolog->string ?t))))

(defun mng-latest-obj-name-perception (name before experiment-trace)
  (let ((desigs
         ;; sort descending by end-time
         (sort
          (cut:force-ll
           (all-objects-in-interval (timesteps-between (start-time experiment-trace)
                                                       before)))
          #'string>
          :key (lambda (task-desig-pair)
                 (cdr (task-interval (cdr task-desig-pair)))))))
    (loop for (desig . task) in desigs
       for mng-desig = (mongo-get-designator desig)
       when (and mng-desig
                 (equal (cdr (assoc 'name mng-desig))
                        name))
        return mng-desig)))

(defun mng-extract-pose-relation (mng-desig owl-time)
  "extracts the pose from a designator retrieved from mongodb."
  (if mng-desig
      (assert-single
       (mapcar (lambda (bdg)
                 (cut:with-vars-bound (?spec) bdg
                   ?spec))
               (remove nil (cut:force-ll
                            (crs:prolog `(mongo-desig-pose-rel ,mng-desig ,owl-time ?spec))))))))

;; TODO: in-gripper!!!!
;; db.logged_designators.find({"designator._designator_type" : "object", "designator.NAME" : "PANCAKEMIX0", "designator.AT.IN" : "GRIPPER"}).pretty()
;; (defparameter *ids* (mapcar (lambda (doc)
;;                (cl-mongo:get-element :_id (cl-mongo:get-element "designator" doc)))
;;                (cl-mongo:docs (cl-mongo:db.find "logged_designators" (cl-mongo:kv "designator.AT.IN" "GRIPPER") :limit 0))))

(defun make-pose-helper (x y z qx qy qz qw)
  "helper to create a 6D pose in just one function call."
  (cl-transforms:make-pose
   (cl-transforms:make-3d-vector x y z)
   (cl-transforms:make-quaternion qx qy qz qw)))

(crs:def-fact-group mng-desig->qualitative-pos (mongo-desig-pose-rel mongo-desig-pose)
  (crs:<- (mongo-desig-pose-rel ?desig ?owl-time ?res)
    (mongo-desig-prop ?desig (at ?spec))
    (mongo-desig-prop ?spec (_designator_type "LOCATION"))
    (mongo-desig-prop ?desig (name ?obj-name-str))
    (crs:lisp-fun intern ?obj-name-str ?obj-name)

    (crs:or
     (crs:-> (mongo-desig-has-qualitative-at ?obj-name ?owl-time ?spec ?res1)
             (and
              (crs:bound ?res1)
              (crs:equal ?res1 ?res)
              (crs:format "got qualitative: ~a!~%" ?res1)))

     (crs:-> (mongo-desig-has-quantitative-at ?obj-name ?owl-time ?spec ?res2)
             (and
              (crs:bound ?res2)
              (crs:equal ?res2 ?res)))))

  (crs:<- (mongo-desig-has-qualitative-at ?obj-name ?owl-time ?desig ?res)
    (mongo-desig-prop ?desig (on ?sth))
    (mongo-desig-prop ?desig (name ?nme))
    (crs:lisp-pred identity ?nme)
    (crs:lisp-pred identity ?sth)
    (crs:equal ?res (on ?obj-name ?nme)))

  (crs:<- (mongo-desig-has-qualitative-at ?obj-name ?owl-time ?desig ?res)
    (mongo-desig-prop ?desig (in ?sth))
    (crs:lisp-pred identity ?sth)
    (crs:equal ?res (in ?obj-name ?sth)))

  (crs:<- (mongo-desig-pose ?desig ?resulting-pose)
    (mongo-desig-prop ?desig (pose ?pose))
    (mongo-desig-prop ?pose (pose ?pose2))

    ;; make sure the pose is in map coordinates
    (mongo-desig-prop ?pose (header ?header))
    (mongo-desig-prop ?header (frame_id ?frame))
    (crs:lisp-pred search "map" ?frame)

    (mongo-desig-prop ?pose2 (position ?position))
    (crs:equal ?position ((x . ?x) (y . ?y) (z . ?z)))
    (mongo-desig-prop ?pose2 (orientation ?orientation))
    (crs:equal ?orientation ((x . ?ox) (y . ?oy) (z . ?oz) (w . ?ow)))

    (crs:lisp-fun make-pose-helper ?x ?y ?z ?ox ?oy ?oz ?ow ?resulting-pose))

  (crs:<- (mongo-desig-has-quantitative-at ?obj-name ?owl-time ?desig ?res)
    (mongo-desig-pose ?desig ?obj-pose)
    (crs:lisp-fun cl-semantic-map-utils:get-semantic-map ?semmap)
    (crs:lisp-fun discretize-pose ?obj-pose ?semmap ?owl-time :name ?obj-name ?res))

  ;; to extract a pose from an object designator
  ;; TODO: unify with above stuff
  (crs:<- (mongo-obj-desig->pose ?desig ?pose)
    (mongo-desig-prop ?desig (at ?spec))
    (mongo-desig-prop ?spec (_designator_type "LOCATION"))
    (mongo-desig-pose ?spec ?pose)))

(defun mongo-obj-desig->pose (mng-desig)
  (if mng-desig
    (cut:with-vars-bound (?p)
        (assert-max-single
         (cut:force-ll (crs:prolog `(mongo-obj-desig->pose ,mng-desig ?p))))
      ?p)))

(defun pose-reachablility (side pose owl-time)
  "`pose' has to be in the /map frame."
  (let ((map->tll (lookup-mongo-transform "/torso_lift_link" "/map" owl-time)))
    ;; TODO: grasp orientation/offset?
    (pr2-reachability-costmap:pose-reachability
     (pr2-reachability-costmap:get-reachability-map side)
     (cl-transforms:transform-pose map->tll pose))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pose-distance (p1 p2)
  (cl-transforms:v-dist
   (cl-transforms:origin p1) (cl-transforms:origin p2)))

(defun gripper-pose->bl (side owl-time)
  (cl-transforms:transform->pose
   (lookup-mongo-transform "/base_link"
                           (ecase side
                             (:left
                              "/l_wrist_roll_link")
                             (:right
                              "/r_wrist_roll_link"))
                                          owl-time)))

(defun gripper-at-p (side p owl-time)
  (< (pose-distance p (gripper-pose->bl side owl-time))
     *pose-epsilon*))

(defun arms-parked (owl-time)
  (list (if (gripper-at-p :left *carry-pose-left* owl-time)
            '(arm-parked left))
        (if (gripper-at-p :right *carry-pose-right* owl-time)
            '(arm-parked right))))

(defun gripper-did-move (side start end)
  (> (pose-distance (gripper-pose->bl side start) (gripper-pose->bl side end))
     *pose-epsilon*))

(defun grippers-moved (start end)
  (list (if (gripper-did-move :left start end)
            '(gripper-moved left))
        (if (gripper-did-move :right start end)
            '(gripper-moved right))))

(defun base-moved (start end)
  (list (if (> (pose-distance (robot-pose start) (robot-pose end))
            *pose-epsilon*)
            '(base-moved pr2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gripper-state (side owl-time)
  (let ((prefix (ecase side
                  (:left "l")
                  (:right "r")))
        (symb (ecase side
                  (:left 'left)
                  (:right 'right))))
    (let ((left (cl-transforms:transform->pose
                 (lookup-mongo-transform "/base_link"
                                         (format nil "/~a_gripper_l_finger_tip_link" prefix)
                                         owl-time)))
          (right (cl-transforms:transform->pose
                  (lookup-mongo-transform "/base_link"
                                          (format nil "/~a_gripper_r_finger_tip_link" prefix)
                                          owl-time))))
      (if (< (pose-distance left right) *gripper-open-distance*)
          `(gripper-closed ,symb)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: robot-at, gripper-at predicates, parking positions

(defun robot-world-state-at (owl-time semantic-map &key (before nil))
  (let ((rpose (discretize-pose (robot-pose owl-time) semantic-map owl-time :name 'pr2))
        (lgripper (discretize-pose (robot-gripper-pose :left owl-time) semantic-map owl-time
                                   :name 'lgripper))
        (rgripper (discretize-pose (robot-gripper-pose :right owl-time) semantic-map owl-time
                                   :name 'rgripper)))
    (remove nil
            (append
             '((robot pr2)
               (gripper-left lgripper)
               (gripper-right rgripper))
             rpose lgripper rgripper
             (arms-parked owl-time)
             (list (gripper-state :left owl-time)
                   (gripper-state :right owl-time))
             (if (and *use-moved-predicates* before)
                 (append
                  (grippers-moved before owl-time)
                  (base-moved before owl-time)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; in-view, modelled after "obj_visible_in_camera" in knowrob_mongo
(defun in-camera-view (pose owl-time camera-id)
  (flet ((read-owl-value (id predicate &key (keep-string nil))
           (let ((val (owl-literal->lisp
                       (assert-single
                        (re-pl-utils:owl-has-query :subject id :predicate predicate)))))
             (if keep-string
                 val
                 (read-from-string val)))))
    (let ((hfov (read-owl-value camera-id #"srdl2comp:hfov"))
          (imgx (read-owl-value camera-id #"srdl2comp:imageSizeX"))
          (imgy (read-owl-value camera-id #"srdl2comp:imageSizeY"))
          (camera-frame (concatenate
                         'string "/"
                         (read-owl-value camera-id #"srdl2comp:urdfName" :keep-string t))))
      (let ((vfov (* (/ imgy imgx ) hfov))
            (cam-tf (lookup-mongo-transform camera-frame "/map" owl-time)))
        (let ((origin-in-cam-frame (cl-transforms:origin
                                    (cl-transforms:transform-pose cam-tf pose))))
          (with-slots ((x cl-transforms:x) (y cl-transforms:y) (z cl-transforms:z))
              origin-in-cam-frame
            (and (< (abs (atan y x)) (/ hfov 2))
                 (< (abs (atan z x)) (/ vfov 2)))))))))

(defun owl-literal->lisp (lit)
  (assert-single (last (alexandria:flatten lit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; on-top-of predicate
;; FIXME: checks just axis-aligned (should be ok for iai kitchen map though)
(defun on-top-of (pose experiment)
  (flet ((mp-edge (side direction mp)
           (let ((op (ecase side
                       (:min #'-)
                       (:max #'+))))
             (funcall op
                      (funcall direction
                               (cl-transforms:origin
                                (cl-semantic-map-utils:pose mp)))

                      (/ (funcall direction
                                  (cl-semantic-map-utils:dimensions mp))
                         2)))))
    (loop for mp in (cl-semantic-map-utils:semantic-map-parts (semantic-map experiment))
       with obj-origin = (cl-transforms:origin pose)
       with closest = (cons nil 10000)
       for dist = (cl-transforms:v-dist
                   obj-origin
                   (cl-transforms:origin (cl-semantic-map-utils:pose mp))) do
         (when (and
                (> (cl-transforms:z obj-origin)
                   (mp-edge :max #'cl-transforms:z mp))
                (< (cl-transforms:z obj-origin)
                   (+ (mp-edge :max #'cl-transforms:z mp)
                      ;; FIXME: hardcoded values are baaad
                      0.15))
                (> (cl-transforms:x obj-origin)
                   (mp-edge :min #'cl-transforms:x mp))
                (< (cl-transforms:x obj-origin)
                   (mp-edge :max #'cl-transforms:x mp))
                (> (cl-transforms:y obj-origin)
                   (mp-edge :min #'cl-transforms:y mp))
                (< (cl-transforms:y obj-origin)
                   (mp-edge :max #'cl-transforms:y mp))
                ;; we just want the relation to the closest map object
                (< dist (cdr closest)))
           (setf (car closest) mp
                 (cdr closest) dist)
           ;; (format t "~a~%"
           ;;         (cl-semantic-map-utils:name mp))
           )
         finally (return (car closest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun map->robot-frame (pose owl-time)
  (let ((map->bl (lookup-mongo-transform "/base_link" "/map" owl-time)))
    (cl-transforms:transform-pose map->bl pose)))

(defun manip-object-world-state (timestamp unique-manip-obj exp-trace)
  (mng-extract-pose-relation
   (mng-latest-obj-name-perception (slot-value unique-manip-obj 'name) timestamp
                                   exp-trace)
   timestamp))

(defun all-manip-objects-world-state (timestamp exp-trace)
  (append
   (loop for x in '(center on left right both none)
        collect (list (intern (format nil "~a-CONSTANT" x)) x))
   ;; type declarations
   (loop for obj in (manipulation-objects exp-trace)
      collect (list (intern (slot-value obj 'type))
                    (intern (slot-value obj 'name))))
   ;; TODO: keep only relevant objects
   (loop for semmap-obj in (cl-semantic-map-utils:semantic-map-parts
                            (cl-semantic-map-utils:get-semantic-map))
      for name = (cl-semantic-map-utils:name semmap-obj)
      for type = (cl-semantic-map-utils:obj-type semmap-obj)
      collect (list (intern (string-upcase type))
                    (intern (string-upcase name))))
   ;; relational positions
   (loop for obj in (manipulation-objects exp-trace)
      append
        (manip-object-world-state timestamp obj exp-trace))
   ;; reachability of unique-objects and such
   (object-properties
    (list
     (lambda (pose name time)
       (if (> (pose-reachablility :left pose time) 0)
           `(reachable-left ,name)))
     (lambda (pose name time)
       (if (> (pose-reachablility :right pose time) 0)
           `(reachable-right ,name)))
     (lambda (pose name time)
       (if (in-camera-view pose time #"pr2:pr2_head_mount_kinect_rgb_link")
           `(in-view ,name)))
     (lambda (pose name time)
       (let ((mp (on-top-of pose exp-trace)))
         (if mp
             `(on-top-of ,name ,(intern (string-upcase (cl-semantic-map-utils:name mp))))))))
    timestamp exp-trace)))

(defun object-properties (flist timestamp exp-trace)
  (loop for obj in (manipulation-objects exp-trace)
      for obj-name = (slot-value obj 'name)
      for obj-symbol = (intern obj-name)
      for obj-pose = (mongo-obj-desig->pose
                      (mng-latest-obj-name-perception obj-name timestamp exp-trace))
     when obj-pose
     append
       (loop for func in flist
            for res = (funcall func obj-pose obj-symbol timestamp)
            when res
            collect res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find error for action
;; TODO: parameters?
(defun action-failure (popm-id)
  (let ((failure (assert-max-single
                  (alexandria:flatten
                   (re-pl-utils:owl-has-query :subject popm-id
                                              :predicate #"knowrob:eventFailure")))))
    (if failure
        (let ((type-id
               (assert-single
                (remove #"owl:namedIndividual" (re-pl-utils:rdf-type failure) :test #'equal))))
          (loop for c across (shorten-uri type-id)
             for i from 0
             with res = ()
             do
               (when (and (> i 0) (upper-case-p c))
                 (push #\- res))
               (push (char-upcase c) res)
             finally (return (intern (coerce (nreverse res) 'string))))))))

;; HACK: I get multiple instances of the same failure, at different levels in the tree
(defun failures-at-time (ts)
  (cut:force-ll (json-prolog:prolog `(and ("failure_attribute" ?failure  ,#"knowrob:startTime" ,ts)
                                          ("failure_class" ?failure ?fclass)))))

(defun world-state-before-action (popm-id exp-trace)
  (destructuring-bind (start . end)
      (task-interval popm-id)
    (append
     (all-manip-objects-world-state start exp-trace)
     (robot-world-state-at start (semantic-map exp-trace)))))

(defun world-state-after-action (popm-id exp-trace)
  (destructuring-bind (start . end)
      (task-interval popm-id)
    (remove nil
     (append
      ;; TODO: timestamp +1?
      (all-manip-objects-world-state end exp-trace)
      (robot-world-state-at end (semantic-map exp-trace) :before start)
      (let ((af (action-failure popm-id)))
        (if af
            (list (list af))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: am not able to find the in-gripper designator via knowrob
;; (defun in-gripper-within-experiment (experiment)
;;    (let ((mongo-docs (cl-mongo:docs
;;                      (cl-mongo:db.find "logged_designators"
;;                                        (cl-mongo:kv "designator.OBJ.AT.IN" "GRIPPER") :limit 0))))
;;     (loop for doc in mongo-docs
;;          for id = (cl-mongo:get-element :_id (cl-mongo:get-element "designator" doc))
;;          for time = (cl-mongo:get-element "__recorded" doc)
;;          collect (cons id time))))

;; example for new desig format: (mongo-get-designator "designator_iKFiq84q0W3wCn")
;; (mongo-get-designator "designator_iKFiq84q0W3wCn")
;; "http://ias.cs.tum.edu/kb/cram_log.owl#PerformOnProcessModule_7Tfbe0aW"

;; mod_execution_trace:perform_time_check(t1 t2 res):
;; res = 1: t1 < t2
;; res = 2: t1 > t2
;; res = 0: t1 == t2
;; TODO:
;; extract ws also from the next action designator
(defun object-state-from-action-desigs (after before)
  (mapcar (lambda (bdgs)
            (cut:with-vars-bound (?p) bdgs
              (mongo-get-designator
               (get-action-designator-for-perform-pm (re-pl-utils:prolog->string ?p)))))
          (cut:force-ll
           (json-prolog:prolog `(and ("owl_individual_of" ?p ,#"knowrob:PerformOnProcessModule")
                                     ("task_start" ?p ?ps)
                                     ("perform_time_check" ,after ?ps 1)
                                     ("perform_time_check" ,before ?ps 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun all-poses (frame timestamps &key (base-frame "/map"))
  (loop for ts in timestamps
       collect
       (cl-transforms:transform->pose (lookup-mongo-transform base-frame frame ts))))

(defun poses->file (poses filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (format out "# x y z~%")
    (dolist (p poses)
      (let ((origin (cl-transforms:origin p)))
          (format out "~a ~a ~a~%" (cl-transforms:x origin)
                  (cl-transforms:y origin)
                  (cl-transforms:z origin))))))

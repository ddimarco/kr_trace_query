(in-package :kr-trace-query)

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
   (lookup-mongo-transform "/map" "/base_link" owl-time)))

(defun robot-gripper-pose (side owl-time)
  (cl-transforms:transform->pose
   (lookup-mongo-transform "/map"
                           (ecase side
                             (:left "/l_gripper_palm_link")
                             (:right "/r_gripper_palm_link"))
                           owl-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun z-relation (p1 p2)
  (with-slots ((o1 cl-transforms:origin)) p1
    (with-slots ((o2 cl-transforms:origin)) p2
      (- (cl-transforms:z o1) (cl-transforms:z o2)))))

(defun y-relation (p1 p2)
  (with-slots ((o1 cl-transforms:origin)) p1
    (with-slots ((o2 cl-transforms:origin)) p2
      (- (cl-transforms:y o2) (cl-transforms:y o1)))))

(defun x-relation (p1 p2)
  (with-slots ((o1 cl-transforms:origin)) p1
    (with-slots ((o2 cl-transforms:origin)) p2
      (- (cl-transforms:x o2) (cl-transforms:x o1)))))


(defun get-minimal-distance-relation (pose map-parts)
  ;; assuming everything in map frame
  (let ((rel-distances
         (loop for mp in map-parts
            for mp-pose = (cl-semantic-map-utils:pose mp)
            for left = (y-relation pose mp-pose)
            for above = (z-relation pose mp-pose)
            for behind = (x-relation pose mp-pose)
            collect
              (list
               (if (> left 0)
                   (cons :left left)
                   (cons :right (- left)))
               (if (> above 0)
                   (cons :above above)
                   (cons :below (- above)))
               (if (> behind 0)
                   (cons :behind behind)
                   (cons :in-front (- behind)))
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
       finally (return (list min-lr min-tb min-fb)))))


;; TODO: would probably be better to use a viewpoint from the robot instead of from the map origin
(defun discretize-pose (pose semantic-map &key name)
  (destructuring-bind ((lr-dir lr-dist lr-obj) (tb-dir tb-dist tb-obj) (fb-dir fb-dist fb-obj))
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
              (->relational-id tb-obj)))

     (remove nil
             (list
              (if (eq :behind fb-dir)
                  'behind-of
                  'in-front-of)
              (if name
                  name)
              (->relational-id fb-obj))))))

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
;; TODO: need to add "after" everywhere -> otherwise is used from previous demo

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
       for (mng-desig time collection) = (multiple-value-list (mongo-get-designator desig))
       when (and mng-desig
                 (equal (cdr (assoc 'name mng-desig))
                        name))
        return mng-desig)))



;; TODO: suddenly appearing manipulation objects? -> vien
(defun mng-extract-pose-relation (mng-desig)
  "extracts the pose from a designator retrieved from mongodb."
  (if mng-desig
      (assert-single
       (mapcar (lambda (bdg)
                 (cut:with-vars-bound (?spec) bdg
                   ?spec))
               (remove nil (cut:force-ll
                            (crs:prolog `(mongo-desig-pose-rel ,mng-desig ?spec))))))))

;; TODO: in-gripper!!!!
;; db.logged_designators.find({"designator._designator_type" : "object", "designator.NAME" : "PANCAKEMIX0", "designator.AT.IN" : "GRIPPER"}).pretty()
;; (defparameter *ids* (mapcar (lambda (doc)
;;                (cl-mongo:get-element :_id (cl-mongo:get-element "designator" doc)))
;;                (cl-mongo:docs (cl-mongo:db.find "logged_designators" (cl-mongo:kv "designator.AT.IN" "GRIPPER") :limit 0))))

(crs:def-fact-group mng-desig->qualitative-pos (mongo-desig-pose-rel mongo-desig-pose)
  (crs:<- (mongo-desig-pose-rel ?desig ?res)
    (mongo-desig-prop ?desig (at ?spec))
    (mongo-desig-prop ?spec (_designator_type "LOCATION"))
    (mongo-desig-prop ?desig (name ?obj-name-str))
    (crs:lisp-fun intern ?obj-name-str ?obj-name)

    (crs:or
     (crs:-> (mongo-desig-has-qualitative-at ?obj-name ?spec ?res1)
             (and
              (crs:bound ?res1)
              (crs:equal ?res1 ?res)
              (crs:format "got qualitative: ~a!~%" ?res1)
              (crs:cut)))

     (crs:-> (mongo-desig-has-quantitative-at ?obj-name ?spec ?res2)
             (and
              (crs:bound ?res2)
              (crs:equal ?res2 ?res)
              ))
     ))

  (crs:<- (mongo-desig-has-qualitative-at ?obj-name ?desig ?res)
    (mongo-desig-prop ?desig (on ?sth))
    (mongo-desig-prop ?desig (name ?nme))
    (crs:lisp-pred identity ?nme)
    (crs:lisp-pred identity ?sth)
    (crs:equal ?res (on ?obj-name ?nme)))

  (crs:<- (mongo-desig-has-qualitative-at ?obj-name ?desig ?res)
    (mongo-desig-prop ?desig (in ?sth))
    (crs:lisp-pred identity ?sth)
    (crs:equal ?res (in ?obj-name ?sth)))

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
    (mongo-desig-prop ?desig (at ?spec))
    (mongo-desig-prop ?spec (_designator_type "LOCATION"))
    (mongo-desig-pose ?spec ?pose)))



(defun mongo-obj-desig->pose (mng-desig)
  (if mng-desig
    (cut:with-vars-bound (?p)
        (assert-max-single
         (cut:force-ll (crs:prolog `(mongo-obj-desig->pose ,mng-desig ?p))))
      ?p)))

(defun pose-reachable-p (side pose owl-time)
  "`pose' has to be in the /map frame."
  (let ((map->tll (lookup-mongo-transform "/torso_lift_link" "/map" owl-time)))
    ;; TODO: grasp orientation/offset?
    (pr2-reachability-costmap:pose-reachability
     (pr2-reachability-costmap:get-reachability-map side)
     (cl-transforms:transform-pose map->tll pose))))

(defun manip-object-world-state (timestamp unique-manip-obj exp-trace)
  (mng-extract-pose-relation
   (mng-latest-obj-name-perception (slot-value unique-manip-obj 'name) timestamp
                                   exp-trace)))

(defun all-manip-objects-world-state (timestamp exp-trace)
  (append
   (loop for x in '(center on left right both none)
        collect (list (intern (format nil "~a-CONSTANT" x)) x))
   ;; type declarations
   (loop for obj in (manipulation-objects exp-trace)
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
   (loop for obj in (manipulation-objects exp-trace)
      append
        (manip-object-world-state timestamp obj exp-trace))
   ;; reachability of unique-objects
   (loop for obj in (manipulation-objects exp-trace)
      for obj-name = (slot-value obj 'name)
      for obj-symbol = (intern obj-name)
      for obj-pose = (mongo-obj-desig->pose
                      (mng-latest-obj-name-perception obj-name timestamp exp-trace))
      for in-reach-left = (if obj-pose
                              (pose-reachable-p :left obj-pose timestamp)
                              0)
      for in-reach-right = (if obj-pose
                               (pose-reachable-p :right obj-pose timestamp)
                               0)
      when (> in-reach-left 0)
      collect (list 'reachable-left obj-symbol)
        when (> in-reach-right 0)
        collect (list 'reachable-right obj-symbol))
   ))

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
      (robot-world-state-at end (semantic-map exp-trace))
      (let ((af (action-failure popm-id)))
        (if af
            (list (list af))))))))
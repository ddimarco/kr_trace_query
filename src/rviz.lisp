(in-package :kr-trace-query)

(defparameter *vis-publisher* nil)

(defun make-marker (pose id color type &key (scale '(1 0.1 0.1)) (text nil) (ns "") (mesh-resource "")
                                         (alpha 1.0))
  (roslisp:make-message "visualization_msgs/Marker"
                        (frame_id header) "/map"
                        (stamp header) (roslisp:ros-time)
                        (id) id
                        (type) (roslisp-msg-protocol:symbol-code 'visualization_msgs-msg:marker type)
                        (action) (roslisp-msg-protocol:symbol-code 'visualization_msgs-msg:marker :add)
                        (pose) (cl-tf:pose->msg pose)
                        (x scale) (elt scale 0)
                        (y scale) (elt scale 1)
                        (z scale) (elt scale 2)
                        (r color) (elt color 0)
                        (g color) (elt color 1)
                        (b color) (elt color 2)
                        (text) text
                        (ns) ns
                        (mesh_resource) mesh-resource
                        (a color) alpha))

(defun make-robot-markers (timestamp id)
  (let ((frames->resources
         '(("/base_link" . "base_v0/base.dae")
           ("/torso_lift_link" . "torso_v0/torso_lift.dae")
           ("/head_pan_link" . "head_v0/head_pan.dae")
           ("/l_upper_arm_link" . "upper_arm_v0/upper_arm.dae")
           ("/r_upper_arm_link" . "upper_arm_v0/upper_arm.dae")
           ("/l_forearm_link" . "forearm_v0/forearm.dae")
           ("/r_forearm_link" . "forearm_v0/forearm.dae")
           ("/r_shoulder_lift_link" . "shoulder_v0/shoulder_lift.dae")
           ("/l_shoulder_lift_link" . "shoulder_v0/shoulder_lift.dae")
           ("/l_gripper_l_finger_link" . "gripper_v0/l_finger.dae")
           ;; ("/l_gripper_r_finger_link" . "gripper_v0/l_finger.dae")
           ("/r_gripper_l_finger_link" . "gripper_v0/l_finger.dae")
           ;; ("/r_gripper_r_finger_link" . "gripper_v0/l_finger.dae")
          )))
    (loop for (frame . resource) in frames->resources
         for i from id
         collect
         (make-marker (cl-transforms:transform->pose
                       (lookup-mongo-transform "/map" frame timestamp))
                      i '(1 1 1) :mesh_resource :ns "robot" :scale '(1 1 1)
                      :mesh-resource (format nil "~a~a" "package://pr2_description/meshes/"
                                             resource)))))

(defun random-color ()
  (list (random 1.0) (random 1.0) (random 1.0)))

(defun make-semmap-object-markers (starting-id)
   (loop for semmap-obj in (cl-semantic-map-utils:semantic-map-parts
                            (cl-semantic-map-utils:get-semantic-map))
      for name = (cl-semantic-map-utils:name semmap-obj)
      for type = (cl-semantic-map-utils:obj-type semmap-obj)
      for dims = (cl-semantic-map-utils:dimensions semmap-obj)
      for pose = (cl-semantic-map-utils:pose semmap-obj)
      for id from starting-id
      collect
        (make-marker pose id (random-color) :cube
                     :scale (list (cl-transforms:x dims)
                                  (cl-transforms:y dims)
                                  (cl-transforms:z dims))
                     :ns "semantic-map")))

(defun make-manip-obj-marker (timestamp starting-id experiment)
  (let ((id starting-id)
        (result))
    (loop for obj in (manipulation-objects experiment)
       ;; (unique-manip-objects-from-interval
                     ;;  (timesteps-in-task task))
       for obj-name = (slot-value obj 'name)
       for obj-pose = (mongo-obj-desig->pose
                       (mng-latest-obj-name-perception obj-name timestamp experiment))
       when obj-pose
         do
         (push (make-marker obj-pose id '(0 1 0) :cube :scale '(0.2 0.2 0.2) :ns "manip-obj")
               result)
         (incf id)
         (push (make-marker obj-pose id '(1 0 0) :text_view_facing :text obj-name :ns "manip-obj")
               result)
         (incf id))
    result))

(defun make-marker-array (marker-list)
  (roslisp:make-message "visualization_msgs/MarkerArray"
                         markers (map 'vector #'identity marker-list)))

(defun visualize-timestamp (timestamp experiment-trace &key (start-id 0))
  (when (null *vis-publisher*)
    (setf *vis-publisher* (roslisp:advertise "/visualization_marker_array"
                                             "visualization_msgs/MarkerArray")))
  (let (markers)
    (setf markers (make-robot-markers timestamp start-id))
    (setf markers (append markers (make-semmap-object-markers (+ start-id (length markers)))))
    (setf markers (append markers (make-manip-obj-marker timestamp (+ start-id (length markers))
                                                         experiment-trace)))
    (push (make-marker (cl-transforms:make-identity-pose) (+ start-id (length markers))
                       '(1 0 0) :text_view_facing
                       :text (shorten-uri timestamp)) markers)
    (roslisp:publish *vis-publisher*
                     (roslisp:make-message "visualization_msgs/MarkerArray"
                                   markers (map 'vector #'identity markers)))))

(defun visualize-action (popm-id exp-trace)
  (destructuring-bind (start . end)
      (time-interval popm-id)
    (visualize-timestamp start exp-trace :start-id 0)
    (visualize-timestamp end exp-trace :start-id 500)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reachability

(defun hue2rgb (v1 v2 h)
  (when (< h 0)
    (incf h 1.))
  (when (> h 1)
    (decf h 1))
  (cond
    ((< (* 6 h) 1)
     (+ v1 (* (- v2 v1) 6 h)))
    ((< (* 2 h) 1)
     v2)
    ((< (* 3 h) 2)
     (+ v1 (* (- v2 v1) (- (/ 2 3) h) 6)))
    (t
     v1)))

(defun hsl2rgb (h s l)
  (if (< s 0.01)
      (list (* 255 l) (* 255 l) (* 255 l))

      (let* ((var2 (if (< l 0.5)
                    (* l (+ 1 s))
                    (- (+ s l) (* s l))))
              (var1
               (- (* 2 l) var2)))

        (list
         (* 255 (hue2rgb var1 var2 (+ h (/ 1 3))))
         (* 255 (hue2rgb var1 var2 h))
         (* 255 (hue2rgb var1 var2 (- h (/ 1 3))))))))


(defun reach-color (reach)
  (destructuring-bind (r g b)
      (hsl2rgb (+ (* (- 1 (* reach reach reach)) 1.0) 0.)
               1.0
               0.5)
    (list (/ r 255) (/ g 255) (/ b 255))))


;; FIXME: robot pose vis seems to be off, compared to baselink -> map transform
;; (show-reachability :left :starting-id 420 :transform (lookup-mongo-transform  "/map" "/torso_lift_link"  "http://ias.cs.tum.edu/kb/cram_log.owl#timepoint_1396513242"))
(defun show-reachability (side &key (starting-id 0)
                                 (timestamp nil)
                                 (transform (cl-transforms:make-identity-transform)))
  (when (stringp timestamp)
    (setf transform (lookup-mongo-transform  "/map" "/torso_lift_link" timestamp)))
  (let* ((map (pr2-reachability-costmap:get-reachability-map side))
         (minimum (pr2-reachability-costmap:minimum map))
         (maximum (pr2-reachability-costmap:maximum map))
         (markers)
         (id starting-id))
    (loop for y from (cl-transforms:y minimum)
                  to (cl-transforms:y maximum)
       by (cl-transforms:y (pr2-reachability-costmap:resolution map))
       do (loop for x from (cl-transforms:x minimum)
             to (cl-transforms:x maximum)
             by (cl-transforms:x (pr2-reachability-costmap:resolution map))
             do (loop for z from (cl-transforms:z minimum)
                   to (cl-transforms:z maximum)
                   by (cl-transforms:z (pr2-reachability-costmap:resolution map))
                     for pose = (cl-transforms:make-pose (cl-transforms:make-3d-vector x y z)
                                              (cl-transforms:make-identity-rotation))
                     for reach = (pr2-reachability-costmap:pose-reachability map pose)
                     when (> reach 0)
                   do
                     (push (make-marker
                            (cl-transforms:transform-pose transform pose)
                            id (reach-color reach) :cube :scale '(0.05 0.05 0.05)
                                        :ns "reach" :alpha 0.3)
                           markers)
                     (incf id))))
    (roslisp:publish *vis-publisher* (make-marker-array markers))))

(defun show-coordinate-frames (timestamp &key (starting-id 0))
  (let ((frames
         '("/map" "/base_link" "/torso_lift_link" "/odom_combined"))
        (id starting-id))
    (let (mlist)
      (loop for f in frames
           for tr = (lookup-mongo-transform "/map" f timestamp)
         do
           (push (make-marker (cl-transforms:transform-pose tr (cl-transforms:make-identity-pose))
                         id
                         '(0 1 0)
                         :arrow
                         :scale '(0.5 0.05 0.05)
                         :ns "frames")
                 mlist)
           (incf id)
           (push (make-marker (cl-transforms:transform-pose tr (cl-transforms:make-identity-pose))
                         id
                         '(1 1 1)
                         :text_view_facing
                         :text f
                         :ns "frames")
                 mlist)
           (incf id))

      (roslisp:publish *vis-publisher* (make-marker-array mlist)))))


(defun playback-experiment (experiment)
  (dolist (ts (timesteps-between (start-time experiment) (end-time experiment)))
    (visualize-timestamp ts experiment)
    ;; (asdf:run-shell-command (format nil "scrot /tmp/experiment_~a.png" (timepoint-id->time ts))
    ;; )
    ))

(in-package :ktq)

;; q: task(T), task_goal(T, '(OBJECT-PLACED-AT ?OBJ ?LOC)'), task_start(T, S), task_end(T, E), add_trajectory('/base_link', S, E, 1.0).
;; text: PR2's path during a pick-and-place sequence

(defun pick-and-place-path ()
  (json-prolog:prolog '(and
                        ("task" ?t)
                        ("task_goal" ?t "(OBJECT-PLACED-AT ?OBJ ?LOC)")
                        ("task_start" ?t ?s)
                        ("task_end" ?t ?e))))


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

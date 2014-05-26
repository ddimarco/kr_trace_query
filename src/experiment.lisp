(in-package :kr-trace-query)

(defparameter *experiment* nil)
(defparameter *prada-experiences* nil)
(defun run-extraction (&optional (root
                                  #"cram_log:CRAMAction_EGYg8xlT"
                                  ;;"http://ias.cs.tum.edu/kb/cram_log.owl#CRAMAction_xPY9fAsI"
                                  ))
  (setf *experiment*
        (make-instance 'experiment-trace
                       :root-owlid root
                       :semantic-map (cl-semantic-map-utils:get-semantic-map)))
  (setf *prada-experiences*
        (full-prada-trace *experiment*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass experiment-trace ()
  ((root-owlid :initarg :root-owlid
               :initform (error "Specify a root-owlid when instantiating a trace object.")
               :reader root-owlid)
   (start-time :reader start-time)
   (end-time :reader end-time)
   (semantic-map :initarg :semantic-map
                 :initform (error "Specify a semantic map when instantiating a trace object.")
                 :reader semantic-map)
   (manipulation-objects :reader manipulation-objects)))

(defmethod initialize-instance :after ((trace experiment-trace) &key)
  (let ((root (slot-value trace 'root-owlid)))
    (setf (slot-value trace 'start-time)
          (assert-single-recursive (re-pl-utils:owl-has-query
                                    :subject root
                                    :predicate #"knowrob:startTime"))

          (slot-value trace 'end-time)
          (assert-single-recursive (re-pl-utils:owl-has-query
                                    :subject root
                                    :predicate #"knowrob:endTime"))

          (slot-value trace 'manipulation-objects)
          (unique-manip-objects-from-interval
           (timesteps-between (start-time trace) (end-time trace))))))

;; (defparameter *experiment* (make-instance 'experiment-trace :root-owlid "http://ias.cs.tum.edu/kb/cram_log.owl#CRAMAction_xPY9fAsI" :semantic-map (cl-semantic-map-utils:get-semantic-map)))

(defun experiment-timesteps (trace)
  (timesteps-between (start-time trace) (end-time trace)))

(in-package :kr-trace-query)

(defclass experiment-trace ()
  ((root-owlid :initarg :root-owlid
               :initform nil
               :reader root-owlid)
   (start-time :reader start-time :initarg :start-time)
   (end-time :reader end-time :initarg :end-time)
   (semantic-map :initarg :semantic-map
                 :initform (error "Specify a semantic map when instantiating a trace object.")
                 :reader semantic-map)
   (manipulation-objects :reader manipulation-objects)))

(defmethod initialize-instance :after ((trace experiment-trace) &key)
  (let ((root (slot-value trace 'root-owlid)))
    (if root
        (setf (slot-value trace 'start-time)
              (assert-single-recursive (re-pl-utils:owl-has-query
                                        :subject root
                                        :predicate #"knowrob:startTime"))

              (slot-value trace 'end-time)
              (assert-single-recursive (re-pl-utils:owl-has-query
                                        :subject root
                                        :predicate #"knowrob:endTime")))
        (progn (assert (and (slot-value trace 'start-time) (slot-value trace 'end-time))))))
  (setf
   (slot-value trace 'manipulation-objects)
   (unique-manip-objects-from-interval
    (timesteps-between (start-time trace) (end-time trace)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-experiment (&optional (root #"cram_log:CRAMAction_EGYg8xlT"))
  (setf *experiment*
        (make-instance 'experiment-trace
                       :root-owlid root
                       :semantic-map (cl-semantic-map-utils:get-semantic-map)))
  (setf *prada-experiences*
        (full-prada-trace *experiment*)))

(defun extract-everything ()
  (let ((all-ts (sort-timesteps (all-time-steps))))
    (setf *experiment*
          (make-instance 'experiment-trace
                         :start-time (car all-ts)
                         :end-time (car (last all-ts))
                         :semantic-map (cl-semantic-map-utils:get-semantic-map))))
  (setf *prada-experiences*
        (full-prada-trace *experiment*)))

(defun experiment-timesteps (trace)
  (timesteps-between (start-time trace) (end-time trace)))

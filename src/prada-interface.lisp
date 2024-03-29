(in-package :ktq)

;; TODO: increase range of actions that can be used
(defun usable-actions (&key (action-type nil))
  "only keep actions which can be converted to a relational form"
  (remove-if (lambda (actionid)
               (let ((relational
                      (owl-desig->relational actionid (car (task-interval actionid)))))
                 (or (null relational)
                     (and (identity action-type) (not (eq (car relational) action-type))))))
             (get-all-actions)))

;; to find unusable actions:
;; (defparameter *unusable-actions*
;;      (loop for action in (get-all-actions)
;;         for rel = (owl-desig->relational action)
;;         when (null rel)
;;           collect action))

;; NOTE: learning algorithm needs to be set to use state - action - state data
(defun make-learn-instance (popm-id timestamp exp-trace)
  (multiple-value-bind (action additional-assertions)
      (owl-desig->relational popm-id timestamp)
    (make-instance 'cl-prada:prada-learn-state
                   :world (reverse
                           (append (world-state-before-action popm-id exp-trace)
                                   additional-assertions))
                   :action action
                   :world-after (reverse
                                 (append (world-state-after-action popm-id exp-trace)
                                         additional-assertions))
                   :comment (format nil "id: ~a, duration: ~a" (shorten-uri popm-id)
                                    (destructuring-bind (start . end) (task-interval popm-id)
                                      (- (timepoint-id->time end) (timepoint-id->time start)))))))

(defun full-prada-trace (experiment)
  (let ((timestamps (timesteps-between (start-time experiment) (end-time experiment)))
        (all-actions (usable-actions)))
    (loop for action in all-actions
       for i from 0
       for progress = (float (* 100.0 (/ (1+ i) (length all-actions))))
       for start-time = (assert-single-recursive
                         (re-pl-utils:owl-has-query :subject action :predicate #"knowrob:startTime"))
       for end-time = (assert-single-recursive
                       (re-pl-utils:owl-has-query :subject action :predicate #"knowrob:endTime"))
       when (and (member start-time timestamps :test #'equal)
                 (member end-time timestamps :test #'equal))
       collect
         (progn
           (when (string= start-time end-time)
             (roslisp:ros-warn () "WARNING: action ~a has a duration of length 0!" action))
           (make-learn-instance action start-time experiment))
       do
         (format t "~$% completed~%" progress))))

(defun prada-symbol-defs-from-learn-data (data)
  (let ((symbol-hash (make-hash-table)))
    (labels ((add-symbol-def (name type params)
               (let ((old-def (gethash name symbol-hash)))
                 (if (null old-def)
                     (setf (gethash name symbol-hash) (list type params))
                     (destructuring-bind (otype oparams) old-def
                       (assert (and (eq otype type)
                                    (= oparams params)))))))
             (add-symbol-def-ws (ws)
               (dolist (asrt ws)
                 (add-symbol-def (car asrt) 'cl-prada::primitive (1- (length asrt))))))
      (loop for d in data
         for action = (slot-value d 'cl-prada::action)
         for world-before = (slot-value d 'cl-prada::world)
         for world-after = (slot-value d 'cl-prada::world-after)
         do
           (add-symbol-def (car action) 'cl-prada::action (1- (length action)))
           (add-symbol-def-ws world-before)
           (add-symbol-def-ws world-after))
      (loop for key being the hash-keys of symbol-hash
         using (hash-value value)
         for (type param-count) = value
         collect
           (cl-prada::make-symbol-def `(,type ,key ,param-count))))))

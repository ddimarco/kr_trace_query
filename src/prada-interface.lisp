(in-package :ktq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: should be possible to use all
(defun usable-actions ()
  "only keep actions which can be converted to a relational form"
  (remove-if (lambda (actionid)
               (not (identity
                     (owl-desig->relational actionid (car (task-interval actionid))))))
             (get-all-actions)))

;; NOTE: prada needs to be set to use state - action - state data
(defun make-learn-instance (popm-id timestamp exp-trace)
  (multiple-value-bind (action additional-assertions)
      (owl-desig->relational popm-id timestamp)
    (make-instance 'cl-prada::prada-learn-state
                   :world (append (world-state-before-action popm-id exp-trace)
                                  additional-assertions)
                   :action action
                   :world-after (append (world-state-after-action popm-id exp-trace)
                                        additional-assertions))))

(defun full-prada-trace (experiment)
  (let ((timestamps (timesteps-between (start-time experiment) (end-time experiment))))
    (loop for action in (usable-actions)
       for start-time = (assert-single-recursive
                         (re-pl-utils:owl-has-query :subject action :predicate #"knowrob:startTime"))
       for end-time = (assert-single-recursive
                         (re-pl-utils:owl-has-query :subject action :predicate #"knowrob:endTime"))
         when (and (member start-time timestamps :test #'equal)
                   (member end-time timestamps :test #'equal))
       collect
         (make-learn-instance action start-time experiment))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun tasks-of-type-within-interval (owl-type timesteps)
;;   (cut:force-ll
;;    (re-pl-utils:pl-query (?t)
;;        `(and ("owl_individual_of" ?t ,owl-type)
;;              ("task_start" ?t ?s) ("task_end" ?t ?e)
;;              ("member" ?s ',timesteps)
;;              ("member" ?e ',timesteps))
;;      (re-pl-utils:prolog->string ?t))))

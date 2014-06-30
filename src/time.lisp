(in-package :kr-trace-query)

(defun all-owl-instances-of (cls)
  (cut:with-vars-strictly-bound (?l)
      (car (json-prolog:prolog-simple-1
            (format nil "setof(I, owl_individual_of(I, '~a'), L)" cls)))
    (re-pl-utils:pl-tree->string ?l)))

(defun all-time-steps ()
  (remove 0 (all-owl-instances-of "http://ias.cs.tum.edu/kb/knowrob.owl#TimePoint")
          :key #'timepoint-id->time))

;; owlids are unix timestamps
(defun timepoint-id->time (tp-id)
  (let ((str (subseq tp-id (1+ (position #\_ tp-id :from-end t)))))
    ;; FIXME: in case there is a decimal point in there, just cut it off
    (let ((dot-pos (position #\. str)))
      (when dot-pos
        (setf str (subseq str 0 dot-pos))))
    (if (string= str "")
        0
        (parse-integer str))))

(defun sort-times (timepoints)
  (let ((timepoints (sort timepoints
                          #'<
                          :key (lambda (tp) (timepoint-id->time tp)))))
    timepoints))

(defun ensure-inttime (time)
  (etypecase time
    (integer
     time)
    (string
     (timepoint-id->time time))))

;; FIXME: these operate only on seconds
(defun owl-time->local (owlid)
  (let ((unixtime (ensure-inttime owlid)))
    (local-time:unix-to-timestamp unixtime)))

(defun bson->local (bson-time)
  (local-time:universal-to-timestamp (cl-mongo:bson-time-to-ut bson-time)))

(defun local->bson-time (local)
  "convert a local-time date-time into the bson format used by cl-mongo"
  (local-time:with-decoded-timestamp (:sec second :minute min :hour hour
                                           :day day :month month :year year)
      local
    (cl-mongo:date-time second min hour day month year)))

(defun owl-time->bson (owlid)
  (let ((loc (owl-time->local owlid)))
    (local->bson-time loc)))

(defun ensure-bson-time (time)
  (etypecase time
    (cl-mongo::bson-time
     time)
    (string
     (owl-time->bson time))
    (local-time:timestamp
     (local->bson-time time))))

(defun task-interval (task-id)
  (assert-single
   (cut:force-ll
    (re-pl-utils:pl-query (?s ?e)
        `(and ("task_start" ,task-id ?s)
              ("task_end" ,task-id ?e))
      (cons (re-pl-utils:pl-tree->string ?s)
            (re-pl-utils:pl-tree->string ?e))))))

(defun time-interval (owlid)
  (let ((start-time (assert-single-recursive
                     (re-pl-utils:owl-has-query :subject owlid
                                                :predicate #"knowrob:startTime")))
        (end-time (assert-single-recursive
                   (re-pl-utils:owl-has-query :subject owlid
                                              :predicate #"knowrob:endTime"))))
    (cons start-time end-time)))

(defun sort-timesteps (ts)
  (sort ts #'< :key #'timepoint-id->time))

(defun timesteps-between (start end)
  (let ((timesteps (sort-timesteps (all-time-steps)))
        (int-start (timepoint-id->time start))
        (int-end (timepoint-id->time end)))
    (loop for ts in timesteps
       for int-ts = (timepoint-id->time ts)
         when (and (>= int-ts int-start)
                   (<= int-ts int-end))
         collect ts)))

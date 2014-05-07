(in-package :kr-trace-query)

(defun timepoint-id->time (tp-id)
  (let ((str (subseq tp-id (1+ (position #\_ tp-id :from-end t)))))
    ;; FIXME: in case there is a decimal point in there, just cut it off
    (let ((dot-pos (position #\. str)))
      (when dot-pos
        (setf str (subseq str 0 dot-pos))))
    (parse-integer str)))

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

;; how to get tf timestamp from owl timepoints? -> owlid is a unix timestamp

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

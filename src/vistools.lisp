(in-package :ktq)

(defun owl->graphviz (owlid &key stream (depth 1)
                              (ignore-prop-list '(#"knowrob:taskContext" #"knowrob:goalContext"
                                                  #"rdf:type")))
  (labels ((write-owl (owlid d str &optional visited-list)
             (when (= d depth)
               (format str "digraph G {~%"))

             (unless (member owlid visited-list :test #'equal)
               (let ((props (re-pl-utils:owl-has owlid)))
                 (loop for ppair in props
                    for prop = (car ppair)
                    for sprop = (shorten-owl-id (car ppair))
                    for v = (cdr ppair)
                    with sowlid = (shorten-owl-id owlid)
                    with seen-props = ()
                    unless (member prop
                                   (append ignore-prop-list seen-props)
                                   :test #'equal)
                    do
                      (cond
                        ((stringp v)
                         (format str " ~s -> ~s [label=~s];~%" sowlid
                                 (shorten-owl-id v)
                                 sprop)
                         (when (> d 0)
                           (write-owl v (1- d) str (append (list owlid) visited-list))))

                        ((listp v)
                         (let ((leaf (gensym)))
                           (format str " ~a [label=~s];~%" leaf
                                   (remove #\newline (format nil "~a" v)))
                           (format str " ~s -> ~a [label=~s];~%" sowlid
                                   leaf
                                   sprop))))
                      (push prop seen-props))))

             (when (= d depth)
               (format str "}~%"))))
    (if stream
        (write-owl owlid depth stream)
        (with-open-file (str "/tmp/owl.dot" :direction :output :if-exists :supersede)
          (write-owl owlid depth str)))))

(defun tasks->graphviz (int-time-lst)
  (with-open-file (str "/tmp/bla.dot" :direction :output :if-exists :supersede)
    (format str "digraph G {~%")
    (format str " subgraph cluster_time_points {~%")
    (format str "order=LR; node [style=filled];~%")
    (loop for tp-uri in int-time-lst
       for tp = (timepoint-id->time tp-uri)
       do
         (format str "~a " tp))
    (format str ";~% }~%")

    (loop for tp-uri in int-time-lst
       for tp = (timepoint-id->time tp-uri)
         with prev = nil
       do
         (when prev
           (format str "~a -> ~a;~%" prev tp))
         (setf prev tp))

    (loop for tp-uri in int-time-lst
       for tp = (timepoint-id->time tp-uri)
       for start-tasks = (cut:force-ll
                          (re-pl-utils:pl-query (?t)
                              `("task_start" ?t ,tp) (re-pl-utils:prolog->string  ?t)))
       for end-tasks = (cut:force-ll
                        (re-pl-utils:pl-query (?t)
                            `("task_end" ?t ,tp) (re-pl-utils:prolog->string ?t)))
       do
         (dolist (et end-tasks)
           (format str "  ~a -> ~a;~%" (shorten-uri et) tp))
         (dolist (st start-tasks)
           (format str "  ~a -> ~a;~%" tp (shorten-uri st))))
    (format str "}~%")))

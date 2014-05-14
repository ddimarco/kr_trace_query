(asdf:defsystem kr-trace-query
  :name "kr-trace-query"
  :author "Daniel Di Marco"
  :version "0.1"
  :maintainer "Daniel Di Marco"
  :licence "BSD"
  :depends-on (cram-json-prolog
               re-pl-utils
               cl-mongo
               alexandria
               local-time
               cl-semantic-map-utils
               cl-prada
               pr2-reachability-costmap
               visualization_msgs-msg)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "owl-read-macro" :depends-on ("package"))
             (:file "time" :depends-on ("package"))
             (:file "sandbox" :depends-on ("owl-read-macro" "time"))
             (:file "vistools" :depends-on ("sandbox"))
             (:file "extract-actions" :depends-on ("sandbox"))
             (:file "worldstate" :depends-on ("extract-actions"))
             (:file "queries" :depends-on ("extract-actions"))
             (:file "experiment" :depends-on ("queries"))
             (:file "rviz" :depends-on ("queries"))
             ))))



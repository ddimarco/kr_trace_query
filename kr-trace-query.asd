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
               visualization_msgs-msg
               cl-store
               cl-xml-namespace-read-macro)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "time" :depends-on ("package"))
             (:file "sandbox" :depends-on ("time"))
             (:file "vistools" :depends-on ("sandbox"))
             (:file "extract-actions" :depends-on ("sandbox"))
             (:file "worldstate" :depends-on ("extract-actions"))
             (:file "prada-interface" :depends-on ("extract-actions"))
             (:file "experiment" :depends-on ("prada-interface"))
             (:file "rviz" :depends-on ("experiment"))))))



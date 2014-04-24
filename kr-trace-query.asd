; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem kr-trace-query
  :name "kr-trace-query"
  :author "Daniel Di Marco"
  :version "0.1"
  :maintainer "Daniel Di Marco"
  :licence "BSD"
  :depends-on (cram-json-prolog
               re-pl-utils
               cl-mongo
               alexandria)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "owl-read-macro" :depends-on ("package"))
             (:file "sandbox" :depends-on ("owl-read-macro"))
             ))))



(in-package :asdf-user)

(defsystem "doknil-01"
  :description "Doknil knowledge-base language."
  :version "0.1"
  :author "Massimo Zaniboni <mzan@dokmelody.org>"
  :licence "MIT"
  :depends-on (
    #:trivia
    #:alexandria
    #:serapeum
    #:trivial-types
    #:defstar
    #:str
    #:parse-float
    #:iterate
    #:let-plus
    #:array-operations
    #:bordeaux-threads
    #:postmodern
    #:cl-postgres
    #:s-sql)
  :pathname "src/"
  :serial t
  :components ((:file "doknil"))
  :in-order-to ((asdf:test-op (asdf:test-op :dokmelody-pass01/tests))))

(defsystem "doknil-01/tests"
  :description "Test suite for DokMelody"
  :author "Massimo Zaniboni <mzan@dokmelody.org>"
  :licence "MIT"
  :depends-on (:doknil-01
               #:parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :dokmelody.tests))
  :pathname "tests/"
  :serial t
  :components ((:file "tests")))

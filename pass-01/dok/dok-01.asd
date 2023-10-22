(in-package :asdf-user)

(defsystem "dok-01"
  :description "Bootstrapping pass 01 of Dok programming language."
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
    #:esrap
    #:parse-float
    #:uiop
    )
  :pathname "src/"
  :serial t
  :components (
     (:file "ast")
     (:file "parser")
     (:file "compiler")
     (:file "runtime"))
  :in-order-to ((asdf:test-op (asdf:test-op :dok-01/tests))))

(defsystem "dok-01/tests"
  :description "Test suite for Dok programming language"
  :author "Massimo Zaniboni <mzan@dokmelody.org>"
  :licence "MIT"
  :depends-on (:dok-01
               #:parachute)
  :pathname "tests/"
  :serial t
  :components ((:file "parser"))
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :dok/parser/tests))
  )

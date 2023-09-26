(in-package :asdf-user)

(defsystem "dok-01"
  :description "Bootstrapping pass 01 of Dok programming language."
  :version "0.1"
  :author "Massimo Zaniboni <mzan@dokmelody.org>"
  :licence "MIT"
  :depends-on (
    #:defclass-std
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
    )
  :pathname "src/"
  :serial t
  :components
    ((:file "ast")
     (:file "parser")
     (:file "dok"))
  :in-order-to ((asdf:test-op (asdf:test-op :dok/tests))))

(defsystem "dok-01/tests"
  :description "Test suite for Dok programming language"
  :author "Massimo Zaniboni <mzan@dokmelody.org>"
  :licence "MIT"
  :depends-on (:dok
               #:parachute)
  :pathname "src/tests/"
  :serial t
  :components ((:file "parser"))
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :dok-parser/tests))
  )

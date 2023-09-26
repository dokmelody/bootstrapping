;;;; dokmelody.asd

(defsystem "dokmelody-build"
  :defsystem-depends-on ("overlord")
  :class "overlord:overlord-project-system"
  :target-name #:all
  :serial t
  :depends-on (#:overlord)
  :components ((:file "package")
               (:file "dokmelody-build")))

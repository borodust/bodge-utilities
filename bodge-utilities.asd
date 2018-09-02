(asdf:defsystem :bodge-utilities
  :description "Utility library"
  :version "1.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (alexandria uiop log4cl local-time dissect split-sequence cffi
                          claw static-vectors trivial-gray-streams)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "time")
               (:file "language")
               (:file "collections")
               (:file "foreign")
               (:file "strings")
               (:file "trees")
               (:file "arrays")
               (:file "streams")))

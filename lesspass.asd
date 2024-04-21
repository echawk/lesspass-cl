(asdf:defsystem #:lesspass
  :description "Lesspass v2 implementation."
  :author "Ethan Hawk <ethan.hawk@valpo.edu>"
  :license "MIT"
  :version "1.0.0"
  :depends-on (:ironclad :babel)
  :components ((:file "package")
               (:file "lesspass")))

(asdf:defsystem #:lesspass/test
  :description "Tests for lesspass."
  :author "Ethan Hawk <ethan.hawk@valpo.edu>"
  :license "MIT"
  :depends-on (:lesspass :fiveam)
  :perform (test-op (o s) (uiop:symbol-call '#:fiveam '#:run! :lesspass/test))
  :components ((:file "package")
               (:file "lesspass-test")))


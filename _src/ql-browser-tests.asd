;;;; ql-browser-tests.asd

(asdf:defsystem #:ql-browser-tests
  :description "Tests for ql-browser"
  :author "complexitycollapse <complexitycollapse@github.com>"
  :license  ""
  :version "0.0.1"
  :serial t
  :depends-on (#:ql-browser
	       #:0am)
  :components ((:file "packages")
               (:file "ql-browser-tests")))

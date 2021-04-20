;;;; ql-browser.asd

(asdf:defsystem #:ql-browser
  :description "Describe ql-browser here"
  :author "complexitycollapse <complexitycollapse@github.com>"
  :license  ""
  :version "0.0.1"
  :serial t
  :depends-on (:drakma)
  :components ((:file "packages")
               (:file "ql-browser")))

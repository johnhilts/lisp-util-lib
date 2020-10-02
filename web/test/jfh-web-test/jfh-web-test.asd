;;;; jfh-web-test.asd

(asdf:defsystem #:jfh-web-test
  :description "Specs for jfh-web library"
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:parenscript #:jfh-testing #:jfh-web)
  :components ((:file "package")
               (:file "jfh-web-test")))

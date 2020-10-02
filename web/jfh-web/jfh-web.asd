;;;; jfh-web.asd

(asdf:defsystem #:jfh-web
  :description "Utility library for web"
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:parenscript)
  :components ((:file "package")
               (:file "jfh-web")))

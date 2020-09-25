;; install these: (ql:quickload '(parenscript))
(defpackage :test-jfh-web
  (:use :cl :parenscript :jfh-web :jfh-test))

(in-package :test-jfh-web)

(import-macros-from-lisp 'with-html-elements)

(defun test-with-html-elements ()
  (flet ((setup ()
           (setf *ps-gensym-counter* 0)
           (jfh-web::define-ps-with-html-macro)))
    (setup)
    (test-spec :category "web"
      (test-spec :description "Lisp -> DOM API"
        (test-spec :it "create a simple table"
          (let ((actual (ps (defun output-simple-table () (jfh-web::with-html-elements (table (tr (td "test")))))))
                (expected
                 "function outputSimpleTable() {
    var tableElement0 = createAnElement(parentElement, 'TABLE');
    var trElement1 = createAnElement(tableElement0, 'TR');
    var tdElement2 = createAnElement(trElement1, 'TD');
    __PS_MV_REG = [];
    return setTextNode(tdElement2, 'test');
};"))
            (format t "~&expected:~%****~%~a~%****~%" expected)
            (format t "~&actual:~%****~%~a~%****~%" actual)
            (string= expected actual)))))))
        




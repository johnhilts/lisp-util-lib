(defpackage #:jfh-test
  (:use #:common-lisp))

(in-package #:jfh-test)

(defmacro test-spec (key text &body form)
                                        ; (print (length form))
  (labels
      ((test-r (key text form)
         (list
          (when (or (equal :description key) (equal :category key) (equal :it key))
            `(format t "~&~a~%" ,text))

          (when (listp form)
            (if (equal (car form) 'test-spec)
                `(progn ,@(test-r (cadr form) (caddr form) (cadddr form)))
                `(format t "~&The form: ~a ~a!" ',form (if (eval ,form) "passes" "fails")))))))
    `(progn ,@(test-r key text (car form)))))

(defun an-example ()
  (test-spec :category "arithmatic"
    (test-spec :description "addition"
      (test-spec :it "should add together 2 numbers"
        (= 5 (+ 2 3)))
      (test-spec :it "should add together 2 other numbers"
        (= 10 (+ 6 4))))))

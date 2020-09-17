(defpackage #:jfh-test
  (:use #:common-lisp))

(in-package #:jfh-test)

(defmacro test-spec (key text &body form)
  (labels
      ((test-r (key text form)
         (format t "~&FORM: ~a~%" form)
         (list
          (when (or (equal :description key) (equal :category key) (equal :it key))
            `(format t "~&~a~%" ,text))

          (when (listp form)
            (if (equal (car form) 'test-spec)
                `(progn
                   ,@(test-r (cadr form) (caddr form) ())
                   ,@(mapcar #'(lambda (form) `(progn ,@(test-r (cadr form) (caddr form) (cadddr form)))) (nthcdr 3 form)))
                (unless (null form)
                  `(format t "~&The form: ~a ~a!" ',form (if (eval ,form) "passes" "fails"))))))))
    `(progn ,@(mapcar #'(lambda (e) `(progn ,@(test-r key text e))) form))))

(defun an-example ()
  (test-spec :category "arithmatic"
    (test-spec :description "addition"
      (test-spec :it "should add together 2 numbers"
        (= 5 (+ 2 3)))
      (test-spec :it "should add together 2 other numbers"
        (= 10 (+ 6 4)))
      (test-spec :it "should add together 3 numbers!"
        (= 12 (+ 6 2 4))))
    (test-spec :description "subtraction"
      (test-spec :it "should subtract 1 number from another"
        (= 7 (- 15 8))))))

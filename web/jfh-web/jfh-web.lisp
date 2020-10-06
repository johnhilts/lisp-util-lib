;;;; jfh-web.lisp

(in-package #:jfh-web)

(defun define-ps-with-html-macro ()
  (ps
    (defun create-an-element (parent-element tag)
      (let ((new-element (chain document (create-element tag))))
        (chain parent-element (append-child new-element))
        new-element))
    (defun set-an-attribute (parent-element key value)
      (chain parent-element (set-attribute key value)))
    (defun set-text-node (parent-element text)
      (let ((a-text-node (chain document (create-text-node text))))
        (chain parent-element (append-child a-text-node))))
    (defmacro with-html-elements (elements)
      (flet
          ((expression-attribute-p (value)
             (char= #\( (aref value 0)))
           
           (event-attribute-p (key)
             (string= "on" (subseq key 0 2)))
           
           (cons-pair-p (possible-cons)
             (or
              (and (consp possible-cons) (atom (cdr possible-cons)))
              (and (consp possible-cons) (listp (cdr possible-cons)) (equal '~f (cadr possible-cons))))))

        (labels
            ((parse-expression-for-attribute-value (parent-element key expression-string)
               (let* ((expression (read-from-string expression-string))
                      (function-name (car expression))
                      (parameters (cdr expression)))
                 (if (event-attribute-p key)
                     (let ((event-attribute-key (subseq key 2)))
                       `(chain parent-element-id (add-event-listener ,event-attribute-key (chain ,function-name (bind null ,@parameters)) false)))
                     `(set-an-attribute ,parent-element ,key (,function-name ,@parameters)))))
             
             (process-tag-r (element &optional (parent nil parent-supplied-p) (key-id nil key-id-supplied-p))
               (let* ((tag (car element))
                      (parent-element (gensym (concatenate 'string (string-downcase tag) "Element")))
                      (parent-element-parameter (if parent-supplied-p parent (make-symbol "parent-element")))
                      (key-id-parameter (if key-id-supplied-p key-id (if (some #'(lambda (e) (equal (car e) 'key)) (cdr element)) (ps-gensym "-")))))
                 (cons
                  `(let ((,parent-element (create-an-element ,parent-element-parameter ,(symbol-to-js-string tag)))))
                  (mapcar
                   #'(lambda (e)
                       (cond
                         ((cons-pair-p e)
                          (let* ((key (symbol-to-js-string (car e)))
                                 (value (cdr e)))
                            (if (expression-attribute-p value)
                                (parse-expression-for-attribute-value parent-element key value)
                                `(set-an-attribute ,parent-element ,key ,value))))
                         ((stringp e)
                          `(set-text-node ,parent-element ,e))
                         ((listp e)
                          `(progn
                             ,@(process-tag-r e parent-element key-id-parameter)))
                         ((symbolp e)
                          `(set-text-node ,parent-element ,e))))
                   (cdr element))))))
          
          `(progn ,@(process-tag-r elements)))))))

;; example
(defun todo-list-interaction ()
  (ps
    
    (defun render-todo-list (todo-list)
      (let* ((todo-list-table-body (chain document (get-element-by-id "todo-list-body")))
             (parent-element todo-list-table-body)
             (column-header (chain document (get-element-by-id "todo-list-column-header")))
             (count (length todo-list))
             (use-plural-form (or (> 1 count) (= 0 count))))
        (clear-children parent-element)
        (setf (chain column-header inner-text)
              (if use-plural-form "To-do Items" "To-do Item"))
        (chain todo-list (map
                          #'(lambda (todo index)
                              (let ((checkbox-id (+ "todo-check" index))
                                    (label-id (+ "todo-label" index)))                                
                                (with-html-elements
                                    (tr 
                                     (td
                                      (input (id . "(chain checkbox-id (to-string))") (type . "checkbox") (onclick . "(update-todo (chain index (to-string)))"))
                                      (input (id . "test-check") (type . "button") (onclick . "(update-todo 123)"))
                                      (label (id . "(chain label-id (to-string))") (html-for . "(chain checkbox-id (to-string))") todo)))))

                              t)))))

    (setf (chain window onload) init)))
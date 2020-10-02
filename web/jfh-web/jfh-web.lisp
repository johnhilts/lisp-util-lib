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
          ((get-attribute-value (value)
             (if (char= #\( (aref value 0))
                 (read-from-string value)
                 value))
           
           (cons-pair-p (possible-cons)
             (or
              (and (consp possible-cons) (atom (cdr possible-cons)))
              (and (consp possible-cons) (listp (cdr possible-cons)) (equal '~f (cadr possible-cons))))))
        (labels
            ((process-tag-r (element &optional (parent nil parent-supplied-p) (key-id nil key-id-supplied-p))
               (let* ((tag (car element))
                      (parent-element (gensym (concatenate 'string (string-downcase tag) "Element")))
                      (parent-element-parameter (if parent-supplied-p parent (make-symbol "parent-element")))
                      (key-id-parameter (if key-id-supplied-p key-id (if (some #'(lambda (e) (equal (car e) 'key)) (cdr element)) (ps-gensym "-")))))
                 (cons
                  `(let ((,parent-element (create-an-element ,parent-element-parameter ,(string tag)))))
                  (mapcar
                   #'(lambda (e)
                       (cond
                         ((cons-pair-p e)
                          (let* ((key (string (car e)))
                                 (value (get-attribute-value (string (cdr e)))))
                            `(set-an-attribute ,parent-element ,key ,value)))
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
                                    (tr (key . index)
                                        (td
                                         ;; idea: pass like this: "(updateTodo(chain index (to-string)))"
                                         ;; reformat into this: "(+ \"updateTodo(\" (chain index (to-string)) \")\")"
                                         ;; BUT this is really: "(+ "updateTodo(" (chain index (to-string)) ")")" <-- parenscript inside javascript is really "updateTodo(123)"
                                         ;; then send that mess to read-from-string
                                         (input (id . "todo-check") (type . "checkbox") (onclick . "(+ \"updateTodo(\" (chain index (to-string)) \")\")"))
                                         (input (id . "test-check") (type . "button") (onclick . "(updateTodo 123)"))
                                  (label (id . "todo-label") todo))))
                          
                          (let ((todo-check-box (chain document (get-element-by-id "todo-check")))
                                (todo-label (chain document (get-element-by-id "todo-label"))))
                            (setf (@ todo-check-box id) checkbox-id
                                  (@ todo-label id) label-id
                                  (@ todo-label html-for) checkbox-id)
                            ;; (chain todo-check-box (add-event-listener "click" (chain update-todo (bind null index)) false)))
                            ))

                          t)))))

    (setf (chain window onload) init)))

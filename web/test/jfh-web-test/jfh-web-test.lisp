;;;; jfh-web-test.lisp

(in-package #:jfh-web-test)

(import-macros-from-lisp 'with-html-elements)

(defun test-with-html-elements ()
  (flet ((setup ()
           (setf *ps-gensym-counter* 0)
           (define-ps-with-html-macro)
           t))
    ;;(setup)
    (test-spec :category "web"
      (test-spec :description "Lisp -> DOM API"
        (test-spec :it "create a simple table"
          (let ((actual (ps (defun output-simple-table () (jfh::with-html-elements (table (tr (td "test")))))))
                (expected
                 "function outputSimpleTable() {
    var tableElement0 = createAnElement(parentElement, \"table\");
    var trElement1 = createAnElement(tableElement0, \"tr\");
    var tdElement2 = createAnElement(trElement1, \"td\");
    __PS_MV_REG = [];
    return setTextNode(tdElement2, \"test\");
};"))
            ;; (format t "~%***Expected:~%~a~%***~%" expected)
            ;; (format t "~%***Actual:~%~a~%***~%" actual)
            
            (string= expected actual)))
        (test-spec :it "work with a todo list"
          (let ((actual
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
                                               (jfh::with-html-elements
                                                   (tr
                                                    (td
                                                     (input (id . "(chain checkbox-id (to-string))") (type . "checkbox") (onclick . "(+ \"updateTodo(\" (chain index (to-string)) \")\")") (checked . "t"))
                                                     (input (id . "(chain checkbox-id (to-string))") (type . "button") (onclick . "(updateTodo 123)"))
                                                     (label (id . "(chain label-id (to-string))") todo)))))

                                             t)))))))
                (expected
                 "function renderTodoList(todoList) {
    var todoListTableBody = document.getElementById(\"todo-list-body\");
    var parentElement = todoListTableBody;
    var columnHeader = document.getElementById(\"todo-list-column-header\");
    var count = todoList.length;
    var usePluralForm = 1 > count || 0 === count;
    clearChildren(parentElement);
    columnHeader.innerText = usePluralForm ? \"To-do Items\" : \"To-do Item\";
    __PS_MV_REG = [];
    return todoList.map(function (todo, index) {
        var checkboxId = \"todo-check\" + index;
        var labelId = \"todo-label\" + index;
        var trElement3 = createAnElement(parentElement, \"tr\");
        var tdElement4 = createAnElement(trElement3, \"td\");
        var inputElement5 = createAnElement(tdElement4, \"input\");
        setAnAttribute(inputElement5, \"id\", checkboxId.toString());
        setAnAttribute(inputElement5, \"type\", \"checkbox\");
        inputElement5.addEventListener(\"click\", plus.bind(null, \"updateTodo(\", index.toString(), \")\"), false);
        inputElement5.checked = true;
        var inputElement6 = createAnElement(tdElement4, \"input\");
        setAnAttribute(inputElement6, \"id\", checkboxId.toString());
        setAnAttribute(inputElement6, \"type\", \"button\");
        inputElement6.addEventListener(\"click\", updatetodo.bind(null, 123), false);
        var labelElement7 = createAnElement(tdElement4, \"label\");
        setAnAttribute(labelElement7, \"id\", labelId.toString());
        setTextNode(labelElement7, todo);
        __PS_MV_REG = [];
        return true;
    });
};"))
            ;;            (format t "~%***Expected:~%~a~%***~%" expected)
            ;;            (format t "~%***Actual:~%~a~%***~%" actual)

            (string= expected actual)))
        (test-spec :it "work with a todo list and a cleaner way to assign event handlers"
          (let ((actual
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
                                               (jfh::with-html-elements
                                                   (tr
                                                    (td
                                                     (input (id . "todo-check") (type . "checkbox") (onclick . "(+ \"updateTodo(\" (chain index (to-string)) \")\")"))
                                                     (input (id . "test-check") (type . "button") (onclick . "(updateTodo 123)"))
                                                     (label (id . "todo-label") todo)))))

                                             t)))))))
                (expected
                 "function renderTodoList(todoList) {
    var todoListTableBody = document.getElementById(\"todo-list-body\");
    var parentElement = todoListTableBody;
    var columnHeader = document.getElementById(\"todo-list-column-header\");
    var count = todoList.length;
    var usePluralForm = 1 > count || 0 === count;
    clearChildren(parentElement);
    columnHeader.innerText = usePluralForm ? \"To-do Items\" : \"To-do Item\";
    __PS_MV_REG = [];
    return todoList.map(function (todo, index) {
        var checkboxId = \"todo-check\" + index;
        var labelId = \"todo-label\" + index;
        var trElement8 = createAnElement(parentElement, \"tr\");
        var tdElement9 = createAnElement(trElement8, \"td\");
        var inputElement10 = createAnElement(tdElement9, \"input\");
        setAnAttribute(inputElement10, \"id\", \"todo-check\");
        setAnAttribute(inputElement10, \"type\", \"checkbox\");
        inputElement10.addEventListener(\"click\", plus.bind(null, \"updateTodo(\", index.toString(), \")\"), false);
        var inputElement11 = createAnElement(tdElement9, \"input\");
        setAnAttribute(inputElement11, \"id\", \"test-check\");
        setAnAttribute(inputElement11, \"type\", \"button\");
        inputElement11.addEventListener(\"click\", updatetodo.bind(null, 123), false);
        var labelElement12 = createAnElement(tdElement9, \"label\");
        setAnAttribute(labelElement12, \"id\", \"todo-label\");
        setTextNode(labelElement12, todo);
        __PS_MV_REG = [];
        return true;
    });
};"))
            ;; (format t "~%***Expected:~%~a~%***~%" expected)
            ;; (format t "~%***Actual:~%~a~%***~%" actual)
            (string= expected actual)))

        (test-spec :it "eval an expression in a simple table"
          (and
           (setup)
           (let ((actual (ps (defun output-simple-table () (jfh::with-html-elements (table (tr (td "(+ 1 2)")))))))
                 (expected
                  "function outputSimpleTable() {
    var tableElement13 = createAnElement(parentElement, \"table\");
    var trElement14 = createAnElement(tableElement13, \"tr\");
    var tdElement15 = createAnElement(trElement14, \"td\");
    __PS_MV_REG = [];
    return setTextNode(tdElement15, 1 + 2);
};"))
             ;;             (format t "~%***Expected:~%~a~%***~%" expected)
             ;;             (format t "~%***Actual:~%~a~%***~%" actual)
             
             (string= expected actual))))

        (test-spec :it "KNOWN ISSUE - eval a simple span - why do I need to wrap the span in a div???"
          (let ((actual (ps (defun output-simple-span () (jfh::with-html-elements (div (span "Sample Text."))))))
                (expected
                 "function outputSimpleSpan() {
    var divElement16 = createAnElement(parentElement, \"div\");
    var spanElement17 = createAnElement(divElement16, \"span\");
    __PS_MV_REG = [];
    return setTextNode(spanElement17, \"Sample Text.\");
};"))
            ;;            (format t "~%***Expected:~%~a~%***~%" expected)
            ;;            (format t "~%***Actual:~%~a~%***~%" actual)
            
            (string= expected actual)))

        (test-spec :it "call a function if funcall present"
          (let ((actual (ps (defun use-funcall () (jfh::with-html-elements (div (id . "my-div") (funcall #'some-function "my-div" 1 2 3))))))
                (expected
                 "function useFuncall() {
    var divElement18 = createAnElement(parentElement, \"div\");
    setAnAttribute(divElement18, \"id\", \"my-div\");
    __PS_MV_REG = [];
    return someFunction(\"my-div\", 1, 2, 3);
};"))
            ;; (format t "~%***Expected:~%~a~%***~%" expected)
            ;; (format t "~%***Actual:~%~a~%***~%" actual)
            
            (string= expected actual)))))))

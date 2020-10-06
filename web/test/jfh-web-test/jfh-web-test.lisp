;;;; jfh-web-test.lisp

(in-package #:jfh-web-test)

(import-macros-from-lisp 'with-html-elements)

(defun test-with-html-elements ()
  (flet ((setup ()
           (setf *ps-gensym-counter* 0)
           (define-ps-with-html-macro)))
    (setup)
    (test-spec :category "web"
      (test-spec :description "Lisp -> DOM API"
        (test-spec :it "create a simple table"
          (let ((actual (ps (defun output-simple-table () (jfh::with-html-elements (table (tr (td "test")))))))
                (expected
                 "function outputSimpleTable() {
    var tableElement0 = createAnElement(parentElement, 'table');
    var trElement1 = createAnElement(tableElement0, 'tr');
    var tdElement2 = createAnElement(trElement1, 'td');
    __PS_MV_REG = [];
    return setTextNode(tdElement2, 'test');
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
                                                     (input (id . "todo-check") (type . "checkbox") (onclick . "(+ \"updateTodo(\" (chain index (to-string)) \")\")"))
                                                     (input (id . "test-check") (type . "button") (onclick . "(updateTodo 123)"))
                                                     (label (id . "todo-label") todo))))
                                               
                                               (let ((todo-check-box (chain document (get-element-by-id "todo-check")))
                                                     (todo-label (chain document (get-element-by-id "todo-label"))))
                                                 (setf (@ todo-check-box id) checkbox-id
                                                       (@ todo-label id) label-id
                                                       (@ todo-label html-for) checkbox-id)))

                                             t)))))))
                (expected
                 "function renderTodoList(todoList) {
    var todoListTableBody = document.getElementById('todo-list-body');
    var parentElement = todoListTableBody;
    var columnHeader = document.getElementById('todo-list-column-header');
    var count = todoList.length;
    var usePluralForm = 1 > count || 0 === count;
    clearChildren(parentElement);
    columnHeader.innerText = usePluralForm ? 'To-do Items' : 'To-do Item';
    __PS_MV_REG = [];
    return todoList.map(function (todo, index) {
        var checkboxId = 'todo-check' + index;
        var labelId = 'todo-label' + index;
        var trElement3 = createAnElement(parentElement, 'tr');
        var tdElement4 = createAnElement(trElement3, 'td');
        var inputElement5 = createAnElement(tdElement4, 'input');
        setAnAttribute(inputElement5, 'id', 'todo-check');
        setAnAttribute(inputElement5, 'type', 'checkbox');
        parentElementId.addEventListener('click', plus.bind(null, 'updateTodo(', index.toString(), ')'), false);
        var inputElement6 = createAnElement(tdElement4, 'input');
        setAnAttribute(inputElement6, 'id', 'test-check');
        setAnAttribute(inputElement6, 'type', 'button');
        parentElementId.addEventListener('click', updatetodo.bind(null, 123), false);
        var labelElement7 = createAnElement(tdElement4, 'label');
        setAnAttribute(labelElement7, 'id', 'todo-label');
        setTextNode(labelElement7, todo);
        var todoCheckBox = document.getElementById('todo-check');
        var todoLabel = document.getElementById('todo-label');
        todoCheckBox.id = checkboxId;
        todoLabel.id = labelId;
        todoLabel.htmlFor = checkboxId;
        __PS_MV_REG = [];
        return true;
    });
};"))
            ;; (format t "~%***Expected:~%~a~%***~%" expected)
            ;; (format t "~%***Actual:~%~a~%***~%" actual)

            (string= expected actual)))
        ;; next:
        ;; - add logic to parse cons pair value into event handler, if it's a string and the key starts with "on"
        ;; - if a string starts with a (, then assume it's an expression and parse it for evaluation!
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
    var todoListTableBody = document.getElementById('todo-list-body');
    var parentElement = todoListTableBody;
    var columnHeader = document.getElementById('todo-list-column-header');
    var count = todoList.length;
    var usePluralForm = 1 > count || 0 === count;
    clearChildren(parentElement);
    columnHeader.innerText = usePluralForm ? 'To-do Items' : 'To-do Item';
    __PS_MV_REG = [];
    return todoList.map(function (todo, index) {
        var checkboxId = 'todo-check' + index;
        var labelId = 'todo-label' + index;
        var trElement8 = createAnElement(parentElement, 'tr');
        var tdElement9 = createAnElement(trElement8, 'td');
        var inputElement10 = createAnElement(tdElement9, 'input');
        setAnAttribute(inputElement10, 'id', 'todo-check');
        setAnAttribute(inputElement10, 'type', 'checkbox');
        parentElementId.addEventListener('click', plus.bind(null, 'updateTodo(', index.toString(), ')'), false);
        var inputElement11 = createAnElement(tdElement9, 'input');
        setAnAttribute(inputElement11, 'id', 'test-check');
        setAnAttribute(inputElement11, 'type', 'button');
        parentElementId.addEventListener('click', updatetodo.bind(null, 123), false);
        var labelElement12 = createAnElement(tdElement9, 'label');
        setAnAttribute(labelElement12, 'id', 'todo-label');
        setTextNode(labelElement12, todo);
        __PS_MV_REG = [];
        return true;
    });
};"))
            ;; (format t "~%***Expected:~%~a~%***~%" expected)
            ;; (format t "~%***Actual:~%~a~%***~%" actual)
            (string= expected actual)))))))

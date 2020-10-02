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
    var tableElement0 = createAnElement(parentElement, 'TABLE');
    var trElement1 = createAnElement(tableElement0, 'TR');
    var tdElement2 = createAnElement(trElement1, 'TD');
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
        var trElement3 = createAnElement(parentElement, 'TR');
        setAnAttribute(trElement3, 'KEY', 'INDEX');
        var tdElement4 = createAnElement(trElement3, 'TD');
        var inputElement5 = createAnElement(tdElement4, 'INPUT');
        setAnAttribute(inputElement5, 'ID', 'todo-check');
        setAnAttribute(inputElement5, 'TYPE', 'checkbox');
        setAnAttribute(inputElement5, 'ONCLICK', 'updateTodo(' + index.toString() + ')');
        var inputElement6 = createAnElement(tdElement4, 'INPUT');
        setAnAttribute(inputElement6, 'ID', 'test-check');
        setAnAttribute(inputElement6, 'TYPE', 'button');
        setAnAttribute(inputElement6, 'ONCLICK', updatetodo(123));
        var labelElement7 = createAnElement(tdElement4, 'LABEL');
        setAnAttribute(labelElement7, 'ID', 'todo-label');
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
                                                   (tr (key . index)
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
        var trElement8 = createAnElement(parentElement, 'TR');
        setAnAttribute(trElement8, 'KEY', 'INDEX');
        var tdElement9 = createAnElement(trElement8, 'TD');
        var inputElement10 = createAnElement(tdElement9, 'INPUT');
        setAnAttribute(inputElement10, 'ID', 'todo-check');
        setAnAttribute(inputElement10, 'TYPE', 'checkbox');
        setAnAttribute(inputElement10, 'ONCLICK', 'updateTodo(' + index.toString() + ')');
        var inputElement11 = createAnElement(tdElement9, 'INPUT');
        setAnAttribute(inputElement11, 'ID', 'test-check');
        setAnAttribute(inputElement11, 'TYPE', 'button');
        setAnAttribute(inputElement11, 'ONCLICK', updatetodo(123));
        var labelElement12 = createAnElement(tdElement9, 'LABEL');
        setAnAttribute(labelElement12, 'ID', 'todo-label');
        setTextNode(labelElement12, todo);
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
            (string= expected actual)))))))

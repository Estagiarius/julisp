;;; common-lisp-planner/tests/test-todo.lisp
;;;
;;; This file contains unit tests for the ToDo (task management) module (`planner/todo`).
;;; It uses the FiveAM testing framework and the helper macros defined in
;;; `tests/packages.lisp` (e.g., `with-clean-todo-data`) to ensure
;;; tests run in an isolated environment and do not affect actual data files.

(in-package #:planner-app/tests)

;; Define the test suite for the ToDo module.
;; This suite is part of the main `planner-app-test-suite`.
(def-suite todo-suite
  :description "Unit tests for the ToDo module (task management)."
  :in planner-app-test-suite)

;; Switch to the todo-suite for defining tests.
(in-suite todo-suite)

;;; Test Cases

(test test-add-task
  "Test adding new tasks to the todo list."
  (with-clean-todo-data
    (let ((task (planner/todo:add-task :description "Test Task 1"
                                       :due-date (get-universal-time)
                                       :priority 1)))
      (is (not (null task)))
      (is (= (planner/todo:task-id task) 1))
      (is (= (length planner/todo:*tasks*) 1))
      (is (eq (first planner/todo:*tasks*) task))
      (is (string= (planner/todo:task-description (first planner/todo:*tasks*)) "Test Task 1"))
      (is (= (planner/todo:task-priority task) 1))
      (is (eq (planner/todo:task-status task) :pending)))
    
    (with-clean-todo-data
      (let ((task2 (planner/todo:add-task :description "Test Task 2" :priority 2)))
        (is (not (null task2)))
        (is (= (planner/todo:task-id task2) 1)) ; ID should be 1 as it's a new clean context
        (is (= (length planner/todo:*tasks*) 1))))))

(test test-mark-task-completed
  "Test marking tasks as completed."
  (with-clean-todo-data
    (let* ((task (planner/todo:add-task :description "To Complete"))
           (task-id (planner/todo:task-id task)))
      (is (eq (planner/todo:task-status task) :pending))
      (let ((updated-task (planner/todo:mark-task-completed task-id)))
        (is (not (null updated-task)))
        (is (eq (planner/todo:task-status updated-task) :completed))
        ;; Verify in the list
        (let ((refetched-task (planner/todo:find-task task-id)))
          (is (not (null refetched-task)))
          (is (eq (planner/todo:task-status refetched-task) :completed))))
      ;; Test with non-existent ID
      (is (null (planner/todo:mark-task-completed 999))))))

(test test-set-task-priority
  "Test setting the priority of tasks."
  (with-clean-todo-data
    (let* ((task (planner/todo:add-task :description "Priority Test" :priority 1))
           (task-id (planner/todo:task-id task)))
      (is (= (planner/todo:task-priority task) 1))
      (let ((updated-task (planner/todo:set-task-priority task-id 5)))
        (is (not (null updated-task)))
        (is (= (planner/todo:task-priority updated-task) 5))
        ;; Verify in the list
        (let ((refetched-task (planner/todo:find-task task-id)))
          (is (not (null refetched-task)))
          (is (= (planner/todo:task-priority refetched-task) 5))))
      ;; Test with non-existent ID
      (is (null (planner/todo:set-task-priority 999 5)))
      ;; Test with non-integer priority
      (signals simple-warning (planner/todo:set-task-priority task-id "high"))
      (is (= (planner/todo:task-priority (planner/todo:find-task task-id)) 5)) ; Should not change
      )))

(test test-get-pending-tasks-summary
  "Test retrieving a summary of pending (overdue and due today) tasks."
  (with-clean-todo-data
    (let* ((current-time (get-universal-time))
           ;; Helper to get start of day for consistent time reference in tests
           (start-of-today (planner/todo::get-universal-time-for-start-of-day current-time))
           (start-of-yesterday (planner/todo::get-universal-time-for-start-of-day (- current-time (* 24 60 60))))
           (start-of-tomorrow (planner/todo::get-universal-time-for-start-of-day (+ current-time (* 24 60 60)))))

      ;; Setup tasks
      (let ((task-overdue (planner/todo:add-task :description "Overdue Task" :due-date (- start-of-today 3600) :status :pending)) ; Due yesterday
            (task-due-today1 (planner/todo:add-task :description "Due Today 1" :due-date (+ start-of-today 3600) :status :pending)) ; Due today
            (task-due-today2 (planner/todo:add-task :description "Due Today 2 (no time)" :due-date start-of-today :status :pending)) ; Due today at 00:00
            (task-completed-overdue (planner/todo:add-task :description "Completed Overdue" :due-date start-of-yesterday :status :completed))
            (task-future (planner/todo:add-task :description "Future Task" :due-date (+ start-of-tomorrow 3600) :status :pending))
            (task-no-due-date (planner/todo:add-task :description "No Due Date" :status :pending)))
            
        (declare (ignorable task-completed-overdue task-future task-no-due-date))

        (let ((summary (planner/todo:get-pending-tasks-summary)))
          (let ((overdue-list (getf summary :overdue-tasks))
                (due-today-list (getf summary :due-today-tasks)))
            (is (= (length overdue-list) 1) "Checking number of overdue tasks")
            (is (member task-overdue overdue-list :test #'eq) "Checking specific overdue task")

            (is (= (length due-today-list) 2) "Checking number of due today tasks")
            (is (member task-due-today1 due-today-list :test #'eq) "Checking specific due today task 1")
            (is (member task-due-today2 due-today-list :test #'eq) "Checking specific due today task 2 (at 00:00)")
            
            ;; Ensure other tasks are not included
            (is (not (member task-completed-overdue overdue-list)))
            (is (not (member task-completed-overdue due-today-list)))
            (is (not (member task-future overdue-list)))
            (is (not (member task-future due-today-list)))
            (is (not (member task-no-due-date overdue-list)))
            (is (not (member task-no-due-date due-today-list))))))
        
    ;; Test with empty tasks list
    (with-clean-todo-data
        (let ((summary (planner/todo:get-pending-tasks-summary)))
          (is (null (getf summary :overdue-tasks)))
          (is (null (getf summary :due-today-tasks)))))))

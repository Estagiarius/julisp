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

(test test-edit-task
  "Test editing various attributes of tasks."
  (with-clean-todo-data
    (is (string= "Scenario 1: Editing multiple fields" "Scenario 1: Editing multiple fields") "Test edit-task: Full update")
    (let* ((time1 (get-universal-time))
           (time2 (+ time1 3600))
           (task (planner/todo:add-task :description "Initial Task"
                                        :due-date time1
                                        :priority 1
                                        :status :pending
                                        :notes "Initial notes."))
           (task-id (planner/todo:task-id task)))
      (let ((edited-task (planner/todo:edit-task task-id
                                                 :description "Updated Task"
                                                 :due-date time2
                                                 :priority 2
                                                 :status :completed
                                                 :notes "Updated notes.")))
        (is (not (null edited-task)))
        (is (string= (planner/todo:task-description edited-task) "Updated Task"))
        (is (= (planner/todo:task-due-date edited-task) time2))
        (is (= (planner/todo:task-priority edited-task) 2))
        (is (eq (planner/todo:task-status edited-task) :completed))
        (is (string= (planner/todo:task-notes edited-task) "Updated notes.")))
      
      (let ((refetched-task (planner/todo:find-task task-id)))
        (is (not (null refetched-task)))
        (is (string= (planner/todo:task-description refetched-task) "Updated Task"))
        (is (= (planner/todo:task-due-date refetched-task) time2))
        (is (= (planner/todo:task-priority refetched-task) 2))
        (is (eq (planner/todo:task-status refetched-task) :completed))
        (is (string= (planner/todo:task-notes refetched-task) "Updated notes.")))))

  (with-clean-todo-data
    (is (string= "Scenario 2: Partial update" "Scenario 2: Partial update") "Test edit-task: Partial update")
    (let* ((original-due-date (+ (get-universal-time) 7200))
           (original-priority 3)
           (original-notes "Original partial notes.")
           (task (planner/todo:add-task :description "Partial Original Desc"
                                        :due-date original-due-date
                                        :priority original-priority
                                        :status :pending
                                        :notes original-notes))
           (task-id (planner/todo:task-id task)))
      
      (let ((edited-task (planner/todo:edit-task task-id
                                                 :description "Partial New Desc"
                                                 :status :in-progress))) ; Assuming :in-progress is a valid status
        (is (not (null edited-task)))
        (is (string= (planner/todo:task-description edited-task) "Partial New Desc"))
        (is (eq (planner/todo:task-status edited-task) :in-progress))
        ;; Check unchanged fields
        (is (= (planner/todo:task-due-date edited-task) original-due-date))
        (is (= (planner/todo:task-priority edited-task) original-priority))
        (is (string= (planner/todo:task-notes edited-task) original-notes)))
        
      (let ((refetched-task (planner/todo:find-task task-id)))
        (is (not (null refetched-task)))
        (is (string= (planner/todo:task-description refetched-task) "Partial New Desc"))
        (is (eq (planner/todo:task-status refetched-task) :in-progress))
        ;; Check unchanged fields on refetched
        (is (= (planner/todo:task-due-date refetched-task) original-due-date))
        (is (= (planner/todo:task-priority refetched-task) original-priority))
        (is (string= (planner/todo:task-notes refetched-task) original-notes)))))

  (with-clean-todo-data
    (is (string= "Scenario 3: Editing specific field types" "Scenario 3: Editing specific field types") "Test edit-task: Specific field types")
    (let* ((task (planner/todo:add-task :description "Field Type Test" :status :pending :priority 1))
           (task-id (planner/todo:task-id task))
           (new-time (+ (get-universal-time) 10000)))
      ;; Status pending to completed
      (planner/todo:edit-task task-id :status :completed)
      (is (eq (planner/todo:task-status (planner/todo:find-task task-id)) :completed))
      ;; Status completed to pending
      (planner/todo:edit-task task-id :status :pending)
      (is (eq (planner/todo:task-status (planner/todo:find-task task-id)) :pending))
      ;; Priority
      (planner/todo:edit-task task-id :priority 10)
      (is (= (planner/todo:task-priority (planner/todo:find-task task-id)) 10))
      ;; Due-date to new time
      (planner/todo:edit-task task-id :due-date new-time)
      (is (= (planner/todo:task-due-date (planner/todo:find-task task-id)) new-time))
      ;; Due-date to nil
      (planner/todo:edit-task task-id :due-date nil)
      (is (null (planner/todo:task-due-date (planner/todo:find-task task-id))))))

  (with-clean-todo-data
    (is (string= "Scenario 4: Editing non-existent task" "Scenario 4: Editing non-existent task") "Test edit-task: Non-existent ID")
    (is (null (planner/todo:edit-task 999 :description "Doesn't exist")))))

(test test-remove-task
  "Test removing tasks from the todo list."
  (with-clean-todo-data
    (is (string= "Scenario 1: Successful removal" "Scenario 1: Successful removal") "Test remove-task: Successful removal")
    (let* ((task1 (planner/todo:add-task :description "Task to remove"))
           (task2 (planner/todo:add-task :description "Task to keep"))
           (task1-id (planner/todo:task-id task1))
           (task2-id (planner/todo:task-id task2)) ; Store ID to verify presence later
           (initial-count (length planner/todo:*tasks*)))
      (is (= initial-count 2))
      (is (planner/todo:remove-task task1-id))
      (is (= (length planner/todo:*tasks*) (- initial-count 1)))
      (is (null (planner/todo:find-task task1-id)))
      (is (not (null (planner/todo:find-task task2-id)))))) ; Check task2 is still there

  (with-clean-todo-data
    (is (string= "Scenario 2: Removing non-existent task" "Scenario 2: Removing non-existent task") "Test remove-task: Non-existent ID")
    (planner/todo:add-task :description "Some task")
    (let ((initial-count (length planner/todo:*tasks*)))
      (is (not (planner/todo:remove-task 999))) ; remove-task returns nil for non-existent
      (is (= (length planner/todo:*tasks*) initial-count))))))

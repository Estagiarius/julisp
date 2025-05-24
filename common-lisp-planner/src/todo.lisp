;;; common-lisp-planner/src/todo.lisp

(defpackage #:planner/todo
  (:use #:common-lisp #:planner/data-structures #:planner/file-ops)
  (:documentation "Manages tasks for the planner application.
This includes adding, finding, editing, removing, and marking tasks as completed.
It also provides functionality for task prioritization, viewing/filtering tasks,
and generating summaries and statistics related to tasks.
Task data is persisted to a file.")
  (:export #:*tasks*
           #:*tasks-filepath*
           #:save-tasks
           #:load-tasks
           #:generate-task-id
           #:add-task
           #:find-task
           #:edit-task
           #:remove-task
           #:mark-task-completed
           ;; Re-exporting task accessors for convenience
           #:task-id
           #:task-description
           #:task-due-date
           #:task-priority
           #:task-status
           #:task-notes
           ;; New functions
           #:set-task-priority
           #:view-tasks
           ;; Reminder function
           #:get-pending-tasks-summary
           ;; Statistics function
           #:get-task-statistics))

(in-package #:planner/todo)

;;; Global Variables
(defvar *tasks* nil "A list holding all current task objects in memory.
Populated by `load-tasks` and managed by functions like `add-task`, `remove-task`, etc.")
(defvar *tasks-filepath* "common-lisp-planner/data/tasks.dat"
  "The default filepath where task data is persisted.
Used by `save-tasks` and `load-tasks`.")
(defvar *next-task-id* 0 "A counter used by `generate-task-id` for unique integer IDs for new tasks.
Updated by `load-tasks` based on loaded data to prevent ID collisions.")

;;; Helper Functions for Persistence

(defun save-tasks ()
  "Saves the current list of tasks from `*tasks*` to the file specified by `*tasks-filepath*`.
Uses `planner/file-ops:save-data`. Returns `t` on success, `nil` on failure."
  (planner/file-ops:save-data *tasks* *tasks-filepath*))

(defun load-tasks ()
  "Loads tasks from the file specified by `*tasks-filepath*` into `*tasks*`.
If loading fails or the file doesn't exist, `*tasks*` is set to an empty list.
Updates `*next-task-id*` to be greater than the maximum ID found in loaded tasks.
Returns the list of loaded tasks (which is also set to `*tasks*`)."
  (let ((loaded-data (planner/file-ops:load-data *tasks-filepath*)))
    (setf *tasks* (if loaded-data loaded-data nil))
    ;; Attempt to update *next-task-id* to avoid collisions if tasks were loaded
    (when *tasks*
      (let ((max-id 0))
        (dolist (task *tasks*)
          (when (and (task-id task) (numberp (task-id task)))
            (setf max-id (max max-id (task-id task)))))
        (setf *next-task-id* (1+ max-id))))
    *tasks*))

;;; Core Task Management Functions

(defun generate-task-id ()
  "Generates a new unique ID for a task (simple incrementing integer)."
  (incf *next-task-id*))

(defun add-task (&key description due-date priority notes (status :pending))
  "Creates a new task with the provided details, adds it to `*tasks*`, and saves.
- `description`: A string describing the task (required).
- `due-date`: Optional. A universal time integer representing the due date.
- `priority`: Optional. An integer representing task priority (default 0).
- `notes`: Optional. A string for additional notes.
- `status`: Optional. A keyword symbol for task status (default :pending).
Returns the newly created task object."
  (let ((new-task (make-task
                   :id (generate-task-id) ; Assign a new unique ID
                   :description (or description "")
                   :due-date due-date ; Expected to be a universal time integer or nil
                   :priority (or priority 0)
                   :status status
                   :notes (or notes ""))))
    (push new-task *tasks*)
    (save-tasks)
    new-task))

(defun find-task (id)
  "Searches *tasks* for a task with the given id.
   Returns the task object if found, otherwise nil."
  (find id *tasks* :key #'task-id :test #'equal))

(defun edit-task (id &key description due-date priority status notes)
  "Finds a task by its `id` and updates its slots with any non-nil provided arguments.
Changes are saved to the persistent store via `save-tasks`.
Returns the updated task object if found and modified, `nil` otherwise."
  (let ((task-to-edit (find-task id)))
    (when task-to-edit
      ;; Update only provided fields (non-nil)
      (when description (setf (task-description task-to-edit) description))
      (when due-date (setf (task-due-date task-to-edit) due-date))
      (when priority (setf (task-priority task-to-edit) priority))
      (when status (setf (task-status task-to-edit) status))
      (when notes (setf (task-notes task-to-edit) notes))
      (save-tasks)
      task-to-edit)))

(defun remove-task (id)
  "Removes the task with the given id from *tasks*.
   Saves changes and returns t if removed, nil otherwise."
  (let ((original-length (length *tasks*)))
    (setf *tasks* (remove-if (lambda (task) (equal (task-id task) id)) *tasks*))
    (when (< (length *tasks*) original-length)
      (save-tasks)
      t)))

(defun mark-task-completed (id)
  "Finds a task by its `id`, sets its status to `:completed`, and saves changes.
Returns the updated task object if found and marked, `nil` otherwise."
  (let ((task-to-mark (find-task id)))
    (when task-to-mark
      ;; Directly set status to :completed
      (setf (task-status task-to-mark) :completed)
      (save-tasks)
      task-to-mark)))

;;; Initialisation Note:
;;; load-tasks would typically be called by a main application setup function
;;; or when this system/module is loaded. For example:
;;; (load-tasks)
;;; This ensures data is loaded from the file when the system starts.
;;; For now, it needs to be called manually after loading this file
;;; if you want to populate *tasks* from the .dat file.

;; Example of initial load, can be uncommented or called separately
;; (load-tasks)


;;; Helper function for formatting due dates
(defun format-task-due-date (universal-time)
  "Formats a universal time integer into a 'YYYY-MM-DD' string.
If `universal-time` is nil, returns 'No due date'.
This is an internal helper, primarily for `view-tasks`."
  (if universal-time
      (multiple-value-bind (sec min hr day mon yr)
          (decode-universal-time universal-time)
        (declare (ignore sec min hr))
        (format nil "~4,'0D-~2,'0D-~2,'0D" yr mon day))
      "No due date"))

;;; New Exported Functions

(defun set-task-priority (id new-priority)
  "Finds a task by its `id` and updates its `priority`.
`new-priority` must be an integer. Saves changes.
Returns the updated task object if found and priority is valid, `nil` otherwise.
A warning is issued if `new-priority` is not an integer."
  (unless (integerp new-priority)
    (warn "SET-TASK-PRIORITY: new-priority must be an integer, but got ~S." new-priority)
    (return-from set-task-priority nil))
  (let ((task (find-task id)))
    (when task
      (setf (task-priority task) new-priority)
      (save-tasks)
      task)))

(defun view-tasks (&key (sort-by :priority) (filter-status nil) (filter-priority nil))
  "Displays tasks from `*tasks*` with options for filtering and sorting.
- `sort-by`: Keyword indicating sort order. Can be `:priority` (ascending),
             `:due-date` (earliest first, `nil`s last), or `:id` (ascending).
             Defaults to `:priority`. Unrecognized keys result in unsorted list and a warning.
- `filter-status`: Keyword to filter tasks by status (e.g., `:pending`, `:completed`).
- `filter-priority`: Integer to filter tasks by a specific priority level.
Prints tasks to standard output. Returns `t` if tasks were displayed, `nil` otherwise."
  (let ((filtered-tasks *tasks*))

    ;; Apply status filter
    (when filter-status
      (setf filtered-tasks
            (remove-if-not (lambda (task) (eq (task-status task) filter-status))
                           filtered-tasks)))

    ;; Apply priority filter
    (when filter-priority
      (unless (integerp filter-priority)
        (warn "VIEW-TASKS: filter-priority must be an integer, but got ~S. Ignoring priority filter." filter-priority)
        (setf filter-priority nil)) ; Reset if not an integer to avoid error with =
      (when filter-priority
         (setf filtered-tasks
               (remove-if-not (lambda (task) (= (task-priority task) filter-priority))
                              filtered-tasks))))
    
    (unless filtered-tasks
      (format t "No tasks match your criteria.~%")
      (return-from view-tasks nil))

    ;; Apply sorting
    (let ((sorted-tasks filtered-tasks))
      (case sort-by
        (:priority
         (setf sorted-tasks (sort (copy-list filtered-tasks) #'< :key #'task-priority))) ; Sort a copy
        (:due-date
         ;; Sort by due date, nil due dates (represented by most-positive-fixnum) at the end
         (setf sorted-tasks (sort (copy-list filtered-tasks) #'<
                                  :key (lambda (task)
                                         (or (task-due-date task) most-positive-fixnum)))))
        (:id
         (setf sorted-tasks (sort (copy-list filtered-tasks) #'< :key #'task-id)))
        (otherwise ; Including nil or unrecognized sort key
         (unless (member sort-by '(nil :priority :due-date :id)) ; Check if it's a known key or intentionally nil
            (warn "VIEW-TASKS: Unrecognized sort-by key '~A'. Tasks will be displayed in current filtered order." sort-by))
         ;; If sort-by is nil or unrecognized, `sorted-tasks` remains `filtered-tasks`
         ;; which is fine, but we make a copy if no sort was applied to ensure non-destructive behavior if needed later.
         ;; However, since we print and return, direct use of filtered-tasks is okay here if no other sort happens.
         ;; For safety and consistency with sorted branches, let's use a copy if no sort key is applied.
         (setf sorted-tasks (copy-list filtered-tasks))
         ))

      (if sorted-tasks
          (progn
            (format t "~&Tasks (~A matching criteria, sorted by ~A):~%"
                    (length sorted-tasks)
                    (if (member sort-by '(nil :priority :due-date :id)) (or sort-by "default (addition order)") "default (addition order)"))
            (dolist (task sorted-tasks)
              (format t "ID: ~3A | Prio: ~A | Status: ~9A | Due: ~12A | ~A~%"
                      (task-id task)
                      (task-priority task)
                      (string-downcase (symbol-name (task-status task)))
                      (format-task-due-date (task-due-date task))
                      (task-description task)))
            t) ; Return t if tasks were displayed
          (format t "No tasks match your criteria.~%")))) ; This path might be redundant due to earlier check
    nil) ; Default return if no tasks were displayed


;;; Reminder Function for Tasks

(defun get-universal-time-for-start-of-day (universal-time)
  "Internal helper: Returns the universal time for the start of the day (00:00:00)
   for the given `universal-time` (must be an integer).
   Returns `nil` if `universal-time` is not a valid integer."
  (unless (integerp universal-time)
    ;; This function is internal; direct user input is not expected here.
    ;; A simple nil return is fine for internal error propagation.
    (return-from get-universal-time-for-start-of-day nil))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour))
    (encode-universal-time 0 0 0 day month year)))

(defun get-pending-tasks-summary ()
  "Analyzes `*tasks*` and returns a summary of pending tasks.
The summary is a property list containing:
- `:overdue-tasks`: A list of pending tasks whose due date (if an integer) is before the start of the current day.
- `:due-today-tasks`: A list of pending tasks whose due date (if an integer) is on the current day.
Tasks without a valid integer due date or not in :pending status are ignored.
Returns `nil` for a category if no tasks match or if date calculations fail (e.g. start-of-today is nil)."
  (let ((overdue-tasks nil)
        (due-today-tasks nil)
        (current-time (get-universal-time)) ; Not strictly needed here but good for context
        (start-of-today (get-universal-time-for-start-of-day (get-universal-time)))
        (start-of-tomorrow (get-universal-time-for-start-of-day (+ (get-universal-time) (* 24 60 60))))) ; Helper for "today" boundary

    (unless (and start-of-today start-of-tomorrow)
      (warn "GET-PENDING-TASKS-SUMMARY: Could not determine start of today or tomorrow. Summary may be incomplete.")
      ;; Return with empty lists if date boundaries can't be established
      (return-from get-pending-tasks-summary (list :overdue-tasks nil :due-today-tasks nil)))
      
    (dolist (task *tasks*)
      (when (and (eq (task-status task) :pending) ; Only consider pending tasks
                 (task-due-date task)
                 (integerp (task-due-date task))) ; Due date must be a valid universal time
        (let ((due-date (task-due-date task)))
          (cond
            ;; Overdue: due-date is before the start of today
            ((< due-date start-of-today)
             (push task overdue-tasks))
            ;; Due today: due-date is on or after start of today AND before start of tomorrow
            ((and (>= due-date start-of-today)
                  (< due-date start-of-tomorrow))
             (push task due-today-tasks))))))
    
    (list :overdue-tasks (nreverse overdue-tasks) ; nreverse for chronological order if tasks are pushed
          :due-today-tasks (nreverse due-today-tasks))))


;;; Statistics Function

(defun get-task-statistics ()
  "Calculates statistics based on the current list of tasks in `*tasks*`.
Returns a property list containing:
- `:total-tasks`: The total number of tasks.
- `:completed-tasks`: The count of tasks with status `:completed`.
- `:pending-tasks`: The count of tasks with status `:pending`.
  (Note: Other statuses are not explicitly counted but would affect `total-tasks`).
- `:percentage-completed`: The percentage of tasks that are completed.
  Calculated as `(completed-tasks / total-tasks) * 100`. Returns `0.0` if there are no tasks."
  (let ((total-tasks 0)
        (completed-tasks 0)
        (pending-tasks 0))
    (dolist (task *tasks*)
      (incf total-tasks)
      (case (task-status task)
        (:completed (incf completed-tasks))
        (:pending (incf pending-tasks))
        ;; Other statuses (if any) contribute to total_tasks but not to completed/pending explicitly here
        ))
    
    (let ((percentage-completed (if (zerop total-tasks) ; Avoid division by zero
                                    0.0
                                    (float (/ (* completed-tasks 100.0) total-tasks)))))
      (list :total-tasks total-tasks
            :completed-tasks completed-tasks
            :pending-tasks pending-tasks
            :percentage-completed percentage-completed))))

;;; common-lisp-planner/src/todo.lisp

(defpackage #:planner/todo
  (:use #:common-lisp #:planner/data-structures #:planner/file-ops)
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
           #:view-tasks))

(in-package #:planner/todo)

;;; Global Variables
(defvar *tasks* nil "List of task objects currently in memory.")
(defvar *tasks-filepath* "common-lisp-planner/data/tasks.dat" "Filepath for storing task data.")
(defvar *next-task-id* 0 "Counter for generating simple integer task IDs.")

;;; Helper Functions for Persistence

(defun save-tasks ()
  "Saves the current content of *tasks* to *tasks-filepath*."
  (planner/file-ops:save-data *tasks* *tasks-filepath*))

(defun load-tasks ()
  "Loads tasks from *tasks-filepath* and sets *tasks*.
   If loading fails or file not found, *tasks* is set to an empty list.
   Also, it attempts to set *next-task-id* to one greater than the highest
   numeric ID found, if tasks are loaded and have numeric IDs."
  (let ((loaded-data (planner/file-ops:load-data *tasks-filepath*)))
    (setf *tasks* (if loaded-data loaded-data nil))
    ;; Attempt to update *next-task-id* if tasks were loaded
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
  "Creates a new task, adds it to *tasks*, and saves.
   Returns the newly added task object.
   due-date is expected to be a universal time integer or nil.
   priority is expected to be an integer."
  (let ((new-task (make-task
                   :id (generate-task-id)
                   :description (or description "")
                   :due-date due-date ; universal time integer or nil
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
  "Finds a task by id and updates its slots with non-nil provided arguments.
   Saves changes and returns the updated task, or nil if not found."
  (let ((task-to-edit (find-task id)))
    (when task-to-edit
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
  "Finds a task by id and sets its status to :completed.
   Saves changes and returns the updated task, or nil if not found."
  (let ((task-to-mark (find-task id)))
    (when task-to-mark
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
  "Formats a universal time into 'YYYY-MM-DD' string or 'No due date'."
  (if universal-time
      (multiple-value-bind (sec min hr day mon yr)
          (decode-universal-time universal-time)
        (declare (ignore sec min hr))
        (format nil "~4,'0D-~2,'0D-~2,'0D" yr mon day))
      "No due date"))

;;; New Exported Functions

(defun set-task-priority (id new-priority)
  "Finds the task by id, updates its priority, saves, and returns the task.
   Returns nil if task not found or new-priority is not an integer."
  (unless (integerp new-priority)
    (warn "SET-TASK-PRIORITY: new-priority must be an integer.")
    (return-from set-task-priority nil))
  (let ((task (find-task id)))
    (when task
      (setf (task-priority task) new-priority)
      (save-tasks)
      task)))

(defun view-tasks (&key (sort-by :priority) (filter-status nil) (filter-priority nil))
  "Displays tasks, allowing filtering by status and/or priority, and sorting.
   Sort-by can be :priority, :due-date, or :id.
   filter-status can be a keyword like :pending or :completed.
   filter-priority can be an integer."
  (let ((filtered-tasks *tasks*))

    ;; Apply filters
    (when filter-status
      (setf filtered-tasks
            (remove-if-not (lambda (task) (eq (task-status task) filter-status))
                           filtered-tasks)))

    (when filter-priority
      (unless (integerp filter-priority)
        (warn "VIEW-TASKS: filter-priority must be an integer. Ignoring priority filter.")
        (setf filter-priority nil)) ; Reset if not an integer
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
         (setf sorted-tasks (sort filtered-tasks #'< :key #'task-priority)))
        (:due-date
         ;; Sort by due date, nil due dates at the end
         (setf sorted-tasks (sort filtered-tasks #'<
                                  :key (lambda (task)
                                         (or (task-due-date task) most-positive-fixnum)))))
        (:id
         (setf sorted-tasks (sort filtered-tasks #'< :key #'task-id)))
        (otherwise ; Including nil or unrecognized sort key
         (unless (member sort-by '(nil :priority :due-date :id))
            (warn "VIEW-TASKS: Unrecognized sort-by key '~A'. Tasks will be unsorted." sort-by))
         ;; No specific sort, maintain current order (which is reverse chronological of addition)
         ;; or make a copy if `sort` above was destructive and we want to preserve original order.
         ;; For now, if sort-by is nil or not recognized, it uses the filtered-tasks as is.
         ))

      (if sorted-tasks
          (progn
            (format t "~&Tasks (~A filtered, sorted by ~A):~%" (length sorted-tasks) (or sort-by "default order"))
            (dolist (task sorted-tasks)
              (format t "ID: ~3A | Prio: ~A | Status: ~9A | Due: ~12A | ~A~%"
                      (task-id task)
                      (task-priority task)
                      (string-downcase (symbol-name (task-status task)))
                      (format-task-due-date (task-due-date task))
                      (task-description task)))
            t) ; Return t if tasks were displayed
          (format t "No tasks match your criteria.~%"))))
    nil) ; Return nil if no tasks displayed (already handled by unless filtered-tasks, but good for clarity)

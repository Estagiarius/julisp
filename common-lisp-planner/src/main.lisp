;;; common-lisp-planner/src/main.lisp
;;;
;;; IMPORTANT LOAD ORDER:
;;; For the planner to work correctly when loading files manually, ensure the
;;; following files from the 'src/' directory are loaded in approximately this order
;;; BEFORE loading this 'main.lisp' file:
;;; 1. data-structures.lisp
;;; 2. file-ops.lisp
;;; 3. i18n.lisp      (for internationalization)
;;; 4. config.lisp    (for language configuration)
;;; 5. calendar.lisp
;;; 6. todo.lisp
;;; 7. locations.lisp
;;; 8. notes.lisp
;;; 9. materials.lisp
;;;
;;; This main.lisp file, defining the planner-app package, should be loaded after these.
;;; Ideally, use an ASDF system definition for robust dependency management.

(defpackage #:planner-app
  (:use #:common-lisp
        #:planner/data-structures
        #:planner/file-ops
        #:planner/calendar
        #:planner/todo
        #:planner/locations
        #:planner/notes
        #:planner/materials
        #:planner/config  ; Added for language config
        #:planner/i18n)   ; Added for i18n functions
  (:documentation "The main package for the Common Lisp Planner CLI application.
It brings together all modules (calendar, todo, notes, etc.) and provides
the command-line interface (REPL) for user interaction.
Depends on `local-time` for parsing date/time strings from user input.")
  (:import-from #:local-time #:parse-timestring #:timestamp-to-universal #:format-timestring)
  (:export #:start-planner))

(in-package #:planner-app)

;; Define filepaths for language dictionaries
(defvar *lang-en-filepath* "common-lisp-planner/lang/en.lispdata"
  "Filepath for the English language dictionary.")
(defvar *lang-pt-br-filepath* "common-lisp-planner/lang/pt-br.lispdata"
  "Filepath for the Portuguese (Brazil) language dictionary.")

;; Attempt to load local-time at compile/load/execute time.
;; local-time is crucial for parsing date/time strings provided by the user in the CLI.
;; If Quicklisp is not set up or local-time cannot be loaded, a warning is issued.
;; The application might still run but date/time parsing features will likely fail.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case (ql:quickload :local-time :silent t)
    (error (c)
      (warn "Quicklisp or the 'local-time' library not available. Date/time parsing features will be limited or fail. Error: ~A" c)
      ;; Consider defining fallback dummy functions for parse-datetime-string if partial functionality is desired without local-time.
      ;; For now, it will lead to errors when date/time parsing is attempted by command handlers.
      )))

;;; Internationalization Initialization
(defun initialize-internationalization ()
  "Loads language configuration and the corresponding dictionary.
Defaults to English if no configuration or specific dictionary is found."
  (let ((configured-lang (planner/config:load-language-config)))
    (if (and configured-lang 
             (planner/i18n:load-language-dictionary configured-lang 
                                                    (case configured-lang
                                                      (:pt-br *lang-pt-br-filepath*)
                                                      (:en    *lang-en-filepath*)
                                                      ;; Add other languages here
                                                      (t "")))) ; Empty path if lang not mapped
        (planner/i18n:set-current-language configured-lang)
        ;; Else, default to English
        (progn
          (planner/i18n:set-current-language :en)
          (unless (planner/i18n:load-language-dictionary :en *lang-en-filepath*)
            (warn "MAIN: Failed to load default English dictionary. Translations will not work."))))))

;;; Data Initialization
(defun initialize-planner-data ()
  "Loads all planner data from their respective default '.dat' files.
This function calls the 'load-*' functions for events, tasks, locations, notes, and materials.
Prints a message upon completion."
  (planner/calendar:load-events)
  (planner/todo:load-tasks)
  (planner/locations:load-locations)
  (planner/notes:load-notes)
  (planner/materials:load-materials)
  (format t (trs "Planner data initialized.~%")))

;;; Command Parsing
(defun parse-command-line (line)
  "Parses a command line string into a command string and an association list of arguments.
The command is the first word, lowercased.
Arguments can be positional (indexed by their integer position) or keyword-based (e.g., '--title \"My Event\"').
Keyword arguments are converted to keyword symbols (e.g., :TITLE).
Example: \"add-event --title Test --start Now\" -> \"add-event\", ((:TITLE . \"Test\") (:START . \"Now\"))
Example: \"view-day 2023-01-01\" -> \"view-day\", ((0 . \"2023-01-01\"))"
  (let* ((parts (loop for item in (uiop:split-string line :separator " ") ; Requires uiop for split-string
                      when (plusp (length item)) collect item)) ; Filter out empty strings
         (command (if parts (string-downcase (first parts)) ""))
         (args nil)
         (raw-args (rest parts))
         (i 0))
    (loop while (< i (length raw-args))
          do (let ((current (nth i raw-args)))
               (if (and (>= (length current) 2) (string= (subseq current 0 2) "--"))
                   (progn
                     (when (< (1+ i) (length raw-args))
                       (push (cons (intern (string-upcase (subseq current 2)) :keyword)
                                   (nth (1+ i) raw-args))
                             args)
                       (incf i 2)) ; Consume key and value
                     (incf i)) ; Consume only key if no value
                   (progn
                     (push (cons i current) args) ; Store positional arg with its index as key
                     (incf i)))))
    (values command (nreverse args))))


;;; Date/Time Parsing Helper
(defun parse-datetime-string (datetime-str)
  "Parses a datetime string into a universal time integer.
Supported formats primarily include 'YYYY-MM-DD HH:MM' or 'YYYY-MM-DD'.
Relies on the `local-time` library's `parse-timestring` and `timestamp-to-universal`.
Returns the universal time as an integer, or NIL if `datetime-str` is nil, empty, or parsing fails.
A warning is issued by `local-time` or this function upon parsing failure."
  (when (or (null datetime-str) (string= datetime-str ""))
    (return-from parse-datetime-string nil))
  (handler-case
      (timestamp-to-universal ; Convert local-time timestamp to universal time
       (parse-timestring datetime-str :fail-on-error t)) ; `:fail-on-error t` makes it signal an error
    (local-time:timestamp-parse-error (e) ; Specifically catch local-time errors
      (format t (trs "Error parsing date/time string '~A': ~A~%") datetime-str e)
      nil)
    (error (e) ; Catch other potential errors (e.g., if local-time is not loaded)
      (warn "PARSE-DATETIME-STRING: An unexpected error occurred while parsing '~A': ~A" datetime-str e)
      nil)))

(defun parse-integer-arg (arg-string &optional arg-name)
  "Safely parses an argument string into an integer.
If parsing fails, prints an error message including `arg-name` (if provided) and returns NIL."
  (handler-case (parse-integer arg-string)
    (error (e)
      (format t (trs "Error: Invalid integer value for ~A: '~A'. Details: ~A~%")
              (or arg-name "argument") arg-string e)
      nil)))

(defun get-arg (args key &optional default)
  "Retrieves a value from the parsed arguments list `args` (an alist).
`key` can be an integer for positional arguments or a keyword symbol for keyword arguments.
Returns the argument value if found, otherwise `default` (which is `nil` if not provided)."
  (let ((pair (assoc key args)))
    (if pair (cdr pair) default)))


;;; Command Handler Functions

(defun handle-help ()
  "Displays a help message listing all available CLI commands and their basic syntax."
  (format t (trs "~&Available commands:~%"))
  (format t (trs "  add-event --title <title> --start <YYYY-MM-DD HH:MM> [--end <YYYY-MM-DD HH:MM>] [--desc <desc>] [--loc-id <id>]~%"))
  (format t (trs "  view-day <YYYY-MM-DD>~%"))
  (format t (trs "  view-week <YYYY-MM-DD> (shows week containing this date)~%"))
  (format t (trs "  view-month <YYYY> <MM>~%"))
  (format t (trs "  upcoming-events [--hours <N>] (default 24h)~%"))
  (format t (trs "  add-task --desc <desc> [--due <YYYY-MM-DD>] [--prio <N>] [--notes <notes>]~%"))
  (format t (trs "  view-tasks [--sort-by <id|due-date|priority>] [--filter-status <pending|completed>] [--filter-prio <N>]~%"))
  (format t (trs "  edit-task --id <id> [--desc <desc>] [--due <date/time>] [--prio <prio>] [--notes <notes>] [--status <status>]~%"))
  (format t (trs "  remove-task --id <id>~%"))
  (format t (trs "  complete-task <id>~%"))
  (format t (trs "  set-task-prio <id> <priority>~%"))
  (format t (trs "  pending-tasks~%"))
  (format t (trs "  task-stats~%"))
  (format t (trs "  add-location --name <name> [--addr <address>] [--desc <desc>]~%"))
  (format t (trs "  view-locations~%"))
  (format t (trs "  add-note --title <title> --content <content> [--cat <category>]~%"))
  (format t (trs "  view-notes [--cat <category>] [--search <term>]~%"))
  (format t (trs "  cat-note <id> <category>~%"))
  (format t (trs "  add-material --name <name> --path <path> [--cat <category>]~%"))
  (format t (trs "  view-materials [--cat <category>]~%"))
  (format t (trs "  event-counts [--past-days <N>] [--future-days <N>]~%"))
  (format t (trs "  edit-event --id <id> [--title <title>] [--start <date/time>] [--end <date/time>] [--desc <desc>] [--loc-id <loc-id>]~%"))
  (format t (trs "  remove-event --id <id>~%"))
  (format t (trs "  set-lang <lang-code>        Set application language (e.g., en, pt-br).~%"))
  (format t (trs "  help~%"))
  (format t (trs "  quit / exit~%")))

(defun handle-quit ()
  "Handles the 'quit' or 'exit' command, terminating the planner application."
  (format t (trs "Exiting planner. Goodbye!~%"))
  (uiop:quit)) ; uiop:quit is a common way to exit Lisp applications.

(defun handle-add-event (args)
  "Handles the 'add-event' command.
Expects arguments via --key value pairs:
  --title (required): Title of the event.
  --start (required): Start date/time (YYYY-MM-DD or YYYY-MM-DD HH:MM).
  --end (optional): End date/time.
  --desc (optional): Description of the event.
  --loc-id (optional): Integer ID of an existing location.
Prints success or error messages."
  (let ((title (get-arg args :title))
        (start-str (get-arg args :start-time)) ; Consistent with planner/calendar slot names
        (end-str (get-arg args :end-time))
        (description (get-arg args :description))
        (loc-id-str (get-arg args :location-id)))
    (unless title
      (format t (trs "Error: --title is required for add-event.~%"))
      (return-from handle-add-event))
    (unless start-str
      (format t (trs "Error: --start is required for add-event (YYYY-MM-DD HH:MM).~%"))
      (return-from handle-add-event))
    
    (let ((start-time (parse-datetime-string start-str))
          (end-time (parse-datetime-string end-str))
          (loc-id (when loc-id-str (parse-integer-arg loc-id-str "location ID"))))
      (unless start-time
        (format t (trs "Error: Invalid start time format for add-event.~%")) ; parse-datetime-string also prints its own
        (return-from handle-add-event))
      (if (and loc-id-str (null loc-id)) ; only error if loc-id-str was provided but failed to parse
          (return-from handle-add-event)) ; parse-integer-arg prints its own

      (let ((event (planner/calendar:add-event :title title
                                               :start-time start-time
                                               :end-time end-time
                                               :description description
                                               :location-id loc-id)))
        (if event
            (format t (trs "Event added with ID ~A.~%") (planner/calendar:event-id event))
            (format t (trs "Error: Could not add event.~%")))))))

(defun handle-set-lang (args)
  "Handles the 'set-lang' command to change the application language."
  (let ((lang-code-str (get-arg args 0)))
    (unless lang-code-str
      (format t (trs "Error: Language code required for set-lang (e.g., en, pt-br).~%"))
      (return-from handle-set-lang))

    (let ((lang-code-keyword (cond
                               ((string-equal (string-downcase lang-code-str) "en") :en)
                               ((string-equal (string-downcase lang-code-str) "pt-br") :pt-br)
                               (t nil))))
      (unless lang-code-keyword
        (format t (trs "Error: Unsupported language code '~A'. Supported codes are 'en', 'pt-br'.~%") lang-code-str)
        (return-from handle-set-lang))

      ;; Determine filepath based on keyword
      (let* ((target-filepath (case lang-code-keyword
                                (:en *lang-en-filepath*)
                                (:pt-br *lang-pt-br-filepath*)
                                (t ""))) ; Should not happen due to previous check, but good for robustness
             (old-lang (planner/i18n:get-current-language)) ; Store old language for revert
             (old-lang-filepath (case old-lang
                                  (:en *lang-en-filepath*)
                                  (:pt-br *lang-pt-br-filepath*)
                                  (t ""))))

        (if (planner/i18n:load-language-dictionary lang-code-keyword target-filepath)
            (progn
              (planner/i18n:set-current-language lang-code-keyword)
              (if (planner/config:save-language-config lang-code-keyword)
                  ;; Key for trs: "Language set to ~A.~%"
                  (format t (trs "Language set to ~A.~%") lang-code-str) 
                  (progn
                    ;; Key for trs: "Error: Could not save language preference.~%"
                    (format t (trs "Error: Could not save language preference.~%"))
                    ;; Revert language setting if save failed
                    (planner/i18n:set-current-language old-lang)
                    ;; Attempt to reload old dictionary
                    (unless (planner/i18n:load-language-dictionary old-lang old-lang-filepath)
                      (warn "SET-LANG: Failed to reload original dictionary for ~A after config save failure." old-lang)))))
            (progn
              ;; Key for trs: "Error: Could not load dictionary for language '~A'. Language not changed.~%"
              (format t (trs "Error: Could not load dictionary for language '~A'. Language not changed.~%") lang-code-str)
              ;; Ensure current language and its dictionary are still correctly loaded if target failed
              ;; This part might be redundant if load-language-dictionary doesn't change *current-language* on failure,
              ;; but it's a safeguard.
              (unless (eq old-lang (planner/i18n:get-current-language))
                 (planner/i18n:set-current-language old-lang))
              (unless (planner/i18n:load-language-dictionary old-lang old-lang-filepath)
                 (warn "SET-LANG: Failed to reload original dictionary for ~A after new dictionary load failure." old-lang)))))))))

(defun handle-edit-event (args)
  "Handles the 'edit-event --id <id> [--title ...] ...' command.
Updates an existing event's attributes. At least one optional attribute must be provided.
Required:
  --id <id>: Integer ID of the event to edit.
Optional:
  --title <title>: New title.
  --start <YYYY-MM-DD HH:MM>: New start date/time.
  --end <YYYY-MM-DD HH:MM>: New end date/time.
  --desc <description>: New description.
  --loc-id <location-id>: New integer ID of an existing location.
Prints success or error messages."
  (let ((id-str (get-arg args :id))
        (event-id nil)
        (call-args (list))
        (changes-made nil)) ; Flag to check if any valid change was provided

    (unless id-str
      (format t (trs "Error: --id is required for edit-event.~%"))
      (return-from handle-edit-event))
    (setf event-id (parse-integer-arg id-str "event ID"))
    (unless event-id
      (return-from handle-edit-event)) ; parse-integer-arg prints its own error

    ;; Optional arguments
    (when-let ((title (get-arg args :title)))
      (setf call-args (list* :title title call-args) changes-made t))

    (when-let ((start-str (get-arg args :start)))
      (let ((start-time (parse-datetime-string start-str)))
        (if start-time
            (setf call-args (list* :start-time start-time call-args) changes-made t)
            (progn (format t (trs "Error: Invalid --start date/time format. Event not updated.~%")) ; parse-datetime-string also prints
                   (return-from handle-edit-event)))))

    (when-let ((end-str (get-arg args :end)))
      (let ((end-time (parse-datetime-string end-str)))
        (if end-time
            (setf call-args (list* :end-time end-time call-args) changes-made t)
            (progn (format t (trs "Error: Invalid --end date/time format. Event not updated.~%")) ; parse-datetime-string also prints
                   (return-from handle-edit-event)))))

    (when-let ((description (get-arg args :desc))) ; :desc for input, :description for backend
      (setf call-args (list* :description description call-args) changes-made t))

    (when-let ((loc-id-str (get-arg args :loc-id)))
      (let ((loc-id (parse-integer-arg loc-id-str "location ID")))
        (if loc-id
            (setf call-args (list* :location-id loc-id call-args) changes-made t)
            (progn (format t (trs "Error: Invalid --loc-id format. Event not updated.~%")) ; parse-integer-arg also prints
                   (return-from handle-edit-event)))))
    
    (unless changes-made
      (format t (trs "Error: No valid fields provided to update for event ~A.~%") event-id)
      (return-from handle-edit-event))

    (if (apply #'planner/calendar:edit-event event-id (nreverse call-args))
        (format t (trs "Event ~A updated successfully.~%") event-id)
        (format t (trs "Event ~A not found or not updated (it might already have the specified values or ID is invalid).~%") event-id))))

(defun handle-remove-event (args)
  "Handles the 'remove-event --id <id>' command.
Removes an event by its ID.
Required:
  --id <id>: Integer ID of the event to remove.
Prints success or error messages."
  (let ((id-str (get-arg args :id)))
    (unless id-str
      (format t (trs "Error: --id is required for remove-event.~%"))
      (return-from handle-remove-event))
    (let ((event-id (parse-integer-arg id-str "event ID")))
      (unless event-id
        (return-from handle-remove-event)) ; parse-integer-arg prints its own error

      (if (planner/calendar:remove-event event-id)
          (format t (trs "Event ~A removed successfully.~%") event-id)
          (format t (trs "Event ~A not found or could not be removed.~%") event-id)))))

(defun handle-add-task (args)
  "Handles the 'add-task' command.
Expects arguments via --key value pairs:
  --desc (required): Description of the task.
  --due (optional): Due date (YYYY-MM-DD or YYYY-MM-DD HH:MM).
  --prio (optional): Integer priority (default 0).
  --notes (optional): Additional notes for the task.
  --status (optional): Task status (e.g., 'pending', 'completed'), defaults to :pending.
Prints success or error messages."
  (let ((description (get-arg args :desc))
        (due-date-str (get-arg args :due))
        (priority-str (get-arg args :prio))
        (notes (get-arg args :notes))
        (status-str (get-arg args :status)))

    (unless description
      (format t (trs "Error: --desc (description) is required for add-task.~%"))
      (return-from handle-add-task))

    (let ((due-date (parse-datetime-string due-date-str))
          (priority (if priority-str (parse-integer-arg priority-str "priority") 0)) ; Default prio 0
          (status (if status-str (intern (string-upcase status-str) :keyword) :pending))) ; Default status :pending

      (when (and priority-str (null priority)) ; Error if priority string provided but failed parse
        (return-from handle-add-task)) ; parse-integer-arg prints its own
      
      (when (and due-date-str (null due-date)) ; Error if due date string provided but failed parse
        (format t (trs "Error: Invalid due date format. Task not added.~%")) ; parse-datetime-string also prints
        (return-from handle-add-task))

      (let ((task (planner/todo:add-task :description description
                                         :due-date due-date
                                         :priority priority
                                         :notes notes
                                         :status status)))
        (if task
            (format t (trs "Task added with ID ~A.~%") (planner/todo:task-id task))
            (format t (trs "Error: Could not add task.~%"))))))

(defun handle-edit-task (args)
  "Handles the 'edit-task --id <id> [--desc ...] ...' command.
Updates an existing task's attributes. At least one optional attribute must be provided.
Required:
  --id <id>: Integer ID of the task to edit.
Optional:
  --desc <description>: New description.
  --due <YYYY-MM-DD HH:MM>: New due date/time.
  --prio <priority>: New integer priority.
  --notes <notes>: New notes.
  --status <status>: New status (e.g., 'pending', 'completed').
Prints success or error messages."
  (let ((id-str (get-arg args :id))
        (task-id nil)
        (call-args (list))
        (changes-made nil))

    (unless id-str
      (format t (trs "Error: --id is required for edit-task.~%"))
      (return-from handle-edit-task))
    (setf task-id (parse-integer-arg id-str "task ID"))
    (unless task-id
      (return-from handle-edit-task)) ; parse-integer-arg prints its own error

    ;; Optional arguments
    (when-let ((description (get-arg args :desc)))
      (setf call-args (list* :description description call-args) changes-made t))

    (when-let ((due-str (get-arg args :due)))
      (let ((due-date (parse-datetime-string due-str)))
        (if due-date
            (setf call-args (list* :due-date due-date call-args) changes-made t)
            (progn (format t (trs "Error: Invalid --due date/time format. Task not updated.~%")) ; parse-datetime-string also prints
                   (return-from handle-edit-task)))))

    (when-let ((prio-str (get-arg args :prio)))
      (let ((priority (parse-integer-arg prio-str "priority")))
        (if priority
            (setf call-args (list* :priority priority call-args) changes-made t)
            (progn (format t (trs "Error: Invalid --prio format. Task not updated.~%")) ; parse-integer-arg also prints
                   (return-from handle-edit-task)))))
    
    (when-let ((notes (get-arg args :notes)))
      (setf call-args (list* :notes notes call-args) changes-made t))

    (when-let ((status-str (get-arg args :status)))
      (let ((lower-status-str (string-downcase status-str)))
        (if (or (string= lower-status-str "pending") (string= lower-status-str "completed"))
            (let ((status-keyword (intern (string-upcase status-str) :keyword)))
              (setf call-args (list* :status status-keyword call-args) changes-made t))
            (progn
              (format t (trs "Error: Invalid status value '~A'. Must be 'pending' or 'completed'. Task not updated.~%") status-str)
              (return-from handle-edit-task)))))
            
    (unless changes-made
      (format t (trs "Error: No valid fields provided to update for task ~A.~%") task-id)
      (return-from handle-edit-task))

    (if (apply #'planner/todo:edit-task task-id (nreverse call-args))
        (format t (trs "Task ~A updated successfully.~%") task-id)
        (format t (trs "Task ~A not found or not updated (it might already have the specified values or ID is invalid).~%") task-id))))

(defun handle-remove-task (args)
  "Handles the 'remove-task --id <id>' command.
Removes a task by its ID.
Required:
  --id <id>: Integer ID of the task to remove.
Prints success or error messages."
  (let ((id-str (get-arg args :id)))
    (unless id-str
      (format t (trs "Error: --id is required for remove-task.~%"))
      (return-from handle-remove-task))
    (let ((task-id (parse-integer-arg id-str "task ID")))
      (unless task-id
        (return-from handle-remove-task)) ; parse-integer-arg prints its own error

      (if (planner/todo:remove-task task-id)
          (format t (trs "Task ~A removed successfully.~%") task-id)
          (format t (trs "Task ~A not found or could not be removed.~%") task-id)))))

(defun handle-view-tasks (args)
  "Handles the 'view-tasks' command.
Supports optional filtering and sorting:
  --sort-by <id|due-date|priority> (default: priority)
  --filter-status <pending|completed> (optional)
  --filter-prio <integer> (optional)
Calls `planner/todo:view-tasks` with the processed arguments."
  (let ((sort-by-str (get-arg args :sort-by "priority"))
        (filter-status-str (get-arg args :filter-status))
        (filter-priority-str (get-arg args :filter-prio)))

    (let ((sort-by (intern (string-upcase sort-by-str) :keyword))
          (filter-status (when filter-status-str (intern (string-upcase filter-status-str) :keyword)))
          (filter-priority (when filter-priority-str (parse-integer-arg filter-priority-str "filter priority"))))
      
      (when (and filter-priority-str (null filter-priority)) ; Error if provided but failed parse
        (return-from handle-view-tasks)) 

      (planner/todo:view-tasks :sort-by sort-by
                               :filter-status filter-status
                               :filter-priority filter-priority))))

(defun handle-mark-task-completed (args)
  "Handles the 'complete-task <id>' command.
Expects one positional argument: the integer ID of the task to mark as completed."
  (let ((id-str (get-arg args 0)))
    (unless id-str
      (format t (trs "Error: Task ID is required.~%"))
      (return-from handle-mark-task-completed))
    (let ((id (parse-integer-arg id-str "task ID")))
      (when id
        (if (planner/todo:mark-task-completed id)
            (format t (trs "Task ~A marked as completed.~%") id)
            (format t (trs "Error: Could not mark task ~A as completed. Ensure ID is valid.~%") id))))))

(defun handle-set-task-priority (args)
  "Handles the 'set-task-prio <id> <priority>' command.
Expects two positional arguments:
  1. Task ID (integer).
  2. New priority (integer)."
  (let ((id-str (get-arg args 0))
        (priority-str (get-arg args 1)))
    (unless id-str
      (format t (trs "Error: Task ID is required.~%"))
      (return-from handle-set-task-priority))
    (unless priority-str
      (format t (trs "Error: New priority is required.~%"))
      (return-from handle-set-task-priority))
    
    (let ((id (parse-integer-arg id-str "task ID"))
          (priority (parse-integer-arg priority-str "priority")))
      (when (and id priority) ; Both must parse successfully
        (if (planner/todo:set-task-priority id priority)
            (format t (trs "Task ~A priority set to ~A.~%") id priority)
            (format t (trs "Error: Could not set priority for task ~A. Ensure ID is valid and priority is an integer.~%") id))))))

(defun handle-get-pending-tasks-summary (args)
  "Handles the 'pending-tasks' command.
Displays a summary of overdue and due-today tasks by calling `planner/todo:get-pending-tasks-summary`."
  (declare (ignore args)) ; This command takes no arguments
  (let ((summary (planner/todo:get-pending-tasks-summary)))
    (format t (trs "Pending Task Summary:~%"))
    (let ((overdue (getf summary :overdue-tasks))
          (due-today (getf summary :due-today-tasks)))
      (if overdue
          (progn
            (format t (trs "  Overdue Tasks (~A):~%") (length overdue))
            (dolist (task overdue)
              (format t (trs "    ID: ~A, Due: ~A, Desc: ~A~%")
                      (planner/todo:task-id task) 
                      (planner/todo::format-task-due-date (planner/todo:task-due-date task))
                      (planner/todo:task-description task))))
          (format t (trs "  No overdue tasks.~%")))
      (if due-today
          (progn
            (format t (trs "  Tasks Due Today (~A):~%") (length due-today))
            (dolist (task due-today)
              (format t (trs "    ID: ~A, Prio: ~A, Desc: ~A~%")
                      (planner/todo:task-id task)
                      (planner/todo:task-priority task)
                      (planner/todo:task-description task))))
          (format t (trs "  No tasks due today.~%"))))))

(defun handle-get-task-statistics (args)
  "Handles the 'task-stats' command.
Displays task statistics (total, completed, pending, percentage) by calling `planner/todo:get-task-statistics`."
  (declare (ignore args)) ; This command takes no arguments
  (let ((stats (planner/todo:get-task-statistics)))
    (format t (trs "Task Statistics:~%"))
    (format t (trs "  Total Tasks: ~A~%") (getf stats :total-tasks))
    (format t (trs "  Completed Tasks: ~A~%") (getf stats :completed-tasks))
    (format t (trs "  Pending Tasks: ~A~%") (getf stats :pending-tasks))
    (format t (trs "  Percentage Completed: ~,2F%~%") (getf stats :percentage-completed))))

(defun handle-add-location (args)
  "Handles the 'add-location' command.
Expects arguments via --key value pairs:
  --name (required): Name of the location.
  --addr (optional): Address of the location.
  --desc (optional): Description of the location.
Prints success or error messages."
  (let ((name (get-arg args :name))
        (address (get-arg args :addr))
        (description (get-arg args :desc)))
    (unless name
      (format t (trs "Error: --name is required for add-location.~%"))
      (return-from handle-add-location))
    (let ((location (planner/locations:add-location :name name :address address :description description)))
      (if location
          (format t (trs "Location added with ID ~A.~%") (planner/locations:location-id location))
          (format t (trs "Error: Could not add location (name might be empty or invalid).~%"))))))

(defun handle-view-locations (args)
  "Handles the 'view-locations' command. Takes no arguments.
Calls `planner/locations:view-locations` to display all locations."
  (declare (ignore args))
  (planner/locations:view-locations))

(defun handle-add-note (args)
  "Handles the 'add-note' command.
Expects arguments via --key value pairs:
  --title (required): Title of the note.
  --content (required): Content of the note.
  --cat (optional): Category for the note.
Prints success or error messages."
  (let ((title (get-arg args :title))
        (content (get-arg args :content))
        (category (get-arg args :cat)))
    (unless title
      (format t (trs "Error: --title is required for add-note.~%"))
      (return-from handle-add-note))
    (unless content
      (format t (trs "Error: --content is required for add-note.~%"))
      (return-from handle-add-note))
    (let ((note (planner/notes:add-note :title title :content content :category category)))
      (if note
          (format t (trs "Note added with ID ~A.~%") (planner/notes:note-id note))
          (format t (trs "Error: Could not add note (title or content might be empty).~%"))))))

(defun handle-view-notes (args)
  "Handles the 'view-notes' command.
Supports optional filtering:
  --cat <category> (optional): Filter by category.
  --search <term> (optional): Filter by search term in title or content.
Calls `planner/notes:view-notes` with the processed arguments."
  (let ((category (get-arg args :cat))
        (search-term (get-arg args :search)))
    (planner/notes:view-notes :category category :search-term search-term)))

(defun handle-categorize-note (args)
  "Handles the 'cat-note <id> <category>' command.
Expects two positional arguments:
  1. Note ID (integer).
  2. New category (string or symbol-name)."
  (let ((id-str (get-arg args 0))
        (category (get-arg args 1)))
    (unless id-str
      (format t (trs "Error: Note ID is required.~%"))
      (return-from handle-categorize-note))
    (unless category
      (format t (trs "Error: Category is required.~%"))
      (return-from handle-categorize-note))
    (let ((id (parse-integer-arg id-str "note ID")))
      (when id ; Only proceed if ID parsed correctly
        (if (planner/notes:categorize-note id category)
            (format t (trs "Note ~A category set to ~A.~%") id category)
            (format t (trs "Error: Could not categorize note ~A. Ensure ID is valid.~%") id))))))

(defun handle-add-material (args)
  "Handles the 'add-material' command.
Expects arguments via --key value pairs:
  --name (required): Name of the material/file.
  --path (required): File path or URL to the material.
  --cat (optional): Category for the material.
Prints success or error messages."
  (let ((name (get-arg args :name))
        (path (get-arg args :path))
        (category (get-arg args :cat)))
    (unless name
      (format t (trs "Error: --name is required for add-material.~%"))
      (return-from handle-add-material))
    (unless path
      (format t (trs "Error: --path is required for add-material.~%"))
      (return-from handle-add-material))
    (let ((material (planner/materials:add-material-metadata :name name :file-path path :category category)))
      (if material
          (format t (trs "Material metadata added with ID ~A.~%") (planner/materials:material-id material))
          (format t (trs "Error: Could not add material (name or path might be empty).~%"))))))

(defun handle-view-materials (args)
  "Handles the 'view-materials' command.
Supports optional filtering:
  --cat <category> (optional): Filter by category.
Calls `planner/materials:view-materials` with the processed arguments."
  (let ((category (get-arg args :cat)))
    (planner/materials:view-materials :category category)))

(defun handle-get-event-counts (args)
  "Handles the 'event-counts' command.
Supports optional arguments:
  --past-days <N> (default: 30)
  --future-days <N> (default: 7)
Displays counts of events in the specified past and future periods."
  (let* ((past-days-str (get-arg args :past-days "30"))
         (future-days-str (get-arg args :future-days "7"))
         (past-days (parse-integer-arg past-days-str "past-days"))
         (future-days (parse-integer-arg future-days-str "future-days")))
    
    (when (and (not (null past-days)) (not (null future-days))) ; Both must parse successfully
        (let ((counts (planner/calendar:get-event-counts :past-days past-days :future-days future-days)))
          (format t (trs "Event Counts:~%"))
          (format t (trs "  Events in the past ~A days: ~A~%") past-days (getf counts :events-past-period))
          (format t (trs "  Events in the next ~A days: ~A~%") future-days (getf counts :events-future-period))))))


(defun handle-view-calendar-day (args)
  "Handles the 'view-day <YYYY-MM-DD>' command.
Expects one positional argument: the date string.
Calls `planner/calendar:view-events-for-day`."
  (let ((date-str (get-arg args 0)))
    (unless date-str
      (format t (trs "Error: Date string (YYYY-MM-DD) is required for view-day.~%"))
      (return-from handle-view-calendar-day))
    (let ((universal-time (parse-datetime-string date-str)))
      (if universal-time
          (planner/calendar:view-events-for-day universal-time)
          (format t (trs "Error: Invalid date format '~A'. Please use YYYY-MM-DD.~%") date-str))))) ; parse-datetime-string also prints

(defun handle-view-calendar-week (args)
  "Handles the 'view-week <YYYY-MM-DD>' command.
Expects one positional argument: a date string for any day within the desired week.
Calls `planner/calendar:view-events-for-week`."
  (let ((date-str (get-arg args 0)))
    (unless date-str
      (format t (trs "Error: Date string (YYYY-MM-DD) for a day within the week is required.~%"))
      (return-from handle-view-calendar-week))
    (let ((universal-time (parse-datetime-string date-str)))
      (if universal-time
          (planner/calendar:view-events-for-week universal-time)
          (format t (trs "Error: Invalid date format '~A'. Please use YYYY-MM-DD.~%") date-str))))) ; parse-datetime-string also prints

(defun handle-view-calendar-month (args)
  "Handles the 'view-month <YYYY> <MM>' command.
Expects two positional arguments: year and month (numeric).
Calls `planner/calendar:view-events-for-month`."
  (let ((year-str (get-arg args 0))
        (month-str (get-arg args 1)))
    (unless (and year-str month-str)
      (format t (trs "Error: Year and Month are required for view-month.~%"))
      (return-from handle-view-calendar-month))
    (let ((year (parse-integer-arg year-str "year"))
          (month (parse-integer-arg month-str "month")))
      (when (and year month) ; Both must parse successfully
        (if (and (<= 1 month 12) (> year 0)) ; Basic validation for month and year
            (planner/calendar:view-events-for-month year month)
            (format t (trs "Error: Invalid year or month provided.~%"))))))) ; parse-integer-arg also prints

(defun handle-get-upcoming-events (args)
  "Handles the 'upcoming-events' command.
Supports optional argument:
  --hours <N> (default: 24): Number of hours ahead to check for upcoming events.
Calls `planner/calendar:get-upcoming-events` and displays them."
  (let* ((hours-str (get-arg args :hours "24"))
         (hours (parse-integer-arg hours-str "hours")))
    (when hours ; Only proceed if hours parsed successfully
      (let ((upcoming (planner/calendar:get-upcoming-events :within-hours hours)))
        (if upcoming
            (progn
              (format t (trs "Upcoming events within the next ~A hours:~%") hours)
              (dolist (event upcoming)
                (format t (trs "- ~A: ~A (ID: ~A) [~A]~%") ; Note: this key format is specific
                        (planner/calendar::universal-time-to-time-string (planner/calendar:event-start-time event))
                        (planner/calendar:event-title event)
                        (planner/calendar:event-id event)
                        (planner/calendar::universal-time-to-date-string (planner/calendar:event-start-time event))
                        )))
            (format t (trs "No upcoming events within the next ~A hours.~%") hours))))))

;;; Main REPL
(defun start-planner ()
  "Starts the main command-line interface (REPL) for the planner application.
Initializes data from files, then enters a loop to read and process user commands.
Type 'help' for available commands, 'quit' or 'exit' to terminate."
  (initialize-internationalization) ; Load language settings first
  (initialize-planner-data)
  (format t (trs "~&Welcome to the Common Lisp Planner!~%"))
  (format t (trs "Type 'help' for a list of commands, 'quit' or 'exit' to leave.~%"))
  (loop
    (format t (trs "planner> ")) ; Prompt also needs translation if desired
    (finish-output) ; Ensure prompt is displayed before reading input
    (let ((line (read-line *standard-input* nil :eof)))
      (when (or (eq line :eof) (string-equal line "quit") (string-equal line "exit"))
        (handle-quit) ; handle-quit itself will use trs for its message
        (return))
      (when (plusp (length (string-trim '(#\Space #\Tab) line)))
        (handler-case
            (multiple-value-bind (command cmd-args) (parse-command-line line)
              (cond
                ((string-equal command "help") (handle-help))
                ((string-equal command "add-event") (handle-add-event cmd-args))
                ((string-equal command "view-day") (handle-view-calendar-day cmd-args))
                ((string-equal command "view-week") (handle-view-calendar-week cmd-args))
                ((string-equal command "view-month") (handle-view-calendar-month cmd-args))
                ((string-equal command "upcoming-events") (handle-get-upcoming-events cmd-args))
                ((string-equal command "add-task") (handle-add-task cmd-args))
                ((string-equal command "view-tasks") (handle-view-tasks cmd-args))
                ((string-equal command "edit-task") (handle-edit-task cmd-args))
                ((string-equal command "remove-task") (handle-remove-task cmd-args))
                ((string-equal command "complete-task") (handle-mark-task-completed cmd-args))
                ((string-equal command "set-task-prio") (handle-set-task-priority cmd-args))
                ((string-equal command "pending-tasks") (handle-get-pending-tasks-summary cmd-args))
                ((string-equal command "task-stats") (handle-get-task-statistics cmd-args))
                ((string-equal command "add-location") (handle-add-location cmd-args))
                ((string-equal command "view-locations") (handle-view-locations cmd-args))
                ((string-equal command "add-note") (handle-add-note cmd-args))
                ((string-equal command "view-notes") (handle-view-notes cmd-args))
                ((string-equal command "cat-note") (handle-categorize-note cmd-args))
                ((string-equal command "add-material") (handle-add-material cmd-args))
                ((string-equal command "view-materials") (handle-view-materials cmd-args))
                ((string-equal command "event-counts") (handle-get-event-counts cmd-args))
                ((string-equal command "edit-event") (handle-edit-event cmd-args))
                ((string-equal command "remove-event") (handle-remove-event cmd-args))
                ((string-equal command "set-lang") (handle-set-lang cmd-args))
                ((string-equal command "quit") (handle-quit) (return))
                ((string-equal command "exit") (handle-quit) (return))
                (t (format t (trs "Unknown command: '~A'. Type 'help' for available commands.~%") command))))
          (error (e)
            (format t (trs "An error occurred: ~A~%") e))))))))

;; To run the planner (after loading this file and its dependencies):
;; (planner-app:start-planner)

;; For development, ensure uiop is available for split-string, or use a different split mechanism.
;; Quicklisp typically makes uiop available.

;;; common-lisp-planner/src/calendar.lisp

(defpackage #:planner/calendar
  (:use #:common-lisp #:planner/data-structures #:planner/file-ops)
  (:documentation "Manages calendar events, including adding, finding, editing, removing, and viewing events.
It handles persistence of event data to a file and provides functions for querying events
based on time periods (day, week, month) and for upcoming event reminders.")
  (:export #:*events*
           #:*events-filepath*
           #:save-events
           #:load-events
           #:generate-event-id
           #:add-event
           #:find-event
           #:edit-event
           #:remove-event
           #:event-id ; Re-exporting for convenience if used with planner/calendar package prefix
           #:event-title
           #:event-start-time
           #:event-end-time
           #:event-description
           #:event-location-id
           ;; New view functions
           #:view-events-for-day
           #:view-events-for-week
           #:view-events-for-month
           ;; Reminder function
           #:get-upcoming-events
           ;; Statistics function
           #:get-event-counts))

(in-package #:planner/calendar)

;;; Global Variables
(defvar *events* nil "A list holding all current event objects in memory.
This list is populated by `load-events` and manipulated by functions like `add-event`, `remove-event`, etc.")
(defvar *events-filepath* "common-lisp-planner/data/events.dat"
  "The default filepath where event data is persisted.
Used by `save-events` and `load-events`.")
(defvar *next-event-id* 0 "A counter used by `generate-event-id` to produce unique integer IDs for new events.
It is updated by `load-events` to be greater than the maximum ID found in the loaded data.")

;; Constants for time calculations
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +seconds-in-day+ (* 60 60 24) "Number of seconds in a day."))

;;; Helper Functions for Persistence

(defun save-events ()
  "Saves the current list of events from `*events*` to the file specified by `*events-filepath*`.
Uses `planner/file-ops:save-data` for serialization.
Returns `t` on success, `nil` on failure."
  (planner/file-ops:save-data *events* *events-filepath*))

(defun load-events ()
  "Loads events from the file specified by `*events-filepath*` into the `*events*` list.
If loading fails or the file is not found, `*events*` is initialized to an empty list.
Also, it updates `*next-event-id*` to one greater than the highest numeric ID found among
the loaded events to prevent ID collisions. Returns the list of loaded events (which is also set to `*events*`)."
  (let ((loaded-data (planner/file-ops:load-data *events-filepath*)))
    (setf *events* (if loaded-data loaded-data nil))
    ;; Attempt to update *next-event-id* to avoid collisions if events were loaded
    (when *events*
      (let ((max-id 0))
        (dolist (event *events*)
          (when (and (event-id event) (numberp (event-id event)))
            (setf max-id (max max-id (event-id event)))))
        (setf *next-event-id* (1+ max-id))))
    *events*))

;;; Core Event Management Functions

(defun generate-event-id ()
  "Generates a new unique ID for an event (simple incrementing integer)."
  (incf *next-event-id*))

(defun add-event (&key title start-time end-time description location-id)
  "Creates a new event with the given details, adds it to the `*events*` list, and saves all events.
`start-time` and `end-time` are expected to be universal time integers or nil.
`location-id` can be an ID linking to a location object or nil.
Returns the newly created event object."
  (let ((new-event (make-event
                    :id (generate-event-id) ; Assign a new unique ID
                    :title (or title "")
                    :start-time start-time
                    :end-time end-time
                    :description (or description "")
                    :location-id location-id)))
    (push new-event *events*)
    (save-events)
    new-event))

(defun find-event (id)
  "Searches *events* for an event with the given id.
   Returns the event object if found, otherwise nil."
  (find id *events* :key #'event-id :test #'equal))

(defun edit-event (id &key title start-time end-time description location-id)
  "Finds an event by its `id` and updates its slots with any non-nil provided arguments.
The changes are saved to the persistent store via `save-events`.
Returns the updated event object if found and modified, otherwise `nil` if the event with `id` is not found."
  (let ((event-to-edit (find-event id)))
    (when event-to-edit
      ;; Update only the fields that are provided (non-nil)
      (when title (setf (event-title event-to-edit) title))
      (when start-time (setf (event-start-time event-to-edit) start-time))
      (when end-time (setf (event-end-time event-to-edit) end-time))
      (when description (setf (event-description event-to-edit) description))
      (when location-id (setf (event-location-id event-to-edit) location-id))
      (save-events)
      event-to-edit)))

(defun remove-event (id)
  "Removes the event with the given id from *events*.
   Saves changes and returns t if removed, nil otherwise."
  (let ((original-length (length *events*)))
    (setf *events* (remove-if (lambda (e) (equal (event-id e) id)) *events*))
    (when (< (length *events*) original-length)
      (save-events)
      t)))

;;; Initialisation Note:
;;; load-events would typically be called by a main application setup function
;;; or when this system/module is loaded. For example:
;;; (load-events)
;;; This ensures data is loaded from the file when the system starts.
;;; For now, it needs to be called manually after loading this file
;;; if you want to populate *events* from the .dat file.

;; Example of initial load, can be uncommented or called separately
;; (load-events)


;;; Date/Time Helper Functions (Internal)

(defun universal-time-to-date-string (universal-time)
  "Converts a universal time integer to a 'YYYY-MM-DD' formatted string.
Returns NIL if `universal-time` is not a valid integer."
  (when (and universal-time (integerp universal-time))
    (multiple-value-bind (sec min hr day mon yr)
        (decode-universal-time universal-time)
      (declare (ignore sec min hr))
      (format nil "~4,'0D-~2,'0D-~2,'0D" yr mon day))))

(defun universal-time-to-time-string (universal-time)
  "Converts a universal time integer to an 'HH:MM' formatted string.
Returns NIL if `universal-time` is not a valid integer."
  (when (and universal-time (integerp universal-time))
    (multiple-value-bind (sec min hr)
        (decode-universal-time universal-time)
      (declare (ignore sec))
      (format nil "~2,'0D:~2,'0D" hr min))))

(defun events-on-date (target-universal-time events-list)
  "Filters `events-list` to return only events that occur on the specific date
represented by `target-universal-time`.
`target-universal-time` should be an integer representing a universal time.
This function compares year, month, and day components.
Assumes `event-start-time` for events in `events-list` are universal time integers.
Returns a new list of matching events, or NIL if `target-universal-time` is invalid."
  (unless (integerp target-universal-time)
    (warn "EVENTS-ON-DATE: target-universal-time must be an integer, but got ~S." target-universal-time)
    (return-from events-on-date nil))
  (multiple-value-bind (s m h target-day target-month target-year)
      (decode-universal-time target-universal-time)
    (declare (ignore s m h))
    (loop for event in events-list
          when (and (event-start-time event) (integerp (event-start-time event)))
          do (multiple-value-bind (es em eh event-day event-month event-year)
                 (decode-universal-time (event-start-time event))
               (declare (ignore es em eh))
               (when (and (= event-year target-year)
                          (= event-month target-month)
                          (= event-day target-day))
                 (collect event))))))

(defun get-start-of-day (universal-time)
  "Calculates and returns the universal time for the very start of the day (00:00:00)
for the given `universal-time` (which should be an integer).
Returns NIL if `universal-time` is not a valid integer."
  (unless (integerp universal-time)
    (warn "GET-START-OF-DAY: universal-time must be an integer, but got ~S." universal-time)
    (return-from get-start-of-day nil))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour))
    (encode-universal-time 0 0 0 day month year)))

(defun get-start-of-week (universal-time &optional (week-start-day :monday))
  "Calculates and returns the universal time for the start of the week (00:00:00)
containing the given `universal-time`.
`week-start-day` can be :monday or :sunday.
Common Lisp's `decode-universal-time` returns day-of-week as 0 for Monday, ..., 6 for Sunday.
Returns NIL if `universal-time` is not a valid integer."
  (unless (integerp universal-time)
    (warn "GET-START-OF-WEEK: universal-time must be an integer, but got ~S." universal-time)
    (return-from get-start-of-week nil))
  (multiple-value-bind (second minute hour day month year day-of-week)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour))
    (let ((days-to-subtract (ecase week-start-day
                              (:monday day-of-week)
                              (:sunday (mod (1+ day-of-week) 7)))))
      (encode-universal-time 0 0 0
                             (- day days-to-subtract)
                             month
                             year))))

;;; Exported View Functions

(defun view-events-for-day (date-universal-time)
  "Prints events scheduled for the specific date represented by `date-universal-time` to standard output.
`date-universal-time` is expected to be an integer (universal time).
Output includes event time, title, and ID.
Returns `(values)` (i.e., no specific useful value)."
  (unless (integerp date-universal-time)
    (warn "VIEW-EVENTS-FOR-DAY: date-universal-time must be an integer, but got ~S." date-universal-time)
    (return-from view-events-for-day))
  (let* ((date-str (universal-time-to-date-string date-universal-time))
         (events-for-day (events-on-date date-universal-time *events*)))
    (format t "Events for ~A:~%" date-str)
    (if events-for-day
        (dolist (event events-for-day)
          (format t "- (~A) ~A (ID: ~A)~%"
                  (universal-time-to-time-string (event-start-time event))
                  (event-title event)
                  (event-id event)))
        (format t "No events for ~A.~%" date-str))
    (values)))


;;; Reminder Function

(defun get-upcoming-events (&key (within-hours 24))
  "Filters `*events*` to find events whose `start-time` (must be an integer universal time)
is after the current time and within the specified `within-hours` from the current time.
`within-hours` defaults to 24.
Returns a list of these upcoming event objects, sorted by start time, or `nil` if none are found
or if `within-hours` is invalid."
  (unless (and (numberp within-hours) (plusp within-hours))
    (warn "GET-UPCOMING-EVENTS: within-hours must be a positive number, but got ~S. Defaulting to 24." within-hours)
    (setf within-hours 24))
  (let* ((current-time (get-universal-time))
         (cutoff-time (+ current-time (* within-hours 60 60)))
         (upcoming-events nil))
    (setf upcoming-events
          (loop for event in *events*
                when (and (event-start-time event)
                          (integerp (event-start-time event)) ; Ensure it's a universal time
                          (> (event-start-time event) current-time)
                          (<= (event-start-time event) cutoff-time))
                collect event))
    (if upcoming-events (sort upcoming-events #'< :key #'event-start-time) nil)))


;;; Statistics Function

(defun get-event-counts (&key (past-days 30) (future-days 7))
  "Calculates the number of events within a specified past period and a future period,
relative to the current time.
Events must have an integer `start-time` to be considered.
- `:past-days`: Number of days into the past to check (e.g., 30 for events in the last 30 days).
- `:future-days`: Number of days into the future to check (e.g., 7 for events in the next 7 days).
Invalid (non-negative integer) day parameters default to 0.
Returns a property list like `(:events-past-period count1 :events-future-period count2)`."
  (unless (and (integerp past-days) (>= past-days 0))
    (warn "GET-EVENT-COUNTS: past-days must be a non-negative integer, but got ~S. Defaulting to 0." past-days)
    (setf past-days 0))
  (unless (and (integerp future-days) (>= future-days 0))
    (warn "GET-EVENT-COUNTS: future-days must be a non-negative integer, but got ~S. Defaulting to 0." future-days)
    (setf future-days 0))

  (let* ((current-time (get-universal-time))
         (past-cutoff-time (- current-time (* past-days +seconds-in-day+))) ; Start of the past period
         (future-cutoff-time (+ current-time (* future-days +seconds-in-day+))) ; End of the future period
         (events-past-period 0)
         (events-future-period 0))

    (dolist (event *events*)
      (when (and (event-start-time event) (integerp (event-start-time event)))
        (let ((event-time (event-start-time event)))
          ;; Count past events: event_time is between past_cutoff_time and current_time (exclusive of current_time for past)
          (when (and (>= event-time past-cutoff-time)
                     (< event-time current-time))
            (incf events-past-period))
          ;; Count future events: event_time is on or after current_time and before or on future_cutoff_time
          (when (and (>= event-time current-time)
                     (<= event-time future-cutoff-time))
            (incf events-future-period)))))
    
    (list :events-past-period events-past-period
          :events-future-period events-future-period)))

(defun view-events-for-week (date-universal-time)
  "Prints events for the calendar week that contains the given `date-universal-time`.
The week is considered to start on Monday. `date-universal-time` must be an integer.
Output includes event date, time, title, and ID, sorted by start time.
Returns `(values)`."
  (unless (integerp date-universal-time)
    (warn "VIEW-EVENTS-FOR-WEEK: date-universal-time must be an integer, but got ~S." date-universal-time)
    (return-from view-events-for-week))
  (let* ((start-of-week (get-start-of-week date-universal-time :monday))
         (end-of-week (if start-of-week (+ start-of-week (* 7 +seconds-in-day+)) nil))
         (week-events nil))
    (unless start-of-week
      (warn "VIEW-EVENTS-FOR-WEEK: Could not determine start of week.")
      (return-from view-events-for-week))
      
    (format t "Events for the week starting ~A:~%" (universal-time-to-date-string start-of-week))

    (setf week-events
          (loop for event in *events*
                when (and (event-start-time event) 
                          (integerp (event-start-time event))
                          (>= (event-start-time event) start-of-week)
                          (< (event-start-time event) end-of-week))
                collect event))
    
    (if week-events
        (let ((sorted-events (sort week-events #'< :key #'event-start-time)))
          (dolist (event sorted-events)
            (format t "- [~A] (~A) ~A (ID: ~A)~%"
                    (universal-time-to-date-string (event-start-time event))
                    (universal-time-to-time-string (event-start-time event))
                    (event-title event)
                    (event-id event))))
        (format t "No events this week.~%"))
    (values)))

(defun view-events-for-month (year month)
  "Prints events for the specified `year` and `month`.
`year` and `month` must be integers (e.g., 2023, 1 for January).
Output includes event date, time, title, and ID, sorted by start time.
Returns `(values)`."
  (unless (and (integerp year) (plusp year) (integerp month) (<= 1 month 12))
    (warn "VIEW-EVENTS-FOR-MONTH: year (~S) and month (~S) must be valid positive integers (month 1-12)." year month)
    (return-from view-events-for-month))
  (let ((month-str (format nil "~4,'0D-~2,'0D" year month))
        (month-events nil))
    (format t "Events for ~A:~%" month-str)
    (setf month-events
          (loop for event in *events*
                when (and (event-start-time event) (integerp (event-start-time event)))
                do (multiple-value-bind (sec min hr day event-month event-year)
                       (decode-universal-time (event-start-time event))
                     (declare (ignore sec min hr day))
                     (when (and (= event-year year)
                                (= event-month month))
                       (collect event)))))

    (if month-events
        (let ((sorted-events (sort month-events #'< :key #'event-start-time)))
          (dolist (event sorted-events)
            (format t "- [~A] (~A) ~A (ID: ~A)~%"
                    (universal-time-to-date-string (event-start-time event))
                    (universal-time-to-time-string (event-start-time event))
                    (event-title event)
                    (event-id event))))
        (format t "No events for ~A.~%" month-str))
    (values)))

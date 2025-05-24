;;; common-lisp-planner/src/calendar.lisp

(defpackage #:planner/calendar
  (:use #:common-lisp #:planner/data-structures #:planner/file-ops)
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
           #:view-events-for-month))

(in-package #:planner/calendar)

;;; Global Variables
(defvar *events* nil "List of event objects currently in memory.")
(defvar *events-filepath* "common-lisp-planner/data/events.dat" "Filepath for storing event data.")
(defvar *next-event-id* 0 "Counter for generating simple integer event IDs.")

;; Constants for time calculations
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +seconds-in-day+ (* 60 60 24)))

;;; Helper Functions for Persistence

(defun save-events ()
  "Saves the current content of *events* to *events-filepath*."
  (planner/file-ops:save-data *events* *events-filepath*))

(defun load-events ()
  "Loads events from *events-filepath* and sets *events*.
   If loading fails or file not found, *events* is set to an empty list.
   Also, it attempts to set *next-event-id* to one greater than the highest
   numeric ID found, if events are loaded and have numeric IDs."
  (let ((loaded-data (planner/file-ops:load-data *events-filepath*)))
    (setf *events* (if loaded-data loaded-data nil))
    ;; Attempt to update *next-event-id* if events were loaded
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
  "Creates a new event, adds it to *events*, and saves.
   Returns the newly added event object."
  (let ((new-event (make-event
                    :id (generate-event-id)
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
  "Finds an event by id and updates its slots with non-nil provided arguments.
   Saves changes and returns the updated event, or nil if not found."
  (let ((event-to-edit (find-event id)))
    (when event-to-edit
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
  "Converts a universal time to a YYYY-MM-DD string."
  (when (and universal-time (integerp universal-time))
    (multiple-value-bind (sec min hr day mon yr)
        (decode-universal-time universal-time)
      (declare (ignore sec min hr))
      (format nil "~4,'0D-~2,'0D-~2,'0D" yr mon day))))

(defun universal-time-to-time-string (universal-time)
  "Converts a universal time to an HH:MM string."
  (when (and universal-time (integerp universal-time))
    (multiple-value-bind (sec min hr)
        (decode-universal-time universal-time)
      (declare (ignore sec))
      (format nil "~2,'0D:~2,'0D" hr min))))

(defun events-on-date (target-universal-time events-list)
  "Returns events from events-list that occur on the date of target-universal-time.
   Assumes event-start-time is a universal time integer."
  (unless (integerp target-universal-time)
    (warn "EVENTS-ON-DATE: target-universal-time must be an integer.")
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
  "Returns the universal time for the start of the day (00:00:00) for the given universal-time."
  (unless (integerp universal-time)
    (warn "GET-START-OF-DAY: universal-time must be an integer.")
    (return-from get-start-of-day nil))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour))
    (encode-universal-time 0 0 0 day month year)))

(defun get-start-of-week (universal-time &optional (week-start-day :monday))
  "Returns the universal time for the start of the week (Monday or Sunday).
   DAY-OF-WEEK is 0 for Monday, 1 for Tuesday, ..., 6 for Sunday."
  (unless (integerp universal-time)
    (warn "GET-START-OF-WEEK: universal-time must be an integer.")
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
  "Prints events for the given date (universal time) to standard output."
  (unless (integerp date-universal-time)
    (warn "VIEW-EVENTS-FOR-DAY: date-universal-time must be an integer.")
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

(defun view-events-for-week (date-universal-time)
  "Prints events for the week containing the given date (universal time).
   Week starts on Monday."
  (unless (integerp date-universal-time)
    (warn "VIEW-EVENTS-FOR-WEEK: date-universal-time must be an integer.")
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
  "Prints events for the given year and month."
  (unless (and (integerp year) (plusp year) (integerp month) (<= 1 month 12))
    (warn "VIEW-EVENTS-FOR-MONTH: year and month must be valid positive integers (month 1-12).")
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

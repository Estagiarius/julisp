;;; common-lisp-planner/tests/test-calendar.lisp
;;;
;;; This file contains unit tests for the calendar module (`planner/calendar`).
;;; It uses the FiveAM testing framework and the helper macros defined in
;;; `tests/packages.lisp` (e.g., `with-clean-calendar-data`) to ensure
;;; tests run in an isolated environment and do not affect actual data files.

(in-package #:planner-app/tests)

;; Define the test suite for the Calendar module.
;; This suite is part of the main `planner-app-test-suite`.
(def-suite calendar-suite
  :description "Unit tests for the Calendar module (event management)."
  :in planner-app-test-suite)

;; Switch to the calendar-suite for defining tests.
(in-suite calendar-suite)

;;; Test Cases

(test test-add-event
  "Test adding new events to the calendar."
  (with-clean-calendar-data
    (let ((event (planner/calendar:add-event :title "Test Event 1"
                                             :start-time (get-universal-time)
                                             :description "A test event.")))
      (is (not (null event)))
      (is (= (planner/calendar:event-id event) 1))
      (is (= (length planner/calendar:*events*) 1))
      (is (eq (first planner/calendar:*events*) event))
      (is (string= (planner/calendar:event-title (first planner/calendar:*events*)) "Test Event 1"))
      
      (let ((event2 (planner/calendar:add-event :title "Test Event 2"
                                               :start-time (+ (get-universal-time) 3600))))
        (is (not (null event2)))
        (is (= (planner/calendar:event-id event2) 2)) ; ID should increment
        (is (= (length planner/calendar:*events*) 2))))))

(test test-find-event
  "Test finding events by their ID."
  (with-clean-calendar-data
    (let* ((time1 (+ (get-universal-time) 1000))
           (event1 (planner/calendar:add-event :title "Find Me" :start-time time1)))
      (is (not (null event1)))
      (let ((found-event (planner/calendar:find-event (planner/calendar:event-id event1))))
        (is (eq found-event event1))
        (is (string= (planner/calendar:event-title found-event) "Find Me")))
      (is (null (planner/calendar:find-event 999)))) ; Non-existent ID
    (is (null (planner/calendar:find-event 1)) "Finding in empty list should be nil")))

(test test-edit-event
  "Test editing existing events."
  (with-clean-calendar-data
    (let* ((original-title "Original Title")
           (new-title "Edited Title")
           (original-time (+ (get-universal-time) 2000))
           (new-time (+ (get-universal-time) 4000))
           (event (planner/calendar:add-event :title original-title :start-time original-time)))
      (is (not (null event)))
      (let ((event-id (planner/calendar:event-id event)))
        (let ((edited-event (planner/calendar:edit-event event-id
                                                         :title new-title
                                                         :start-time new-time
                                                         :description "New Description")))
          (is (not (null edited-event)))
          (is (string= (planner/calendar:event-title edited-event) new-title))
          (is (= (planner/calendar:event-start-time edited-event) new-time))
          (is (string= (planner/calendar:event-description edited-event) "New Description")))
        
        ;; Verify changes are persistent in *events*
        (let ((refetched-event (planner/calendar:find-event event-id)))
          (is (not (null refetched-event)))
          (is (string= (planner/calendar:event-title refetched-event) new-title))
          (is (= (planner/calendar:event-start-time refetched-event) new-time))))
      ;; Test editing non-existent event
      (is (null (planner/calendar:edit-event 999 :title "Does not exist"))))))

(test test-remove-event
  "Test removing events from the calendar."
  (with-clean-calendar-data
    (let* ((event1 (planner/calendar:add-event :title "To Be Removed" :start-time (get-universal-time)))
           (event2 (planner/calendar:add-event :title "To Keep" :start-time (+ (get-universal-time) 100)))
           (event1-id (planner/calendar:event-id event1)))
      (is (= (length planner/calendar:*events*) 2))
      (is (planner/calendar:remove-event event1-id))
      (is (= (length planner/calendar:*events*) 1))
      (is (null (planner/calendar:find-event event1-id)))
      (is (not (null (planner/calendar:find-event (planner/calendar:event-id event2)))))
      ;; Test removing non-existent event
      (is (not (planner/calendar:remove-event 999))))))

(test test-get-upcoming-events
  "Test retrieving upcoming events within specified time windows."
  (with-clean-calendar-data
    (let* ((current-time (get-universal-time))
           ;; Event in the past, should not be included
           (event-past (planner/calendar:add-event :title "Past Event" :start-time (- current-time 3600)))
           ;; Event upcoming within 1 hour and default 24 hours
           (event-upcoming-soon (planner/calendar:add-event :title "Upcoming Soon" :start-time (+ current-time 600))) ; In 10 mins
           (event-upcoming-later (planner/calendar:add-event :title "Upcoming Later" :start-time (+ current-time (* 20 3600)))) ; In 20 hours
           (event-far-future (planner/calendar:add-event :title "Far Future" :start-time (+ current-time (* 48 3600))))) ; In 48 hours
      (declare (ignorable event-past event-far-future)) ; May not be picked up by default 24h window

      (let ((upcoming-default (planner/calendar:get-upcoming-events))) ; Default 24 hours
        (is (= (length upcoming-default) 2))
        (is (member event-upcoming-soon upcoming-default :test #'eq))
        (is (member event-upcoming-later upcoming-default :test #'eq)))
      
      (let ((upcoming-1hr (planner/calendar:get-upcoming-events :within-hours 1)))
        (is (= (length upcoming-1hr) 1))
        (is (member event-upcoming-soon upcoming-1hr :test #'eq)))

      ;; Test with string start_time (should be ignored by get-upcoming-events)
      (planner/calendar:add-event :title "String Time Event" :start-time "2023-12-25 10:00:00")
      (let ((upcoming-after-string (planner/calendar:get-upcoming-events)))
        (is (= (length upcoming-after-string) 2)) ; Should still be 2, string time ignored
        (is (not (member-if (lambda (e) (string= (planner/calendar:event-title e) "String Time Event"))
                            upcoming-after-string))))
      
      ;; Test with empty list
      (setf planner/calendar:*events* nil)
      (is (null (planner/calendar:get-upcoming-events))))))

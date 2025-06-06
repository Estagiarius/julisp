;;; common-lisp-planner/src/gui.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Load cl-cffi-gtk library
  (handler-case (ql:quickload :cl-cffi-gtk :silent t)
    (error (c)
      (warn "Failed to load cl-cffi-gtk via Quicklisp: ~A. GUI cannot be started." c)))

  ;; Load the main application logic to access initialize-planner-data
  ;; This assumes gui.lisp and main.lisp are in the same directory (src/)
  ;; A more robust solution would use ASDF.
  (handler-case (load "main.lisp" :if-does-not-exist nil)
    (error (c)
      (warn "Failed to load main.lisp: ~A. Data initialization might fail." c))))

(defpackage #:planner-app/gui
  (:use #:common-lisp #:gtk #:gdk #:glib #:gobject
        #:planner/calendar #:planner/todo #:planner/notes #:planner/data-structures)
  (:import-from #:planner-app #:initialize-planner-data)
  (:export #:start-gui))

(in-package #:planner-app/gui)

;;; Global Variables for GUI elements that need to be accessed/updated
(defvar *main-task-list-store* nil "The GtkListStore for the main task display.")
(defvar *main-task-tree-view* nil "The GtkTreeView for the main task display.")
(defvar *main-event-list-store* nil "The GtkListStore for the event display.")
(defvar *main-event-tree-view* nil "The GtkTreeView for the event display.")
(defvar *main-calendar* nil "The GtkCalendar widget.")
(defvar *main-notes-list-store* nil "The GtkListStore for the notes display.")
(defvar *main-notes-tree-view* nil "The GtkTreeView for the notes display.")


;;; Helper Functions for GUI Formatting & Data Conversion
(defun status-string-to-keyword (status-str)
  "Converts a status string (e.g., \"Pending\") to its corresponding keyword (e.g., :pending)."
  (cond
    ((string-equal status-str "Pending") :pending)
    ((string-equal status-str "Completed") :completed)
    (t :unknown)))

(defun keyword-to-status-string (status-keyword)
  "Converts a status keyword (e.g., :pending) to its display string (e.g., \"Pending\")."
  (if status-keyword
      (string-capitalize (symbol-name status-keyword))
      "N/A"))

(defun format-task-due-date-for-gui (due-date)
  "Formats a universal time (or nil) into 'YYYY-MM-DD' string or 'N/A'."
  (if (and due-date (integerp due-date))
      (multiple-value-bind (sec min hr day mon yr)
          (decode-universal-time due-date)
        (declare (ignore sec min hr))
        (format nil "~4,'0D-~2,'0D-~2,'0D" yr mon day))
      "N/A"))

(defun format-task-status-for-gui (status-symbol)
  "Formats a task status keyword symbol (e.g., :pending) into a display string (e.g., \"Pending\")."
  (if status-symbol
      (string-capitalize (symbol-name status-symbol))
      "N/A"))

(defun format-event-time-for-gui (universal-time)
  "Formats universal time into 'HH:MM' string or 'N/A'."
  (if (and universal-time (integerp universal-time))
      (multiple-value-bind (sec min hr)
          (decode-universal-time universal-time)
        (declare (ignore sec))
        (format nil "~2,'0D:~2,'0D" hr min))
      "N/A"))

(defun universal-time-from-ymd (year month day)
  "Converts year, month, day to a universal time integer (start of day)."
  (encode-universal-time 0 0 0 day month year))

(defun datetime-to-universal (year month day hour minute)
  "Converts year, month (0-indexed from GTK), day, hour, minute to universal time.
   Month is adjusted to be 1-indexed for encode-universal-time."
  (encode-universal-time 0 minute hour day (1+ month) year))

(defun decode-universal-time-to-ymdhm (universal-time)
  "Decodes universal time into year, month (0-indexed for GTK), day, hour, minute.
   Returns multiple values: year, month, day, hour, minute.
   Returns (values nil nil nil nil nil) if universal-time is nil or not an integer."
  (if (and universal-time (integerp universal-time))
      (multiple-value-bind (sec min hr day mth yr)
          (decode-universal-time universal-time)
        (declare (ignore sec))
        (values yr (1- mth) day hr min)) ; GTK month is 0-indexed
      (values nil nil nil nil nil)))

(defun leap-year-p (year)
  "Returns t if year is a leap year, nil otherwise."
  (cond ((zerop (mod year 400)) t)
        ((zerop (mod year 100)) nil)
        ((zerop (mod year 4)) t)
        (t nil)))

(defun days-in-month (year month-1-indexed)
  "Returns the number of days in a given month (1-indexed) and year."
  (case month-1-indexed
    (2 (if (leap-year-p year) 29 28))
    ((4 6 9 11) 30)
    (t 31)))

(defun format-note-creation-date-for-gui (universal-time)
  "Formats a universal time into 'YYYY-MM-DD' string or 'N/A'."
  (if (and universal-time (integerp universal-time))
    (format-task-due-date-for-gui universal-time)
    "N/A"))

(defun format-note-content-snippet (content-string &optional (length 50))
  "Returns a snippet of the content string, truncated to `length` and with newlines replaced."
  (if content-string
      (let ((cleaned-string (remove #\Newline (remove #\Return content-string))))
        (if (> (length cleaned-string) length)
            (concatenate 'string (subseq cleaned-string 0 (- length 3)) "...")
            cleaned-string))
      "N/A"))


;;; Calendar and Event Display Update Functions
(defun update-calendar-marks (calendar-widget)
  "Clears existing marks and marks days with events in the currently displayed month on the calendar."
  (gtk-calendar-clear-marks calendar-widget)
  (multiple-value-bind (year month-0-indexed day-ignored)
      (gtk-calendar-get-date calendar-widget)
    (declare (ignore day-ignored))
    (let ((month-1-indexed (1+ month-0-indexed))
          (num-days (days-in-month year (1+ month-0-indexed))))
      (loop for day from 1 to num-days
            do (let* ((universal-time-for-day (universal-time-from-ymd year month-1-indexed day))
                      (events-on-day (planner/calendar:get-events-for-day universal-time-for-day)))
                 (when events-on-day
                   (gtk-calendar-mark-day calendar-widget day)))))))

(defun update-event-display (event-list-store calendar-widget)
  "Clears and repopulates the event-list-store with events for the selected date on the calendar."
  (gtk-list-store-clear event-list-store)
  (multiple-value-bind (year month day) (gtk-calendar-get-date calendar-widget)
    (let ((events (planner/calendar:get-events-for-day (universal-time-from-ymd year (1+ month) day))))
      (if events
          (dolist (event events)
            (let ((iter (gtk-list-store-append event-list-store)))
              (gtk-list-store-set event-list-store iter
                                  0 (if (event-id event) (princ-to-string (event-id event)) "N/A")
                                  1 (event-title event)
                                  2 (format-event-time-for-gui (event-start-time event))
                                  3 (format-event-time-for-gui (event-end-time event))
                                  4 (event-description event)
                                  5 (if (event-location-id event) (princ-to-string (event-location-id event)) "N/A"))))
          (format t "No events for selected date: ~A-~A-~A~%" year (1+ month) day)))))

;;; Task Display Update Function
(defun populate-task-list-display ()
  "Clears and repopulates the main task list display (*main-task-list-store*) from planner/todo:*tasks*."
  (when *main-task-list-store*
    (gtk-list-store-clear *main-task-list-store*)
    (if planner/todo:*tasks*
        (let ((sorted-tasks (sort (copy-list planner/todo:*tasks*) #'< :key #'task-id)))
          (dolist (task sorted-tasks)
            (let ((iter (gtk-list-store-append *main-task-list-store*)))
              (gtk-list-store-set *main-task-list-store* iter
                                  0 (if (task-id task) (princ-to-string (task-id task)) "N/A")
                                  1 (task-description task)
                                  2 (princ-to-string (task-priority task))
                                  3 (format-task-status-for-gui (task-status task))
                                  4 (format-task-due-date-for-gui (task-due-date task))))))
        (warn "GUI: No tasks found in planner/todo:*tasks* or tasks not loaded for populate-task-list-display.")))
  (values))

;;; Notes Tab Functions
(defun populate-notes-list-display ()
  "Clears and repopulates the main notes list display (*main-notes-list-store*) from planner/notes:*notes*."
  (when *main-notes-list-store*
    (gtk-list-store-clear *main-notes-list-store*)
    (if planner/notes:*notes*
        (let ((sorted-notes (sort (copy-list planner/notes:*notes*) #'< :key #'note-id)))
          (dolist (note sorted-notes)
            (let ((iter (gtk-list-store-append *main-notes-list-store*)))
              (gtk-list-store-set *main-notes-list-store* iter
                                  0 (if (note-id note) (princ-to-string (note-id note)) "N/A")
                                  1 (note-title note)
                                  2 (if (note-category note) (princ-to-string (note-category note)) "N/A")
                                  3 (format-note-creation-date-for-gui (note-creation-date note))
                                  4 (format-note-content-snippet (note-content note))))))
        (warn "GUI: No notes found in planner/notes:*notes* or notes not loaded for populate-notes-list-display.")))
  (values))

(defun show-add-note-dialog (parent-window)
  "Creates and runs the 'Add New Note' dialog."
  (let* ((dialog (make-instance 'gtk-dialog :title "Add New Note" :transient-for parent-window :modal t))
         (content-area (gtk-dialog-content-area dialog))
         (grid (make-instance 'gtk-grid :orientation :vertical :row-spacing 10 :column-spacing 10 :border-width 10)))
    (gtk-dialog-add-buttons dialog "Cancel" :cancel "Add" :ok)
    (let ((title-label (make-instance 'gtk-label :label "Title:"))
          (title-entry (make-instance 'gtk-entry :hexpand t)))
      (gtk-grid-attach grid title-label 0 0 1 1) (gtk-grid-attach grid title-entry 1 0 1 1)
      (let ((cat-label (make-instance 'gtk-label :label "Category (Optional):"))
            (cat-entry (make-instance 'gtk-entry :hexpand t)))
        (gtk-grid-attach grid cat-label 0 1 1 1) (gtk-grid-attach grid cat-entry 1 1 1 1)
        (let* ((content-label (make-instance 'gtk-label :label "Content:"))
               (content-view (make-instance 'gtk-text-view :wrap-mode :word-char :height-request 150))
               (content-scrolled (make-instance 'gtk-scrolled-window :hscrollbar-policy :automatic :vscrollbar-policy :automatic)))
          (gtk-container-add content-scrolled content-view)
          (gtk-grid-attach grid content-label 0 2 1 1) (gtk-grid-attach grid content-scrolled 1 2 1 1)
          (gtk-box-pack-start content-area grid :expand t :fill t :padding 0)
          (gtk-widget-show-all content-area)
          (let ((response (gtk-dialog-run dialog)))
            (when (eq response :ok)
              (let ((title-text (gtk-entry-text title-entry))
                    (cat-text (gtk-entry-text cat-entry))
                    (content-text (let* ((buffer (gtk-text-view-buffer content-view)) (start (gtk-text-buffer-get-start-iter buffer)) (end (gtk-text-buffer-get-end-iter buffer)))
                                    (gtk-text-buffer-get-text buffer start end nil))))
                (if (and title-text (> (length title-text) 0) content-text (> (length content-text) 0))
                    (progn (planner/notes:add-note :title title-text :content content-text :category (if (string= cat-text "") nil cat-text))
                           (populate-notes-list-display))
                    (let ((error-dialog (make-instance 'gtk-message-dialog :transient-for dialog :modal t :message-type :error :buttons :ok :text "Validation Error" :secondary-text "Title and Content cannot be empty.")))
                      (gtk-dialog-run error-dialog) (gtk-widget-destroy error-dialog)))))
            (gtk-widget-destroy dialog)))))))

(defun handle-new-note-button-clicked (button)
  (declare (ignore button))
  (show-add-note-dialog (gtk-widget-toplevel *main-notes-tree-view*)))

;;; Dialogs and handlers for Tasks
(defun show-add-task-dialog (parent-window)
  "Creates and runs the 'Add New Task' dialog."
  (let* ((dialog (make-instance 'gtk-dialog :title "Add New Task" :transient-for parent-window :modal t))
         (content-area (gtk-dialog-content-area dialog))
         (grid (make-instance 'gtk-grid :orientation :vertical :row-spacing 5 :column-spacing 5 :border-width 10)))
    (gtk-dialog-add-buttons dialog "Cancel" :cancel "Add" :ok)
    (let ((desc-label (make-instance 'gtk-label :label "Description:")) (desc-entry (make-instance 'gtk-entry :hexpand t)))
      (gtk-grid-attach grid desc-label 0 0 1 1) (gtk-grid-attach grid desc-entry 1 0 1 1)
      (let ((prio-label (make-instance 'gtk-label :label "Priority (1-5):"))
            (prio-spin (make-instance 'gtk-spin-button :adjustment (make-instance 'gtk-adjustment :value 3 :lower 1 :upper 5 :step-increment 1))))
        (gtk-grid-attach grid prio-label 0 1 1 1) (gtk-grid-attach grid prio-spin 1 1 1 1)
        (let ((due-label (make-instance 'gtk-label :label "Due Date:")) (due-calendar (make-instance 'gtk-calendar)))
          (gtk-grid-attach grid due-label 0 2 1 1) (gtk-grid-attach grid due-calendar 1 2 1 1)
          (let* ((notes-label (make-instance 'gtk-label :label "Notes:"))
                 (notes-view (make-instance 'gtk-text-view :wrap-mode :word-char :height-request 100))
                 (notes-scrolled (make-instance 'gtk-scrolled-window :hscrollbar-policy :automatic :vscrollbar-policy :automatic)))
            (gtk-container-add notes-scrolled notes-view)
            (gtk-grid-attach grid notes-label 0 3 1 1) (gtk-grid-attach grid notes-scrolled 1 3 1 1)
            (let ((status-label (make-instance 'gtk-label :label "Status:")) (status-combo (make-instance 'gtk-combo-box-text)))
              (gtk-combo-box-text-append-text status-combo "Pending") (gtk-combo-box-text-append-text status-combo "Completed")
              (setf (gtk-combo-box-active status-combo) 0)
              (gtk-grid-attach grid status-label 0 4 1 1) (gtk-grid-attach grid status-combo 1 4 1 1)
              (gtk-box-pack-start content-area grid :expand t :fill t :padding 0) (gtk-widget-show-all content-area)
              (let ((response (gtk-dialog-run dialog)))
                (when (eq response :ok)
                  (let* ((desc-text (gtk-entry-text desc-entry)) (prio-val (gtk-spin-button-value-as-int prio-spin))
                         (notes-text (let* ((buffer (gtk-text-view-buffer notes-view)) (start (gtk-text-buffer-get-start-iter buffer)) (end (gtk-text-buffer-get-end-iter buffer)))
                                       (gtk-text-buffer-get-text buffer start end nil)))
                         (status-keyword (status-string-to-keyword (gtk-combo-box-text-active-text status-combo)))
                         (selected-year nil) (selected-month nil) (selected-day nil) (due-date-universal nil))
                    (multiple-value-setq (selected-year selected-month selected-day) (gtk-calendar-get-date due-calendar))
                    (setf due-date-universal (universal-time-from-ymd selected-year (1+ selected-month) selected-day))
                    (planner/todo:add-task :description desc-text :priority prio-val :due-date due-date-universal :notes notes-text :status status-keyword)
                    (populate-task-list-display)))
                (gtk-widget-destroy dialog))))))))))

(defun get-selected-task-id ()
  "Retrieves the ID of the currently selected task in *main-task-tree-view*. Returns the integer ID or NIL."
  (let* ((selection (gtk-tree-view-selection *main-task-tree-view*)) (selected-row (gtk-tree-selection-selected selection)))
    (when selected-row
      (let ((iter selected-row))
        (let ((id-str (gtk-tree-model-get-value *main-task-list-store* iter 0))) (ignore-errors (parse-integer id-str)))))))

(defun show-edit-task-dialog (parent-window task-id)
  "Creates and runs the 'Edit Task' dialog, pre-filled for `task-id`."
  (let ((task (planner/todo:find-task task-id)))
    (unless task
      (let ((error-dialog (make-instance 'gtk-message-dialog :transient-for parent-window :modal t :message-type :error :buttons :ok :text (format nil "Error: Task with ID ~A not found." task-id))))
        (gtk-dialog-run error-dialog) (gtk-widget-destroy error-dialog) (return-from show-edit-task-dialog)))
    (let* ((dialog (make-instance 'gtk-dialog :title (format nil "Edit Task (ID: ~A)" task-id) :transient-for parent-window :modal t))
           (content-area (gtk-dialog-content-area dialog))
           (grid (make-instance 'gtk-grid :orientation :vertical :row-spacing 5 :column-spacing 5 :border-width 10)))
      (gtk-dialog-add-buttons dialog "Cancel" :cancel "Update" :ok)
      (let ((desc-label (make-instance 'gtk-label :label "Description:")) (desc-entry (make-instance 'gtk-entry :hexpand t :text (task-description task))))
        (gtk-grid-attach grid desc-label 0 0 1 1) (gtk-grid-attach grid desc-entry 1 0 1 1)
        (let ((prio-label (make-instance 'gtk-label :label "Priority (1-5):"))
              (prio-spin (make-instance 'gtk-spin-button :adjustment (make-instance 'gtk-adjustment :value (task-priority task) :lower 1 :upper 5 :step-increment 1))))
          (gtk-grid-attach grid prio-label 0 1 1 1) (gtk-grid-attach grid prio-spin 1 1 1 1)
          (let ((due-label (make-instance 'gtk-label :label "Due Date:")) (due-calendar (make-instance 'gtk-calendar)))
            (when (task-due-date task) (multiple-value-bind (s m h day month year) (decode-universal-time (task-due-date task)) (declare (ignore s m h)) (gtk-calendar-select-day due-calendar day (1- month) year)))
            (gtk-grid-attach grid due-label 0 2 1 1) (gtk-grid-attach grid due-calendar 1 2 1 1)
            (let* ((notes-label (make-instance 'gtk-label :label "Notes:"))
                   (notes-view (make-instance 'gtk-text-view :wrap-mode :word-char :height-request 100))
                   (notes-buffer (gtk-text-view-buffer notes-view))
                   (notes-scrolled (make-instance 'gtk-scrolled-window :hscrollbar-policy :automatic :vscrollbar-policy :automatic)))
              (setf (gtk-text-buffer-text notes-buffer) (task-notes task))
              (gtk-container-add notes-scrolled notes-view)
              (gtk-grid-attach grid notes-label 0 3 1 1) (gtk-grid-attach grid notes-scrolled 1 3 1 1)
              (let ((status-label (make-instance 'gtk-label :label "Status:")) (status-combo (make-instance 'gtk-combo-box-text)))
                (gtk-combo-box-text-append-text status-combo "Pending") (gtk-combo-box-text-append-text status-combo "Completed")
                (let ((current-status-str (keyword-to-status-string (task-status task))))
                  (loop for i from 0 below 2 when (string-equal current-status-str (gtk-combo-box-text-get-active-text status-combo)) do (setf (gtk-combo-box-active status-combo) i) (return)
                        finally (gtk-combo-box-text-append-text status-combo current-status-str) (setf (gtk-combo-box-active status-combo) i)))
                (gtk-grid-attach grid status-label 0 4 1 1) (gtk-grid-attach grid status-combo 1 4 1 1)
                (gtk-box-pack-start content-area grid :expand t :fill t :padding 0) (gtk-widget-show-all content-area)
                (let ((response (gtk-dialog-run dialog)))
                  (when (eq response :ok)
                    (let* ((desc-text (gtk-entry-text desc-entry)) (prio-val (gtk-spin-button-value-as-int prio-spin))
                           (notes-text (let* ((buffer (gtk-text-view-buffer notes-view)) (start (gtk-text-buffer-get-start-iter buffer)) (end (gtk-text-buffer-get-end-iter buffer)))
                                         (gtk-text-buffer-get-text buffer start end nil)))
                           (status-keyword (status-string-to-keyword (gtk-combo-box-text-active-text status-combo)))
                           (selected-year nil) (selected-month nil) (selected-day nil) (due-date-universal nil))
                      (multiple-value-setq (selected-year selected-month selected-day) (gtk-calendar-get-date due-calendar))
                      (setf due-date-universal (universal-time-from-ymd selected-year (1+ selected-month) selected-day))
                      (planner/todo:edit-task task-id :description desc-text :priority prio-val :due-date due-date-universal :notes notes-text :status status-keyword)
                      (populate-task-list-display)))
                  (gtk-widget-destroy dialog))))))))))

(defun handle-new-task-button-clicked (button)
  (declare (ignore button))
  (show-add-task-dialog (gtk-widget-toplevel *main-task-tree-view*)))

(defun handle-edit-task-button-clicked (button)
  (declare (ignore button))
  (let ((task-id (get-selected-task-id)))
    (if task-id (show-edit-task-dialog (gtk-widget-toplevel *main-task-tree-view*) task-id)
        (let ((info-dialog (make-instance 'gtk-message-dialog :transient-for (gtk-widget-toplevel *main-task-tree-view*) :modal t :message-type :info :buttons :ok :text "Please select a task to edit.")))
          (gtk-dialog-run info-dialog) (gtk-widget-destroy info-dialog)))))

(defun handle-delete-task-button-clicked (button)
  (declare (ignore button))
  (let ((task-id (get-selected-task-id)))
    (if task-id
        (let* ((parent-win (gtk-widget-toplevel *main-task-tree-view*))
               (confirmation-dialog (make-instance 'gtk-message-dialog :transient-for parent-win :modal t :destroy-with-parent t :message-type :question :buttons :yes-no :text (format nil "Are you sure you want to delete task ID: ~a?" task-id) :secondary-text "This action cannot be undone.")))
          (let ((response (gtk-dialog-run confirmation-dialog))) (gtk-widget-destroy confirmation-dialog)
            (when (eq response :yes) (planner/todo:remove-task task-id) (populate-task-list-display))))
        (let ((info-dialog (make-instance 'gtk-message-dialog :transient-for (gtk-widget-toplevel *main-task-tree-view*) :modal t :message-type :info :buttons :ok :text "Please select a task to delete.")))
          (gtk-dialog-run info-dialog) (gtk-widget-destroy info-dialog)))))

;;; Dialogs and handlers for Events
(defun show-add-event-dialog (parent-window)
  "Creates and runs the 'Add New Event' dialog."
  (let* ((dialog (make-instance 'gtk-dialog :title "Add New Event" :transient-for parent-window :modal t))
         (content-area (gtk-dialog-content-area dialog))
         (grid (make-instance 'gtk-grid :orientation :vertical :row-spacing 10 :column-spacing 10 :border-width 10)))
    (gtk-dialog-add-buttons dialog "Cancel" :cancel "Add" :ok)
    (let ((title-label (make-instance 'gtk-label :label "Title:")) (title-entry (make-instance 'gtk-entry :hexpand t)))
      (gtk-grid-attach grid title-label 0 0 1 1) (gtk-grid-attach grid title-entry 1 0 1 1)
      (let* ((desc-label (make-instance 'gtk-label :label "Description:"))
             (desc-view (make-instance 'gtk-text-view :wrap-mode :word-char :height-request 100))
             (desc-scrolled (make-instance 'gtk-scrolled-window :hscrollbar-policy :automatic :vscrollbar-policy :automatic)))
        (gtk-container-add desc-scrolled desc-view)
        (gtk-grid-attach grid desc-label 0 1 1 1) (gtk-grid-attach grid desc-scrolled 1 1 1 1)
        (let ((start-date-label (make-instance 'gtk-label :label "Start Date:")) (start-date-calendar (make-instance 'gtk-calendar))
              (start-time-label (make-instance 'gtk-label :label "Start Time (HH:MM):")) (start-time-box (make-instance 'gtk-box :orientation :horizontal :spacing 6))
              (start-hour-spin (make-instance 'gtk-spin-button :adjustment (make-instance 'gtk-adjustment :value 12 :lower 0 :upper 23 :step-increment 1)))
              (start-minute-spin (make-instance 'gtk-spin-button :adjustment (make-instance 'gtk-adjustment :value 0 :lower 0 :upper 59 :step-increment 5))))
          (gtk-box-pack-start start-time-box start-hour-spin :expand t :fill t :padding 0) (gtk-box-pack-start start-time-box start-minute-spin :expand t :fill t :padding 0)
          (gtk-grid-attach grid start-date-label 0 2 1 1) (gtk-grid-attach grid start-date-calendar 1 2 1 1)
          (gtk-grid-attach grid start-time-label 0 3 1 1) (gtk-grid-attach grid start-time-box 1 3 1 1)
          (let ((end-date-label (make-instance 'gtk-label :label "End Date:")) (end-date-calendar (make-instance 'gtk-calendar))
                (end-time-label (make-instance 'gtk-label :label "End Time (HH:MM):")) (end-time-box (make-instance 'gtk-box :orientation :horizontal :spacing 6))
                (end-hour-spin (make-instance 'gtk-spin-button :adjustment (make-instance 'gtk-adjustment :value 13 :lower 0 :upper 23 :step-increment 1)))
                (end-minute-spin (make-instance 'gtk-spin-button :adjustment (make-instance 'gtk-adjustment :value 0 :lower 0 :upper 59 :step-increment 5))))
            (multiple-value-bind (y m d) (gtk-calendar-get-date start-date-calendar) (gtk-calendar-select-day end-date-calendar d m y))
            (gtk-box-pack-start end-time-box end-hour-spin :expand t :fill t :padding 0) (gtk-box-pack-start end-time-box end-minute-spin :expand t :fill t :padding 0)
            (gtk-grid-attach grid end-date-label 0 4 1 1) (gtk-grid-attach grid end-date-calendar 1 4 1 1)
            (gtk-grid-attach grid end-time-label 0 5 1 1) (gtk-grid-attach grid end-time-box 1 5 1 1)
            (let ((loc-id-label (make-instance 'gtk-label :label "Location ID (Optional):")) (loc-id-entry (make-instance 'gtk-entry)))
              (gtk-grid-attach grid loc-id-label 0 6 1 1) (gtk-grid-attach grid loc-id-entry 1 6 1 1)
              (gtk-box-pack-start content-area grid :expand t :fill t :padding 0) (gtk-widget-show-all content-area)
              (let ((response (gtk-dialog-run dialog)))
                (when (eq response :ok)
                  (let* ((title-text (gtk-entry-text title-entry))
                         (desc-text (let* ((buffer (gtk-text-view-buffer desc-view)) (start (gtk-text-buffer-get-start-iter buffer)) (end (gtk-text-buffer-get-end-iter buffer))) (gtk-text-buffer-get-text buffer start end nil)))
                         (loc-id-text (gtk-entry-text loc-id-entry)) (loc-id (unless (string= loc-id-text "") (ignore-errors (parse-integer loc-id-text))))
                         (start-y 0) (start-m 0) (start-d 0) (end-y 0) (end-m 0) (end-d 0) (start-universal nil) (end-universal nil))
                    (multiple-value-setq (start-y start-m start-d) (gtk-calendar-get-date start-date-calendar))
                    (setf start-universal (datetime-to-universal start-y start-m start-d (gtk-spin-button-value-as-int start-hour-spin) (gtk-spin-button-value-as-int start-minute-spin)))
                    (multiple-value-setq (end-y end-m end-d) (gtk-calendar-get-date end-date-calendar))
                    (setf end-universal (datetime-to-universal end-y end-m end-d (gtk-spin-button-value-as-int end-hour-spin) (gtk-spin-button-value-as-int end-minute-spin)))
                    (if (and title-text (> (length title-text) 0))
                        (if (and start-universal end-universal (>= end-universal start-universal))
                            (progn (planner/calendar:add-event :title title-text :start-time start-universal :end-time end-universal :description desc-text :location-id loc-id)
                                   (update-event-display *main-event-list-store* *main-calendar*) (update-calendar-marks *main-calendar*))
                            (let ((error-dialog (make-instance 'gtk-message-dialog :transient-for dialog :modal t :message-type :error :buttons :ok :text "Validation Error" :secondary-text "End date/time must be after start date/time.")))
                              (gtk-dialog-run error-dialog) (gtk-widget-destroy error-dialog)))
                        (let ((error-dialog (make-instance 'gtk-message-dialog :transient-for dialog :modal t :message-type :error :buttons :ok :text "Validation Error" :secondary-text "Title cannot be empty.")))
                          (gtk-dialog-run error-dialog) (gtk-widget-destroy error-dialog)))))
                (gtk-widget-destroy dialog)))))))))

(defun handle-new-event-button-clicked (button)
  (declare (ignore button))
  (show-add-event-dialog (gtk-widget-toplevel *main-task-tree-view*)))

(defun get-selected-event-id ()
  "Retrieves the ID of the currently selected event in *main-event-tree-view*. Returns integer ID or NIL."
  (let* ((selection (gtk-tree-view-selection *main-event-tree-view*)) (selected-row (gtk-tree-selection-selected selection)))
    (when selected-row
      (let ((iter selected-row))
        (let ((id-str (gtk-tree-model-get-value *main-event-list-store* iter 0))) (ignore-errors (parse-integer id-str)))))))

(defun show-edit-event-dialog (parent-window event-id)
  "Creates and runs the 'Edit Event' dialog, pre-filled for `event-id`."
  (let ((event (planner/calendar:find-event event-id)))
    (unless event
      (let ((error-dialog (make-instance 'gtk-message-dialog :transient-for parent-window :modal t :message-type :error :buttons :ok :text (format nil "Error: Event with ID ~A not found." event-id))))
        (gtk-dialog-run error-dialog) (gtk-widget-destroy error-dialog) (return-from show-edit-event-dialog)))
    (let* ((dialog (make-instance 'gtk-dialog :title (format nil "Edit Event (ID: ~A)" event-id) :transient-for parent-window :modal t))
           (content-area (gtk-dialog-content-area dialog))
           (grid (make-instance 'gtk-grid :orientation :vertical :row-spacing 10 :column-spacing 10 :border-width 10)))
      (gtk-dialog-add-buttons dialog "Cancel" :cancel "Update" :ok)
      (let ((title-label (make-instance 'gtk-label :label "Title:")) (title-entry (make-instance 'gtk-entry :hexpand t :text (event-title event))))
        (gtk-grid-attach grid title-label 0 0 1 1) (gtk-grid-attach grid title-entry 1 0 1 1)
        (let* ((desc-label (make-instance 'gtk-label :label "Description:"))
               (desc-view (make-instance 'gtk-text-view :wrap-mode :word-char :height-request 100))
               (desc-buffer (gtk-text-view-buffer desc-view))
               (desc-scrolled (make-instance 'gtk-scrolled-window :hscrollbar-policy :automatic :vscrollbar-policy :automatic)))
          (setf (gtk-text-buffer-text desc-buffer) (event-description event))
          (gtk-container-add desc-scrolled desc-view)
          (gtk-grid-attach grid desc-label 0 1 1 1) (gtk-grid-attach grid desc-scrolled 1 1 1 1)
          (multiple-value-bind (s-yr s-mth s-day s-hr s-min) (decode-universal-time-to-ymdhm (event-start-time event))
            (let ((start-date-label (make-instance 'gtk-label :label "Start Date:")) (start-date-calendar (make-instance 'gtk-calendar))
                  (start-time-label (make-instance 'gtk-label :label "Start Time (HH:MM):")) (start-time-box (make-instance 'gtk-box :orientation :horizontal :spacing 6))
                  (start-hour-spin (make-instance 'gtk-spin-button :adjustment (make-instance 'gtk-adjustment :value (or s-hr 12) :lower 0 :upper 23 :step-increment 1)))
                  (start-minute-spin (make-instance 'gtk-spin-button :adjustment (make-instance 'gtk-adjustment :value (or s-min 0) :lower 0 :upper 59 :step-increment 5))))
              (when s-day (gtk-calendar-select-day start-date-calendar s-day s-mth s-yr))
              (gtk-box-pack-start start-time-box start-hour-spin :expand t :fill t :padding 0) (gtk-box-pack-start start-time-box start-minute-spin :expand t :fill t :padding 0)
              (gtk-grid-attach grid start-date-label 0 2 1 1) (gtk-grid-attach grid start-date-calendar 1 2 1 1)
              (gtk-grid-attach grid start-time-label 0 3 1 1) (gtk-grid-attach grid start-time-box 1 3 1 1)
              (multiple-value-bind (e-yr e-mth e-day e-hr e-min) (decode-universal-time-to-ymdhm (event-end-time event))
                (let ((end-date-label (make-instance 'gtk-label :label "End Date:")) (end-date-calendar (make-instance 'gtk-calendar))
                      (end-time-label (make-instance 'gtk-label :label "End Time (HH:MM):")) (end-time-box (make-instance 'gtk-box :orientation :horizontal :spacing 6))
                      (end-hour-spin (make-instance 'gtk-spin-button :adjustment (make-instance 'gtk-adjustment :value (or e-hr 13) :lower 0 :upper 23 :step-increment 1)))
                      (end-minute-spin (make-instance 'gtk-spin-button :adjustment (make-instance 'gtk-adjustment :value (or e-min 0) :lower 0 :upper 59 :step-increment 5))))
                  (if e-day (gtk-calendar-select-day end-date-calendar e-day e-mth e-yr) (gtk-calendar-select-day end-date-calendar s-day s-mth s-yr))
                  (gtk-box-pack-start end-time-box end-hour-spin :expand t :fill t :padding 0) (gtk-box-pack-start end-time-box end-minute-spin :expand t :fill t :padding 0)
                  (gtk-grid-attach grid end-date-label 0 4 1 1) (gtk-grid-attach grid end-date-calendar 1 4 1 1)
                  (gtk-grid-attach grid end-time-label 0 5 1 1) (gtk-grid-attach grid end-time-box 1 5 1 1)
                  (let ((loc-id-label (make-instance 'gtk-label :label "Location ID (Optional):")) (loc-id-entry (make-instance 'gtk-entry :text (if (event-location-id event) (princ-to-string (event-location-id event)) ""))))
                    (gtk-grid-attach grid loc-id-label 0 6 1 1) (gtk-grid-attach grid loc-id-entry 1 6 1 1)
                    (gtk-box-pack-start content-area grid :expand t :fill t :padding 0) (gtk-widget-show-all content-area)
                    (let ((response (gtk-dialog-run dialog)))
                      (when (eq response :ok)
                        (let* ((title-text (gtk-entry-text title-entry))
                               (desc-text (let* ((buffer (gtk-text-view-buffer desc-view)) (start (gtk-text-buffer-get-start-iter buffer)) (end (gtk-text-buffer-get-end-iter buffer))) (gtk-text-buffer-get-text buffer start end nil)))
                               (loc-id-text (gtk-entry-text loc-id-entry)) (loc-id (unless (string= loc-id-text "") (ignore-errors (parse-integer loc-id-text))))
                               (start-y 0) (start-m 0) (start-d 0) (end-y 0) (end-m 0) (end-d 0) (start-universal nil) (end-universal nil))
                          (multiple-value-setq (start-y start-m start-d) (gtk-calendar-get-date start-date-calendar))
                          (setf start-universal (datetime-to-universal start-y start-m start-d (gtk-spin-button-value-as-int start-hour-spin) (gtk-spin-button-value-as-int start-minute-spin)))
                          (multiple-value-setq (end-y end-m end-d) (gtk-calendar-get-date end-date-calendar))
                          (setf end-universal (datetime-to-universal end-y end-m end-d (gtk-spin-button-value-as-int end-hour-spin) (gtk-spin-button-value-as-int end-minute-spin)))
                          (if (and title-text (> (length title-text) 0))
                              (if (and start-universal end-universal (>= end-universal start-universal))
                                  (progn (planner/calendar:edit-event event-id :title title-text :start-time start-universal :end-time end-universal :description desc-text :location-id loc-id)
                                         (update-event-display *main-event-list-store* *main-calendar*) (update-calendar-marks *main-calendar*))
                                  (let ((error-dialog (make-instance 'gtk-message-dialog :transient-for dialog :modal t :message-type :error :buttons :ok :text "Validation Error" :secondary-text "End date/time must be after start date/time.")))
                                    (gtk-dialog-run error-dialog) (gtk-widget-destroy error-dialog)))
                              (let ((error-dialog (make-instance 'gtk-message-dialog :transient-for dialog :modal t :message-type :error :buttons :ok :text "Validation Error" :secondary-text "Title cannot be empty.")))
                                (gtk-dialog-run error-dialog) (gtk-widget-destroy error-dialog)))))
                      (gtk-widget-destroy dialog))))))))))

(defun handle-edit-event-button-clicked (button)
  (declare (ignore button))
  (let ((event-id (get-selected-event-id)))
    (if event-id (show-edit-event-dialog (gtk-widget-toplevel *main-event-tree-view*) event-id)
        (let ((info-dialog (make-instance 'gtk-message-dialog :transient-for (gtk-widget-toplevel *main-event-tree-view*) :modal t :message-type :info :buttons :ok :text "Please select an event to edit.")))
          (gtk-dialog-run info-dialog) (gtk-widget-destroy info-dialog)))))

(defun handle-delete-event-button-clicked (button)
  (declare (ignore button))
  (let ((event-id (get-selected-event-id)))
    (if event-id
        (let* ((parent-win (gtk-widget-toplevel *main-event-tree-view*))
               (confirmation-dialog (make-instance 'gtk-message-dialog :transient-for parent-win :modal t :destroy-with-parent t :message-type :question :buttons :yes-no :text (format nil "Are you sure you want to delete event ID: ~a?" event-id) :secondary-text "This action cannot be undone.")))
          (let ((response (gtk-dialog-run confirmation-dialog))) (gtk-widget-destroy confirmation-dialog)
            (when (eq response :yes) (planner/calendar:remove-event event-id) (update-event-display *main-event-list-store* *main-calendar*) (update-calendar-marks *main-calendar*))))
        (let ((info-dialog (make-instance 'gtk-message-dialog :transient-for (gtk-widget-toplevel *main-event-tree-view*) :modal t :message-type :info :buttons :ok :text "Please select an event to delete.")))
          (gtk-dialog-run info-dialog) (gtk-widget-destroy info-dialog)))))


(defun create-main-window ()
  "Creates and shows the main application window, now including a GtkTreeView for tasks and a calendar view."
  (let* ((window (make-instance 'gtk-window
                               :type :toplevel
                               :title "Common Lisp Planner"
                               :default-width 1000
                               :default-height 700
                               :border-width 12
                               :window-position :center))
         (main-vbox (make-instance 'gtk-box :orientation :vertical :spacing 12))
         (task-controls-hbox (make-instance 'gtk-box :orientation :horizontal :spacing 6))
                                              :vscrollbar-policy :automatic))
         (calendar-hbox (make-instance 'gtk-box :orientation :horizontal :spacing 6))
         (event-controls-hbox (make-instance 'gtk-box :orientation :horizontal :spacing 6))
         (event-scrolled-window (make-instance 'gtk-scrolled-window
                                               :shadow-type :etched-in
                                               :hscrollbar-policy :automatic
                                               :vscrollbar-policy :automatic)))

    ;; Initialize global GTK widget variables
    (setf *main-task-list-store* (make-instance 'gtk-list-store
    ;; Enable single selection mode for the event tree view
    (let ((selection (gtk-tree-view-selection *main-event-tree-view*)))
      (setf (gtk-tree-selection-mode selection) :single))

    (setf *main-calendar* (make-instance 'gtk-calendar))


    ;; Connect the "destroy" signal
    (g-signal-connect window "destroy" (lambda (widget) (declare (ignore widget)) (gtk:leave-gtk-main)))

    ;; 1. Task Control Buttons
    (let ((new-task-button (make-instance 'gtk-button :label "New Task"))
          (edit-task-button (make-instance 'gtk-button :label "Edit Selected Task"))
          (delete-task-button (make-instance 'gtk-button :label "Delete Selected Task")))
      (g-signal-connect new-task-button "clicked" #'handle-new-task-button-clicked)
      (g-signal-connect edit-task-button "clicked" #'handle-edit-task-button-clicked)
      (g-signal-connect delete-task-button "clicked" #'handle-delete-task-button-clicked)
      (gtk-box-pack-start task-controls-hbox new-task-button :expand nil :fill nil :padding 3)
      (gtk-box-pack-start task-controls-hbox edit-task-button :expand nil :fill nil :padding 3)
      (gtk-box-pack-start task-controls-hbox delete-task-button :expand nil :fill nil :padding 3))

    ;; Add task controls hbox to main vbox
    (gtk-box-pack-start main-vbox task-controls-hbox :expand nil :fill nil :padding 3)


    ;; 2. Task List
    (populate-task-list-display) ; Initial population

    (loop for col-title in '("ID" "Description" "Priority" "Status" "Due Date")
          for col-idx from 0
          do (let* ((renderer (make-instance 'gtk-cell-renderer-text))
                    (column (gtk-tree-view-column-new-with-attributes col-title renderer "text" col-idx)))
               (gtk-tree-view-append-column *main-task-tree-view* column)
               (when (= col-idx 1) ; Make Description column expand
                 (setf (gtk-tree-view-column-expand column) t))))

    (gtk-container-add task-scrolled-window *main-task-tree-view*)
    (gtk-box-pack-start main-vbox task-scrolled-window :expand t :fill t :padding 6)


    ;; 3. Event Controls
    (let ((new-event-button (make-instance 'gtk-button :label "New Event"))
          (edit-event-button (make-instance 'gtk-button :label "Edit Selected Event"))
          (delete-event-button (make-instance 'gtk-button :label "Delete Selected Event")))
      (g-signal-connect new-event-button "clicked" #'handle-new-event-button-clicked)
      (g-signal-connect edit-event-button "clicked" #'handle-edit-event-button-clicked)
      (g-signal-connect delete-event-button "clicked" #'handle-delete-event-button-clicked)
      (gtk-box-pack-start event-controls-hbox new-event-button :expand nil :fill nil :padding 3)
      (gtk-box-pack-start event-controls-hbox edit-event-button :expand nil :fill nil :padding 3)
      (gtk-box-pack-start event-controls-hbox delete-event-button :expand nil :fill nil :padding 3))
    (gtk-box-pack-start main-vbox event-controls-hbox :expand nil :fill nil :padding 3)


    ;; 4. Calendar and Event List part (calendar-hbox)
    (gtk-box-pack-start calendar-hbox *main-calendar* :expand nil :fill nil :padding 6)

    (loop for col-title in '("ID" "Title" "Start" "End" "Description" "Location ID")
          for col-idx from 0
          do (let* ((renderer (make-instance 'gtk-cell-renderer-text))
                    (column (gtk-tree-view-column-new-with-attributes col-title renderer "text" col-idx)))
               (gtk-tree-view-append-column *main-event-tree-view* column)
               (when (= col-idx 1) (setf (gtk-tree-view-column-sizing column) :fixed) (setf (gtk-tree-view-column-fixed-width column) 150)) ; Title
               (when (= col-idx 4) (setf (gtk-tree-view-column-expand column) t)))) ; Description expands

    (gtk-container-add event-scrolled-window *main-event-tree-view*)
    (gtk-box-pack-start calendar-hbox event-scrolled-window :expand t :fill t :padding 6)

    (gtk-box-pack-start main-vbox calendar-hbox :expand t :fill t :padding 6)

    (gtk-container-add window main-vbox)

    (g-signal-connect *main-calendar* "day-selected"
                      (lambda (widget)
                        (declare (ignore widget))
                        (update-event-display *main-event-list-store* *main-calendar*)))
    (g-signal-connect *main-calendar* "month-changed"
                      (lambda (widget)
                        (update-calendar-marks widget)
                        ;; Also update event display for the new month's currently selected/default day
                        (update-event-display *main-event-list-store* widget)))

    (update-event-display *main-event-list-store* *main-calendar*)
    (update-calendar-marks *main-calendar*) ; Initial marking

    (gtk-widget-show-all window)
    window))

(defun start-gui ()
>>>>>>> REPLACE

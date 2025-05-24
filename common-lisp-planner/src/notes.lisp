;;; common-lisp-planner/src/notes.lisp

(defpackage #:planner/notes
  (:use #:common-lisp #:planner/data-structures #:planner/file-ops)
  (:export #:*notes*
           #:*notes-filepath*
           #:*next-note-id*
           #:save-notes
           #:load-notes
           #:generate-note-id
           #:add-note
           #:find-note
           #:categorize-note
           #:view-notes
           ;; Re-exporting note accessors for convenience
           #:note-id
           #:note-title
           #:note-content
           #:note-category
           #:note-creation-date))

(in-package #:planner/notes)

;;; Global Variables
(defvar *notes* nil "List of note objects currently in memory.")
(defvar *notes-filepath* "common-lisp-planner/data/notes.dat" "Filepath for storing note data.")
(defvar *next-note-id* 0 "Counter for generating simple integer note IDs.")

;;; Helper Functions for Persistence

(defun save-notes ()
  "Saves the current content of *notes* to *notes-filepath*."
  (planner/file-ops:save-data *notes* *notes-filepath*))

(defun load-notes ()
  "Loads notes from *notes-filepath* and sets *notes*.
   If loading fails or file not found, *notes* is set to an empty list.
   Also, it attempts to set *next-note-id* to one greater than the highest
   numeric ID found, if notes are loaded and have numeric IDs."
  (let ((loaded-data (planner/file-ops:load-data *notes-filepath*)))
    (setf *notes* (if loaded-data loaded-data nil))
    (when *notes*
      (let ((max-id 0))
        (dolist (note *notes*)
          (when (and (note-id note) (numberp (note-id note)))
            (setf max-id (max max-id (note-id note)))))
        (setf *next-note-id* (1+ max-id))))
    *notes*))

;;; Internal Helper Functions
(defun format-creation-date (universal-time)
  "Formats a universal time into 'YYYY-MM-DD' string."
  (when universal-time
    (multiple-value-bind (sec min hr day mon yr)
        (decode-universal-time universal-time)
      (declare (ignore sec min hr))
      (format nil "~4,'0D-~2,'0D-~2,'0D" yr mon day))))

;;; Core Note Management Functions

(defun generate-note-id ()
  "Generates a new unique ID for a note (simple incrementing integer)."
  (incf *next-note-id*))

(defun add-note (&key title content category)
  "Creates a new note, adds it to *notes*, and saves.
   Requires `title` and `content`. `category` is optional.
   Returns the newly added note object, or nil if title or content are missing."
  (unless (and title (stringp title) (> (length (string-trim '(#\Space #\Tab #\Newline) title)) 0))
    (warn "ADD-NOTE: Title is a required field and cannot be empty.")
    (return-from add-note nil))
  (unless (and content (stringp content) (> (length (string-trim '(#\Space #\Tab #\Newline) content)) 0))
    (warn "ADD-NOTE: Content is a required field and cannot be empty.")
    (return-from add-note nil))

  (let ((new-note (make-note
                   :id (generate-note-id)
                   :title title
                   :content content
                   :category category
                   :creation-date (get-universal-time))))
    (push new-note *notes*)
    (save-notes)
    new-note))

(defun find-note (id)
  "Searches *notes* for a note with the given id.
   Returns the note object if found, otherwise nil."
  (find id *notes* :key #'note-id :test #'equal))

(defun categorize-note (id new-category)
  "Finds the note by id, updates its category, saves, and returns the note.
   Returns nil if note not found."
  (let ((note (find-note id)))
    (when note
      (setf (note-category note) new-category)
      (save-notes)
      note)))

(defun view-notes (&key category search-term)
  "Displays notes, allowing filtering by category and/or search term.
   Category matching is case-insensitive if category is a string.
   Search term matching is case-insensitive for title and content."
  (let ((filtered-notes *notes*))

    ;; Filter by category
    (when category
      (setf filtered-notes
            (remove-if-not (lambda (note)
                             (let ((note-cat (note-category note)))
                               (cond
                                 ((and (stringp category) (stringp note-cat))
                                  (string-equal category note-cat))
                                 ((and (symbolp category) (symbolp note-cat))
                                  (eq category note-cat))
                                 (t nil)))) ; Mismatched types or nil category
                           filtered-notes)))

    ;; Filter by search term
    (when (and search-term (stringp search-term) (> (length search-term) 0))
      (let ((search-lower (string-downcase search-term)))
        (setf filtered-notes
              (remove-if-not (lambda (note)
                               (or (search search-lower (string-downcase (note-title note)))
                                   (search search-lower (string-downcase (note-content note)))))
                             filtered-notes))))

    (if filtered-notes
        (progn
          (format t "~&Notes (~A matching):~%" (length filtered-notes))
          (dolist (note filtered-notes)
            (format t "ID: ~A | Date: ~A | Category: ~A | Title: ~A | Content: ~A~%"
                    (note-id note)
                    (format-creation-date (note-creation-date note))
                    (or (note-category note) "N/A")
                    (note-title note)
                    (let ((content (note-content note)))
                      (if (> (length content) 50)
                          (concatenate 'string (subseq content 0 47) "...")
                          content)))))
          t) ; Return t if notes were displayed
        (progn
          (if (or category search-term)
              (format t "No notes match your criteria.~%")
              (format t "No notes available.~%"))
          nil)))) ; Return nil if no notes displayed

;;; Initialisation Note:
;;; load-notes would typically be called by a main application setup function
;;; or when this system/module is loaded. For example:
;;; (load-notes)
;;; This ensures data is loaded from the file when the system starts.

;; Example of initial load, can be uncommented or called separately
;; (load-notes)

;;; common-lisp-planner/src/notes.lisp

(defpackage #:planner/notes
  (:use #:common-lisp #:planner/data-structures #:planner/file-ops)
  (:documentation "Manages textual notes for the planner application.
This includes adding, finding, categorizing, and viewing notes.
Notes can be filtered by category or searched by content.
Note data is persisted to a file.")
  (:export #:*notes*
           #:*notes-filepath*
           #:*next-note-id* ; Exported for completeness.
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
(defvar *notes* nil "A list holding all current note objects in memory.
Populated by `load-notes` and managed by functions like `add-note`, `categorize-note`, etc.")
(defvar *notes-filepath* "common-lisp-planner/data/notes.dat"
  "The default filepath where note data is persisted.
Used by `save-notes` and `load-notes`.")
(defvar *next-note-id* 0 "A counter used by `generate-note-id` for unique integer IDs for new notes.
Updated by `load-notes` based on loaded data to prevent ID collisions.")

;;; Helper Functions for Persistence

(defun save-notes ()
  "Saves the current list of notes from `*notes*` to the file specified by `*notes-filepath*`.
Uses `planner/file-ops:save-data`. Returns `t` on success, `nil` on failure."
  (planner/file-ops:save-data *notes* *notes-filepath*))

(defun load-notes ()
  "Loads notes from the file specified by `*notes-filepath*` into `*notes*`.
If loading fails or the file doesn't exist, `*notes*` is set to an empty list.
Updates `*next-note-id*` to be greater than the maximum ID found in loaded notes.
Returns the list of loaded notes (which is also set to `*notes*`)."
  (let ((loaded-data (planner/file-ops:load-data *notes-filepath*)))
    (setf *notes* (if loaded-data loaded-data nil))
    ;; Attempt to update *next-note-id* to avoid collisions if notes were loaded
    (when *notes*
      (let ((max-id 0))
        (dolist (note *notes*)
          (when (and (note-id note) (numberp (note-id note)))
            (setf max-id (max max-id (note-id note)))))
        (setf *next-note-id* (1+ max-id))))
    *notes*))

;;; Internal Helper Functions
(defun format-creation-date (universal-time)
  "Formats a universal time integer into a 'YYYY-MM-DD' string.
Returns `nil` if `universal-time` is `nil`.
This is an internal helper, primarily for `view-notes`."
  (when universal-time ; Only proceed if universal-time is not nil
    (multiple-value-bind (sec min hr day mon yr)
        (decode-universal-time universal-time)
      (declare (ignore sec min hr))
      (format nil "~4,'0D-~2,'0D-~2,'0D" yr mon day))))

;;; Core Note Management Functions

(defun generate-note-id ()
  "Generates a new unique ID for a note (simple incrementing integer)."
  (incf *next-note-id*))

(defun add-note (&key title content category)
  "Creates a new note with the provided details, adds it to `*notes*`, and saves.
- `title`: A string for the note's title (required and must be non-empty).
- `content`: A string for the note's content (required and must be non-empty).
- `category`: Optional. A string or symbol for categorizing the note.
The `creation-date` is automatically set to the current universal time.
Returns the newly created note object, or `nil` if `title` or `content` are missing or empty."
  (unless (and title (stringp title) (> (length (string-trim '(#\Space #\Tab #\Newline) title)) 0))
    (warn "ADD-NOTE: Title is a required field and cannot be empty, but got '~A'." title)
    (return-from add-note nil))
  (unless (and content (stringp content) (> (length (string-trim '(#\Space #\Tab #\Newline) content)) 0))
    (warn "ADD-NOTE: Content is a required field and cannot be empty.") ; Content itself not shown in warning for brevity
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
  "Finds a note by its `id` and updates its `category` to `new-category`.
Saves changes to the persistent store.
Returns the updated note object if found and categorized, `nil` otherwise."
  (let ((note (find-note id)))
    (when note
      ;; `new-category` can be a string or symbol as per data-structure
      (setf (note-category note) new-category)
      (save-notes)
      note)))

(defun view-notes (&key category search-term)
  "Displays notes from `*notes*`, with optional filtering.
- `category`: If provided, filters notes by this category. Matching is case-insensitive
              if both `category` and the note's category are strings. If both are symbols,
              `eq` is used.
- `search-term`: If provided as a non-empty string, filters notes where the `search-term`
                 (case-insensitive) appears in either the note's title or content.
Prints notes to standard output, including a snippet of the content.
Returns `t` if notes were displayed, `nil` otherwise."
  (let ((filtered-notes *notes*))

    ;; Filter by category
    (when category
      (setf filtered-notes
            (remove-if-not (lambda (note)
                             (let ((note-cat (note-category note)))
                               (cond
                                 ((and (stringp category) (stringp note-cat))
                                  (string-equal category note-cat)) ; Case-insensitive for strings
                                 ((and (symbolp category) (symbolp note-cat))
                                  (eq category note-cat)) ; Direct comparison for symbols
                                 (t nil)))) ; Mismatched types or if note-cat is nil
                           filtered-notes)))

    ;; Filter by search term (in title or content)
    (when (and search-term (stringp search-term) (> (length search-term) 0))
      (let ((search-lower (string-downcase search-term)))
        (setf filtered-notes
              (remove-if-not (lambda (note)
                               (or (search search-lower (string-downcase (note-title note)))
                                   (search search-lower (string-downcase (note-content note)))))
                             filtered-notes))))

    (if filtered-notes
        (progn
          (format t "~&Notes (~A matching criteria):~%" (length filtered-notes))
          (dolist (note filtered-notes)
            (format t "ID: ~3A | Date: ~10A | Category: ~15A | Title: ~A~%"
                    (note-id note)
                    (or (format-creation-date (note-creation-date note)) "N/A")
                    (let ((cat (note-category note)))
                         (cond ((stringp cat) (if (> (length cat) 0) cat "N/A"))
                               ((symbolp cat) (symbol-name cat))
                               (t "N/A")))
                    (note-title note))
            (format t "           Content: ~A~%~%" ; Indent content for readability
                    (let ((content (note-content note)))
                      (if (> (length content) 70) ; Show a bit more content before truncating
                          (concatenate 'string (subseq content 0 67) "...")
                          content)))))
          t) ; Return t if notes were displayed
        (progn
          (if (or category search-term)
              (format t "No notes match your filtering criteria.~%")
              (format t "No notes available to display.~%"))
          nil)))) ; Return nil if no notes displayed

;;; Initialisation Note:
;;; load-notes would typically be called by a main application setup function
;;; or when this system/module is loaded. For example:
;;; (load-notes)
;;; This ensures data is loaded from the file when the system starts.

;; Example of initial load, can be uncommented or called separately
;; (load-notes)

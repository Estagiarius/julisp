;;; common-lisp-planner/src/materials.lisp

(defpackage #:planner/materials
  (:use #:common-lisp #:planner/data-structures #:planner/file-ops)
  (:documentation "Manages metadata for materials or files associated with the planner.
This includes adding, finding, and viewing material metadata entries.
Material metadata is persisted to a file.")
  (:export #:*materials*
           #:*materials-filepath*
           #:*next-material-id* ; Exported for completeness.
           #:save-materials
           #:load-materials
           #:generate-material-id
           #:add-material-metadata
           #:find-material
           #:view-materials
           ;; Re-exporting material accessors for convenience
           #:material-id
           #:material-name
           #:material-file-path
           #:material-category
           #:material-upload-date))

(in-package #:planner/materials)

;;; Global Variables
(defvar *materials* nil "A list holding all current material metadata objects in memory.
Populated by `load-materials` and managed by functions like `add-material-metadata`.")
(defvar *materials-filepath* "common-lisp-planner/data/materials.dat"
  "The default filepath where material metadata is persisted.
Used by `save-materials` and `load-materials`.")
(defvar *next-material-id* 0 "A counter used by `generate-material-id` for unique integer IDs for new material entries.
Updated by `load-materials` based on loaded data to prevent ID collisions.")

;;; Helper Functions for Persistence

(defun save-materials ()
  "Saves the current list of material metadata from `*materials*` to the file specified by `*materials-filepath*`.
Uses `planner/file-ops:save-data`. Returns `t` on success, `nil` on failure."
  (planner/file-ops:save-data *materials* *materials-filepath*))

(defun load-materials ()
  "Loads material metadata from the file specified by `*materials-filepath*` into `*materials*`.
If loading fails or the file doesn't exist, `*materials*` is set to an empty list.
Updates `*next-material-id*` to be greater than the maximum ID found in loaded materials.
Returns the list of loaded material metadata (which is also set to `*materials*`)."
  (let ((loaded-data (planner/file-ops:load-data *materials-filepath*)))
    (setf *materials* (if loaded-data loaded-data nil))
    ;; Attempt to update *next-material-id* to avoid collisions if materials were loaded
    (when *materials*
      (let ((max-id 0))
        (dolist (mat *materials*)
          (when (and (material-id mat) (numberp (material-id mat)))
            (setf max-id (max max-id (material-id mat)))))
        (setf *next-material-id* (1+ max-id))))
    *materials*))

;;; Internal Helper Functions
(defun format-upload-date (universal-time)
  "Formats a universal time integer into a 'YYYY-MM-DD' string.
Returns `nil` if `universal-time` is `nil`.
This is an internal helper, primarily for `view-materials`."
  (when universal-time ; Only proceed if universal-time is not nil
    (multiple-value-bind (sec min hr day mon yr)
        (decode-universal-time universal-time)
      (declare (ignore sec min hr))
      (format nil "~4,'0D-~2,'0D-~2,'0D" yr mon day))))

;;; Core Material Metadata Management Functions

(defun generate-material-id ()
  "Generates a new unique ID for a material (simple incrementing integer)."
  (incf *next-material-id*))

(defun add-material-metadata (&key name file-path category)
  "Creates new material metadata with the provided details, adds it to `*materials*`, and saves.
- `name`: A string for the material's name (required and must be non-empty).
- `file-path`: A string for the material's file path or URL (required and must be non-empty).
- `category`: Optional. A string or symbol for categorizing the material.
The `upload-date` is automatically set to the current universal time.
Returns the newly created material metadata object, or `nil` if `name` or `file-path` are missing or empty."
  (unless (and name (stringp name) (> (length (string-trim '(#\Space #\Tab #\Newline) name)) 0))
    (warn "ADD-MATERIAL-METADATA: Name is a required field and cannot be empty, but got '~A'." name)
    (return-from add-material-metadata nil))
  (unless (and file-path (stringp file-path) (> (length (string-trim '(#\Space #\Tab #\Newline) file-path)) 0))
    (warn "ADD-MATERIAL-METADATA: File path is a required field and cannot be empty, but got '~A'." file-path)
    (return-from add-material-metadata nil))

  (let ((new-material (make-material
                       :id (generate-material-id)
                       :name name
                       :file-path file-path
                       :category category
                       :upload-date (get-universal-time))))
    (push new-material *materials*)
    (save-materials)
    new-material))

(defun find-material (id)
  "Searches *materials* for a material with the given id.
   Returns the material object if found, otherwise nil."
  (find id *materials* :key #'material-id :test #'equal))

(defun view-materials (&key category)
  "Displays material metadata from `*materials*`, with optional filtering by category.
- `category`: If provided, filters materials by this category. Matching is case-insensitive
              if both `category` and the material's category are strings. If both are symbols,
              `eq` is used.
Prints material metadata to standard output.
Returns `t` if materials were displayed, `nil` otherwise."
  (let ((filtered-materials *materials*))

    ;; Filter by category
    (when category
      (setf filtered-materials
            (remove-if-not (lambda (mat)
                             (let ((mat-cat (material-category mat)))
                               (cond
                                 ((and (stringp category) (stringp mat-cat))
                                  (string-equal category mat-cat)) ; Case-insensitive for strings
                                 ((and (symbolp category) (symbolp mat-cat))
                                  (eq category mat-cat)) ; Direct comparison for symbols
                                 (t nil)))) ; Mismatched types or if mat-cat is nil
                           filtered-materials)))

    (if filtered-materials
        (progn
          (format t "~&Materials (~A matching criteria):~%" (length filtered-materials))
          (dolist (mat filtered-materials)
            (format t "ID: ~3A | Uploaded: ~10A | Category: ~15A | Name: ~A~%"
                    (material-id mat)
                    (or (format-upload-date (material-upload-date mat)) "N/A")
                    (let ((cat (material-category mat)))
                         (cond ((stringp cat) (if (> (length cat) 0) cat "N/A"))
                               ((symbolp cat) (symbol-name cat))
                               (t "N/A")))
                    (material-name mat))
            (format t "           Path: ~A~%~%" ; Indent path for readability
                    (material-file-path mat))))
          t) ; Return t if materials were displayed
        (progn
          (if category
              (format t "No materials match your filtering criteria.~%")
              (format t "No materials available to display.~%"))
          nil)))) ; Return nil if no materials displayed

;;; Initialisation Note:
;;; load-materials would typically be called by a main application setup function
;;; or when this system/module is loaded. For example:
;;; (load-materials)
;;; This ensures data is loaded from the file when the system starts.

;; Example of initial load, can be uncommented or called separately
;; (load-materials)

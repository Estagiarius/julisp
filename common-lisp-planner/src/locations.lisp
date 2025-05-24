;;; common-lisp-planner/src/locations.lisp

(defpackage #:planner/locations
  (:use #:common-lisp #:planner/data-structures #:planner/file-ops)
  (:documentation "Manages location data for the planner application.
This includes adding, finding, and viewing locations, which can be associated with events.
Location data is persisted to a file.")
  (:export #:*locations*
           #:*locations-filepath*
           #:*next-location-id* ; Exported for completeness, though direct modification by users is rare.
           #:save-locations
           #:load-locations
           #:generate-location-id
           #:add-location
           #:find-location
           #:view-locations
           ;; Re-exporting location accessors for convenience
           #:location-id
           #:location-name
           #:location-address
           #:location-description))

(in-package #:planner/locations)

;;; Global Variables
(defvar *locations* nil "A list holding all current location objects in memory.
Populated by `load-locations` and managed by functions like `add-location`.")
(defvar *locations-filepath* "common-lisp-planner/data/locations.dat"
  "The default filepath where location data is persisted.
Used by `save-locations` and `load-locations`.")
(defvar *next-location-id* 0 "A counter used by `generate-location-id` for unique integer IDs for new locations.
Updated by `load-locations` based on loaded data to prevent ID collisions.")

;;; Helper Functions for Persistence

(defun save-locations ()
  "Saves the current list of locations from `*locations*` to the file specified by `*locations-filepath*`.
Uses `planner/file-ops:save-data`. Returns `t` on success, `nil` on failure."
  (planner/file-ops:save-data *locations* *locations-filepath*))

(defun load-locations ()
  "Loads locations from the file specified by `*locations-filepath*` into `*locations*`.
If loading fails or the file doesn't exist, `*locations*` is set to an empty list.
Updates `*next-location-id*` to be greater than the maximum ID found in loaded locations.
Returns the list of loaded locations (which is also set to `*locations*`)."
  (let ((loaded-data (planner/file-ops:load-data *locations-filepath*)))
    (setf *locations* (if loaded-data loaded-data nil))
    ;; Attempt to update *next-location-id* to avoid collisions if locations were loaded
    (when *locations*
      (let ((max-id 0))
        (dolist (loc *locations*)
          (when (and (location-id loc) (numberp (location-id loc)))
            (setf max-id (max max-id (location-id loc)))))
        (setf *next-location-id* (1+ max-id))))
    *locations*))

;;; Core Location Management Functions

(defun generate-location-id ()
  "Generates a new unique ID for a location (simple incrementing integer)."
  (incf *next-location-id*))

(defun add-location (&key name address description)
  "Creates a new location with the given details, adds it to `*locations*`, and saves.
- `name`: A string for the location's name (required and must be non-empty).
- `address`: Optional. A string for the location's address.
- `description`: Optional. A string for a description of the location.
Returns the newly created location object, or `nil` if `name` is not provided or is empty."
  (unless (and name (stringp name) (> (length (string-trim '(#\Space #\Tab #\Newline) name)) 0))
    (warn "ADD-LOCATION: Name is a required field and cannot be empty, but got '~A'." name)
    (return-from add-location nil))

  (let ((new-location (make-location
                       :id (generate-location-id)
                       :name name
                       :address (or address "")
                       :description (or description ""))))
    (push new-location *locations*)
    (save-locations)
    new-location))

(defun find-location (id)
  "Searches *locations* for a location with the given id.
   Returns the location object if found, otherwise nil."
  (find id *locations* :key #'location-id :test #'equal))

(defun view-locations ()
  "Prints all locations currently stored in `*locations*` to standard output.
Displays ID, Name, Address (or 'N/A'), and Description (or 'N/A').
Returns `t` if locations were displayed, `nil` if no locations are saved."
  (if *locations*
      (progn
        (format t "~&Locations (~A):~%" (length *locations*))
        (dolist (loc *locations*)
          (format t "ID: ~A | Name: ~A | Address: ~A | Description: ~A~%"
                  (location-id loc)
                  (location-name loc)
                  (if (and (location-address loc) (> (length (location-address loc)) 0))
                      (location-address loc) "N/A")
                  (if (and (location-description loc) (> (length (location-description loc)) 0))
                      (location-description loc) "N/A"))))
      (format t "No locations saved.~%"))
  (if *locations* t nil))


;;; Initialisation Note:
;;; load-locations would typically be called by a main application setup function
;;; or when this system/module is loaded. For example:
;;; (load-locations)
;;; This ensures data is loaded from the file when the system starts.

;; Example of initial load, can be uncommented or called separately
;; (load-locations)

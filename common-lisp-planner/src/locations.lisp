;;; common-lisp-planner/src/locations.lisp

(defpackage #:planner/locations
  (:use #:common-lisp #:planner/data-structures #:planner/file-ops)
  (:export #:*locations*
           #:*locations-filepath*
           #:*next-location-id* ; Exporting for completeness, though direct use might be rare
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
(defvar *locations* nil "List of location objects currently in memory.")
(defvar *locations-filepath* "common-lisp-planner/data/locations.dat" "Filepath for storing location data.")
(defvar *next-location-id* 0 "Counter for generating simple integer location IDs.")

;;; Helper Functions for Persistence

(defun save-locations ()
  "Saves the current content of *locations* to *locations-filepath*."
  (planner/file-ops:save-data *locations* *locations-filepath*))

(defun load-locations ()
  "Loads locations from *locations-filepath* and sets *locations*.
   If loading fails or file not found, *locations* is set to an empty list.
   Also, it attempts to set *next-location-id* to one greater than the highest
   numeric ID found, if locations are loaded and have numeric IDs."
  (let ((loaded-data (planner/file-ops:load-data *locations-filepath*)))
    (setf *locations* (if loaded-data loaded-data nil))
    ;; Attempt to update *next-location-id* if locations were loaded
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
  "Creates a new location, adds it to *locations*, and saves.
   Requires `name`. `address` and `description` are optional.
   Returns the newly added location object, or nil if name is not provided."
  (unless (and name (stringp name) (> (length (string-trim '(#\Space #\Tab #\Newline) name)) 0))
    (warn "ADD-LOCATION: Name is a required field and cannot be empty.")
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
  "Prints all locations in *locations* to standard output.
   Returns t if locations were displayed, nil otherwise."
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

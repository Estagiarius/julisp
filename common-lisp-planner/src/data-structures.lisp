;;; common-lisp-planner/src/data-structures.lisp

(defpackage #:planner/data-structures
  (:use #:cl)
  (:documentation "Defines the core data structures used throughout the Common Lisp Planner application, such as events, tasks, locations, notes, and materials.")
  (:export #:event
           #:event-id
           #:event-title
           #:event-start-time
           #:event-end-time
           #:event-description
           #:event-location-id
           #:task
           #:task-id
           #:task-description
           #:task-due-date
           #:task-priority
           #:task-status
           #:task-notes
           #:location
           #:location-id
           #:location-name
           #:location-address
           #:location-description
           #:note
           #:note-id
           #:note-title
           #:note-content
           #:note-category
           #:note-creation-date
           #:material
           #:material-id
           #:material-name
           #:material-file-path
           #:material-category
           #:material-upload-date))

(in-package #:planner/data-structures)

(defstruct event
  "Represents an event in the planner."
  (id nil :type (or string integer) :documentation "Unique identifier for the event. Can be a string or integer.")
  (title "" :type string :documentation "Title of the event.")
  (start-time nil :type (or string integer) :documentation "Start time of the event. Stored as a universal time integer if parsed, otherwise could be a string placeholder.")
  (end-time nil :type (or string integer) :documentation "End time of the event. Stored as a universal time integer if parsed, otherwise could be a string placeholder.")
  (description "" :type string :documentation "Detailed description of the event.")
  (location-id nil :type (or string integer) :documentation "Identifier for a linked location (if any)."))

(defstruct task
  "Represents a task in the planner."
  (id nil :type (or string integer) :documentation "Unique identifier for the task.")
  (description "" :type string :documentation "Description of the task.")
  (due-date nil :type (or string integer) :documentation "Due date of the task, typically a universal time integer.")
  (priority 0 :type integer :documentation "Priority of the task (e.g., 0-5).")
  (status :pending :type symbol :documentation "Status of the task (e.g., :pending, :completed).")
  (notes "" :type string :documentation "Additional notes for the task."))

(defstruct location
  "Represents a location, often linked to events."
  (id nil :type (or string integer) :documentation "Unique identifier for the location.")
  (name "" :type string :documentation "Name of the location.")
  (address "" :type string :documentation "Physical address of the location.")
  (description "" :type string :documentation "Description of the location."))

(defstruct note
  "Represents a textual note."
  (id nil :type (or string integer) :documentation "Unique identifier for the note.")
  (title "" :type string :documentation "Title of the note.")
  (content "" :type string :documentation "Main content of the note.")
  (category nil :type (or string symbol) :documentation "Category for organizing the note (e.g., :work, \"personal\").")
  (creation-date nil :type (or string integer) :documentation "Date the note was created, typically a universal time integer."))

(defstruct material
  "Represents metadata for an associated material or file."
  (id nil :type (or string integer) :documentation "Unique identifier for the material metadata entry.")
  (name "" :type string :documentation "Display name of the material/file.")
  (file-path "" :type string :documentation "File path or URL to the material.")
  (category nil :type (or string symbol) :documentation "Category for organizing the material.")
  (upload-date nil :type (or string integer) :documentation "Date the material was added/uploaded, typically a universal time integer."))

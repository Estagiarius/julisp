;;; common-lisp-planner/src/data-structures.lisp

(defpackage #:planner/data-structures
  (:use #:cl)
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
  (id nil :type (or string integer))
  (title "" :type string)
  (start-time nil :type (or string integer)) ; Using integer for timestamp for now
  (end-time nil :type (or string integer))   ; Using integer for timestamp for now
  (description "" :type string)
  (location-id nil :type (or string integer)))

(defstruct task
  "Represents a task in the planner."
  (id nil :type (or string integer))
  (description "" :type string)
  (due-date nil :type (or string integer)) ; Using integer for timestamp for now
  (priority 0 :type integer)
  (status :pending :type symbol)
  (notes "" :type string))

(defstruct location
  "Represents a location."
  (id nil :type (or string integer))
  (name "" :type string)
  (address "" :type string)
  (description "" :type string))

(defstruct note
  "Represents a note."
  (id nil :type (or string integer))
  (title "" :type string)
  (content "" :type string)
  (category nil :type (or string symbol))
  (creation-date nil :type (or string integer))) ; Using integer for timestamp for now

(defstruct material
  "Represents a material or a file."
  (id nil :type (or string integer))
  (name "" :type string)
  (file-path "" :type string)
  (category nil :type (or string symbol))
  (upload-date nil :type (or string integer))) ; Using integer for timestamp for now

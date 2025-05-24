;;; common-lisp-planner/src/file-ops.lisp

(defpackage #:planner/file-ops
  (:use #:cl #:planner/data-structures)
  (:documentation "Provides generic functions for saving and loading Lisp data to/from files.
This module uses a simple text-based serialization approach with `prin1` and `read`,
where each Lisp object is written on a new line. This is suitable for relatively simple
data structures and is human-readable to some extent, but may not be robust for
complex objects or across different Lisp implementations if type information is critical
and not self-evident from the printed representation.")
  (:export #:save-data
           #:load-data))

(in-package #:planner/file-ops)

(defun save-data (list-of-items filepath)
  "Saves a list of Lisp objects to a file, one item per line.
   Uses `prin1` for serialization, which prints the object in a Lisp-readable format.
   Overwrites the file if it exists (`:if-exists :supersede`).
   Creates the file if it does not exist (`:if-does-not-exist :create`).
   Returns `t` on successful save, `nil` on failure (e.g., file errors)."
  (handler-case
      (with-open-file (stream filepath
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (unless stream
          (warn "SAVE-DATA: Could not open file ~S for writing." filepath)
          (return-from save-data nil))
        (dolist (item list-of-items)
          (prin1 item stream)
          (terpri stream))
        t)
    (file-error (e)
      (warn "SAVE-DATA: File error while trying to write to ~S: ~A" filepath e)
      nil)
    (error (e)
      (warn "SAVE-DATA: An unexpected error occurred while writing to ~S: ~A" filepath e)
      nil)))

(defun load-data (filepath)
  "Loads a list of Lisp objects from a file previously saved by `save-data`.
   Expects one Lisp S-expression per line, readable by `read`.
   Returns the list of loaded objects.
   Returns `nil` if the file does not exist, cannot be opened, or if a read error occurs
   (other than a clean end-of-file)."
  (handler-case
      (unless (probe-file filepath) ; Check if file exists before trying to open
        (warn "LOAD-DATA: File ~S does not exist." filepath)
        (return-from load-data nil))
      (with-open-file (stream filepath
                              :direction :input)
        (unless stream
          (warn "LOAD-DATA: Could not open file ~S for reading." filepath)
          (return-from load-data nil))
        (loop for item = (read stream nil :eof)
              until (eq item :eof)
              collect item))
    (file-error (e)
      (warn "LOAD-DATA: File error while trying to read from ~S: ~A" filepath e)
      nil)
    (end-of-file ()
      ;; This can happen if the file is empty or malformed after the last complete s-expression
      ;; In the current loop structure, :eof is handled, but an explicit EOF condition
      ;; might occur if (read) is called when already at EOF in some CL implementations
      ;; or if the file is empty.
      (warn "LOAD-DATA: Reached end of file unexpectedly or file ~S is empty." filepath)
      nil)
    (reader-error (e)
      (warn "LOAD-DATA: Error reading S-expression from file ~S: ~A" filepath e)
      nil)
    (error (e)
      (warn "LOAD-DATA: An unexpected error occurred while reading from ~S: ~A" filepath e)
      nil))))

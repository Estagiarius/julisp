;;; common-lisp-planner/src/file-ops.lisp

(defpackage #:planner/file-ops
  (:use #:cl #:planner/data-structures)
  (:export #:save-data
           #:load-data))

(in-package #:planner/file-ops)

(defun save-data (list-of-items filepath)
  "Saves a list of Lisp objects to a file, one item per line.
   Uses prin1 for serialization. Overwrites the file if it exists.
   Returns t on success, nil on failure."
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
  "Loads a list of Lisp objects from a file.
   Expects one S-expression per line, as saved by save-data.
   Returns the list of objects, or nil if the file does not exist or an error occurs."
  (handler-case
      (unless (probe-file filepath)
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

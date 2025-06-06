;;; common-lisp-planner/tests/packages.lisp

(defpackage #:planner-app/tests
  (:use #:common-lisp)
  (:documentation "The main test package for the Common Lisp Planner application.
It defines the top-level test suite and provides helper macros for setting up
test environments for different modules. It uses the FiveAM testing framework.")
  (:import-from #:fiveam #:def-suite #:in-suite #:test #:is #:run! #:signals #:finishes)
  (:export #:planner-app-test-suite
           #:run-all-tests
           #:with-clean-calendar-data
           #:with-clean-todo-data))

(in-package #:planner-app/tests)

;; Load FiveAM at compile/load/execute time.
;; This ensures that the testing framework is available when test files are compiled and run.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case (ql:quickload :fiveam :silent t)
    (error (c)
      (warn "Failed to load FiveAM using Quicklisp: ~A. Tests cannot be run." c))))

;; Define a top-level test suite for the entire application.
;; Individual module test suites will be part of this main suite.
(def-suite planner-app-test-suite
  :description "Main test suite for the Common Lisp Planner Application. Contains all other specific test suites.")

(defun run-all-tests ()
  "Runs all test suites defined under `planner-app-test-suite`.
This is the primary function to execute all unit tests for the application."
  (fiveam:run! 'planner-app-test-suite))

;; Helper macro for running calendar tests with a clean data environment.
(defmacro with-clean-calendar-data (&body body)
  "Executes BODY in a context where calendar-specific data (`planner/calendar:*events*`
and `planner/calendar:*next-event-id*`) is reset to initial values (nil and 0).
It also locally redefines `planner/calendar:save-events` to be a mock function
that does nothing, preventing file I/O during tests. This ensures test isolation
and avoids side effects on actual data files."
  `(let ((planner/calendar:*events* nil)
         (planner/calendar:*next-event-id* 0))
     (flet ((planner/calendar:save-events ()
              ;; Mock implementation: Do nothing to avoid file I/O during tests.
              ;; Returns nil, assuming the original save-events returns t or nil.
              nil))
       ;; Declare ignorable to suppress warnings if the mock is not called in all tests.
       (declare (ignorable #'planner/calendar:save-events))
       ,@body)))

;; Helper macro for running todo tests with a clean data environment.
(defmacro with-clean-todo-data (&body body)
  "Executes BODY in a context where todo-specific data (`planner/todo:*tasks*`
and `planner/todo:*next-task-id*`) is reset to initial values (nil and 0).
It also locally redefines `planner/todo:save-tasks` to be a mock function
that does nothing, preventing file I/O during tests. This ensures test isolation
and avoids side effects on actual data files."
  `(let ((planner/todo:*tasks* nil)
         (planner/todo:*next-task-id* 0))
     (flet ((planner/todo:save-tasks ()
              ;; Mock implementation: Do nothing.
              nil))
        (declare (ignorable #'planner/todo:save-tasks))
       ,@body)))

;; General Note on Mocking:
;; The `flet` approach used here for mocking `save-events` and `save-tasks`
;; works because these functions are global and directly called from within the
;; code being tested. For more complex scenarios, such as mocking functions
;; that are not globally defined or are part of other packages in more intricate ways,
;; more advanced mocking libraries (e.g., `cl-mock`) or dynamic rebinding techniques
;; might be necessary. These simple flet mocks are effective for the current project structure.

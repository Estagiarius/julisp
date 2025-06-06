;;; build.lisp
;;; Build script for creating standalone executables for the Common Lisp Planner.

;; Ensure Quicklisp is loaded.
;; This path might need adjustment based on the actual Quicklisp setup location.
;; For the sandbox, Quicklisp is typically set up in /tmp/quicklisp/
(let ((quicklisp-init (merge-pathnames #P"quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init))
  ;; Fallback for sandbox environment if home directory setup is not standard
  (unless (find-package :ql)
    (let ((sandbox-ql-setup #P"/tmp/quicklisp/setup.lisp"))
      (if (probe-file sandbox-ql-setup)
          (load sandbox-ql-setup)
          (warn "Quicklisp setup not found at standard locations or /tmp/quicklisp/. System loading might fail.")))))

;; Ensure ASDF can find the local project.
;; In the sandbox, the project is at /app/
(pushnew #P"/app/" asdf:*central-registry*)

;; Load the planner systems
;; Explicitly load dependencies to ensure they are part of the image.
(format t "~&Loading :common-lisp-planner system...~%")
(ql:quickload :common-lisp-planner)
(format t "~&Loading :cl-cffi-gtk system (for GUI)...~%")
(ql:quickload :cl-cffi-gtk)
(format t "~&Loading :planner-app/gui system (for GUI)...~%")
(ql:quickload :planner-app/gui) ; This loads the GUI package and its dependencies.


;; Define entry point functions for CLI and GUI
(defun cli-main ()
  "Main entry point for the Command Line Interface version of the planner."
  (handler-case (planner-app:start-planner)
    (error (c)
      (format *error-output* "~&An error occurred in the CLI: ~a~%" c)
      (uiop:quit 1))))

(defun gui-main ()
  "Main entry point for the Graphical User Interface version of the planner."
  (handler-case (planner-app/gui:start-gui)
    (error (c)
      (format *error-output* "~&An error occurred in the GUI: ~a~%" c)
      (uiop:quit 1))))

(defun main-for-executable ()
  "Default main function for the combined executable.
   Currently defaults to starting the GUI.
   Could be extended to parse command-line arguments to choose CLI or GUI."
  (gui-main))


(format t "~&Building GUI executable 'planner-gui'...~%")
;; Attempt to build the GUI executable.
;; For GUI applications, ensuring the main thread context is correct is important.
;; SBCL's save-lisp-and-die typically captures the current Lisp environment.
;; If start-gui is called, it sets up its own main loop.
(sb-ext:save-lisp-and-die "planner-gui"
                          :toplevel #'gui-main ; Using gui-main directly for a GUI-specific build
                          :executable t
                          :save-runtime-options t
                          ;; :application-type :gui ; May be needed on some platforms (e.g. Windows, macOS)
                                                 ; For Linux, often not strictly necessary but can be good practice.
                          )

(format t "~&GUI executable 'planner-gui' build process initiated.~%")
(format t "~&Note: Building a CLI executable would require a separate SBCL invocation and image, or a more complex build setup.~%")
(format t "~&Exiting build script.~%")

(uiop:quit 0) ; Exit SBCL cleanly after the build attempt.

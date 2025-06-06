;;; common-lisp-planner.asd

(asdf:defsystem #:common-lisp-planner
  :name "common-lisp-planner"
  :version "0.2.0"
  :author "Jules AI Assistant"
  :license "MIT"
  :description "A command-line and basic GUI planner application."
  :depends-on (#:local-time #:cl-cffi-gtk)
  ;; :serial t ; Explicit :depends-on is used for components.
  :components ((:file "src/data-structures")
               (:file "src/file-ops" :depends-on ("src/data-structures"))
               (:file "src/calendar" :depends-on ("src/data-structures" "src/file-ops"))
               (:file "src/todo" :depends-on ("src/data-structures" "src/file-ops"))
               (:file "src/locations" :depends-on ("src/data-structures" "src/file-ops"))
               (:file "src/notes" :depends-on ("src/data-structures" "src/file-ops"))
               (:file "src/materials" :depends-on ("src/data-structures" "src/file-ops"))
               (:file "src/main" :depends-on ("src/calendar" "src/todo" "src/locations" "src/notes" "src/materials"))
               (:file "src/gui" :depends-on ("src/main"
                                             "src/calendar" "src/todo" "src/notes" "src/materials"
                                             "src/data-structures" "src/file-ops"))))

(asdf:defsystem #:common-lisp-planner/tests
  :name "common-lisp-planner/tests"
  :version "0.2.0"
  :author "Jules AI Assistant"
  :license "MIT"
  :description "Test system for the Common Lisp Planner application."
  :depends-on (#:common-lisp-planner #:fiveam)
  :components ((:file "tests/packages")
               (:file "tests/test-calendar" :depends-on ("tests/packages"))
               (:file "tests/test-todo" :depends-on ("tests/packages")))
  :perform (asdf:test-op (o c)
             (declare (ignorable o c))
             (uiop:symbol-call '#:planner-app/tests '#:run-all-tests)))

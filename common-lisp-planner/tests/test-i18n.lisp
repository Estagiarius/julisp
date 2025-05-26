;;; common-lisp-planner/tests/test-i18n.lisp

(in-package #:planner-app/tests)

;; Ensures that symbols from i18n and config are available.
;; This might be better placed in tests/packages.lisp if it's a general need.
;; For now, ensuring these are used by the test package.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:planner/i18n)
    (warn "test-i18n: planner/i18n package not found. Ensure i18n.lisp is loaded."))
  (unless (find-package '#:planner/config)
    (warn "test-i18n: planner/config package not found. Ensure config.lisp is loaded."))
  (unless (find-package '#:planner-app)
    (warn "test-i18n: planner-app package not found. Ensure main.lisp is loaded for handle-set-lang etc.")))

(def-suite i18n-suite
  :description "Unit tests for the Internationalization (i18n) module and language configuration."
  :in planner-app-test-suite)

(in-suite i18n-suite)

;; Helper to ensure dictionaries are loaded for tests
(defvar *i18n-test-setup-done* nil "Flag to ensure setup runs once per test load.")

(defun setup-i18n-for-test ()
  "Loads language dictionaries for testing purposes.
Ensures that the paths are relative to the common-lisp-planner directory."
  ;; Assuming tests are run from the root of the common-lisp-planner directory
  ;; or that the ASDF system (if used) correctly resolves these paths.
  ;; For robustness, one might use *load-truename* or ASDF to find system path.
  (let ((base-path (if *load-truename* 
                       (make-pathname :directory (pathname-directory *load-truename*) :name nil :type nil)
                       (make-pathname :directory '(:relative "common-lisp-planner"))))) ; Fallback if *load-truename* is nil
     ;; Construct full paths relative to the 'common-lisp-planner' directory
     ;; The test files are in common-lisp-planner/tests/
     ;; The lang files are in common-lisp-planner/lang/
     ;; The main.lisp (for planner-app) is in common-lisp-planner/src/
     ;; So, if *load-truename* is .../tests/test-i18n.lisp, then base-path is .../tests/
     ;; We need to go up one level for lang.
     ;; A simpler, more robust way if ASDF is assumed: (asdf:system-relative-pathname :common-lisp-planner "lang/en.lispdata")
     ;; For now, using hardcoded relative paths assuming execution from project root or correct load path.
    (planner/i18n:load-language-dictionary :en "common-lisp-planner/lang/en.lispdata")
    (planner/i18n:load-language-dictionary :pt-br "common-lisp-planner/lang/pt-br.lispdata")))

;; Call setup once when the test file is loaded.
;; This is a common pattern if test state needs to be initialized once.
;; For FiveAM, fixtures or suite setup might be more idiomatic for complex cases.
(unless *i18n-test-setup-done*
  (setup-i18n-for-test)
  (setf *i18n-test-setup-done* t))

;; Macro to create a temporary test configuration file environment
(defmacro with-test-config-file ((&optional (test-filepath "common-lisp-planner/data/test-config.dat")) &body body)
  `(let ((planner/config:*config-filepath* ,test-filepath))
     (unwind-protect
          (progn 
            ;; Ensure the directory for the test config file exists
            (ensure-directories-exist planner/config:*config-filepath*)
            ,@body)
       (when (probe-file planner/config:*config-filepath*)
         (delete-file planner/config:*config-filepath*)))))

;; Placeholder for actual tests to be added in subsequent steps
(test test-trs-english
  "Test trs function for English translations."
  (planner/i18n:set-current-language :en)
  (is (string= (planner/i18n:trs "Welcome to the Common Lisp Planner!~%") 
               "Welcome to the Common Lisp Planner!~%"))
  (is (string= (planner/i18n:trs "Unknown command: '~A'. Type 'help' for available commands.~%" "badcmd")
               "Unknown command: 'badcmd'. Type 'help' for available commands.~%")))

(test test-trs-portuguese
  "Test trs function for Portuguese translations."
  (planner/i18n:set-current-language :pt-br)
  (is (string= (planner/i18n:trs "Welcome to the Common Lisp Planner!~%")
               "Bem-vindo ao Common Lisp Planner!~%"))
  (is (string= (planner/i18n:trs "Unknown command: '~A'. Type 'help' for available commands.~%" "maucomando")
               "Comando desconhecido: 'maucomando'. Digite 'help' para comandos disponíveis.~%")))

(test test-trs-missing-key
  "Test trs function with a key not present in the dictionary."
  (planner/i18n:set-current-language :en) ; Ensure a known dictionary is active
  (let ((missing-key "This key does not exist anywhere."))
    (is (string= (planner/i18n:trs missing-key) missing-key))
    (is (string= (planner/i18n:trs missing-key "arg1" "arg2")
                 (format nil missing-key "arg1" "arg2")))))

(test test-trs-missing-language-dict
  "Test trs function when the current language dictionary is not loaded."
  (planner/i18n:set-current-language :fr) ; Assume :fr dictionary is not loaded
  (let ((test-key "Hello World"))
    (is (string= (planner/i18n:trs test-key) test-key))
    (is (string= (planner/i18n:trs test-key "France") 
                 (format nil test-key "France"))))
  ;; Reset to a known language to avoid affecting other tests if run in same image
  (planner/i18n:set-current-language :en))

(test test-save-load-language-config
  "Test saving and loading language configuration."
  (with-test-config-file ("common-lisp-planner/data/test-config-save-load.dat")
    ;; Test with :pt-br
    (is-true (planner/config:save-language-config :pt-br) "Saving :pt-br should succeed.")
    (is-true (probe-file planner/config:*config-filepath*) "Config file should exist after saving :pt-br.")
    (is (eq (planner/config:load-language-config) :pt-br) "Loading config should return :pt-br.")

    ;; Test with :en, should overwrite
    (is-true (planner/config:save-language-config :en) "Saving :en should succeed.")
    (is-true (probe-file planner/config:*config-filepath*) "Config file should still exist after saving :en.")
    (is (eq (planner/config:load-language-config) :en) "Loading config should return :en.")))

(test test-load-language-config-no-file
  "Test loading language configuration when the config file does not exist."
  (with-test-config-file ("common-lisp-planner/data/test-config-no-file.dat")
    ;; Ensure the file does not exist (it's deleted by with-test-config-file at the end,
    ;; and this specific file path should be unique for this test run if macro is used per test)
    (when (probe-file planner/config:*config-filepath*)
      (delete-file planner/config:*config-filepath*))
    (is-false (probe-file planner/config:*config-filepath*) "Config file should not exist before loading.")
    (is (null (planner/config:load-language-config)) "Loading non-existent config file should return nil.")))

(test test-load-language-config-invalid-file
  "Test loading language configuration from an invalid or malformed config file."
  (with-test-config-file ("common-lisp-planner/data/test-config-invalid.dat")
    ;; Scenario 1: File with non-Lisp readable content (e.g., just a plain string)
    (with-open-file (stream planner/config:*config-filepath* :direction :output :if-exists :supersede)
      (write-string "This is not a valid Lisp form for config" stream))
    (signals warning (planner/config:load-language-config)) ; Expect a warning
    (is (null (planner/config:load-language-config)) 
        "Loading config from file with plain string should return nil.")

    ;; Scenario 2: File with incorrect Lisp data structure
    (planner/config:save-language-config :en) ; Save a valid one first to ensure file exists.
    (with-open-file (stream planner/config:*config-filepath* :direction :output :if-exists :supersede)
      (prin1 '(:language "en") stream)) ; String "en" instead of keyword :en
    (signals warning (planner/config:load-language-config))
    (is (null (planner/config:load-language-config)) 
        "Loading config from file with incorrect Lisp structure (string instead of keyword) should return nil.")

    (with-open-file (stream planner/config:*config-filepath* :direction :output :if-exists :supersede)
      (prin1 '(:wrong-key :en) stream)) ; Wrong keyword for the config key
    (signals warning (planner/config:load-language-config))
    (is (null (planner/config:load-language-config))
        "Loading config from file with a wrong main key should return nil.")
        
    (with-open-file (stream planner/config:*config-filepath* :direction :output :if-exists :supersede)
      (prin1 "just-a-symbol" stream)) ; A symbol, not a list
    (signals warning (planner/config:load-language-config))
    (is (null (planner/config:load-language-config))
        "Loading config from file with a symbol instead of a list should return nil.")))

;; Tests for functions in main.lisp that interact with i18n and config
(test test-handle-set-lang-success
  "Test handle-set-lang successfully changes language and saves config."
  (with-test-config-file ("common-lisp-planner/data/test-main-set-lang.dat")
    (planner/i18n:set-current-language :en) ; Start with English
    (planner/i18n:load-language-dictionary :en planner-app::*lang-en-filepath*) ; Ensure EN is loaded
    (planner/i18n:load-language-dictionary :pt-br planner-app::*lang-pt-br-filepath*) ; Ensure PT-BR is loaded

    ;; Simulate calling 'set-lang pt-br'
    ;; parse-command-line for "set-lang pt-br" would produce ("set-lang" ((0 . "pt-br")))
    (planner-app::handle-set-lang '((0 . "pt-br")))
    
    (is (eq (planner/i18n:get-current-language) :pt-br)
        "Current language should be :pt-br after handle-set-lang.")
    (is (eq (planner/config:load-language-config) :pt-br)
        "Config file should store :pt-br after handle-set-lang success.")
    
    ;; Reset to English for other tests
    (planner-app::handle-set-lang '((0 . "en")))
    (is (eq (planner/i18n:get-current-language) :en)
        "Current language should be back to :en.")))

(test test-handle-set-lang-invalid-code
  "Test handle-set-lang with an unsupported language code."
  (with-test-config-file ("common-lisp-planner/data/test-main-set-lang-invalid.dat")
    (planner/i18n:set-current-language :en)
    (planner/i18n:load-language-dictionary :en planner-app::*lang-en-filepath*)
    
    (planner-app::handle-set-lang '((0 . "fr"))) ; "fr" is not supported
    
    (is (eq (planner/i18n:get-current-language) :en)
        "Current language should remain :en after attempting to set an invalid code.")
    (is (eq (planner/config:load-language-config) :en) 
        "Config file should still reflect :en (or nil if it was the first time and failed).")))

(test test-handle-set-lang-missing-dictionary-file
  "Test handle-set-lang when a dictionary file is missing or fails to load."
  (with-test-config-file ("common-lisp-planner/data/test-main-set-lang-missing-dict.dat")
    (planner/i18n:set-current-language :en)
    (planner/i18n:load-language-dictionary :en planner-app::*lang-en-filepath*)
    
    ;; Temporarily sabotage the :pt-br dictionary loading for this test
    ;; This is a bit of an internal hack for testing.
    ;; A more robust way would be to shadow/mock load-language-dictionary
    ;; or provide a temporary invalid path for *lang-pt-br-filepath*.
    (let ((original-pt-br-dict (gethash :pt-br planner/i18n::*language-dictionaries*))
          (planner-app::*lang-pt-br-filepath* "path/to/nonexistent/pt-br.lispdata")) ; Set to an invalid path
      (unwind-protect
           (progn
             (remhash :pt-br planner/i18n::*language-dictionaries*) ; Ensure it's not loaded
             (planner-app::handle-set-lang '((0 . "pt-br"))))
        ;; Cleanup: Restore the original :pt-br dictionary if it existed
        (if original-pt-br-dict
            (setf (gethash :pt-br planner/i18n::*language-dictionaries*) original-pt-br-dict)
            (remhash :pt-br planner/i18n::*language-dictionaries*))
        ;; And restore the original path variable for other tests
        (setf planner-app::*lang-pt-br-filepath* "common-lisp-planner/lang/pt-br.lispdata"))
      
      (is (eq (planner/i18n:get-current-language) :en)
          "Language should remain :en if :pt-br dictionary fails to load.")
      (is (not (eq (planner/config:load-language-config) :pt-br))
          "Config file should not be :pt-br if dictionary load failed."))))

(test test-initialize-i18n-with-config
  "Test initialize-internationalization when a valid config exists."
  (with-test-config-file ("common-lisp-planner/data/test-init-with-config.dat")
    (planner/config:save-language-config :pt-br)
    (planner/i18n:load-language-dictionary :pt-br planner-app::*lang-pt-br-filepath*) ; Ensure dict is loadable

    (planner-app::initialize-internationalization)
    
    (is (eq (planner/i18n:get-current-language) :pt-br)
        "Current language should be set to :pt-br from config file.")))

(test test-initialize-i18n-no-config
  "Test initialize-internationalization when no config file exists."
  (with-test-config-file ("common-lisp-planner/data/test-init-no-config.dat")
    (when (probe-file planner/config:*config-filepath*)
      (delete-file planner/config:*config-filepath*)) ; Ensure it doesn't exist
    (planner/i18n:load-language-dictionary :en planner-app::*lang-en-filepath*) ; Ensure default :en can be loaded

    (planner-app::initialize-internationalization)
    
    (is (eq (planner/i18n:get-current-language) :en)
        "Current language should default to :en when no config file.")))

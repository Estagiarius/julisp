;;; common-lisp-planner/src/config.lisp

(defpackage #:planner/config
  (:use #:common-lisp)
  (:documentation "Manages user configurations for the Planner, like language preference.")
  (:export #:save-language-config
           #:load-language-config
           #:*config-filepath*)) ; Export for potential use elsewhere if needed

(in-package #:planner/config)

(defvar *config-filepath* "common-lisp-planner/data/config.dat"
  "Default filepath for storing user configurations.")

(defun save-language-config (lang-code)
  "Saves the language configuration to *config-filepath*.
`lang-code` should be a keyword (e.g., :en, :pt-br).
The data is saved as a Lisp readable list: (:language lang-code).
Returns t on success, nil on failure."
  (handler-case
      (with-open-file (stream *config-filepath*
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (prin1 `(:language ,lang-code) stream)
        (terpri stream) ; Ensure newline for readability / future additions
        t)
    (error (e)
      (warn "CONFIG: Error saving language configuration to ~S: ~A" *config-filepath* e)
      nil)))

(defun load-language-config ()
  "Loads the language configuration from *config-filepath*.
Returns the language code keyword (e.g., :en, :pt-br) if found and valid.
Returns nil if the file doesn't exist, is empty, malformed, or config is invalid."
  (handler-case
      (when (probe-file *config-filepath*)
        (with-open-file (stream *config-filepath* :direction :input)
          (let ((config-data (read stream nil :eof)))
            (if (and (listp config-data)
                     (eq (first config-data) :language)
                     (keywordp (second config-data)))
                (second config-data)
                (progn
                  (warn "CONFIG: Invalid or malformed data in ~S." *config-filepath*)
                  nil)))))
    (error (e)
      (warn "CONFIG: Error loading language configuration from ~S: ~A" *config-filepath* e)
      nil)))

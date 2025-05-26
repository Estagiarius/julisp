;;; common-lisp-planner/src/i18n.lisp

(defpackage #:planner/i18n
  (:use #:common-lisp)
  (:documentation "Internationalization (i18n) support for the Planner.
Manages language dictionaries and provides translation functionality.")
  (:export #:*current-language*
           #:load-language-dictionary
           #:trs
           #:set-current-language ;; Added for external setting
           #:get-current-language)) ;; Added for external getting

(in-package #:planner/i18n)

(defvar *current-language* :en
  "The currently active language code (e.g., :en, :pt-br).")

(defvar *language-dictionaries* (make-hash-table :test 'equal)
  "A hash table storing language dictionaries.
Keys are language codes (keywords like :en), values are alists of (\"key\" . \"translation\").")

(defun load-language-dictionary (lang-code filepath)
  "Loads a language dictionary from `filepath` for the given `lang-code`.
The file is expected to contain a single Lisp alist.
Returns t on success, nil on failure (e.g., file not found, read error)."
  (handler-case
      (with-open-file (stream filepath :direction :input :if-does-not-exist nil)
        (when stream
          (let ((dict-data (read stream)))
            (if (listp dict-data) ; Basic check for alist structure
                (progn
                  (setf (gethash lang-code *language-dictionaries*) dict-data)
                  (format t ";; Loaded ~A dictionary from ~A~%" lang-code filepath) ; Diagnostic message
                  t)
                (progn
                  (warn "I18N: Dictionary file ~S for ~A does not contain a valid list." filepath lang-code)
                  nil))))
        (unless stream
          (warn "I18N: Dictionary file ~S for ~A not found." filepath lang-code)
          nil))
    (error (e)
      (warn "I18N: Error loading dictionary file ~S for ~A: ~A" filepath lang-code e)
      nil)))

(defun trs (key-string &rest args)
  "Translates `key-string` into the `*current-language*`.
If `args` are provided, `format nil` is used with the translated string and `args`.
If the key is not found in the current language's dictionary, the `key-string` itself is used (and formatted with `args` if any)."
  (let* ((current-dict (gethash *current-language* *language-dictionaries*))
         (translation-pair (assoc key-string current-dict :test #'string=)))
    (let ((format-string (if translation-pair
                             (cdr translation-pair)
                             key-string)))
      (if args
          (apply #'format nil format-string args)
          format-string))))

(defun set-current-language (lang-code)
  "Sets the *current-language*. Expects a keyword like :en or :pt-br."
  (setf *current-language* lang-code))
  
(defun get-current-language ()
  "Returns the current value of *current-language*."
  *current-language*)

;; Example initial load for default languages (can be moved to main app initialization)
;; (load-language-dictionary :en "common-lisp-planner/lang/en.lispdata")
;; (load-language-dictionary :pt-br "common-lisp-planner/lang/pt-br.lispdata")

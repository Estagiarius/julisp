# Manifest of Translatable String Keys for Common Lisp Planner

This document outlines the string keys used for internationalization (i18n) within the Common Lisp Planner application. These keys are used by the translation system (`planner/i18n:trs`) to look up localized strings.

## Source of Truth

The canonical list of all translatable string keys is maintained in the English dictionary file:
`common-lisp-planner/lang/en.lispdata`

This file contains an association list where each key (the English string used in the source code) is mapped to itself.

## Corresponding Translation Files

Translations for other languages are provided in their respective `.lispdata` files within this `lang/` directory. For example:
*   `pt-br.lispdata`: Contains translations for Brazilian Portuguese.

## Structure of Dictionary Files

Each `.lispdata` file should contain a single Common Lisp association list (alist). Each element in the alist is a cons cell: `("english_key_string" . "translated_string")`.

Example from `pt-br.lispdata`:
```lisp
(
  ("Welcome to the Common Lisp Planner!~%" . "Bem-vindo ao Common Lisp Planner!~%")
  ("Unknown command: '~A'. Type 'help' for available commands.~%" . "Comando desconhecido: '~A'. Digite 'help' para comandos disponíveis.~%")
  ;; ... and so on for all translatable strings
)
```

## Key Categories (Examples)

While the `en.lispdata` file provides the full list, here are some examples of keys grouped by typical usage:

*   **General Application Messages:**
    *   `"Welcome to the Common Lisp Planner!~%"`
    *   `"Type 'help' for a list of commands, 'quit' or 'exit' to leave.~%"`
    *   `"planner> "`
    *   `"Exiting planner. Goodbye!~%"`
    *   `"Planner data initialized.~%"`

*   **Command Feedback (Success/Error for generic operations):**
    *   `"Error: --id is required.~%"`
    *   `"~A added with ID ~A.~%"` (e.g., "Event added with ID 1.")
    *   `"~A ~A updated.~%"` (e.g., "Task 1 updated.")
    *   `"~A ~A not found or not updated.~%"`
    *   `"~A ~A removed.~%"`
    *   `"~A ~A not found or could not be removed.~%"`

*   **Specific Error Messages:**
    *   `"Error: Language code required for set-lang (e.g., en, pt-br).~%"`
    *   `"Error: Unsupported language code '~A'. Supported codes are 'en', 'pt-br'.~%"`
    *   `"Error: Invalid integer value for ~A: '~A'. Details: ~A~%"`
    *   `"Error parsing date/time string '~A': ~A~%"`

*   **Help Command Strings:**
    *   Each line of the `handle-help` output that describes a command, e.g.:
        *   `"  add-event --title <title> --start <YYYY-MM-DD HH:MM> [--end <YYYY-MM-DD HH:MM>] [--desc <desc>] [--loc-id <id>]~%"`
        *   `"  set-lang <lang-code>        Set application language (e.g., en, pt-br).~%"`

---

To add new translations or languages, or to modify existing ones, please refer to the structure of these files and ensure consistency with the keys defined in `en.lispdata`.

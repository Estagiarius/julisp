# Common Lisp Planner

## Project Overview

The Common Lisp Planner is an application written in Common Lisp designed to help users manage various aspects of their lives, including events, tasks, notes, locations, and materials.

### File Structure

The project follows a standard directory layout:

-   `src/`: Contains all the Common Lisp source code files.
    -   `main.lisp`: This file, located within `src/`, serves as the primary entry point for the application, especially for its command-line interface (CLI) and overall startup.
-   `data/`: This directory is designated for storing data files used by the application. These files typically have a `.dat` extension.
-   `tests/`: Contains unit tests for the project to ensure code quality and functionality.

## Command-Line Operation

The "command-line mode" for the Common Lisp Planner is not a typical OS shell command that you run directly (e.g., `./planner --add-event`). Instead, it operates as an interactive session within a Common Lisp Read-Eval-Print Loop (REPL).

### Startup Process

To start the planner in its interactive command-line mode, follow these steps:

1.  **Start your Common Lisp REPL:** Open your preferred Common Lisp environment (e.g., SBCL, CLISP, CCL).
2.  **Navigate to the Project Root:** Ensure your REPL's current directory is the project root (`common-lisp-planner/`). You can usually do this with a command like `(uiop:chdir "/path/to/common-lisp-planner/")` or specific commands for your Lisp environment (e.g., `:cd /path/to/common-lisp-planner/` in SLIME).
3.  **Load Core Files:** Load the necessary source files in the correct order. If you are loading manually, the general order from the `src/` directory should be:
    *   `data-structures.lisp`
    *   `file-ops.lisp`
    *   `i18n.lisp` (for internationalization)
    *   `config.lisp` (for language configuration management)
    *   Followed by modules like `calendar.lisp`, `todo.lisp`, `locations.lisp`, `notes.lisp`, `materials.lisp`.
    *   Finally, load the main application file: `(load "src/main.lisp")`
    
    Example sequence in your REPL (assuming your REPL's current directory is the project root):
    ```lisp
    (load "src/data-structures.lisp")
    (load "src/file-ops.lisp")
    (load "src/i18n.lisp")
    (load "src/config.lisp")
    (load "src/calendar.lisp")
    (load "src/todo.lisp")
    (load "src/locations.lisp")
    (load "src/notes.lisp")
    (load "src/materials.lisp")
    ;; ... ensure all other individual modules from src/ are loaded ...
    (load "src/main.lisp")
    ```
    *(Note: Using an ASDF system definition is recommended for more robust dependency handling in larger Lisp projects, as this manual loading order can be error-prone.)*

4.  **Start the Planner:**
    Once all files are loaded, start the planner's CLI by running:
    ```lisp
    (planner-app:start-planner)
    ```

### Interactive Session

Once `(planner-app:start-planner)` is executed:

-   A welcome message will be displayed in the REPL.
-   The standard Lisp prompt will be replaced by a custom prompt: `planner>`.
-   At this `planner>` prompt, users can type specific commands to interact with the application (e.g., `add-event`, `view-tasks`, `help`).

The application will then parse these textual commands and call the appropriate internal Lisp functions to perform the requested actions, displaying results or further prompts as needed.

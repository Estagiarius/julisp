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

1.  **Start a Common Lisp REPL:** Open your preferred Common Lisp environment (e.g., SBCL, CLISP, CCL).
2.  **Load the application:** Within the REPL, load the main application file using a Lisp command. Typically, this would be something like:
    ```lisp
    (load "path/to/common-lisp-planner/src/main.lisp")
    ```
    *(Ensure you replace `"path/to/common-lisp-planner/"` with the actual path to the project on your system.)*
3.  **Start the planner application:** After the code is loaded, execute the main startup function by typing the following into the REPL:
    ```lisp
    (planner-app:start-planner)
    ```

### Interactive Session

Once `(planner-app:start-planner)` is executed:

-   A welcome message will be displayed in the REPL.
-   The standard Lisp prompt will be replaced by a custom prompt: `planner>`.
-   At this `planner>` prompt, users can type specific commands to interact with the application (e.g., `add-event`, `view-tasks`, `help`).

The application will then parse these textual commands and call the appropriate internal Lisp functions to perform the requested actions, displaying results or further prompts as needed.

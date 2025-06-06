# Common Lisp Planner - A CLI Application

## Overview

The Common Lisp Planner is a command-line interface (CLI) and basic Graphical User Interface (GUI) application designed to help users manage events, tasks, notes, locations, and materials. It provides functionalities to add, view, and manage various aspects of personal or project planning. Data is persisted to `.dat` files in the `data/` directory.

## Features

*   **Calendar Management:** Add, find, edit, remove, and view events (day, week, month). Get reminders for upcoming events. Calendar view marks days with events.
*   **To-Do List:** Add, find, edit, remove, and mark tasks as complete. Set task priorities. View tasks with filtering and sorting. Get summaries of pending/overdue tasks.
*   **Notes:** Add, find, categorize, and view notes. Search notes by category or content.
*   **Locations:** Manage location metadata for events.
*   **Materials:** Manage metadata for related files or materials.
*   **CLI Access:** All functionalities are accessible via a dedicated command-line interface running within the Lisp REPL.
*   **GUI Access:** A basic GTK-based GUI provides a tabbed interface for Planner (Tasks & Calendar/Events) and Notes.
*   **Basic Statistics:** Get counts of past/future events and statistics on task completion.
*   **Simple Reminders:** Identify upcoming events and pending/overdue tasks.
*   **Data Persistence:** Planner data is saved to and loaded from files.

## Dependencies

*   **Common Lisp Implementation:** A standard Common Lisp implementation such as SBCL (Steel Bank Common Lisp), CCL (Clozure Common Lisp), or ECL.
*   **Quicklisp:** The de facto library manager for Common Lisp, used to load project dependencies.
*   **ASDF (Another System Definition Facility):** Used to define and load the project systems.
*   **`local-time` library:** Used for robust date and time parsing.
*   **`cl-cffi-gtk` library:** Used for the GTK GUI. System-level GTK3 development libraries are also required.
*   **`fiveam` library:** A testing framework used for unit tests.

All Lisp library dependencies are managed by ASDF and Quicklisp via the `common-lisp-planner.asd` file.

## Setup

1.  **Prerequisites:**
    *   A Common Lisp implementation (e.g., SBCL, CCL, ECL).
    *   [Quicklisp](https://www.quicklisp.org/beta/) installed and configured (for managing dependencies).
    *   For the GUI: GTK3 development libraries.
        *   On Debian/Ubuntu: `sudo apt-get install libgtk-3-dev`
        *   On Fedora: `sudo dnf install gtk3-devel`
        *   On macOS (using Homebrew): `brew install gtk+3`

2.  **Clone the Repository:**
    ```bash
    git clone <repository-url> # Replace <repository-url> with the actual URL
    cd common-lisp-planner
    ```

3.  **Make Project Discoverable by ASDF/Quicklisp:**
    *   **Option 1 (Recommended):** Place the `common-lisp-planner` project directory (the one containing `common-lisp-planner.asd`) into your Quicklisp local projects directory. Typically, this is `~/quicklisp/local-projects/`.
        ```bash
        # Example:
        # mkdir -p ~/quicklisp/local-projects/
        # cp -r path/to/common-lisp-planner ~/quicklisp/local-projects/
        ```
    *   **Option 2:** Create a symbolic link from your Quicklisp local projects directory to your project directory.
        ```bash
        # Example:
        # ln -s /path/to/your/common-lisp-planner ~/quicklisp/local-projects/common-lisp-planner
        ```
    *   **Option 3:** Add the project's parent directory to ASDF's source registry in your Lisp implementation's init file (e.g., `~/.sbclrc` for SBCL).
        ```lisp
        ;; Example for .sbclrc or equivalent
        (pushnew #P"/path/to/your/common-lisp-planner-parent-directory/" asdf:*central-registry*)
        ```

4.  **Install Dependencies (via Quicklisp):**
    *   Start your Lisp REPL.
    *   If you haven't already, ensure Quicklisp is loaded (this might be in your Lisp init file):
        ```lisp
        ;; (load "~/quicklisp/setup.lisp") ; If not automatically loaded
        ```
    *   The first time you load the system (see "Running the Application" below), Quicklisp will automatically download and install the required Lisp library dependencies (`local-time`, `cl-cffi-gtk`, `fiveam` for tests) as specified in `common-lisp-planner.asd`.

## Running the Application

1.  Start your Common Lisp REPL.
2.  Load the application system using Quicklisp:
    ```lisp
    (ql:quickload :common-lisp-planner)
    ```
    This command will compile and load all source files for the main application.

3.  **To run the Command Line Interface (CLI):**
    ```lisp
    (planner-app:start-planner)
    ```
    You should see a welcome message and the `planner>` prompt. Type `help` for commands.

4.  **To run the Graphical User Interface (GUI):**
    ```lisp
    (planner-app/gui:start-gui)
    ```
    (Note: Ensure your environment has a running X server or equivalent for GUI display, and that GTK3 libraries are correctly installed on your system.)

## CLI Usage

Type `help` at the `planner>` prompt for a comprehensive list of commands and their syntax.

**Examples:**

*   Add a new event:
    ```
    planner> add-event --title "Team Meeting" --start "2024-01-15 10:00" --desc "Discuss project X"
    ```
*   View tasks, sorted by due date, filtering for pending tasks:
    ```
    planner> view-tasks --sort-by due-date --filter-status pending
    ```
*   Add a new note:
    ```
    planner> add-note --title "Shopping List" --content "Milk, Eggs, Bread"
    ```
*   View events for a specific day:
    ```
    planner> view-day 2024-01-15
    ```

## Running Tests

1.  Start your Common Lisp REPL.
2.  Load and run the test system using ASDF:
    ```lisp
    (asdf:test-system :common-lisp-planner/tests)
    ```
    Quicklisp will handle downloading `fiveam` and compiling the main system if they haven't been loaded already.
3.  The tests will run automatically, and FiveAM will print a summary of the results to the REPL.

## Building a Standalone Executable (Experimental)

A build script `build.lisp` is provided to attempt to create a standalone executable using SBCL. This is particularly useful for distributing the application without requiring users to have a full Common Lisp development environment (though they will still need necessary runtime libraries like GTK for the GUI version).

**Prerequisites for Building:**
*   SBCL (Steel Bank Common Lisp) installed.
*   Quicklisp setup, with the project and its dependencies (`local-time`, `cl-cffi-gtk`) loadable via ASDF (e.g., project placed in `~/quicklisp/local-projects/`).

**How to Attempt the Build:**
1.  Navigate to the project's root directory in your terminal.
2.  Run the build script with SBCL:
    ```bash
    sbcl --no-userinit --load build.lisp
    ```
3.  If successful, this script attempts to create an executable named `planner-gui` (for the GUI version) in the project root.

**Important Notes & Caveats:**
*   **ASDF Loading:** The build script relies on ASDF correctly loading the `:common-lisp-planner` system. If you encounter ASDF errors (like 'file not found' for components), ensure your ASDF source registry is configured correctly to find the project, or that the project is in `~/quicklisp/local-projects/`. Some environments might require specific ASDF configuration tweaks.
*   **GUI Executable Runtime Dependencies:** If you build the GUI executable (`planner-gui`), it will still require the GTK3 runtime libraries to be installed on the system where it's run. The executable bundles the Lisp code and Lisp runtime, not the C libraries for GTK.
*   **Build Success:** Successful creation of the executable can depend on your specific SBCL version, operating system, and the configuration of foreign libraries (like GTK).
*   **CLI Executable:** The `build.lisp` script is currently focused on building the GUI executable. To build a CLI version, you would need to modify the `:toplevel` function in the `sb-ext:save-lisp-and-die` call within `build.lisp` to point to `cli-main` instead of `gui-main`.

## Project Structure

*   **`common-lisp-planner.asd`**: The ASDF system definition file for the project, including the main application and the test system.
*   **`build.lisp`**: A script to attempt building a standalone executable (primarily for the GUI).
*   **`src/`**: Contains the core Lisp source code for the planner.
    *   `data-structures.lisp`: Defines common data structures (events, tasks, etc.).
    *   `file-ops.lisp`: Handles saving and loading data to/from files.
    *   `calendar.lisp`: Event management logic.
    *   `todo.lisp`: Task management logic.
    *   `locations.lisp`: Location management logic.
    *   `notes.lisp`: Note management logic.
    *   `materials.lisp`: Material metadata management logic.
    *   `main.lisp`: Implements the Command Line Interface (CLI) and application entry point for the CLI.
    *   `gui.lisp`: Implements the GTK-based Graphical User Interface.
*   **`data/`**: Default directory where data files (e.g., `events.dat`, `tasks.dat`) are stored. This directory is created automatically if it doesn't exist when data is first saved by the application.
*   **`tests/`**: Contains unit tests for the project.
    *   `packages.lisp`: Defines the test package and test runner utilities.
    *   `test-calendar.lisp`: Tests for the calendar module.
    *   `test-todo.lisp`: Tests for the todo module.
*   **`README.md`**: This file.

---
*This project serves as a demonstration of building a CLI and basic GUI application in Common Lisp, covering various aspects of software development including modular design, data persistence, and unit testing.*

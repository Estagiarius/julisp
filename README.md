# Common Lisp Planner - A CLI Application

## Overview

The Common Lisp Planner is a command-line interface (CLI) application designed to help users manage events, tasks, notes, locations, and materials. It provides functionalities to add, view, and manage various aspects of personal or project planning, all within a Lisp REPL environment. Data is persisted to `.dat` files in the `data/` directory.

## Features

*   **Calendar Management:** Add, find, edit, remove, and view events (day, week, month). Get reminders for upcoming events.
*   **To-Do List:** Add, find, edit, remove, and mark tasks as complete. Set task priorities. View tasks with filtering and sorting. Get summaries of pending/overdue tasks.
*   **Notes:** Add, find, categorize, and view notes. Search notes by category or content.
*   **Locations:** Manage location metadata for events.
*   **Materials:** Manage metadata for related files or materials.
*   **CLI Access:** All functionalities are accessible via a dedicated command-line interface running within the Lisp REPL.
*   **Basic Statistics:** Get counts of past/future events and statistics on task completion.
*   **Simple Reminders:** Identify upcoming events and pending/overdue tasks.
*   **Data Persistence:** Planner data is saved to and loaded from files.

## Dependencies

*   **Common Lisp Implementation:** A standard Common Lisp implementation such as SBCL (Steel Bank Common Lisp) or CCL (Clozure Common Lisp).
*   **Quicklisp:** The de facto library manager for Common Lisp, used to load project dependencies.
*   **`local-time` library:** Used for robust date and time parsing in the CLI. Loaded via Quicklisp.
*   **`fiveam` library:** A testing framework used for unit tests. Loaded via Quicklisp.

## Setup

1.  **Clone the Repository:**
    ```bash
    git clone <repository-url>
    cd common-lisp-planner
    ```
2.  **Ensure Quicklisp is Installed:**
    If you don't have Quicklisp, visit [quicklisp.org](https://www.quicklisp.org) for installation instructions.

3.  **Load Dependencies:**
    Dependencies (`local-time` and `fiveam`) are loaded automatically via `ql:quickload` when the relevant Lisp files are loaded:
    *   `local-time` is loaded when `src/main.lisp` is compiled/loaded.
    *   `fiveam` is loaded when `tests/packages.lisp` is compiled/loaded.

## Running the Application

1.  **Load the Main Application File:**
    Start your Common Lisp REPL and load the main application file. Ensure your REPL's current directory is the project root (`common-lisp-planner/`).
    ```lisp
    (load "src/main.lisp")
    ```
    This will compile and load all necessary source files from the `src/` directory.

2.  **Start the Planner:**
    Once loaded, start the planner's CLI by running:
    ```lisp
    (planner-app:start-planner)
    ```
    You should see a welcome message and the `planner>` prompt.

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

1.  **Load the Test Setup File:**
    In your Lisp REPL (from the project root):
    ```lisp
    (load "tests/packages.lisp")
    ```
    This will load FiveAM and the test definitions.

2.  **Run All Tests:**
    ```lisp
    (planner-app/tests:run-all-tests)
    ```
    Test results will be printed to the REPL.

## Project Structure

*   **`src/`**: Contains the core Lisp source code for the planner.
    *   `data-structures.lisp`: Defines common data structures (events, tasks, etc.).
    *   `file-ops.lisp`: Handles saving and loading data to/from files.
    *   `calendar.lisp`: Event management logic.
    *   `todo.lisp`: Task management logic.
    *   `locations.lisp`: Location management logic.
    *   `notes.lisp`: Note management logic.
    *   `materials.lisp`: Material metadata management logic.
    *   `main.lisp`: Implements the Command Line Interface (CLI) and application entry point.
*   **`data/`**: Default directory where data files (e.g., `events.dat`, `tasks.dat`) are stored. This directory is created automatically if it doesn't exist when data is first saved.
*   **`tests/`**: Contains unit tests for the project.
    *   `packages.lisp`: Defines the test package and test runner utilities.
    *   `test-calendar.lisp`: Tests for the calendar module.
    *   `test-todo.lisp`: Tests for the todo module.
    *   *(Other test files for different modules would go here)*
*   **`README.md`**: This file.

---
*This project serves as a demonstration of building a CLI application in Common Lisp, covering various aspects of software development including modular design, data persistence, and unit testing.*

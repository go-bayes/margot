#' Create Database of Measures (DEPRECATED)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated. Please use `boilerplate::boilerplate_manage_measures()` instead.
#' Install the new package with `devtools::install_github("go-bayes/boilerplate")`.
#'
#' @param ... All arguments (ignored)
#'
#' @return A message indicating the function is deprecated.
#'
#' @examples
#' \dontrun{
#' # This function is deprecated. Use instead:
#' # devtools::install_github("go-bayes/boilerplate")
#' # library(boilerplate)
#' # boilerplate::boilerplate_manage_measures(...)
#' }
#'
#' @import lifecycle
#' @import cli
#'
#' @export
margot_create_database <- function(...) {
  lifecycle::deprecate_warn(
    when = "0.2.1.23",
    what = "margot_create_database()",
    with = "boilerplate::boilerplate_manage_measures()"
  )

  cli::cli_alert_warning("This function is deprecated. Please use boilerplate::boilerplate_manage_measures() instead.")
  cli::cli_alert_info("Install the new package with: devtools::install_github(\"go-bayes/boilerplate\")")
  cli::cli_alert_info("After installation, load the package with: library(boilerplate)")
}
#' #' Make Measure Databases
#' #'
#' #' This function provides a command-line interface for managing a database of longitudinal measures.
#' #' It allows users to create, view, modify, and delete measures, as well as save and load measure databases.
#' #'
#' #' @param measures_path Character string. The path to the directory where measure databases are stored.
#' #'   If NULL (default), the current working directory is used.
#' #'
#' #' @return This function does not return a value. It runs an interactive command-line interface.
#' #'
#' #' @details
#' #' The function provides the following main operations:
#' #' \itemize{
#' #'   \item Create a new measures database
#' #'   \item Open an existing measures database
#' #'   \item List available .rds files in the specified directory
#' #'   \item List measures in the current database
#' #'   \item Add a new measure
#' #'   \item Delete an existing measure
#' #'   \item Modify an existing measure
#' #'   \item Save the current measures data
#' #'   \item Batch edit measures
#' #' }
#' #'
#' #' Each measure in the database contains the following fields:
#' #' \itemize{
#' #'   \item name: The name of the measure
#' #'   \item items: A list of items or questions in the measure
#' #'   \item description: A description of the measure
#' #'   \item reference: A reference for the measure
#' #'   \item waves: The waves in which the measure was used
#' #'   \item keywords: Keywords associated with the measure
#' #' }
#' #'
#' #' @note
#' #' This function uses a command-line interface and is designed to be run interactively.
#' #' It uses local state management and does not modify global variables.
#' #' Changes are only saved to disk when explicitly requested by the user.
#' #'
#' #' @import rlang
#' #' @import here
#' #' @import cli
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Run the function with the default path (current working directory)
#' #' margot_create_database()
#' #'
#' #' # Run the function with a specific path
#' #' margot_create_database("path/to/measures/directory")
#' #' }
#' #'
#' #' @export
#' margot_create_database <- function(measures_path = NULL) {
#'   require(rlang)
#'   require(here)
#'   require(cli)
#'
#'   measures_path <- measures_path %||% here::here()
#'   current_file <- NULL  # Initialize current_file at the top level
#'
#'   # Modify the get_input function to handle multiline input
#'   get_input <- function(prompt, allow_empty = FALSE, multiline = FALSE) {
#'     if (multiline) {
#'       return(get_multiline_input(prompt))
#'     }
#'
#'     while (TRUE) {
#'       input <- trimws(readline(cli::col_cyan(paste0(prompt, " (enter 'b' to go back): "))))
#'       if (tolower(input) == "b") {
#'         return("__back__")
#'       }
#'       if (input != "" || allow_empty)
#'         return(input)
#'       cli::cli_alert_danger("Input cannot be empty. Please try again.")
#'     }
#'   }
#'
#'   # Add a new function for multiline input
#'   get_multiline_input <- function(prompt) {
#'     cli::cli_text(cli::col_cyan(prompt))
#'     cli::cli_text(cli::col_cyan("Enter your text (press Enter twice on an empty line to finish):"))
#'     lines <- character()
#'     empty_line_count <- 0
#'     repeat {
#'       line <- readline()
#'       if (line == "") {
#'         empty_line_count <- empty_line_count + 1
#'         if (empty_line_count == 2) {
#'           break
#'         }
#'       } else {
#'         empty_line_count <- 0
#'       }
#'       lines <- c(lines, line)
#'     }
#'     paste(lines, collapse = "\n")
#'   }
#'
#'   # Modify the enter_or_modify_measure function
#'   enter_or_modify_measure <- function(existing_measure = NULL) {
#'     measure <- existing_measure %||% list()
#'     fields <- c("name", "items", "description", "reference", "waves", "keywords")
#'
#'     for (field in fields) {
#'       current_value <- measure[[field]]
#'
#'       if (field == "description") {
#'         cli::cli_text(cli::col_grey("Example: Frequency of alcohol consumption was measured using a single item..."))
#'         cli::cli_text("Current value: {.val {if (is.null(current_value)) 'None' else current_value}}")
#'         new_value <- get_input(
#'           cli::col_blue("Enter new description:"),
#'           allow_empty = TRUE,
#'           multiline = TRUE
#'         )
#'       } else if (field == "items") {
#'         if (is.null(current_value)) {
#'           measure[[field]] <- list()
#'           cli::cli_h3("Enter items (press Enter without typing anything to finish):")
#'           cli::cli_text("Example: How often do you have a drink containing alcohol?")
#'           item_num <- 1
#'           repeat {
#'             item <- get_input(cli::col_blue(paste("Item", item_num, "(or press enter to finish):")), allow_empty = TRUE)
#'             if (item == "__back__") return("__back__")
#'             if (item == "") break
#'             measure[[field]][[item_num]] <- item
#'             item_num <- item_num + 1
#'           }
#'           if (length(measure[[field]]) == 0) {
#'             cli::cli_alert_danger("Error: At least one item is required. Please enter an item.")
#'             next
#'           }
#'         } else {
#'           cli::cli_h3("Current items:")
#'           cli::cli_ol(current_value)
#'           modify <- tolower(get_input("Do you want to modify the items? (y/n): ")) == "y"
#'           if (modify == "__back__") return("__back__")
#'           if (modify) {
#'             measure[[field]] <- list()
#'             cli::cli_h3("Enter new items (press Enter without typing anything to finish):")
#'             item_num <- 1
#'             repeat {
#'               item <- get_input(cli::col_blue(paste("Item", item_num, "(or press enter to finish):")), allow_empty = TRUE)
#'               if (item == "__back__") return("__back__")
#'               if (item == "") break
#'               measure[[field]][[item_num]] <- item
#'               item_num <- item_num + 1
#'             }
#'           }
#'         }
#'       } else {
#'         example <- switch(
#'           field,
#'           name = "Example: alcohol_frequency",
#'           reference = "Example: [@nzavs2009]",
#'           waves = "Example: 1-current or 1-15",
#'           keywords = 'Example: alcohol, frequency, consumption (optional, press enter to skip)'
#'         )
#'         cli::cli_text(cli::col_grey(example))
#'         cli::cli_text("Current value: {.val {if (is.null(current_value)) 'None' else paste(current_value, collapse = ', ')}}")
#'
#'         new_value <- get_input(
#'           cli::col_blue(paste("Enter new", field, "(press enter to keep current or skip):")),
#'           allow_empty = TRUE
#'         )
#'       }
#'
#'       # Update the measure with the new value
#'       if (!is.null(new_value) && new_value != "" && new_value != "__back__") {
#'         if (field == "keywords" && new_value != "") {
#'           keywords <- strsplit(new_value, ",")[[1]]
#'           keywords <- sapply(keywords, trimws)
#'           measure[[field]] <- keywords  # Store as a character vector, not a single string
#'         } else {
#'           measure[[field]] <- new_value
#'         }
#'       }
#'     }
#'
#'     return(measure)
#'   }
#'
#'   list_rds_files <- function() {
#'     files <- list.files(measures_path, pattern = "\\.rds$")
#'     if (length(files) == 0) {
#'       cli::cli_alert_warning("No .rds files found in the directory.")
#'       return(NULL)
#'     } else {
#'       cli::cli_h2("Available .rds files:")
#'       cli::cli_ol(files)
#'       return(files)
#'     }
#'   }
#'
#'   load_data <- function(file_name) {
#'     file_path <- file.path(measures_path, file_name)
#'     if (file.exists(file_path)) {
#'       measure_data <- readRDS(file_path)
#'       cli::cli_alert_success("Data loaded from: {.file {file_path}}")
#'       return(list(measure_data = measure_data, current_file = file_name))
#'     } else {
#'       cli::cli_alert_danger("File not found: {.file {file_path}}")
#'       cli::cli_alert_info("Available files in {.file {measures_path}}:")
#'       list_rds_files()
#'       return(NULL)
#'     }
#'   }
#'
#'   save_data <- function(measure_data, file_name, backup = FALSE) {
#'     tryCatch({
#'       file_path <- file.path(measures_path, file_name)
#'       saveRDS(measure_data, file = file_path)
#'       cli::cli_alert_success("Data saved as: {.file {file_path}}")
#'
#'       if (backup) {
#'         backup_file <- paste0("backup_", file_name)
#'         backup_path <- file.path(measures_path, backup_file)
#'         saveRDS(measure_data, file = backup_path)
#'         cli::cli_alert_success("Backup saved as: {.file {backup_path}}")
#'       }
#'       return(file_name)
#'     }, error = function(e) {
#'       cli::cli_alert_danger("Error saving data: {e$message}")
#'       return(NULL)
#'     })
#'   }
#'
#'   list_measures <- function(measure_data) {
#'     return(names(measure_data))
#'   }
#'
#'   delete_measure <- function(measure_data, name) {
#'     if (name %in% names(measure_data)) {
#'       measure_data[[name]] <- NULL
#'       measure_data <- measure_data[order(names(measure_data))]
#'       cli::cli_alert_success("Measure {.val {name}} deleted successfully.")
#'     } else {
#'       cli::cli_alert_danger("Measure {.val {name}} not found.")
#'     }
#'     return(measure_data)
#'   }
#'
#'   create_new_database <- function() {
#'     cli::cli_h2("Creating a new measures database")
#'
#'     db_name <- get_input("Enter a name for the new database (without .rds extension): ")
#'     if (db_name == "__back__")
#'       return(NULL)
#'     if (!grepl("\\.rds$", db_name)) {
#'       db_name <- paste0(db_name, ".rds")
#'     }
#'
#'     full_path <- file.path(measures_path, db_name)
#'     cli::cli_alert_info("The database will be created at: {.file {full_path}}")
#'     confirm <- tolower(get_input("Is this correct? (y/n): "))
#'     if (confirm == "__back__")
#'       return(NULL)
#'
#'     if (confirm == "y") {
#'       new_measure_data <- list()
#'       saveRDS(new_measure_data, file = full_path)
#'       cli::cli_alert_success("New measures database '{.file {db_name}}' created.")
#'       return(list(measure_data = new_measure_data, current_file = db_name))
#'     } else {
#'       cli::cli_alert_warning("Database creation cancelled.")
#'       return(NULL)
#'     }
#'   }
#'
#'   review_and_save_measure <- function(measure_data, measure, current_file, is_new = TRUE) {
#'     if (is.character(measure) && measure == "__back__") {
#'       return(list(saved = FALSE, measure_data = measure_data))
#'     }
#'
#'     while (TRUE) {
#'       cli::cli_h2("Review your entries:")
#'       print(measure)
#'       cli::cli_h3("What would you like to do?")
#'       cli::cli_ol(c(
#'         "Save measure",
#'         "Modify measure",
#'         "Start over",
#'         "Cancel"
#'       ))
#'
#'       choice <- get_input("Enter your choice: ")
#'       if (choice == "__back__")
#'         return(list(saved = FALSE, measure_data = measure_data))
#'       choice <- as.integer(choice)
#'
#'       if (is.na(choice)) {
#'         cli::cli_alert_danger("Invalid choice. Please enter a number.")
#'         next
#'       }
#'
#'       if (choice == 1) {
#'         if (is_new) {
#'           measure_data[[measure$name]] <- measure
#'         } else {
#'           for (field in names(measure)) {
#'             measure_data[[measure$name]][[field]] <- measure[[field]]
#'           }
#'         }
#'         measure_data <- measure_data[order(names(measure_data))]
#'         cli::cli_alert_success("Measure {.val {measure$name}} saved successfully.")
#'
#'         # Automatically save the measures data file
#'         if (!is.null(current_file)) {
#'           saveRDS(measure_data, file = file.path(measures_path, current_file))
#'           cli::cli_alert_success("Measure modified and database updated.")
#'           cli::cli_alert_success("Database saved.")
#'         } else {
#'           cli::cli_alert_warning("Database not saved. No current file set.")
#'         }
#'
#'         return(list(saved = TRUE, measure_data = measure_data))
#'       } else if (choice == 2) {
#'         modified_measure <- enter_or_modify_measure(measure)
#'         if (is.character(modified_measure) && modified_measure == "__back__") {
#'           next
#'         } else {
#'           measure <- modified_measure
#'         }
#'       } else if (choice == 3) {
#'         new_measure <- enter_or_modify_measure()
#'         if (is.character(new_measure) && new_measure == "__back__") {
#'           next
#'         } else {
#'           measure <- new_measure
#'           is_new <- TRUE
#'         }
#'       } else if (choice == 4) {
#'         return(list(saved = FALSE, measure_data = measure_data))
#'       } else {
#'         cli::cli_alert_danger("Invalid choice. Please try again.")
#'       }
#'     }
#'   }
#'
#'   batch_edit_measures <- function(measures_data, field, old_value, new_value) {
#'     edited_count <- 0
#'
#'     for (measure_name in names(measures_data)) {
#'       measure <- measures_data[[measure_name]]
#'       if (field %in% names(measure)) {
#'         if (is.character(measure[[field]])) {
#'           if (measure[[field]] == old_value) {
#'             measures_data[[measure_name]][[field]] <- new_value
#'             edited_count <- edited_count + 1
#'           }
#'         } else if (is.list(measure[[field]])) {
#'           for (i in seq_along(measure[[field]])) {
#'             if (measure[[field]][[i]] == old_value) {
#'               measures_data[[measure_name]][[field]][[i]] <- new_value
#'               edited_count <- edited_count + 1
#'             }
#'           }
#'         }
#'       }
#'     }
#'
#'     cli::cli_alert_success("Edited {edited_count} entries.")
#'     return(measures_data)
#'   }
#'
#'   copy_measure_info <- function(measure_data, current_file) {
#'     measures <- names(measure_data)
#'     if (length(measures) == 0) {
#'       cli::cli_alert_warning("No measures available to copy from.")
#'       return(measure_data)
#'     }
#'
#'     cli::cli_h3("Available measures to copy from:")
#'     cli::cli_ol(measures)
#'
#'     from_choice <- as.integer(get_input("Enter the number of the measure to copy from: "))
#'     if (is.na(from_choice) || from_choice < 1 || from_choice > length(measures)) {
#'       cli::cli_alert_danger("Invalid choice. Returning to main menu.")
#'       return(measure_data)
#'     }
#'     from_measure <- measures[from_choice]
#'
#'     fields <- c("description", "reference", "waves", "keywords", "items")
#'     available_fields <- fields[fields %in% names(measure_data[[from_measure]])]
#'
#'     cli::cli_h3("Available fields to copy:")
#'     cli::cli_ol(available_fields)
#'
#'     field_choice <- as.integer(get_input("Enter the number of the field to copy (0 to copy all): "))
#'     if (is.na(field_choice) || field_choice < 0 || field_choice > length(available_fields)) {
#'       cli::cli_alert_danger("Invalid choice. Returning to main menu.")
#'       return(measure_data)
#'     }
#'
#'     if (field_choice == 0) {
#'       fields_to_copy <- available_fields
#'     } else {
#'       fields_to_copy <- available_fields[field_choice]
#'     }
#'
#'     # Create a new measure or modify an existing one
#'     new_measure <- list()
#'     for (field in fields_to_copy) {
#'       new_measure[[field]] <- measure_data[[from_measure]][[field]]
#'     }
#'
#'     # Use enter_or_modify_measure to create or modify the measure
#'     modified_measure <- enter_or_modify_measure(new_measure)
#'
#'     if (is.character(modified_measure) && modified_measure == "__back__") {
#'       cli::cli_alert_info("Operation cancelled. Returning to main menu.")
#'       return(measure_data)
#'     }
#'
#'     # Review and save the new/modified measure
#'     result <- review_and_save_measure(measure_data, modified_measure, current_file)
#'     if (result$saved) {
#'       cli::cli_alert_success("Measure created/modified successfully with copied information.")
#'       return(result$measure_data)
#'     } else {
#'       cli::cli_alert_warning("Changes were not saved.")
#'       return(measure_data)
#'     }
#'   }
#'   run_gui <- function() {
#'     cli::cli_h1("Welcome to the Margot's Measures Database Manager")
#'     measure_data <- list()
#'     current_file <- NULL
#'
#'     repeat {
#'       cli::cli_h2("Boilerplate Measures Manager")
#'       cli::cli_ol(c(
#'         "Create new measures database",
#'         "Open existing measures database",
#'         "List available .rds files",
#'         "Quit"
#'       ))
#'
#'       choice <- get_input("Enter your choice: ")
#'       if (choice == "__back__") {
#'         next
#'       }
#'       choice <- as.integer(choice)
#'
#'       if (is.na(choice)) {
#'         cli::cli_alert_danger("Invalid choice. Please enter a number.")
#'         next
#'       }
#'
#'       if (choice == 1) {
#'         result <- create_new_database()
#'         if (!is.null(result)) {
#'           measure_data <- result$measure_data
#'           current_file <- result$current_file
#'
#'           repeat {
#'             new_measure <- enter_or_modify_measure()
#'             if (is.character(new_measure) && length(new_measure) == 1 && new_measure == "__back__") {
#'               break
#'             }
#'             review_result <- review_and_save_measure(measure_data, new_measure, current_file)
#'             if (review_result$saved) {
#'               measure_data <- review_result$measure_data
#'               cli::cli_alert_success("Measure saved to database.")
#'
#'               continue <- tolower(get_input("Would you like to enter another measure? (y/n): ")) == "y"
#'               if (!continue) {
#'                 break
#'               }
#'             }
#'           }
#'         }
#'       } else if (choice == 2) {
#'         files <- list_rds_files()
#'         if (!is.null(files)) {
#'           file_choice <- get_input("Enter the number of the file you want to open: ")
#'           if (file_choice == "__back__")
#'             next
#'           file_choice <- as.integer(file_choice)
#'           if (is.na(file_choice)) {
#'             cli::cli_alert_danger("Invalid choice. Please enter a number.")
#'             next
#'           }
#'           if (file_choice > 0 && file_choice <= length(files)) {
#'             file_name <- files[file_choice]
#'             result <- load_data(file_name)
#'             if (!is.null(result)) {
#'               measure_data <- result$measure_data
#'               current_file <- result$current_file
#'             }
#'           } else {
#'             cli::cli_alert_danger("Invalid choice. Please try again.")
#'             next
#'           }
#'         } else {
#'           next
#'         }
#'       } else if (choice == 3) {
#'         list_rds_files()
#'         next
#'       } else if (choice == 4) {
#'         confirm_quit <- tolower(get_input(
#'           "Are you sure you want to quit? Unsaved changes will be lost. (y/n): "
#'         ))
#'         if (confirm_quit == "y") {
#'           cli::cli_alert_success("Exiting program. Goodbye!")
#'           return()
#'         } else {
#'           next
#'         }
#'       } else {
#'         cli::cli_alert_danger("Invalid choice. Please try again.")
#'         next
#'       }
#'
#'       if (choice == 1 || choice == 2)
#'         break
#'     }
#'
#'     repeat {
#'       cli::cli_h2("Measures Database Management")
#'       cli::cli_ol(c(
#'         "List measures",
#'         "Add measure",
#'         "Delete measure",
#'         "Modify measure",
#'         "Copy to new/existing measure",
#'         "Create backup measures data",
#'         "Batch edit measures",
#'         "Exit"
#'       ))
#'
#'       choice <- get_input("Enter your choice: ")
#'       if (choice == "__back__") {
#'         break
#'       }
#'       choice <- as.integer(choice)
#'
#'       if (is.na(choice)) {
#'         cli::cli_alert_danger("Invalid choice. Please enter a number.")
#'         next
#'       }
#'
#'       switch(choice,
#'              {
#'                # Option 1: List measures
#'                measures <- list_measures(measure_data)
#'                if (length(measures) > 0) {
#'                  cli::cli_h3("Available measures:")
#'                  cli::cli_ol(measures)
#'                } else {
#'                  cli::cli_alert_warning("No measures available.")
#'                }
#'              },
#'              {
#'                # Option 2: Add measure
#'                new_measure <- enter_or_modify_measure()
#'                if (!is.character(new_measure) || new_measure != "__back__") {
#'                  result <- review_and_save_measure(measure_data, new_measure, current_file)
#'                  measure_data <- result$measure_data
#'                }
#'              },
#'              {
#'                # Option 3: Delete measure
#'                measures <- list_measures(measure_data)
#'                if (length(measures) > 0) {
#'                  cli::cli_h3("Available measures:")
#'                  cli::cli_ol(measures)
#'                  choice <- get_input("Enter the number of the measure to delete: ")
#'                  if (choice != "__back__") {
#'                    choice <- as.integer(choice)
#'                    if (is.na(choice)) {
#'                      cli::cli_alert_danger("Invalid choice. Please enter a number.")
#'                    } else if (choice > 0 && choice <= length(measures)) {
#'                      measure_data <- delete_measure(measure_data, measures[choice])
#'                      saveRDS(measure_data, file = file.path(measures_path, current_file))
#'                      cli::cli_alert_success("Measure deleted and database updated.")
#'                    } else {
#'                      cli::cli_alert_danger("Invalid choice. Please try again.")
#'                    }
#'                  }
#'                } else {
#'                  cli::cli_alert_warning("No measures available to delete.")
#'                }
#'              },
#'              {
#'                # Option 4: Modify measure
#'                measures <- list_measures(measure_data)
#'                if (length(measures) > 0) {
#'                  cli::cli_h3("Available measures:")
#'                  cli::cli_ol(measures)
#'                  choice <- get_input("Enter the number of the measure to modify: ")
#'
#'                  if (choice != "__back__") {
#'                    choice <- as.integer(choice)
#'                    if (is.na(choice)) {
#'                      cli::cli_alert_danger("Invalid choice. Please enter a number.")
#'                    } else if (choice > 0 && choice <= length(measures)) {
#'                      measure <- enter_or_modify_measure(measure_data[[measures[choice]]])
#'                      if (!is.character(measure) || measure != "__back__") {
#'                        result <- review_and_save_measure(measure_data, measure, current_file, is_new = FALSE)
#'                        measure_data <- result$measure_data
#'                      }
#'                    } else {
#'                      cli::cli_alert_danger("Invalid choice. Please try again.")
#'                    }
#'                  }
#'                } else {
#'                  cli::cli_alert_warning("No measures available to modify.")
#'                }
#'              },
#'              {
#'                # Option 5: Copy to new/existing measure
#'                measure_data <- copy_measure_info(measure_data, current_file)
#'              },
#'              {
#'                # Option 6: Create backup measures data
#'                default_name <- gsub("^backup_", "", current_file %||% "measures_data.rds")
#'                cli::cli_h3("Save options:")
#'                cli::cli_ol(c(
#'                  paste("Use backup file name:", paste0("backup_", default_name)),
#'                  "Enter a new file name"
#'                ))
#'                save_choice <- get_input("Enter your choice: ")
#'                if (save_choice != "__back__") {
#'                  save_choice <- as.integer(save_choice)
#'                  if (is.na(save_choice)) {
#'                    cli::cli_alert_warning("Invalid choice. Using default backup name.")
#'                    file_name <- default_name
#'                  } else if (save_choice == 1) {
#'                    file_name <- default_name
#'                  } else if (save_choice == 2) {
#'                    file_name <- get_input("Enter new file name (including .rds extension): ")
#'                    if (file_name == "__back__")
#'                      next
#'                  } else {
#'                    cli::cli_alert_warning("Invalid choice. Using default backup name.")
#'                    file_name <- default_name
#'                  }
#'
#'                  save_data(measure_data, file_name, backup = TRUE)
#'                }
#'              },
#'              {
#'                # Option 7: Batch edit measures
#'                field <- get_input("Enter the field to edit (e.g., 'reference', 'name', 'description'): ")
#'                if (field != "__back__") {
#'                  old_value <- get_input("Enter the old value: ")
#'                  if (old_value != "__back__") {
#'                    new_value <- get_input("Enter the new value: ")
#'                    if (new_value != "__back__") {
#'                      measure_data <- batch_edit_measures(measure_data, field, old_value, new_value)
#'                      saveRDS(measure_data, file = file.path(measures_path, current_file))
#'                      cli::cli_alert_success("Batch edit completed and database updated.")
#'                    }
#'                  }
#'                }
#'              },
#'              {
#'                # Option 8: Exit
#'                cli::cli_alert_success("Exited. Have a nice day! \U0001F600 \U0001F44D")
#'                break
#'              },
#'              {
#'                # Default: Invalid choice
#'                cli::cli_alert_danger("Invalid choice. Please try again.")
#'              }
#'       )
#'     }
#'   }
#'
#'   run_gui()
#' }
#' # margot_create_database <- function(measures_path = NULL) {
#' #   require(rlang)
#' #   require(here)
#' #   require(cli)
#' #
#' #   measures_path <- measures_path %||% here::here()
#' #   current_file <- NULL  # Initialize current_file at the top level
#' #
#' #   get_multiline_input <- function(prompt) {
#' #     cli::cli_text(cli::col_cyan(prompt))
#' #     cli::cli_text(cli::col_cyan("Enter your text (press Enter on an empty line to finish):"))
#' #     lines <- character()
#' #     repeat {
#' #       line <- readline()
#' #       if (line == "") {
#' #         break
#' #       }
#' #       lines <- c(lines, line)
#' #     }
#' #     paste(lines, collapse = "\n")
#' #   }
#' #
#' #
#' #
#' #   get_input <- function(prompt, allow_empty = FALSE, multiline = FALSE) {
#' #     if (multiline) {
#' #       return(get_multiline_input(prompt))
#' #     }
#' #
#' #     while (TRUE) {
#' #       input <- trimws(readline(cli::col_cyan(paste0(prompt, " (enter 'b' to go back): "))))
#' #       if (tolower(input) == "b") {
#' #         return("__back__")
#' #       }
#' #       if (input != "" || allow_empty)
#' #         return(input)
#' #       cli::cli_alert_danger("Input cannot be empty. Please try again.")
#' #     }
#' #   }
#' #
#' #   list_rds_files <- function() {
#' #     files <- list.files(measures_path, pattern = "\\.rds$")
#' #     if (length(files) == 0) {
#' #       cli::cli_alert_warning("No .rds files found in the directory.")
#' #       return(NULL)
#' #     } else {
#' #       cli::cli_h2("Available .rds files:")
#' #       cli::cli_ol(files)
#' #       return(files)
#' #     }
#' #   }
#' #
#' #   load_data <- function(file_name) {
#' #     file_path <- file.path(measures_path, file_name)
#' #     if (file.exists(file_path)) {
#' #       measure_data <- readRDS(file_path)
#' #       cli::cli_alert_success("Data loaded from: {.file {file_path}}")
#' #       return(list(measure_data = measure_data, current_file = file_name))
#' #     } else {
#' #       cli::cli_alert_danger("File not found: {.file {file_path}}")
#' #       cli::cli_alert_info("Available files in {.file {measures_path}}:")
#' #       list_rds_files()
#' #       return(NULL)
#' #     }
#' #   }
#' #
#' #   save_data <- function(measure_data, file_name, backup = FALSE) {
#' #     tryCatch({
#' #       file_path <- file.path(measures_path, file_name)
#' #       saveRDS(measure_data, file = file_path)
#' #       cli::cli_alert_success("Data saved as: {.file {file_path}}")
#' #
#' #       if (backup) {
#' #         backup_file <- paste0("backup_", file_name)
#' #         backup_path <- file.path(measures_path, backup_file)
#' #         saveRDS(measure_data, file = backup_path)
#' #         cli::cli_alert_success("Backup saved as: {.file {backup_path}}")
#' #       }
#' #       return(file_name)
#' #     }, error = function(e) {
#' #       cli::cli_alert_danger("Error saving data: {e$message}")
#' #       return(NULL)
#' #     })
#' #   }
#' #
#' #   enter_or_modify_measure <- function(existing_measure = NULL) {
#' #     measure <- existing_measure %||% list()
#' #     fields <- c("name", "items", "description", "reference", "waves", "keywords")
#' #
#' #     for (field in fields) {
#' #       current_value <- measure[[field]]
#' #
#' #       if (field == "description") {
#' #         cli::cli_text(cli::col_grey("Example: Frequency of alcohol consumption was measured using a single item..."))
#' #         cli::cli_text("Current value: {.val {if (is.null(current_value)) 'None' else current_value}}")
#' #         new_value <- get_input(
#' #           cli::col_blue("Enter new description:"),
#' #           allow_empty = TRUE,
#' #           multiline = TRUE
#' #         )
#' #       } else if (field == "items") {
#' #         if (is.null(current_value)) {
#' #           measure[[field]] <- list()
#' #           cli::cli_h3("Enter items (press Enter without typing anything to finish):")
#' #           cli::cli_text("Example: How often do you have a drink containing alcohol?")
#' #           item_num <- 1
#' #           repeat {
#' #             item <- get_input(cli::col_blue(paste("Item", item_num, "(or press enter to finish):")), allow_empty = TRUE)
#' #             if (item == "__back__") return("__back__")
#' #             if (item == "") break
#' #             measure[[field]][[item_num]] <- item
#' #             item_num <- item_num + 1
#' #           }
#' #           if (length(measure[[field]]) == 0) {
#' #             cli::cli_alert_danger("Error: At least one item is required. Please enter an item.")
#' #             next
#' #           }
#' #         } else {
#' #           cli::cli_h3("Current items:")
#' #           cli::cli_ol(current_value)
#' #           modify <- tolower(get_input("Do you want to modify the items? (y/n): ")) == "y"
#' #           if (modify == "__back__") return("__back__")
#' #           if (modify) {
#' #             measure[[field]] <- list()
#' #             cli::cli_h3("Enter new items (press Enter without typing anything to finish):")
#' #             item_num <- 1
#' #             repeat {
#' #               item <- get_input(cli::col_blue(paste("Item", item_num, "(or press enter to finish):")), allow_empty = TRUE)
#' #               if (item == "__back__") return("__back__")
#' #               if (item == "") break
#' #               measure[[field]][[item_num]] <- item
#' #               item_num <- item_num + 1
#' #             }
#' #           }
#' #         }
#' #       } else {
#' #         example <- switch(
#' #           field,
#' #           name = "Example: alcohol_frequency",
#' #           reference = "Example: [@nzavs2009]",
#' #           waves = "Example: 1-current or 1-15",
#' #           keywords = 'Example: alcohol, frequency, consumption (optional, press enter to skip)'
#' #         )
#' #         cli::cli_text(cli::col_grey(example))
#' #         cli::cli_text("Current value: {.val {if (is.null(current_value)) 'None' else paste(current_value, collapse = ', ')}}")
#' #
#' #         new_value <- get_input(
#' #           cli::col_blue(paste("Enter new", field, "(press enter to keep current or skip):")),
#' #           allow_empty = TRUE
#' #         )
#' #         if (new_value == "__back__") return("__back__")
#' #
#' #         if (field == "keywords" && new_value != "") {
#' #           keywords <- strsplit(new_value, ",")[[1]]
#' #           keywords <- sapply(keywords, trimws)
#' #           new_value <- keywords  # Store as a character vector, not a single string
#' #         }
#' #
#' #         if (length(new_value) > 0 && new_value != "") {
#' #           measure[[field]] <- new_value
#' #         }
#' #       }
#' #     }
#' #
#' #     return(measure)
#' #   }
#' #
#' #   list_measures <- function(measure_data) {
#' #     return(names(measure_data))
#' #   }
#' #
#' #   delete_measure <- function(measure_data, name) {
#' #     if (name %in% names(measure_data)) {
#' #       measure_data[[name]] <- NULL
#' #       measure_data <- measure_data[order(names(measure_data))]
#' #       cli::cli_alert_success("Measure {.val {name}} deleted successfully.")
#' #     } else {
#' #       cli::cli_alert_danger("Measure {.val {name}} not found.")
#' #     }
#' #     return(measure_data)
#' #   }
#' #
#' #   create_new_database <- function() {
#' #     cli::cli_h2("Creating a new measures database")
#' #
#' #     db_name <- get_input("Enter a name for the new database (without .rds extension): ")
#' #     if (db_name == "__back__")
#' #       return(NULL)
#' #     if (!grepl("\\.rds$", db_name)) {
#' #       db_name <- paste0(db_name, ".rds")
#' #     }
#' #
#' #     full_path <- file.path(measures_path, db_name)
#' #     cli::cli_alert_info("The database will be created at: {.file {full_path}}")
#' #     confirm <- tolower(get_input("Is this correct? (y/n): "))
#' #     if (confirm == "__back__")
#' #       return(NULL)
#' #
#' #     if (confirm == "y") {
#' #       new_measure_data <- list()
#' #       saveRDS(new_measure_data, file = full_path)
#' #       cli::cli_alert_success("New measures database '{.file {db_name}}' created.")
#' #       return(list(measure_data = new_measure_data, current_file = db_name))
#' #     } else {
#' #       cli::cli_alert_warning("Database creation cancelled.")
#' #       return(NULL)
#' #     }
#' #   }
#' #
#' #   review_and_save_measure <- function(measure_data, measure, current_file, is_new = TRUE) {
#' #     if (is.character(measure) && measure == "__back__") {
#' #       return(list(saved = FALSE, measure_data = measure_data))
#' #     }
#' #
#' #     while (TRUE) {
#' #       cli::cli_h2("Review your entries:")
#' #       print(measure)
#' #       cli::cli_h3("What would you like to do?")
#' #       cli::cli_ol(c(
#' #         "Save measure",
#' #         "Modify measure",
#' #         "Start over",
#' #         "Cancel"
#' #       ))
#' #
#' #       choice <- get_input("Enter your choice: ")
#' #       if (choice == "__back__")
#' #         return(list(saved = FALSE, measure_data = measure_data))
#' #       choice <- as.integer(choice)
#' #
#' #       if (is.na(choice)) {
#' #         cli::cli_alert_danger("Invalid choice. Please enter a number.")
#' #         next
#' #       }
#' #
#' #       if (choice == 1) {
#' #         if (is_new) {
#' #           measure_data[[measure$name]] <- measure
#' #         } else {
#' #           for (field in names(measure)) {
#' #             measure_data[[measure$name]][[field]] <- measure[[field]]
#' #           }
#' #         }
#' #         measure_data <- measure_data[order(names(measure_data))]
#' #         cli::cli_alert_success("Measure {.val {measure$name}} saved successfully.")
#' #
#' #         # Automatically save the measures data file
#' #         if (!is.null(current_file)) {
#' #           saveRDS(measure_data, file = file.path(measures_path, current_file))
#' #           cli::cli_alert_success("Measure modified and database updated.")
#' #           cli::cli_alert_success("Database saved.")
#' #         } else {
#' #           cli::cli_alert_warning("Database not saved. No current file set.")
#' #         }
#' #
#' #         return(list(saved = TRUE, measure_data = measure_data))
#' #       } else if (choice == 2) {
#' #         modified_measure <- enter_or_modify_measure(measure)
#' #         if (is.character(modified_measure) && modified_measure == "__back__") {
#' #           next
#' #         } else {
#' #           measure <- modified_measure
#' #         }
#' #       } else if (choice == 3) {
#' #         new_measure <- enter_or_modify_measure()
#' #         if (is.character(new_measure) && new_measure == "__back__") {
#' #           next
#' #         } else {
#' #           measure <- new_measure
#' #           is_new <- TRUE
#' #         }
#' #       } else if (choice == 4) {
#' #         return(list(saved = FALSE, measure_data = measure_data))
#' #       } else {
#' #         cli::cli_alert_danger("Invalid choice. Please try again.")
#' #       }
#' #     }
#' #   }
#' #
#' #   batch_edit_measures <- function(measures_data, field, old_value, new_value) {
#' #     edited_count <- 0
#' #
#' #     for (measure_name in names(measures_data)) {
#' #       measure <- measures_data[[measure_name]]
#' #       if (field %in% names(measure)) {
#' #         if (is.character(measure[[field]])) {
#' #           if (measure[[field]] == old_value) {
#' #             measures_data[[measure_name]][[field]] <- new_value
#' #             edited_count <- edited_count + 1
#' #           }
#' #         } else if (is.list(measure[[field]])) {
#' #           for (i in seq_along(measure[[field]])) {
#' #             if (measure[[field]][[i]] == old_value) {
#' #               measures_data[[measure_name]][[field]][[i]] <- new_value
#' #               edited_count <- edited_count + 1
#' #             }
#' #           }
#' #         }
#' #       }
#' #     }
#' #
#' #     cli::cli_alert_success("Edited {edited_count} entries.")
#' #     return(measures_data)
#' #   }
#' #
#' #   copy_measure_info <- function(measure_data, current_file) {
#' #     measures <- names(measure_data)
#' #     if (length(measures) == 0) {
#' #       cli::cli_alert_warning("No measures available to copy from.")
#' #       return(measure_data)
#' #     }
#' #
#' #     cli::cli_h3("Available measures to copy from:")
#' #     cli::cli_ol(measures)
#' #
#' #     from_choice <- as.integer(get_input("Enter the number of the measure to copy from: "))
#' #     if (is.na(from_choice) || from_choice < 1 || from_choice > length(measures)) {
#' #       cli::cli_alert_danger("Invalid choice. Returning to main menu.")
#' #       return(measure_data)
#' #     }
#' #     from_measure <- measures[from_choice]
#' #
#' #     fields <- c("description", "reference", "waves", "keywords", "items")
#' #     available_fields <- fields[fields %in% names(measure_data[[from_measure]])]
#' #
#' #     cli::cli_h3("Available fields to copy:")
#' #     cli::cli_ol(available_fields)
#' #
#' #     field_choice <- as.integer(get_input("Enter the number of the field to copy (0 to copy all): "))
#' #     if (is.na(field_choice) || field_choice < 0 || field_choice > length(available_fields)) {
#' #       cli::cli_alert_danger("Invalid choice. Returning to main menu.")
#' #       return(measure_data)
#' #     }
#' #
#' #     if (field_choice == 0) {
#' #       fields_to_copy <- available_fields
#' #     } else {
#' #       fields_to_copy <- available_fields[field_choice]
#' #     }
#' #
#' #     # Create a new measure or modify an existing one
#' #     new_measure <- list()
#' #     for (field in fields_to_copy) {
#' #       new_measure[[field]] <- measure_data[[from_measure]][[field]]
#' #     }
#' #
#' #     # Use enter_or_modify_measure to create or modify the measure
#' #     modified_measure <- enter_or_modify_measure(new_measure)
#' #
#' #     if (is.character(modified_measure) && modified_measure == "__back__") {
#' #       cli::cli_alert_info("Operation cancelled. Returning to main menu.")
#' #       return(measure_data)
#' #     }
#' #
#' #     # Review and save the new/modified measure
#' #     result <- review_and_save_measure(measure_data, modified_measure, current_file)
#' #     if (result$saved) {
#' #       cli::cli_alert_success("Measure created/modified successfully with copied information.")
#' #       return(result$measure_data)
#' #     } else {
#' #       cli::cli_alert_warning("Changes were not saved.")
#' #       return(measure_data)
#' #     }
#' #   }
#' #   run_gui <- function() {
#' #     cli::cli_h1("Welcome to the Margot's Measures Database Manager")
#' #     measure_data <- list()
#' #     current_file <- NULL
#' #
#' #     repeat {
#' #       cli::cli_h2("Boilerplate Measures Manager")
#' #       cli::cli_ol(c(
#' #         "Create new measures database",
#' #         "Open existing measures database",
#' #         "List available .rds files",
#' #         "Quit"
#' #       ))
#' #
#' #       choice <- get_input("Enter your choice: ")
#' #       if (choice == "__back__") {
#' #         next
#' #       }
#' #       choice <- as.integer(choice)
#' #
#' #       if (is.na(choice)) {
#' #         cli::cli_alert_danger("Invalid choice. Please enter a number.")
#' #         next
#' #       }
#' #
#' #       if (choice == 1) {
#' #         result <- create_new_database()
#' #         if (!is.null(result)) {
#' #           measure_data <- result$measure_data
#' #           current_file <- result$current_file
#' #
#' #           repeat {
#' #             new_measure <- enter_or_modify_measure()
#' #             if (is.character(new_measure) && length(new_measure) == 1 && new_measure == "__back__") {
#' #               break
#' #             }
#' #             review_result <- review_and_save_measure(measure_data, new_measure, current_file)
#' #             if (review_result$saved) {
#' #               measure_data <- review_result$measure_data
#' #               cli::cli_alert_success("Measure saved to database.")
#' #
#' #               continue <- tolower(get_input("Would you like to enter another measure? (y/n): ")) == "y"
#' #               if (!continue) {
#' #                 break
#' #               }
#' #             }
#' #           }
#' #         }
#' #       } else if (choice == 2) {
#' #         files <- list_rds_files()
#' #         if (!is.null(files)) {
#' #           file_choice <- get_input("Enter the number of the file you want to open: ")
#' #           if (file_choice == "__back__")
#' #             next
#' #           file_choice <- as.integer(file_choice)
#' #           if (is.na(file_choice)) {
#' #             cli::cli_alert_danger("Invalid choice. Please enter a number.")
#' #             next
#' #           }
#' #           if (file_choice > 0 && file_choice <= length(files)) {
#' #             file_name <- files[file_choice]
#' #             result <- load_data(file_name)
#' #             if (!is.null(result)) {
#' #               measure_data <- result$measure_data
#' #               current_file <- result$current_file
#' #             }
#' #           } else {
#' #             cli::cli_alert_danger("Invalid choice. Please try again.")
#' #             next
#' #           }
#' #         } else {
#' #           next
#' #         }
#' #       } else if (choice == 3) {
#' #         list_rds_files()
#' #         next
#' #       } else if (choice == 4) {
#' #         confirm_quit <- tolower(get_input(
#' #           "Are you sure you want to quit? Unsaved changes will be lost. (y/n): "
#' #         ))
#' #         if (confirm_quit == "y") {
#' #           cli::cli_alert_success("Exiting program. Goodbye!")
#' #           return()
#' #         } else {
#' #           next
#' #         }
#' #       } else {
#' #         cli::cli_alert_danger("Invalid choice. Please try again.")
#' #         next
#' #       }
#' #
#' #       if (choice == 1 || choice == 2)
#' #         break
#' #     }
#' #
#' #     repeat {
#' #       cli::cli_h2("Measures Database Management")
#' #       cli::cli_ol(c(
#' #         "List measures",
#' #         "Add measure",
#' #         "Delete measure",
#' #         "Modify measure",
#' #         "Copy to new/existing measure",
#' #         "Create backup measures data",
#' #         "Batch edit measures",
#' #         "Exit"
#' #       ))
#' #
#' #       choice <- get_input("Enter your choice: ")
#' #       if (choice == "__back__") {
#' #         break
#' #       }
#' #       choice <- as.integer(choice)
#' #
#' #       if (is.na(choice)) {
#' #         cli::cli_alert_danger("Invalid choice. Please enter a number.")
#' #         next
#' #       }
#' #
#' #       switch(choice,
#' #              {
#' #                # Option 1: List measures
#' #                measures <- list_measures(measure_data)
#' #                if (length(measures) > 0) {
#' #                  cli::cli_h3("Available measures:")
#' #                  cli::cli_ol(measures)
#' #                } else {
#' #                  cli::cli_alert_warning("No measures available.")
#' #                }
#' #              },
#' #              {
#' #                # Option 2: Add measure
#' #                new_measure <- enter_or_modify_measure()
#' #                if (!is.character(new_measure) || new_measure != "__back__") {
#' #                  result <- review_and_save_measure(measure_data, new_measure, current_file)
#' #                  measure_data <- result$measure_data
#' #                }
#' #              },
#' #              {
#' #                # Option 3: Delete measure
#' #                measures <- list_measures(measure_data)
#' #                if (length(measures) > 0) {
#' #                  cli::cli_h3("Available measures:")
#' #                  cli::cli_ol(measures)
#' #                  choice <- get_input("Enter the number of the measure to delete: ")
#' #                  if (choice != "__back__") {
#' #                    choice <- as.integer(choice)
#' #                    if (is.na(choice)) {
#' #                      cli::cli_alert_danger("Invalid choice. Please enter a number.")
#' #                    } else if (choice > 0 && choice <= length(measures)) {
#' #                      measure_data <- delete_measure(measure_data, measures[choice])
#' #                      saveRDS(measure_data, file = file.path(measures_path, current_file))
#' #                      cli::cli_alert_success("Measure deleted and database updated.")
#' #                    } else {
#' #                      cli::cli_alert_danger("Invalid choice. Please try again.")
#' #                    }
#' #                  }
#' #                } else {
#' #                  cli::cli_alert_warning("No measures available to delete.")
#' #                }
#' #              },
#' #              {
#' #                # Option 4: Modify measure
#' #                measures <- list_measures(measure_data)
#' #                if (length(measures) > 0) {
#' #                  cli::cli_h3("Available measures:")
#' #                  cli::cli_ol(measures)
#' #                  choice <- get_input("Enter the number of the measure to modify: ")
#' #
#' #                  if (choice != "__back__") {
#' #                    choice <- as.integer(choice)
#' #                    if (is.na(choice)) {
#' #                      cli::cli_alert_danger("Invalid choice. Please enter a number.")
#' #                    } else if (choice > 0 && choice <= length(measures)) {
#' #                      measure <- enter_or_modify_measure(measure_data[[measures[choice]]])
#' #                      if (!is.character(measure) || measure != "__back__") {
#' #                        result <- review_and_save_measure(measure_data, measure, current_file, is_new = FALSE)
#' #                        measure_data <- result$measure_data
#' #                      }
#' #                    } else {
#' #                      cli::cli_alert_danger("Invalid choice. Please try again.")
#' #                    }
#' #                  }
#' #                } else {
#' #                  cli::cli_alert_warning("No measures available to modify.")
#' #                }
#' #              },
#' #              {
#' #                # Option 5: Copy to new/existing measure
#' #                measure_data <- copy_measure_info(measure_data, current_file)
#' #              },
#' #              {
#' #                # Option 6: Create backup measures data
#' #                default_name <- gsub("^backup_", "", current_file %||% "measures_data.rds")
#' #                cli::cli_h3("Save options:")
#' #                cli::cli_ol(c(
#' #                  paste("Use backup file name:", paste0("backup_", default_name)),
#' #                  "Enter a new file name"
#' #                ))
#' #                save_choice <- get_input("Enter your choice: ")
#' #                if (save_choice != "__back__") {
#' #                  save_choice <- as.integer(save_choice)
#' #                  if (is.na(save_choice)) {
#' #                    cli::cli_alert_warning("Invalid choice. Using default backup name.")
#' #                    file_name <- default_name
#' #                  } else if (save_choice == 1) {
#' #                    file_name <- default_name
#' #                  } else if (save_choice == 2) {
#' #                    file_name <- get_input("Enter new file name (including .rds extension): ")
#' #                    if (file_name == "__back__")
#' #                      next
#' #                  } else {
#' #                    cli::cli_alert_warning("Invalid choice. Using default backup name.")
#' #                    file_name <- default_name
#' #                  }
#' #
#' #                  save_data(measure_data, file_name, backup = TRUE)
#' #                }
#' #              },
#' #              {
#' #                # Option 7: Batch edit measures
#' #                field <- get_input("Enter the field to edit (e.g., 'reference', 'name', 'description'): ")
#' #                if (field != "__back__") {
#' #                  old_value <- get_input("Enter the old value: ")
#' #                  if (old_value != "__back__") {
#' #                    new_value <- get_input("Enter the new value: ")
#' #                    if (new_value != "__back__") {
#' #                      measure_data <- batch_edit_measures(measure_data, field, old_value, new_value)
#' #                      saveRDS(measure_data, file = file.path(measures_path, current_file))
#' #                      cli::cli_alert_success("Batch edit completed and database updated.")
#' #                    }
#' #                  }
#' #                }
#' #              },
#' #              {
#' #                # Option 8: Exit
#' #                cli::cli_alert_success("Exited. Have a nice day! \U0001F600 \U0001F44D")
#' #                break
#' #              },
#' #              {
#' #                # Default: Invalid choice
#' #                cli::cli_alert_danger("Invalid choice. Please try again.")
#' #              }
#' #       )
#' #     }
#' #   }
#' #
#' #   run_gui()
#' # }
#' # margot_create_database <- function(measures_path = NULL) {
#' #
#' #   require(rlang)
#' #   require(here)
#' #   require(cli)
#' #
#' #
#' #   measures_path <- measures_path %||% here::here()
#' #
#' #   # New function for multi-line input
#' #   get_multiline_input <- function(prompt) {
#' #     cli::cli_text(cli::col_cyan(prompt))
#' #     cli::cli_text(cli::col_cyan("Enter your text (press Enter twice to finish):"))
#' #     lines <- character()
#' #     repeat {
#' #       line <- readline()
#' #       if (line == "") {
#' #         if (length(lines) > 0 && lines[length(lines)] == "") {
#' #           break
#' #         }
#' #       }
#' #       lines <- c(lines, line)
#' #     }
#' #     paste(lines, collapse = "\n")
#' #   }
#' #
#' #   # Modified get_input function
#' #   get_input <- function(prompt, allow_empty = FALSE, multiline = FALSE) {
#' #     if (multiline) {
#' #       return(get_multiline_input(prompt))
#' #     }
#' #
#' #     while (TRUE) {
#' #       input <- trimws(readline(cli::col_cyan(paste0(prompt, " (enter 'b' to go back): "))))
#' #       if (tolower(input) == "b") {
#' #         return("__back__")
#' #       }
#' #       if (input != "" || allow_empty)
#' #         return(input)
#' #       cli::cli_alert_danger("Input cannot be empty. Please try again.")
#' #     }
#' #   }
#' #
#' #   # Modified enter_or_modify_measure function
#' #   enter_or_modify_measure <- function(existing_measure = NULL) {
#' #     measure <- existing_measure %||% list()
#' #     fields <- c("name", "items", "description", "reference", "waves", "keywords")
#' #
#' #     for (field in fields) {
#' #       current_value <- measure[[field]]
#' #
#' #       if (field == "items") {
#' #         # ... (keep existing items logic)
#' #       } else {
#' #         example <- switch(
#' #           field,
#' #           name = "Example: alcohol_frequency",
#' #           description = "Example: Frequency of alcohol consumption was measured using a single item...",
#' #           reference = "Example: [@nzavs2009]",
#' #           waves = "Example: 1-current or 1-15",
#' #           keywords = 'Example: alcohol, frequency, consumption (optional, press enter to skip)'
#' #         )
#' #         cli::cli_text(cli::col_grey(example))
#' #         cli::cli_text("Current value: {.val {if (is.null(current_value)) 'None' else paste(current_value, collapse = ', ')}}")
#' #
#' #         new_value <- get_input(
#' #           cli::col_blue(paste("Enter new", field, "(press enter to keep current or skip):")),
#' #           allow_empty = TRUE,
#' #           multiline = (field == "description")
#' #         )
#' #         if (new_value == "__back__") return("__back__")
#' #
#' #         if (field == "keywords" && new_value != "") {
#' #           keywords <- strsplit(new_value, ",")[[1]]
#' #           keywords <- sapply(keywords, trimws)
#' #           new_value <- keywords  # Store as a character vector, not a single string
#' #         }
#' #
#' #         if (length(new_value) > 0 && new_value != "") {
#' #           measure[[field]] <- new_value
#' #         }
#' #       }
#' #     }
#' #
#' #     return(measure)
#' #   }
#' #   list_rds_files <- function() {
#' #     files <- list.files(measures_path, pattern = "\\.rds$")
#' #     if (length(files) == 0) {
#' #       cli::cli_alert_warning("No .rds files found in the directory.")
#' #       return(NULL)
#' #     } else {
#' #       cli::cli_h2("Available .rds files:")
#' #       cli::cli_ol(files)
#' #       return(files)
#' #     }
#' #   }
#' #
#' #   load_data <- function(file_name) {
#' #     file_path <- file.path(measures_path, file_name)
#' #     if (file.exists(file_path)) {
#' #       measure_data <- readRDS(file_path)
#' #       cli::cli_alert_success("Data loaded from: {.file {file_path}}")
#' #       return(list(measure_data = measure_data, current_file = file_name))
#' #     } else {
#' #       cli::cli_alert_danger("File not found: {.file {file_path}}")
#' #       cli::cli_alert_info("Available files in {.file {measures_path}}:")
#' #       list_rds_files()
#' #       return(NULL)
#' #     }
#' #   }
#' #
#' #   save_data <- function(measure_data, file_name, backup = FALSE) {
#' #     file_path <- file.path(measures_path, file_name)
#' #     saveRDS(measure_data, file = file_path)
#' #     cli::cli_alert_success("Data saved as: {.file {file_path}}")
#' #     if (backup) {
#' #       backup_file <- paste0(tools::file_path_sans_ext(file_name), "_backup.rds")
#' #       backup_path <- file.path(measures_path, backup_file)
#' #       saveRDS(measure_data, file = backup_path)
#' #       cli::cli_alert_success("Backup saved as: {.file {backup_path}}")
#' #     }
#' #     return(file_name)
#' #   }
#' #
#' #   enter_or_modify_measure <- function(existing_measure = NULL) {
#' #     measure <- existing_measure %||% list()
#' #     fields <- c("name", "items", "description", "reference", "waves", "keywords")
#' #
#' #     for (field in fields) {
#' #       current_value <- measure[[field]]
#' #
#' #       if (field == "items") {
#' #         if (is.null(current_value)) {
#' #           measure[[field]] <- list()
#' #           cli::cli_h3("Enter items (press Enter without typing anything to finish):")
#' #           cli::cli_text("Example: How often do you have a drink containing alcohol?")
#' #           item_num <- 1
#' #           repeat {
#' #             item <- get_input(cli::col_blue(paste("Item", item_num, "(or press enter to finish):")), allow_empty = TRUE)
#' #             if (item == "__back__") return("__back__")
#' #             if (item == "") break
#' #             measure[[field]][[item_num]] <- item
#' #             item_num <- item_num + 1
#' #           }
#' #           if (length(measure[[field]]) == 0) {
#' #             cli::cli_alert_danger("Error: At least one item is required. Please enter an item.")
#' #             next
#' #           }
#' #         } else {
#' #           cli::cli_h3("Current items:")
#' #           cli::cli_ol(current_value)
#' #           modify <- tolower(get_input("Do you want to modify the items? (y/n): ")) == "y"
#' #           if (modify == "__back__") return("__back__")
#' #           if (modify) {
#' #             measure[[field]] <- list()
#' #             cli::cli_h3("Enter new items (press Enter without typing anything to finish):")
#' #             item_num <- 1
#' #             repeat {
#' #               item <- get_input(cli::col_blue(paste("Item", item_num, "(or press enter to finish):")), allow_empty = TRUE)
#' #               if (item == "__back__") return("__back__")
#' #               if (item == "") break
#' #               measure[[field]][[item_num]] <- item
#' #               item_num <- item_num + 1
#' #             }
#' #           }
#' #         }
#' #       } else {
#' #         example <- switch(
#' #           field,
#' #           name = "Example: alcohol_frequency",
#' #           description = "Example: Frequency of alcohol consumption was measured using a single item...",
#' #           reference = "Example: [@nzavs2009]",
#' #           waves = "Example: 1-current or 1-15",
#' #           keywords = 'Example: alcohol, frequency, consumption (optional, press enter to skip)'
#' #         )
#' #         cli::cli_text(cli::col_grey(example))
#' #         cli::cli_text("Current value: {.val {if (is.null(current_value)) 'None' else paste(current_value, collapse = ', ')}}")
#' #
#' #         new_value <- get_input(cli::col_blue(paste("Enter new", field, "(press enter to keep current or skip):")), allow_empty = TRUE)
#' #         if (new_value == "__back__") return("__back__")
#' #
#' #         if (field == "keywords" && new_value != "") {
#' #           keywords <- strsplit(new_value, ",")[[1]]
#' #           keywords <- sapply(keywords, trimws)
#' #           new_value <- keywords  # Store as a character vector, not a single string
#' #         }
#' #
#' #         if (length(new_value) > 0 && new_value != "") {
#' #           measure[[field]] <- new_value
#' #         }
#' #       }
#' #     }
#' #
#' #     return(measure)
#' #   }
#' #
#' #   review_and_save_measure <- function(measure_data, measure, is_new = TRUE) {
#' #     if (is.character(measure) && measure == "__back__") {
#' #       return(list(saved = FALSE, measure_data = measure_data))
#' #     }
#' #
#' #     while (TRUE) {
#' #       cli::cli_h2("Review your entries:")
#' #       print(measure)
#' #       cli::cli_h3("What would you like to do?")
#' #       cli::cli_ol(c(
#' #         "Save measure",
#' #         "Modify measure",
#' #         "Start over",
#' #         "Cancel"
#' #       ))
#' #
#' #       choice <- get_input("Enter your choice: ")
#' #       if (choice == "__back__")
#' #         return(list(saved = FALSE, measure_data = measure_data))
#' #       choice <- as.integer(choice)
#' #
#' #       if (is.na(choice)) {
#' #         cli::cli_alert_danger("Invalid choice. Please enter a number.")
#' #         next
#' #       }
#' #
#' #       if (choice == 1) {
#' #         if (is_new) {
#' #           measure_data[[measure$name]] <- measure
#' #         } else {
#' #           for (field in names(measure)) {
#' #             measure_data[[measure$name]][[field]] <- measure[[field]]
#' #           }
#' #         }
#' #         measure_data <- measure_data[order(names(measure_data))]
#' #         cli::cli_alert_success("Measure {.val {measure$name}} saved successfully.")
#' #         return(list(saved = TRUE, measure_data = measure_data))
#' #       } else if (choice == 2) {
#' #         modified_measure <- enter_or_modify_measure(measure)
#' #         if (is.character(modified_measure) && modified_measure == "__back__") {
#' #           next
#' #         } else {
#' #           measure <- modified_measure
#' #         }
#' #       } else if (choice == 3) {
#' #         new_measure <- enter_or_modify_measure()
#' #         if (is.character(new_measure) && new_measure == "__back__") {
#' #           next
#' #         } else {
#' #           measure <- new_measure
#' #           is_new <- TRUE
#' #         }
#' #       } else if (choice == 4) {
#' #         return(list(saved = FALSE, measure_data = measure_data))
#' #       } else {
#' #         cli::cli_alert_danger("Invalid choice. Please try again.")
#' #       }
#' #     }
#' #   }
#' #
#' #   list_measures <- function(measure_data) {
#' #     return(names(measure_data))
#' #   }
#' #
#' #   delete_measure <- function(measure_data, name) {
#' #     if (name %in% names(measure_data)) {
#' #       measure_data[[name]] <- NULL
#' #       measure_data <- measure_data[order(names(measure_data))]
#' #       cli::cli_alert_success("Measure {.val {name}} deleted successfully.")
#' #     } else {
#' #       cli::cli_alert_danger("Measure {.val {name}} not found.")
#' #     }
#' #     return(measure_data)
#' #   }
#' #
#' #   create_new_database <- function() {
#' #     cli::cli_h2("Creating a new measures database")
#' #
#' #     db_name <- get_input("Enter a name for the new database (without .rds extension): ")
#' #     if (db_name == "__back__")
#' #       return(NULL)
#' #     if (!grepl("\\.rds$", db_name)) {
#' #       db_name <- paste0(db_name, ".rds")
#' #     }
#' #
#' #     full_path <- file.path(measures_path, db_name)
#' #     cli::cli_alert_info("The database will be created at: {.file {full_path}}")
#' #     confirm <- tolower(get_input("Is this correct? (y/n): "))
#' #     if (confirm == "__back__")
#' #       return(NULL)
#' #
#' #     if (confirm == "y") {
#' #       new_measure_data <- list()
#' #       saveRDS(new_measure_data, file = full_path)
#' #       cli::cli_alert_success("New measures database '{.file {db_name}}' created.")
#' #       return(list(measure_data = new_measure_data, current_file = db_name))
#' #     } else {
#' #       cli::cli_alert_warning("Database creation cancelled.")
#' #       return(NULL)
#' #     }
#' #   }
#' #
#' #   batch_edit_measures <- function(measures_data, field, old_value, new_value) {
#' #     edited_count <- 0
#' #
#' #     for (measure_name in names(measures_data)) {
#' #       measure <- measures_data[[measure_name]]
#' #       if (field %in% names(measure)) {
#' #         if (is.character(measure[[field]])) {
#' #           if (measure[[field]] == old_value) {
#' #             measures_data[[measure_name]][[field]] <- new_value
#' #             edited_count <- edited_count + 1
#' #           }
#' #         } else if (is.list(measure[[field]])) {
#' #           for (i in seq_along(measure[[field]])) {
#' #             if (measure[[field]][[i]] == old_value) {
#' #               measures_data[[measure_name]][[field]][[i]] <- new_value
#' #               edited_count <- edited_count + 1
#' #             }
#' #           }
#' #         }
#' #       }
#' #     }
#' #
#' #     cli::cli_alert_success("Edited {edited_count} entries.")
#' #     return(measures_data)
#' #   }
#' #
#' #   # copy measure info
#' #   copy_measure_info <- function(measure_data) {
#' #     measures <- names(measure_data)
#' #     if (length(measures) == 0) {
#' #       cli::cli_alert_warning("No measures available to copy from.")
#' #       return(measure_data)
#' #     }
#' #
#' #     cli::cli_h3("Available measures to copy from:")
#' #     cli::cli_ol(measures)
#' #
#' #     from_choice <- as.integer(get_input("Enter the number of the measure to copy from: "))
#' #     if (is.na(from_choice) || from_choice < 1 || from_choice > length(measures)) {
#' #       cli::cli_alert_danger("Invalid choice. Returning to main menu.")
#' #       return(measure_data)
#' #     }
#' #     from_measure <- measures[from_choice]
#' #
#' #     fields <- c("description", "reference", "waves", "keywords", "items")
#' #     available_fields <- fields[fields %in% names(measure_data[[from_measure]])]
#' #
#' #     cli::cli_h3("Available fields to copy:")
#' #     cli::cli_ol(available_fields)
#' #
#' #     field_choice <- as.integer(get_input("Enter the number of the field to copy (0 to copy all): "))
#' #     if (is.na(field_choice) || field_choice < 0 || field_choice > length(available_fields)) {
#' #       cli::cli_alert_danger("Invalid choice. Returning to main menu.")
#' #       return(measure_data)
#' #     }
#' #
#' #     if (field_choice == 0) {
#' #       fields_to_copy <- available_fields
#' #     } else {
#' #       fields_to_copy <- available_fields[field_choice]
#' #     }
#' #
#' #     # Create a new measure or modify an existing one
#' #     new_measure <- list()
#' #     for (field in fields_to_copy) {
#' #       new_measure[[field]] <- measure_data[[from_measure]][[field]]
#' #     }
#' #
#' #     # Use enter_or_modify_measure to create or modify the measure
#' #     modified_measure <- enter_or_modify_measure(new_measure)
#' #
#' #     if (is.character(modified_measure) && modified_measure == "__back__") {
#' #       cli::cli_alert_info("Operation cancelled. Returning to main menu.")
#' #       return(measure_data)
#' #     }
#' #
#' #     # Review and save the new/modified measure
#' #     result <- review_and_save_measure(measure_data, modified_measure)
#' #     if (result$saved) {
#' #       cli::cli_alert_success("Measure created/modified successfully with copied information.")
#' #       return(result$measure_data)
#' #     } else {
#' #       cli::cli_alert_warning("Changes were not saved.")
#' #       return(measure_data)
#' #     }
#' #   }
#' #
#' #   run_gui <- function() {
#' #     cli::cli_h1("Welcome to the Margot's Measures Database Manager")
#' #     measure_data <- list()
#' #     current_file <- NULL
#' #
#' #     repeat {
#' #       cli::cli_h2("Boilerplate Measures Manager")
#' #       cli::cli_ol(c(
#' #         "Create new measures database",
#' #         "Open existing measures database",
#' #         "List available .rds files",
#' #         "Quit"
#' #       ))
#' #
#' #       choice <- get_input("Enter your choice: ")
#' #       if (choice == "__back__") {
#' #         next
#' #       }
#' #       choice <- as.integer(choice)
#' #
#' #       if (is.na(choice)) {
#' #         cli::cli_alert_danger("Invalid choice. Please enter a number.")
#' #         next
#' #       }
#' #
#' #
#' #       if (choice == 1) {
#' #         result <- create_new_database()
#' #         if (!is.null(result)) {
#' #           measure_data <- result$measure_data
#' #           current_file <- result$current_file
#' #
#' #           repeat {
#' #             new_measure <- enter_or_modify_measure()
#' #             if (is.character(new_measure) && length(new_measure) == 1 && new_measure == "__back__") {
#' #               break
#' #             }
#' #             review_result <- review_and_save_measure(measure_data, new_measure)
#' #             if (review_result$saved) {
#' #               measure_data <- review_result$measure_data
#' #               saveRDS(measure_data, file = file.path(measures_path, current_file))
#' #               cli::cli_alert_success("Measure saved to database.")
#' #
#' #               continue <- tolower(get_input("Would you like to enter another measure? (y/n): ")) == "y"
#' #               if (!continue) {
#' #                 break
#' #               }
#' #             }
#' #           }
#' #         }
#' #       } else if (choice == 2) {
#' #         files <- list_rds_files()
#' #         if (!is.null(files)) {
#' #           file_choice <- get_input("Enter the number of the file you want to open: ")
#' #           if (file_choice == "__back__")
#' #             next
#' #           file_choice <- as.integer(file_choice)
#' #           if (is.na(file_choice)) {
#' #             cli::cli_alert_danger("Invalid choice. Please enter a number.")
#' #             next
#' #           }
#' #           if (file_choice > 0 && file_choice <= length(files)) {
#' #             file_name <- files[file_choice]
#' #             result <- load_data(file_name)
#' #             if (!is.null(result)) {
#' #               measure_data <- result$measure_data
#' #               current_file <- result$current_file
#' #             }
#' #           } else {
#' #             cli::cli_alert_danger("Invalid choice. Please try again.")
#' #             next
#' #           }
#' #         } else {
#' #           next
#' #         }
#' #       } else if (choice == 3) {
#' #         list_rds_files()
#' #         next
#' #       } else if (choice == 4) {
#' #         confirm_quit <- tolower(get_input(
#' #           "Are you sure you want to quit? Unsaved changes will be lost. (y/n): "
#' #         ))
#' #         if (confirm_quit == "y") {
#' #           cli::cli_alert_success("Exiting program. Goodbye!")
#' #           return()
#' #         } else {
#' #           next
#' #         }
#' #       } else {
#' #         cli::cli_alert_danger("Invalid choice. Please try again.")
#' #         next
#' #       }
#' #
#' #       if (choice == 1 || choice == 2)
#' #         break
#' #     }
#' #
#' #     repeat {
#' #       cli::cli_h2("Measures Database Management")
#' #       cli::cli_ol(c(
#' #         "List measures",
#' #         "Add measure",
#' #         "Delete measure",
#' #         "Modify measure",
#' #         "Copy to new/existing measure",  # Updated option name
#' #         "Save measures data",
#' #         "Batch edit measures",
#' #         "Exit"
#' #       ))
#' #
#' #       choice <- get_input("Enter your choice: ")
#' #       if (choice == "__back__") {
#' #         break
#' #       }
#' #       choice <- as.integer(choice)
#' #
#' #       if (is.na(choice)) {
#' #         cli::cli_alert_danger("Invalid choice. Please enter a number.")
#' #         next
#' #       }
#' #
#' #       switch(choice,
#' #              {
#' #                # Option 1: List measures
#' #                measures <- list_measures(measure_data)
#' #                if (length(measures) > 0) {
#' #                  cli::cli_h3("Available measures:")
#' #                  cli::cli_ol(measures)
#' #                } else {
#' #                  cli::cli_alert_warning("No measures available.")
#' #                }
#' #              },
#' #              {
#' #                # Option 2: Add measure
#' #                new_measure <- enter_or_modify_measure()
#' #                if (!is.character(new_measure) || new_measure != "__back__") {
#' #                  result <- review_and_save_measure(measure_data, new_measure)
#' #                  measure_data <- result$measure_data
#' #                  if (result$saved) {
#' #                    saveRDS(measure_data, file = file.path(measures_path, current_file))
#' #                    cli::cli_alert_success("Measure saved to database.")
#' #                  }
#' #                }
#' #              },
#' #              {
#' #                # Option 3: Delete measure
#' #                measures <- list_measures(measure_data)
#' #                if (length(measures) > 0) {
#' #                  cli::cli_h3("Available measures:")
#' #                  cli::cli_ol(measures)
#' #                  choice <- get_input("Enter the number of the measure to delete: ")
#' #                  if (choice != "__back__") {
#' #                    choice <- as.integer(choice)
#' #                    if (is.na(choice)) {
#' #                      cli::cli_alert_danger("Invalid choice. Please enter a number.")
#' #                    } else if (choice > 0 && choice <= length(measures)) {
#' #                      measure_data <- delete_measure(measure_data, measures[choice])
#' #                      saveRDS(measure_data, file = file.path(measures_path, current_file))
#' #                      cli::cli_alert_success("Measure deleted and database updated.")
#' #                    } else {
#' #                      cli::cli_alert_danger("Invalid choice. Please try again.")
#' #                    }
#' #                  }
#' #                } else {
#' #                  cli::cli_alert_warning("No measures available to delete.")
#' #                }
#' #              },
#' #              {
#' #                # Option 4: Modify measure
#' #                measures <- list_measures(measure_data)
#' #                if (length(measures) > 0) {
#' #                  cli::cli_h3("Available measures:")
#' #                  cli::cli_ol(measures)
#' #                  choice <- get_input("Enter the number of the measure to modify: ")
#' #
#' #                  if (choice != "__back__") {
#' #                    choice <- as.integer(choice)
#' #                    if (is.na(choice)) {
#' #                      cli::cli_alert_danger("Invalid choice. Please enter a number.")
#' #                    } else if (choice > 0 && choice <= length(measures)) {
#' #                      measure <- enter_or_modify_measure(measure_data[[measures[choice]]])
#' #                      if (!is.character(measure) || measure != "__back__") {
#' #                        result <- review_and_save_measure(measure_data, measure, is_new = FALSE)
#' #                        measure_data <- result$measure_data
#' #                        if (result$saved) {
#' #                          saveRDS(measure_data, file = file.path(measures_path, current_file))
#' #                          cli::cli_alert_success("Measure modified and database updated.")
#' #                        }
#' #                      }
#' #                    } else {
#' #                      cli::cli_alert_danger("Invalid choice. Please try again.")
#' #                    }
#' #                  }
#' #                } else {
#' #                  cli::cli_alert_warning("No measures available to modify.")
#' #                }
#' #              },
#' #              {
#' #                # Updated option 5: Copy to new/existing measure
#' #                measure_data <- copy_measure_info(measure_data)
#' #                if (!is.null(current_file)) {
#' #                  saveRDS(measure_data, file = file.path(measures_path, current_file))
#' #                  cli::cli_alert_success("Database updated.")
#' #                }
#' #              },
#' #              {
#' #                # Option 6: Save measures data
#' #                default_name <- current_file %||% "measures_data.rds"
#' #                cli::cli_h3("Save options:")
#' #                cli::cli_ol(c(
#' #                  paste("Use current file name:", default_name),
#' #                  "Enter a new file name"
#' #                ))
#' #                save_choice <- get_input("Enter your choice: ")
#' #                if (save_choice != "__back__") {
#' #                  save_choice <- as.integer(save_choice)
#' #                  if (is.na(save_choice)) {
#' #                    cli::cli_alert_warning("Invalid choice. Using default name.")
#' #                    file_name <- default_name
#' #                  } else if (save_choice == 1) {
#' #                    file_name <- default_name
#' #                  } else if (save_choice == 2) {
#' #                    file_name <- get_input("Enter new file name (including .rds extension): ")
#' #                    if (file_name == "__back__")
#' #                      next
#' #                  } else {
#' #                    cli::cli_alert_warning("Invalid choice. Using default name.")
#' #                    file_name <- default_name
#' #                  }
#' #
#' #                  current_file <- save_data(measure_data, file_name)
#' #                }
#' #              },
#' #              {
#' #                # Option 7: Batch edit measures
#' #                field <- get_input("Enter the field to edit (e.g., 'reference', 'name', 'description'): ")
#' #                if (field != "__back__") {
#' #                  old_value <- get_input("Enter the old value: ")
#' #                  if (old_value != "__back__") {
#' #                    new_value <- get_input("Enter the new value: ")
#' #                    if (new_value != "__back__") {
#' #                      measure_data <- batch_edit_measures(measure_data, field, old_value, new_value)
#' #                      saveRDS(measure_data, file = file.path(measures_path, current_file))
#' #                      cli::cli_alert_success("Batch edit completed and database updated.")
#' #                    }
#' #                  }
#' #                }
#' #              },
#' #              {
#' #                # Option 8: Exit
#' #                cli::cli_alert_success("Exited. Have a nice day! \U0001F44D")
#' #                break
#' #              },
#' #              {
#' #                # Default: Invalid choice
#' #                cli::cli_alert_danger("Invalid choice. Please try again.")
#' #              }
#' #       )
#' #     }
#' #   }
#' #
#' #   run_gui()
#' # }

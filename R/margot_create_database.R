#' Make Measure Databases
#'
#' This function provides a command-line interface for managing a database of longitudinal measures.
#' It allows users to create, view, modify, and delete measures, as well as save and load measure databases.
#'
#' @param measures_path Character string. The path to the directory where measure databases are stored.
#'   If NULL (default), the current working directory is used.
#'
#' @return This function does not return a value. It runs an interactive command-line interface.
#'
#' @details
#' The function provides the following main operations:
#' \itemize{
#'   \item Create a new measures database
#'   \item Open an existing measures database
#'   \item List available .rds files in the specified directory
#'   \item List measures in the current database
#'   \item Add a new measure
#'   \item Delete an existing measure
#'   \item Modify an existing measure
#'   \item Save the current measures data
#'   \item Batch edit measures
#' }
#'
#' Each measure in the database contains the following fields:
#' \itemize{
#'   \item name: The name of the measure
#'   \item items: A list of items or questions in the measure
#'   \item description: A description of the measure
#'   \item reference: A reference for the measure
#'   \item waves: The waves in which the measure was used
#'   \item keywords: Keywords associated with the measure
#' }
#'
#' @note
#' This function uses a command-line interface and is designed to be run interactively.
#' It uses local state management and does not modify global variables.
#' Changes are only saved to disk when explicitly requested by the user.
#'
#' @import rlang
#' @import here
#' @import cli
#'
#' @examples
#' \dontrun{
#' # Run the function with the default path (current working directory)
#' margot_create_database()
#'
#' # Run the function with a specific path
#' margot_create_database("path/to/measures/directory")
#' }
#'
#' @export



margot_create_database <- function(measures_path = NULL) {
  measures_path <- measures_path %||% here::here()

  get_input <- function(prompt, allow_empty = FALSE) {
    while (TRUE) {
      input <- trimws(readline(cli::col_cyan(paste0(prompt, " (enter 'b' to go back): "))))
      if (tolower(input) == "b") {
        return("__back__")
      }
      if (input != "" || allow_empty)
        return(input)
      cli::cli_alert_danger("Input cannot be empty. Please try again.")
    }
  }

  list_rds_files <- function() {
    files <- list.files(measures_path, pattern = "\\.rds$")
    if (length(files) == 0) {
      cli::cli_alert_warning("No .rds files found in the directory.")
      return(NULL)
    } else {
      cli::cli_h2("Available .rds files:")
      cli::cli_ol(files)
      return(files)
    }
  }

  load_data <- function(file_name) {
    file_path <- file.path(measures_path, file_name)
    if (file.exists(file_path)) {
      measure_data <- readRDS(file_path)
      cli::cli_alert_success("Data loaded from: {.file {file_path}}")
      return(list(measure_data = measure_data, current_file = file_name))
    } else {
      cli::cli_alert_danger("File not found: {.file {file_path}}")
      cli::cli_alert_info("Available files in {.file {measures_path}}:")
      list_rds_files()
      return(NULL)
    }
  }

  save_data <- function(measure_data, file_name, backup = FALSE) {
    file_path <- file.path(measures_path, file_name)
    saveRDS(measure_data, file = file_path)
    cli::cli_alert_success("Data saved as: {.file {file_path}}")
    if (backup) {
      backup_file <- paste0(tools::file_path_sans_ext(file_name), "_backup.rds")
      backup_path <- file.path(measures_path, backup_file)
      saveRDS(measure_data, file = backup_path)
      cli::cli_alert_success("Backup saved as: {.file {backup_path}}")
    }
    return(file_name)
  }

  enter_or_modify_measure <- function(existing_measure = NULL) {
    measure <- existing_measure %||% list()
    fields <- c("name", "items", "description", "reference", "waves", "keywords")

    for (field in fields) {
      current_value <- measure[[field]]

      if (field == "items") {
        if (is.null(current_value)) {
          measure[[field]] <- list()
          cli::cli_h3("Enter items (press Enter without typing anything to finish):")
          cli::cli_text("Example: How often do you have a drink containing alcohol?")
          item_num <- 1
          repeat {
            item <- get_input(cli::col_blue(paste("Item", item_num, "(or press enter to finish):")), allow_empty = TRUE)
            if (item == "__back__") return("__back__")
            if (item == "") break
            measure[[field]][[item_num]] <- item
            item_num <- item_num + 1
          }
          if (length(measure[[field]]) == 0) {
            cli::cli_alert_danger("Error: At least one item is required. Please enter an item.")
            next
          }
        } else {
          cli::cli_h3("Current items:")
          cli::cli_ol(current_value)
          modify <- tolower(get_input("Do you want to modify the items? (y/n): ")) == "y"
          if (modify == "__back__") return("__back__")
          if (modify) {
            measure[[field]] <- list()
            cli::cli_h3("Enter new items (press Enter without typing anything to finish):")
            item_num <- 1
            repeat {
              item <- get_input(cli::col_blue(paste("Item", item_num, "(or press enter to finish):")), allow_empty = TRUE)
              if (item == "__back__") return("__back__")
              if (item == "") break
              measure[[field]][[item_num]] <- item
              item_num <- item_num + 1
            }
          }
        }
      } else {
        example <- switch(
          field,
          name = "Example: alcohol_frequency",
          description = "Example: Frequency of alcohol consumption was measured using a single item...",
          reference = "Example: [@nzavs2009]",
          waves = "Example: 1-current or 1-15",
          keywords = 'Example: alcohol, frequency, consumption (optional, press enter to skip)'
        )
        cli::cli_text(cli::col_grey(example))
        cli::cli_text("Current value: {.val {if (is.null(current_value)) 'None' else paste(current_value, collapse = ', ')}}")

        new_value <- get_input(cli::col_blue(paste("Enter new", field, "(press enter to keep current or skip):")), allow_empty = TRUE)
        if (new_value == "__back__") return("__back__")

        if (field == "keywords" && new_value != "") {
          keywords <- strsplit(new_value, ",")[[1]]
          keywords <- sapply(keywords, trimws)
          new_value <- keywords  # Store as a character vector, not a single string
        }

        if (length(new_value) > 0 && new_value != "") {
          measure[[field]] <- new_value
        }
      }
    }

    return(measure)
  }

  review_and_save_measure <- function(measure_data, measure, is_new = TRUE) {
    if (is.character(measure) && measure == "__back__") {
      return(list(saved = FALSE, measure_data = measure_data))
    }

    while (TRUE) {
      cli::cli_h2("Review your entries:")
      print(measure)
      cli::cli_h3("What would you like to do?")
      cli::cli_ol(c(
        "Save measure",
        "Modify measure",
        "Start over",
        "Cancel"
      ))

      choice <- get_input("Enter your choice: ")
      if (choice == "__back__")
        return(list(saved = FALSE, measure_data = measure_data))
      choice <- as.integer(choice)

      if (is.na(choice)) {
        cli::cli_alert_danger("Invalid choice. Please enter a number.")
        next
      }

      if (choice == 1) {
        if (is_new) {
          measure_data[[measure$name]] <- measure
        } else {
          for (field in names(measure)) {
            measure_data[[measure$name]][[field]] <- measure[[field]]
          }
        }
        measure_data <- measure_data[order(names(measure_data))]
        cli::cli_alert_success("Measure {.val {measure$name}} saved successfully.")
        return(list(saved = TRUE, measure_data = measure_data))
      } else if (choice == 2) {
        modified_measure <- enter_or_modify_measure(measure)
        if (is.character(modified_measure) && modified_measure == "__back__") {
          next
        } else {
          measure <- modified_measure
        }
      } else if (choice == 3) {
        new_measure <- enter_or_modify_measure()
        if (is.character(new_measure) && new_measure == "__back__") {
          next
        } else {
          measure <- new_measure
          is_new <- TRUE
        }
      } else if (choice == 4) {
        return(list(saved = FALSE, measure_data = measure_data))
      } else {
        cli::cli_alert_danger("Invalid choice. Please try again.")
      }
    }
  }

  list_measures <- function(measure_data) {
    return(names(measure_data))
  }

  delete_measure <- function(measure_data, name) {
    if (name %in% names(measure_data)) {
      measure_data[[name]] <- NULL
      measure_data <- measure_data[order(names(measure_data))]
      cli::cli_alert_success("Measure {.val {name}} deleted successfully.")
    } else {
      cli::cli_alert_danger("Measure {.val {name}} not found.")
    }
    return(measure_data)
  }

  create_new_database <- function() {
    cli::cli_h2("Creating a new measures database")

    db_name <- get_input("Enter a name for the new database (without .rds extension): ")
    if (db_name == "__back__")
      return(NULL)
    if (!grepl("\\.rds$", db_name)) {
      db_name <- paste0(db_name, ".rds")
    }

    full_path <- file.path(measures_path, db_name)
    cli::cli_alert_info("The database will be created at: {.file {full_path}}")
    confirm <- tolower(get_input("Is this correct? (y/n): "))
    if (confirm == "__back__")
      return(NULL)

    if (confirm == "y") {
      new_measure_data <- list()
      saveRDS(new_measure_data, file = full_path)
      cli::cli_alert_success("New measures database '{.file {db_name}}' created.")
      return(list(measure_data = new_measure_data, current_file = db_name))
    } else {
      cli::cli_alert_warning("Database creation cancelled.")
      return(NULL)
    }
  }

  batch_edit_measures <- function(measures_data, field, old_value, new_value) {
    edited_count <- 0

    for (measure_name in names(measures_data)) {
      measure <- measures_data[[measure_name]]
      if (field %in% names(measure)) {
        if (is.character(measure[[field]])) {
          if (measure[[field]] == old_value) {
            measures_data[[measure_name]][[field]] <- new_value
            edited_count <- edited_count + 1
          }
        } else if (is.list(measure[[field]])) {
          for (i in seq_along(measure[[field]])) {
            if (measure[[field]][[i]] == old_value) {
              measures_data[[measure_name]][[field]][[i]] <- new_value
              edited_count <- edited_count + 1
            }
          }
        }
      }
    }

    cli::cli_alert_success("Edited {edited_count} entries.")
    return(measures_data)
  }

  run_gui <- function() {
    cli::cli_h1("Welcome to the Margot's Measures Database Manager")
    measure_data <- list()
    current_file <- NULL

    repeat {
      cli::cli_h2("Boilerplate Measures Manager")
      cli::cli_ol(c(
        "Create new measures database",
        "Open existing measures database",
        "List available .rds files",
        "Quit"
      ))

      choice <- get_input("Enter your choice: ")
      if (choice == "__back__") {
        next
      }
      choice <- as.integer(choice)

      if (is.na(choice)) {
        cli::cli_alert_danger("Invalid choice. Please enter a number.")
        next
      }

      if (choice == 1) {
        result <- create_new_database()
        if (!is.null(result)) {
          measure_data <- result$measure_data
          current_file <- result$current_file

          repeat {
            new_measure <- enter_or_modify_measure()
            if (is.character(new_measure) && length(new_measure) == 1 && new_measure == "__back__") {
              break
            }
            review_result <- review_and_save_measure(measure_data, new_measure)
            if (review_result$saved) {
              measure_data <- review_result$measure_data
              saveRDS(measure_data, file = file.path(measures_path, current_file))
              cli::cli_alert_success("Measure saved to database.")

              continue <- tolower(get_input("Would you like to enter another measure? (y/n): ")) == "y"
              if (!continue) {
                break
              }
            }
          }
        }
      } else if (choice == 2) {
        files <- list_rds_files()
        if (!is.null(files)) {
          file_choice <- get_input("Enter the number of the file you want to open: ")
          if (file_choice == "__back__")
            next
          file_choice <- as.integer(file_choice)
          if (is.na(file_choice)) {
            cli::cli_alert_danger("Invalid choice. Please enter a number.")
            next
          }
          if (file_choice > 0 && file_choice <= length(files)) {
            file_name <- files[file_choice]
            result <- load_data(file_name)
            if (!is.null(result)) {
              measure_data <- result$measure_data
              current_file <- result$current_file
            }
          } else {
            cli::cli_alert_danger("Invalid choice. Please try again.")
            next
          }
        } else {
          next
        }
      } else if (choice == 3) {
        list_rds_files()
        next
      } else if (choice == 4) {
        confirm_quit <- tolower(get_input(
          "Are you sure you want to quit? Unsaved changes will be lost. (y/n): "
        ))
        if (confirm_quit == "y") {
          cli::cli_alert_success("Exiting program. Goodbye!")
          return()
        } else {
          next
        }
      } else {
        cli::cli_alert_danger("Invalid choice. Please try again.")
        next
      }

      if (choice == 1 || choice == 2)
        break
    }

    repeat {
      cli::cli_h2("Measures Database Management")
      cli::cli_ol(c(
        "List measures",
        "Add measure",
        "Delete measure",
        "Modify measure",
        "Save measures data",
        "Batch edit measures",
        "Exit"
      ))

      choice <- get_input("Enter your choice: ")
      if (choice == "__back__") {
        break
      }
      choice <- as.integer(choice)

      if (is.na(choice)) {
        cli::cli_alert_danger("Invalid choice. Please enter a number.")
        next
      }

      switch(choice,
             {
               # Option 1: List measures
               measures <- list_measures(measure_data)
               if (length(measures) > 0) {
                 cli::cli_h3("Available measures:")
                 cli::cli_ol(measures)
               } else {
                 cli::cli_alert_warning("No measures available.")
               }
             },
             {
               # Option 2: Add measure
               new_measure <- enter_or_modify_measure()
               if (!is.character(new_measure) || new_measure != "__back__") {
                 result <- review_and_save_measure(measure_data, new_measure)
                 measure_data <- result$measure_data
                 if (result$saved) {
                   saveRDS(measure_data, file = file.path(measures_path, current_file))
                   cli::cli_alert_success("Measure saved to database.")
                 }
               }
             },
             {
               # Option 3: Delete measure
               measures <- list_measures(measure_data)
               if (length(measures) > 0) {
                 cli::cli_h3("Available measures:")
                 cli::cli_ol(measures)
                 choice <- get_input("Enter the number of the measure to delete: ")
                 if (choice != "__back__") {
                   choice <- as.integer(choice)
                   if (is.na(choice)) {
                     cli::cli_alert_danger("Invalid choice. Please enter a number.")
                   } else if (choice > 0 && choice <= length(measures)) {
                     measure_data <- delete_measure(measure_data, measures[choice])
                     saveRDS(measure_data, file = file.path(measures_path, current_file))
                     cli::cli_alert_success("Measure deleted and database updated.")
                   } else {
                     cli::cli_alert_danger("Invalid choice. Please try again.")
                   }
                 }
               } else {
                 cli::cli_alert_warning("No measures available to delete.")
               }
             },
             {
               # Option 4: Modify measure
               measures <- list_measures(measure_data)
               if (length(measures) > 0) {
                 cli::cli_h3("Available measures:")
                 cli::cli_ol(measures)
                 choice <- get_input("Enter the number of the measure to modify: ")

                 if (choice != "__back__") {
                   choice <- as.integer(choice)
                   if (is.na(choice)) {
                     cli::cli_alert_danger("Invalid choice. Please enter a number.")
                   } else if (choice > 0 && choice <= length(measures)) {
                     measure <- enter_or_modify_measure(measure_data[[measures[choice]]])
                     if (!is.character(measure) || measure != "__back__") {
                       result <- review_and_save_measure(measure_data, measure, is_new = FALSE)
                       measure_data <- result$measure_data
                       if (result$saved) {
                         saveRDS(measure_data, file = file.path(measures_path, current_file))
                         cli::cli_alert_success("Measure modified and database updated.")
                       }
                     }
                   } else {
                     cli::cli_alert_danger("Invalid choice. Please try again.")
                   }
                 }
               } else {
                 cli::cli_alert_warning("No measures available to modify.")
               }
             },
             {
               # Option 5: Save measures data
               default_name <- current_file %||% "measures_data.rds"
               cli::cli_h3("Save options:")
               cli::cli_ol(c(
                 paste("Use current file name:", default_name),
                 "Enter a new file name"
               ))
               save_choice <- get_input("Enter your choice: ")
               if (save_choice != "__back__") {
                 save_choice <- as.integer(save_choice)
                 if (is.na(save_choice)) {
                   cli::cli_alert_warning("Invalid choice. Using default name.")
                   file_name <- default_name
                 } else if (save_choice == 1) {
                   file_name <- default_name
                 } else if (save_choice == 2) {
                   file_name <- get_input("Enter new file name (including .rds extension): ")
                   if (file_name == "__back__")
                     next
                 } else {
                   cli::cli_alert_warning("Invalid choice. Using default name.")
                   file_name <- default_name
                 }

                 current_file <- save_data(measure_data, file_name)
               }
             },
             {
               # Option 6: Batch edit measures
               field <- get_input("Enter the field to edit (e.g., 'reference', 'name', 'description'): ")
               if (field != "__back__") {
                 old_value <- get_input("Enter the old value: ")
                 if (old_value != "__back__") {
                   new_value <- get_input("Enter the new value: ")
                   if (new_value != "__back__") {
                     measure_data <- batch_edit_measures(measure_data, field, old_value, new_value)
                     saveRDS(measure_data, file = file.path(measures_path, current_file))
                     cli::cli_alert_success("Batch edit completed and database updated.")
                   }
                 }
               }
             },
             {
               # Option 7: Exit
               cli::cli_alert_success("Exited. Have a nice day! :)")
               break
             },
             {
               # Default: Invalid choice
               cli::cli_alert_danger("Invalid choice. Please try again.")
             }
      )
    }
  }

  run_gui()
}
# margot_create_database <- function(measures_path = NULL) {
#   measures_path <- measures_path %||% here::here()
#
#   get_input <- function(prompt, allow_empty = FALSE) {
#     while (TRUE) {
#       input <- trimws(readline(paste0(prompt, " (enter 'b' to go back): ")))
#       if (tolower(input) == "b") {
#         return("__back__")
#       }
#       if (input != "" || allow_empty)
#         return(input)
#       cat("Input cannot be empty. Please try again.\n")
#     }
#   }
#
#   list_rds_files <- function() {
#     files <- list.files(measures_path, pattern = "\\.rds$")
#     if (length(files) == 0) {
#       cat("No .rds files found in the directory.\n")
#       return(NULL)
#     } else {
#       cat("Available .rds files:\n")
#       for (i in seq_along(files)) {
#         cat(i, ". ", files[i], "\n", sep = "")
#       }
#       return(files)
#     }
#   }
#
#   load_data <- function(file_name) {
#     file_path <- file.path(measures_path, file_name)
#     if (file.exists(file_path)) {
#       measure_data <- readRDS(file_path)
#       cat("Data loaded from:", file_path, "\n")
#       return(list(measure_data = measure_data, current_file = file_name))
#     } else {
#       cat("File not found:", file_path, "\n")
#       cat("Available files in", measures_path, ":\n")
#       list_rds_files()
#       return(NULL)
#     }
#   }
#
#   save_data <- function(measure_data, file_name, backup = FALSE) {
#     file_path <- file.path(measures_path, file_name)
#     saveRDS(measure_data, file = file_path)
#     cat("Data saved as:", file_path, "\n")
#     if (backup) {
#       backup_file <- paste0(tools::file_path_sans_ext(file_name), "_backup.rds")
#       backup_path <- file.path(measures_path, backup_file)
#       saveRDS(measure_data, file = backup_path)
#       cat("Backup saved as:", backup_path, "\n")
#     }
#     return(file_name)
#   }
#
#   enter_or_modify_measure <- function(existing_measure = NULL) {
#     measure <- existing_measure %||% list()
#     fields <- c("name", "items", "description", "reference", "waves", "keywords")
#
#     for (field in fields) {
#       current_value <- measure[[field]]
#
#       if (field == "items") {
#         if (is.null(current_value)) {
#           measure[[field]] <- list()
#           cat("\nEnter items (press Enter without typing anything to finish):\n")
#           cat("Example: How often do you have a drink containing alcohol?\n")
#           item_num <- 1
#           repeat {
#             item <- get_input(paste("Item", item_num, "(or press enter to finish):"), allow_empty = TRUE)
#             if (item == "__back__") return("__back__")
#             if (item == "") break
#             measure[[field]][[item_num]] <- item
#             item_num <- item_num + 1
#           }
#           if (length(measure[[field]]) == 0) {
#             cat("Error: At least one item is required. Please enter an item.\n")
#             next
#           }
#         } else {
#           cat("\nCurrent items:\n")
#           for (i in seq_along(current_value)) {
#             cat(i, ". ", current_value[[i]], "\n", sep = "")
#           }
#           modify <- tolower(get_input("Do you want to modify the items? (y/n): ")) == "y"
#           if (modify == "__back__") return("__back__")
#           if (modify) {
#             measure[[field]] <- list()
#             cat("Enter new items (press Enter without typing anything to finish):\n")
#             item_num <- 1
#             repeat {
#               item <- get_input(paste("Item", item_num, "(or press enter to finish):"), allow_empty = TRUE)
#               if (item == "__back__") return("__back__")
#               if (item == "") break
#               measure[[field]][[item_num]] <- item
#               item_num <- item_num + 1
#             }
#           }
#         }
#       } else {
#         example <- switch(
#           field,
#           name = "Example: alcohol_frequency",
#           description = "Example: Frequency of alcohol consumption was measured using a single item...",
#           reference = "Example: [@nzavs2009]",
#           waves = "Example: 1-current or 1-15",
#           keywords = 'Example: alcohol, frequency, consumption (optional, press enter to skip)'
#         )
#         cat("\n", example, "\n", sep = "")
#         cat("Current value:", if (is.null(current_value)) "None" else paste(current_value, collapse = ", "), "\n")
#
#         new_value <- get_input(paste("Enter new", field, "(press enter to keep current or skip):"), allow_empty = TRUE)
#         if (new_value == "__back__") return("__back__")
#
#         if (field == "keywords" && new_value != "") {
#           keywords <- strsplit(new_value, ",")[[1]]
#           keywords <- sapply(keywords, trimws)
#           new_value <- keywords  # Store as a character vector, not a single string
#         }
#
#         if (length(new_value) > 0 && new_value != "") {
#           measure[[field]] <- new_value
#         }
#       }
#     }
#
#     return(measure)
#   }
#
#   review_and_save_measure <- function(measure_data, measure, is_new = TRUE) {
#     if (is.character(measure) && measure == "__back__") {
#       return(list(saved = FALSE, measure_data = measure_data))
#     }
#
#     while (TRUE) {
#       cat("\nReview your entries:\n")
#       print(measure)
#       cat("\nWhat would you like to do?\n")
#       cat("1. Save measure\n")
#       cat("2. Modify measure\n")
#       cat("3. Start over\n")
#       cat("4. Cancel\n")
#
#       choice <- get_input("Enter your choice: ")
#       if (choice == "__back__")
#         return(list(saved = FALSE, measure_data = measure_data))
#       choice <- as.integer(choice)
#
#       if (is.na(choice)) {
#         cat("Invalid choice. Please enter a number.\n")
#         next
#       }
#
#       if (choice == 1) {
#         if (is_new) {
#           measure_data[[measure$name]] <- measure
#         } else {
#           for (field in names(measure)) {
#             measure_data[[measure$name]][[field]] <- measure[[field]]
#           }
#         }
#         measure_data <- measure_data[order(names(measure_data))]
#         cat("Measure", measure$name, "saved successfully.\n")
#         return(list(saved = TRUE, measure_data = measure_data))
#       } else if (choice == 2) {
#         modified_measure <- enter_or_modify_measure(measure)
#         if (is.character(modified_measure) && modified_measure == "__back__") {
#           next
#         } else {
#           measure <- modified_measure
#         }
#       } else if (choice == 3) {
#         new_measure <- enter_or_modify_measure()
#         if (is.character(new_measure) && new_measure == "__back__") {
#           next
#         } else {
#           measure <- new_measure
#           is_new <- TRUE
#         }
#       } else if (choice == 4) {
#         return(list(saved = FALSE, measure_data = measure_data))
#       } else {
#         cat("Invalid choice. Please try again.\n")
#       }
#     }
#   }
#
#   list_measures <- function(measure_data) {
#     return(names(measure_data))
#   }
#
#   delete_measure <- function(measure_data, name) {
#     if (name %in% names(measure_data)) {
#       measure_data[[name]] <- NULL
#       measure_data <- measure_data[order(names(measure_data))]
#       cat("Measure", name, "deleted successfully.\n")
#     } else {
#       cat("Measure", name, "not found.\n")
#     }
#     return(measure_data)
#   }
#
#   create_new_database <- function() {
#     cat("\nCreating a new measures database\n")
#
#     db_name <- get_input("Enter a name for the new database (without .rds extension): ")
#     if (db_name == "__back__")
#       return(NULL)
#     if (!grepl("\\.rds$", db_name)) {
#       db_name <- paste0(db_name, ".rds")
#     }
#
#     full_path <- file.path(measures_path, db_name)
#     cat("\nThe database will be created at:", full_path, "\n")
#     confirm <- tolower(get_input("Is this correct? (y/n): "))
#     if (confirm == "__back__")
#       return(NULL)
#
#     if (confirm == "y") {
#       new_measure_data <- list()
#       saveRDS(new_measure_data, file = full_path)
#       cat("New measures database '", db_name, "' created.\n", sep = "")
#       return(list(measure_data = new_measure_data, current_file = db_name))
#     } else {
#       cat("Database creation cancelled.\n")
#       return(NULL)
#     }
#   }
#
#   batch_edit_measures <- function(measures_data, field, old_value, new_value) {
#     edited_count <- 0
#
#     for (measure_name in names(measures_data)) {
#       measure <- measures_data[[measure_name]]
#       if (field %in% names(measure)) {
#         if (is.character(measure[[field]])) {
#           if (measure[[field]] == old_value) {
#             measures_data[[measure_name]][[field]] <- new_value
#             edited_count <- edited_count + 1
#           }
#         } else if (is.list(measure[[field]])) {
#           for (i in seq_along(measure[[field]])) {
#             if (measure[[field]][[i]] == old_value) {
#               measures_data[[measure_name]][[field]][[i]] <- new_value
#               edited_count <- edited_count + 1
#             }
#           }
#         }
#       }
#     }
#
#     cat("Edited", edited_count, "entries.\n")
#     return(measures_data)
#   }
#
#   run_gui <- function() {
#     cat("Welcome to the Margot's Measures Database Manager\n")
#     measure_data <- list()
#     current_file <- NULL
#
#     repeat {
#       cat("\nBoilerplate Measures Manager\n")
#       cat("1. Create new measures database\n")
#       cat("2. Open existing measures database\n")
#       cat("3. List available .rds files\n")
#       cat("4. Quit\n")
#
#       choice <- get_input("Enter your choice: ")
#       if (choice == "__back__") {
#         next
#       }
#       choice <- as.integer(choice)
#
#       if (is.na(choice)) {
#         cat("Invalid choice. Please enter a number.\n")
#         next
#       }
#
#       if (choice == 1) {
#         result <- create_new_database()
#         if (!is.null(result)) {
#           measure_data <- result$measure_data
#           current_file <- result$current_file
#
#           repeat {
#             new_measure <- enter_or_modify_measure()
#             if (is.character(new_measure) && length(new_measure) == 1 && new_measure == "__back__") {
#               break
#             }
#             review_result <- review_and_save_measure(measure_data, new_measure)
#             if (review_result$saved) {
#               measure_data <- review_result$measure_data
#               saveRDS(measure_data, file = file.path(measures_path, current_file))
#               cat("Measure saved to database.\n")
#
#               continue <- tolower(get_input("Would you like to enter another measure? (y/n): ")) == "y"
#               if (!continue) {
#                 break
#               }
#             }
#           }
#         }
#       } else if (choice == 2) {
#         files <- list_rds_files()
#         if (!is.null(files)) {
#           file_choice <- get_input("Enter the number of the file you want to open: ")
#           if (file_choice == "__back__")
#             next
#           file_choice <- as.integer(file_choice)
#           if (is.na(file_choice)) {
#             cat("Invalid choice. Please enter a number.\n")
#             next
#           }
#           if (file_choice > 0 && file_choice <= length(files)) {
#             file_name <- files[file_choice]
#             result <- load_data(file_name)
#             if (!is.null(result)) {
#               measure_data <- result$measure_data
#               current_file <- result$current_file
#             }
#           } else {
#             cat("Invalid choice. Please try again.\n")
#             next
#           }
#         } else {
#           next
#         }
#       } else if (choice == 3) {
#         list_rds_files()
#         next
#       } else if (choice == 4) {
#         confirm_quit <- tolower(get_input(
#           "Are you sure you want to quit? Unsaved changes will be lost. (y/n): "
#         ))
#         if (confirm_quit == "y") {
#           cat("Exiting program. Goodbye!\n")
#           return()
#         } else {
#           next
#         }
#       } else {
#         cat("Invalid choice. Please try again.\n")
#         next
#       }
#
#       if (choice == 1 || choice == 2)
#         break
#     }
#
#     repeat {
#       cat("\nMeasures Database Management\n")
#       cat("1. List measures\n")
#       cat("2. Add measure\n")
#       cat("3. Delete measure\n")
#       cat("4. Modify measure\n")
#       cat("5. Save measures data\n")
#       cat("6. Batch edit measures\n")
#       cat("7. Exit\n")
#
#       choice <- get_input("Enter your choice: ")
#       if (choice == "__back__") {
#         break
#       }
#       choice <- as.integer(choice)
#
#       if (is.na(choice)) {
#         cat("Invalid choice. Please enter a number.\n")
#         next
#       }
#
#       switch(choice,
#              {
#                # Option 1: List measures
#                measures <- list_measures(measure_data)
#                if (length(measures) > 0) {
#                  cat("Available measures:\n")
#                  for (i in seq_along(measures)) {
#                    cat(i, ". ", measures[i], "\n", sep = "")
#                  }
#                } else {
#                  cat("No measures available.\n")
#                }
#              },
#              {
#                # Option 2: Add measure
#                new_measure <- enter_or_modify_measure()
#                if (!is.character(new_measure) || new_measure != "__back__") {
#                  result <- review_and_save_measure(measure_data, new_measure)
#                  measure_data <- result$measure_data
#                  if (result$saved) {
#                    saveRDS(measure_data, file = file.path(measures_path, current_file))
#                    cat("Measure saved to database.\n")
#                  }
#                }
#              },
#              {
#                # Option 3: Delete measure
#                measures <- list_measures(measure_data)
#                if (length(measures) > 0) {
#                  cat("Available measures:\n")
#                  for (i in seq_along(measures)) {
#                    cat(i, ". ", measures[i], "\n", sep = "")
#                  }
#                  choice <- get_input("Enter the number of the measure to delete: ")
#                  if (choice != "__back__") {
#                    choice <- as.integer(choice)
#                    if (is.na(choice)) {
#                      cat("Invalid choice. Please enter a number.\n")
#                    } else if (choice > 0 && choice <= length(measures)) {
#                      measure_data <- delete_measure(measure_data, measures[choice])
#                      saveRDS(measure_data, file = file.path(measures_path, current_file))
#                      cat("Measure deleted and database updated.\n")
#                    } else {
#                      cat("Invalid choice. Please try again.\n")
#                    }
#                  }
#                } else {
#                  cat("No measures available to delete.\n")
#                }
#              },
#              {
#                # Option 4: Modify measure
#                measures <- list_measures(measure_data)
#                if (length(measures) > 0) {
#                  cat("Available measures:\n")
#                  for (i in seq_along(measures)) {
#                    cat(i, ". ", measures[i], "\n", sep = "")
#                  }
#                  choice <- get_input("Enter the number of the measure to modify: ")
#
#                  if (choice != "__back__") {
#                    choice <- as.integer(choice)
#                    if (is.na(choice)) {
#                      cat("Invalid choice. Please enter a number.\n")
#                    } else if (choice > 0 && choice <= length(measures)) {
#                      measure <- enter_or_modify_measure(measure_data[[measures[choice]]])
#                      if (!is.character(measure) || measure != "__back__") {
#                        result <- review_and_save_measure(measure_data, measure, is_new = FALSE)
#                        measure_data <- result$measure_data
#                        if (result$saved) {
#                          saveRDS(measure_data, file = file.path(measures_path, current_file))
#                          cat("Measure modified and database updated.\n")
#                        }
#                      }
#                    } else {
#                      cat("Invalid choice. Please try again.\n")
#                    }
#                  }
#                } else {
#                  cat("No measures available to modify.\n")
#                }
#              },
#              {
#                # Option 5: Save measures data
#                default_name <- current_file %||% "measures_data.rds"
#                cat("\nSave options:\n")
#                cat("1. Use current file name:", default_name, "\n")
#                cat("2. Enter a new file name\n")
#                save_choice <- get_input("Enter your choice: ")
#                if (save_choice != "__back__") {
#                  save_choice <- as.integer(save_choice)
#                  if (is.na(save_choice)) {
#                    cat("Invalid choice. Using default name.\n")
#                    file_name <- default_name
#                  } else if (save_choice == 1) {
#                    file_name <- default_name
#                  } else if (save_choice == 2) {
#                    file_name <- get_input("Enter new file name (including .rds extension): ")
#                    if (file_name == "__back__")
#                      next
#                  } else {
#                    cat("Invalid choice. Using default name.\n")
#                    file_name <- default_name
#                  }
#
#                  current_file <- save_data(measure_data, file_name)
#                }
#              },
#              {
#                # Option 6: Batch edit measures
#                field <- get_input("Enter the field to edit (e.g., 'reference', 'name', 'description'): ")
#                if (field != "__back__") {
#                  old_value <- get_input("Enter the old value: ")
#                  if (old_value != "__back__") {
#                    new_value <- get_input("Enter the new value: ")
#                    if (new_value != "__back__") {
#                      measure_data <- batch_edit_measures(measure_data, field, old_value, new_value)
#                      saveRDS(measure_data, file = file.path(measures_path, current_file))
#                      cat("Batch edit completed and database updated.\n")
#                    }
#                  }
#                }
#              },
#              {
#                # Option 7: Exit
#                cat("Exited. Have a nice day! :)\n")
#                break
#              },
#              {
#                # Default: Invalid choice
#                cat("Invalid choice. Please try again.\n")
#              }
#       )
#     }
#   }
#
#   run_gui()
# }

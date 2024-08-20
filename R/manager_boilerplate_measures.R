#' Manage Boilerplate Measures (deprecated, use `margot_create_database`)
#'
#' This function provides a command-line interface for managing a database of 'boilerplate' for measures.
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
#'
#' @examples
#' \dontrun{
#' # Run the function with the default path (current working directory)
#' manager_boilerplate_measures()
#'
#' # Run the function with a specific path
#' manager_boilerplate_measures("path/to/measures/directory")
#' }
#'
#' @export
#' @keywords internal
#' @importFrom lifecycle deprecate_warn
manager_boilerplate_measures <- function(measures_path = NULL) {
  library(here)
  library(rlang)

  # warning
  lifecycle::deprecate_warn("1.0.0", "manager_boilerplate_measures()", "margot_create_database()")


  measures_path <- measures_path %||% here::here()

  get_input <- function(prompt, allow_empty = FALSE) {
    while (TRUE) {
      input <- trimws(readline(paste0(prompt, " (enter 'b' to go back): ")))
      if (tolower(input) == "b") {
        return("__back__")
      }
      if (input != "" || allow_empty)
        return(input)
      cat("Input cannot be empty. Please try again.\n")
    }
  }

  list_rds_files <- function() {
    files <- list.files(measures_path, pattern = "\\.rds$")
    if (length(files) == 0) {
      cat("No .rds files found in the directory.\n")
      return(NULL)
    } else {
      cat("Available .rds files:\n")
      for (i in seq_along(files)) {
        cat(i, ". ", files[i], "\n", sep = "")
      }
      return(files)
    }
  }

  load_data <- function(file_name) {
    file_path <- file.path(measures_path, file_name)
    if (file.exists(file_path)) {
      measure_data <- readRDS(file_path)
      cat("Data loaded from:", file_path, "\n")
      return(list(measure_data = measure_data, current_file = file_name))
    } else {
      cat("File not found:", file_path, "\n")
      cat("Available files in", measures_path, ":\n")
      list_rds_files()
      return(NULL)
    }
  }

  save_data <- function(measure_data, file_name, backup = FALSE) {
    file_path <- file.path(measures_path, file_name)

    if (file.exists(file_path)) {
      cat("Warning: You are about to overwrite an existing file.\n")
      confirm <- tolower(get_input("Do you want to continue? (y/n): "))
      if (confirm != "y") {
        cat("Save operation cancelled.\n")
        return(file_name)
      }
    }

    saveRDS(measure_data, file = file_path)
    cat("Data saved as:", file_path, "\n")

    if (backup) {
      backup_file <- paste0(tools::file_path_sans_ext(file_name), "_backup.rds")
      backup_path <- file.path(measures_path, backup_file)
      saveRDS(measure_data, file = backup_path)
      cat("Backup saved as:", backup_path, "\n")
    }

    return(file_name)
  }

  enter_or_modify_measure <- function(existing_measure = NULL) {
    measure <- existing_measure %||% list()
    fields <- c("name",
                "items",
                "description",
                "reference",
                "waves",
                "keywords")

    for (field in fields) {
      current_value <- measure[[field]]

      if (field == "items") {
        if (is.null(current_value)) {
          measure[[field]] <- list()
          cat("\nEnter items (press Enter without typing anything to finish):\n")
          cat("Example: How often do you have a drink containing alcohol?\n")
          item_num <- 1
          repeat {
            item <- get_input(paste("Item", item_num, "(or press enter to finish):"),
                              allow_empty = TRUE)
            if (item == "__back__")
              return("__back__")
            if (item == "")
              break
            measure[[field]][[item_num]] <- item
            item_num <- item_num + 1
          }
          if (length(measure[[field]]) == 0) {
            cat("Error: At least one item is required. Please enter an item.\n")
            next
          }
        } else {
          cat("\nCurrent items:\n")
          for (i in seq_along(current_value)) {
            cat(i, ". ", current_value[[i]], "\n", sep = "")
          }
          modify <- tolower(get_input("Do you want to modify the items? (y/n): ")) == "y"
          if (modify == "__back__")
            return("__back__")
          if (modify) {
            measure[[field]] <- list()
            cat("Enter new items (press Enter without typing anything to finish):\n")
            item_num <- 1
            repeat {
              item <- get_input(paste("Item", item_num, "(or press enter to finish):"),
                                allow_empty = TRUE)
              if (item == "__back__")
                return("__back__")
              if (item == "")
                break
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
        cat("\n", example, "\n", sep = "")
        cat("Current value:",
            if (is.null(current_value))
              "None"
            else
              paste(current_value, collapse = ", "),
            "\n")

        new_value <- get_input(
          paste(
            "Enter new",
            field,
            "(press enter to keep current or skip):"
          ),
          allow_empty = TRUE
        )
        if (new_value == "__back__")
          return("__back__")

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
      cat("\nReview your entries:\n")
      print(measure)
      cat("\nWhat would you like to do?\n")
      cat("1. Save measure\n")
      cat("2. Modify measure\n")
      cat("3. Start over\n")
      cat("4. Cancel\n")

      choice <- get_input("Enter your choice: ")
      if (choice == "__back__")
        return(list(saved = FALSE, measure_data = measure_data))
      choice <- as.integer(choice)

      if (is.na(choice)) {
        cat("Invalid choice. Please enter a number.\n")
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
        cat("Measure", measure$name, "saved successfully.\n")
        return(list(saved = TRUE, measure_data = measure_data))
      } else if (choice == 2) {
        modified_measure <- enter_or_modify_measure(measure)
        if (is.character(modified_measure) &&
            modified_measure == "__back__") {
          next
        } else {
          measure <- modified_measure
        }
      } else if (choice == 3) {
        new_measure <- enter_or_modify_measure()
        if (is.character(new_measure) &&
            new_measure == "__back__") {
          next
        } else {
          measure <- new_measure
          is_new <- TRUE
        }
      } else if (choice == 4) {
        return(list(saved = FALSE, measure_data = measure_data))
      } else {
        cat("Invalid choice. Please try again.\n")
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
      cat("Measure", name, "deleted successfully.\n")
    } else {
      cat("Measure", name, "not found.\n")
    }
    return(measure_data)
  }

  create_new_database <- function() {
    cat("\nCreating a new measures database\n")

    db_name <- get_input("Enter a name for the new database (without .rds extension): ")
    if (db_name == "__back__")
      return(NULL)
    if (!grepl("\\.rds$", db_name)) {
      db_name <- paste0(db_name, ".rds")
    }

    full_path <- file.path(measures_path, db_name)
    cat("\nThe database will be created at:", full_path, "\n")
    confirm <- tolower(get_input("Is this correct? (y/n): "))
    if (confirm == "__back__")
      return(NULL)

    if (confirm == "y") {
      cat("New measures database '", db_name, "' created.\n", sep = "")
      return(list(measure_data = list(), current_file = db_name))
    } else {
      cat("Database creation cancelled.\n")
      return(NULL)
    }
  }

  # batch editing
  batch_edit_measures <- function(measures_data,
                                  field,
                                  old_value,
                                  new_value) {
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

    cat("Edited", edited_count, "entries.\n")
    return(measures_data)
  }

  run_gui <- function() {
    cat("Welcome to the Boilerplate Measures Manager\n")
    measure_data <- list()
    current_file <- NULL

    repeat {
      cat("\nBoilerplate Measures Manager\n")
      cat("1. Create new measures database\n")
      cat("2. Open existing measures database\n")
      cat("3. List available .rds files\n")
      cat("4. Quit\n")

      choice <- get_input("Enter your choice: ")
      if (choice == "__back__") {
        next
      }
      choice <- as.integer(choice)

      if (is.na(choice)) {
        cat("Invalid choice. Please enter a number.\n")
        next
      }

      if (choice == 1) {
        result <- create_new_database()
        if (!is.null(result)) {
          measure_data <- result$measure_data
          current_file <- result$current_file
          repeat {
            measure <- enter_or_modify_measure()
            if (measure == "__back__")
              break
            result <- review_and_save_measure(measure_data, measure)
            measure_data <- result$measure_data
            if (result$saved) {
              continue <- tolower(get_input("Would you like to enter another measure? (y/n): ")) == "y"
              if (!continue)
                break
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
            cat("Invalid choice. Please enter a number.\n")
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
            cat("Invalid choice. Please try again.\n")
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
          cat("Exiting program. Goodbye!\n")
          return()
        } else {
          next
        }
      } else {
        cat("Invalid choice. Please try again.\n")
        next
      }

      if (choice == 1 || choice == 2)
        break
    }

    repeat {
      cat("\nMeasures Database Management\n")
      cat("1. List measures\n")
      cat("2. Add measure\n")
      cat("3. Delete measure\n")
      cat("4. Modify measure\n")
      cat("5. Save measures data\n")
      cat("6. Batch edit measures\n")
      cat("7. Exit\n")

      choice <- get_input("Enter your choice: ")
      if (choice == "__back__") {
        break
      }
      choice <- as.integer(choice)

      if (is.na(choice)) {
        cat("Invalid choice. Please enter a number.\n")
        next
      }

      switch(choice, {
        # Option 1: List measures
        measures <- list_measures(measure_data)
        if (length(measures) > 0) {
          cat("Available measures:\n")
          for (i in seq_along(measures)) {
            cat(i, ". ", measures[i], "\n", sep = "")
          }
        } else {
          cat("No measures available.\n")
        }
      }, {
        # Option 2: Add measure
        measure <- enter_or_modify_measure()
        if (!is.character(measure) ||
            measure != "__back__") {
          result <- review_and_save_measure(measure_data, measure)
          measure_data <- result$measure_data
        }
      }, {
        # Option 3: Delete measure
        measures <- list_measures(measure_data)
        if (length(measures) > 0) {
          cat("Available measures:\n")
          for (i in seq_along(measures)) {
            cat(i, ". ", measures[i], "\n", sep = "")
          }
          choice <- get_input("Enter the number of the measure to delete: ")
          if (choice != "__back__") {
            choice <- as.integer(choice)
            if (is.na(choice)) {
              cat("Invalid choice. Please enter a number.\n")
            } else if (choice > 0 &&
                       choice <= length(measures)) {
              measure_data <- delete_measure(measure_data, measures[choice])
            } else {
              cat("Invalid choice. Please try again.\n")
            }
          }
        } else {
          cat("No measures available to delete.\n")
        }
      }, {
        # Option 4: Modify measure
        measures <- list_measures(measure_data)
        if (length(measures) > 0) {
          cat("Available measures:\n")
          for (i in seq_along(measures)) {
            cat(i, ". ", measures[i], "\n", sep = "")
          }
          choice <- get_input("Enter the number of the measure to modify: ")

          if (choice != "__back__") {
            choice <- as.integer(choice)
            if (is.na(choice)) {
              cat("Invalid choice. Please enter a number.\n")
            } else if (choice > 0 &&
                       choice <= length(measures)) {
              measure <- enter_or_modify_measure(measure_data[[measures[choice]]])
              if (!is.character(measure) ||
                  measure != "__back__") {
                result <- review_and_save_measure(measure_data, measure, is_new = FALSE)
                measure_data <- result$measure_data
              }
            } else {
              cat("Invalid choice. Please try again.\n")
            }
          }
        } else {
          cat("No measures available to modify.\n")
        }
      }, {
        # Option 5: Save measures data
        default_name <- current_file %||% "measures_data.rds"
        cat("\nSave options:\n")
        cat("1. Use current file name:", default_name, "\n")
        cat("2. Enter a new file name\n")
        save_choice <- get_input("Enter your choice: ")
        if (save_choice != "__back__") {
          save_choice <- as.integer(save_choice)
          if (is.na(save_choice)) {
            cat("Invalid choice. Using default name.\n")
            file_name <- default_name
          } else if (save_choice == 1) {
            file_name <- default_name
          } else if (save_choice == 2) {
            file_name <- get_input("Enter new file name (including .rds extension): ")
            if (file_name == "__back__")
              next
          } else {
            cat("Invalid choice. Using default name.\n")
            file_name <- default_name
          }

          current_file <- save_data(measure_data, file_name)
        }
      }, {
        # Option 6: Batch edit measures
        field <- get_input("Enter the field to edit (e.g., 'reference', 'name', 'description'): ")
        if (field != "__back__") {
          old_value <- get_input("Enter the old value: ")
          if (old_value != "__back__") {
            new_value <- get_input("Enter the new value: ")
            if (new_value != "__back__") {
              measure_data <- batch_edit_measures(measure_data, field, old_value, new_value)
              cat("Batch edit completed.\n")
            }
          }
        }
      }, {
        # Option 7: Exit
        break
      }, {
        # Default: Invalid choice
        cat("Invalid choice. Please try again.\n")
      })
    }
  }

  run_gui()
}
# manager_boilerplate_measures <- function(measures_path = NULL) {
#   library(here)
#   library(rlang)
#
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
#         cat(i, ". ", files[i], "\n", sep="")
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
#
#     if (file.exists(file_path)) {
#       cat("Warning: You are about to overwrite an existing file.\n")
#       confirm <- tolower(get_input("Do you want to continue? (y/n): "))
#       if (confirm != "y") {
#         cat("Save operation cancelled.\n")
#         return(file_name)
#       }
#     }
#
#     saveRDS(measure_data, file = file_path)
#     cat("Data saved as:", file_path, "\n")
#
#     if (backup) {
#       backup_file <- paste0(tools::file_path_sans_ext(file_name), "_backup.rds")
#       backup_path <- file.path(measures_path, backup_file)
#       saveRDS(measure_data, file = backup_path)
#       cat("Backup saved as:", backup_path, "\n")
#     }
#
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
#             cat(i, ". ", current_value[[i]], "\n", sep="")
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
#         example <- switch(field,
#                           name = "Example: alcohol_frequency",
#                           description = "Example: Frequency of alcohol consumption was measured using a single item...",
#                           reference = "Example: [@nzavs2009]",
#                           waves = "Example: 1-current or 1-15",
#                           keywords = 'Example: alcohol, frequency, consumption (optional, press enter to skip)')
#         cat("\n", example, "\n", sep = "")
#         cat("Current value:", if(is.null(current_value)) "None" else paste(current_value, collapse = ", "), "\n")
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
#       if (choice == "__back__") return(list(saved = FALSE, measure_data = measure_data))
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
#       cat("New measures database '", db_name, "' created.\n", sep = "")
#       return(list(measure_data = list(), current_file = db_name))
#     } else {
#       cat("Database creation cancelled.\n")
#       return(NULL)
#     }
#   }
#
#   # batch editing
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
#     cat("Welcome to the Boilerplate Measures Manager\n")
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
#           repeat {
#             measure <- enter_or_modify_measure()
#             if (measure == "__back__")
#               break
#             result <- review_and_save_measure(measure_data, measure)
#             measure_data <- result$measure_data
#             if (result$saved) {
#               continue <- tolower(get_input("Would you like to enter another measure? (y/n): ")) == "y"
#               if (!continue)
#                 break
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
#                measure <- enter_or_modify_measure()
#                if (!is.character(measure) || measure != "__back__") {
#                  result <- review_and_save_measure(measure_data, measure)
#                  measure_data <- result$measure_data
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
#                  backup <- tolower(get_input("Do you want to create a backup? (y/n): ")) == "y"
#                  if (backup != "__back__") {
#                    current_file <- save_data(measure_data, file_name, backup)
#                  }
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
#                      cat("Batch edit completed.\n")
#                    }
#                  }
#                }
#              },
#              {
#                # Option 7: Exit
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
# manager_boilerplate_measures <- function(measures_path = NULL) {
#   library(here)
#
#   # define function if not using rlang
#   # `%||%` <- function(a, b) {
#   #   if (!is.null(a)) a else b
#   # }
#   #
#
#   measures_path <- measures_path %||% here::here()
#
#   measure_data <- list()
#   current_file <- NULL
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
#         cat(i, ". ", files[i], "\n", sep="")
#       }
#       return(files)
#     }
#   }
#
#   load_data <- function(file_name) {
#     file_path <- file.path(measures_path, file_name)
#     if (file.exists(file_path)) {
#       measure_data <<- readRDS(file_path)
#       current_file <<- file_name
#       cat("Data loaded from:", file_path, "\n")
#     } else {
#       cat("File not found:", file_path, "\n")
#       cat("Available files in", measures_path, ":\n")
#       list_rds_files()
#     }
#   }
#
#   save_data <- function(file_name, backup = FALSE) {
#     file_path <- file.path(measures_path, file_name)
#
#     if (file.exists(file_path)) {
#       cat("Warning: You are about to overwrite an existing file.\n")
#       confirm <- tolower(get_input("Do you want to continue? (y/n): "))
#       if (confirm != "y") {
#         cat("Save operation cancelled.\n")
#         return()
#       }
#     }
#
#     saveRDS(measure_data, file = file_path)
#     current_file <<- file_name
#     cat("Data saved as:", file_path, "\n")
#
#     if (backup) {
#       backup_file <- paste0(tools::file_path_sans_ext(file_name), "_backup.rds")
#       backup_path <- file.path(measures_path, backup_file)
#       saveRDS(measure_data, file = backup_path)
#       cat("Backup saved as:", backup_path, "\n")
#     }
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
#             cat(i, ". ", current_value[[i]], "\n", sep="")
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
#         example <- switch(field,
#                           name = "Example: alcohol_frequency",
#                           description = "Example: Frequency of alcohol consumption was measured using a single item...",
#                           reference = "Example: [@nzavs2009]",
#                           waves = "Example: 1-current or 1-15",
#                           keywords = 'Example: alcohol, frequency, consumption (optional, press enter to skip)')
#         cat("\n", example, "\n", sep = "")
#         cat("Current value:", if(is.null(current_value)) "None" else paste(current_value, collapse = ", "), "\n")
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
#   review_and_save_measure <- function(measure, is_new = TRUE) {
#     if (is.character(measure) && measure == "__back__") {
#       return(FALSE)
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
#       if (choice == "__back__") return(FALSE)
#       choice <- as.integer(choice)
#
#       if (is.na(choice)) {
#         cat("Invalid choice. Please enter a number.\n")
#         next
#       }
#
#       if (choice == 1) {
#         if (is_new) {
#           measure_data[[measure$name]] <<- measure
#         } else {
#           for (field in names(measure)) {
#             measure_data[[measure$name]][[field]] <<- measure[[field]]
#           }
#         }
#         measure_data <<- measure_data[order(names(measure_data))]
#         cat("Measure", measure$name, "saved successfully.\n")
#         return(TRUE)
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
#         return(FALSE)
#       } else {
#         cat("Invalid choice. Please try again.\n")
#       }
#     }
#   }
#
#   list_measures <- function() {
#     return(names(measure_data))
#   }
#
#   delete_measure <- function(name) {
#     if (name %in% names(measure_data)) {
#       measure_data[[name]] <<- NULL
#       measure_data <<- measure_data[order(names(measure_data))]
#       cat("Measure", name, "deleted successfully.\n")
#     } else {
#       cat("Measure", name, "not found.\n")
#     }
#   }
#
#   create_new_database <- function() {
#     cat("\nCreating a new measures database\n")
#
#     db_name <- get_input("Enter a name for the new database (without .rds extension): ")
#     if (db_name == "__back__")
#       return(FALSE)
#     if (!grepl("\\.rds$", db_name)) {
#       db_name <- paste0(db_name, ".rds")
#     }
#
#     full_path <- file.path(measures_path, db_name)
#     cat("\nThe database will be created at:", full_path, "\n")
#     confirm <- tolower(get_input("Is this correct? (y/n): "))
#     if (confirm == "__back__")
#       return(FALSE)
#
#     if (confirm == "y") {
#       measure_data <<- list()
#       current_file <<- db_name
#       cat("New measures database '", db_name, "' created.\n", sep = "")
#       return(TRUE)
#     } else {
#       cat("Database creation cancelled.\n")
#       return(FALSE)
#     }
#   }
#
#   run_gui <- function() {
#     cat("Welcome to the Boilerplate Measures Manager\n")
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
#         if (create_new_database()) {
#           repeat {
#             measure <- enter_or_modify_measure()
#             if (measure == "__back__")
#               break
#             if (review_and_save_measure(measure)) {
#               continue <- tolower(get_input("Would you like to enter another measure? (y/n): ")) == "y"
#               if (!continue)
#                 break
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
#             load_data(file_name)
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
#       cat("6. Return to main menu\n")
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
#                measures <- list_measures()
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
#                measure <- enter_or_modify_measure()
#                if (!is.character(measure) || measure != "__back__") {
#                  review_and_save_measure(measure)
#                }
#              },
#              {
#                measures <- list_measures()
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
#                      delete_measure(measures[choice])
#                    } else {
#                      cat("Invalid choice. Please try again.\n")
#                    }
#                  }
#                } else {
#                  cat("No measures available to delete.\n")
#                }
#              },
#              {
#                measures <- list_measures()
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
#                        review_and_save_measure(measure, is_new = FALSE)
#                      }
#                    } else {
#                      cat("Invalid choice. Please try again.\n")
#                    }
#                  }
#                } else {
#                  cat("No measures available to modify.\n")
#
#                }
#              },
#              {
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
#                  backup <- tolower(get_input("Do you want to create a backup? (y/n): ")) == "y"
#                  if (backup != "__back__") {
#                    save_data(file_name, backup)
#                  }
#                }
#              },
#              {
#                break  # Return to main menu
#              },
#              {
#                cat("Invalid choice. Please try again.\n")
#              }
#       )
#     }
#   }
#
#   run_gui()
# }
# manager_boilerplate_measures <- function(measures_path = NULL) {
#   if (is.null(measures_path)) {
#     measures_path <- here::here()
#   } else {
#     measures_path <- here::here(measures_path)
#   }
#
#   measure_data <- list()
#   current_file <- NULL
#
#   get_input <- function(prompt, allow_empty = FALSE) {
#     while (TRUE) {
#       input <- readline(paste0(prompt, " (press 'q' to quit): "))
#       if (tolower(input) == "q") {
#         stop("Exiting program", call. = FALSE)
#       }
#       if (input != "" || allow_empty) return(input)
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
#       measure_data <<- readRDS(file_path)
#       current_file <<- file_name
#       cat("Data loaded from:", file_path, "\n")
#     } else {
#       cat("File not found:", file_path, "\n")
#       cat("Available files in", measures_path, ":\n")
#       list_rds_files()
#     }
#   }
#
#   save_data <- function(file_name, backup = FALSE) {
#     file_path <- file.path(measures_path, file_name)
#
#     if (file.exists(file_path)) {
#       cat("Warning: You are about to overwrite an existing file.\n")
#       confirm <- tolower(get_input("Do you want to continue? (y/n): "))
#       if (confirm != "y") {
#         cat("Save operation cancelled.\n")
#         return()
#       }
#     }
#
#     saveRDS(measure_data, file = file_path)
#     current_file <<- file_name
#     cat("Data saved as:", file_path, "\n")
#
#     if (backup) {
#       backup_file <- paste0(tools::file_path_sans_ext(file_name), "_backup.rds")
#       backup_path <- file.path(measures_path, backup_file)
#       saveRDS(measure_data, file = backup_path)
#       cat("Backup saved as:", backup_path, "\n")
#     }
#   }
#
#   enter_or_modify_measure <- function(existing_measure = NULL) {
#     measure <- existing_measure %||% list()
#     fields <- c("name", "items", "description", "reference", "waves", "keywords")
#
#     for (field in fields) {
#       current_value <- if (is.null(existing_measure)) "" else existing_measure[[field]]
#
#       if (field == "items") {
#         if (is.null(existing_measure)) {
#           measure[[field]] <- list()
#           cat("\nEnter items (press Enter without typing anything to finish):\n")
#           cat("Example: \"How often do you have a drink containing alcohol?\"\n")
#           item_num <- 1
#           while (TRUE) {
#             item <- get_input(paste("Item", item_num, "(or press enter to finish):"), allow_empty = TRUE)
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
#             cat(i, ". ", current_value[[i]], "\n", sep="")
#           }
#           modify <- tolower(get_input("Do you want to modify the items? (y/n): ")) == "y"
#           if (modify) {
#             measure[[field]] <- list()
#             cat("Enter new items (press Enter without typing anything to finish):\n")
#             item_num <- 1
#             while (TRUE) {
#               item <- get_input(paste("Item", item_num, "(or press enter to finish):"), allow_empty = TRUE)
#               if (item == "") break
#               measure[[field]][[item_num]] <- item
#               item_num <- item_num + 1
#             }
#           } else {
#             measure[[field]] <- current_value
#           }
#         }
#       } else {
#         example <- switch(field,
#                           name = "Example: alcohol_frequency",
#                           description = "Example: Frequency of alcohol consumption was measured using a single item...",
#                           reference = "Example: nzavs2009",
#                           waves = "Example: 1-current or 1-15",
#                           keywords = "Example: alcohol, frequency, consumption")
#         cat("\n", example, "\n", sep = "")
#         cat("Current value:", current_value, "\n")
#         new_value <- get_input(paste("Enter new", field, "(press enter to keep current):"), allow_empty = TRUE)
#         measure[[field]] <- if (new_value == "") current_value else new_value
#       }
#     }
#
#     return(measure)
#   }
#
#   review_and_save_measure <- function(measure, is_new = TRUE) {
#     while (TRUE) {
#       cat("\nReview your entries:\n")
#       print(measure)
#       cat("\nWhat would you like to do?\n")
#       cat("1. Save measure\n")
#       cat("2. Modify a field\n")
#       cat("3. Start over\n")
#       cat("4. Cancel\n")
#
#       choice <- as.integer(get_input("Enter your choice: "))
#
#       if (choice == 1) {
#         if (is_new) {
#           measure_data[[measure$name]] <<- measure
#         } else {
#           # Update the existing measure
#           for (field in names(measure)) {
#             measure_data[[measure$name]][[field]] <<- measure[[field]]
#           }
#         }
#         # Sort measures alphabetically
#         measure_data <<- measure_data[order(names(measure_data))]
#         cat("Measure", measure$name, "saved successfully.\n")
#         return(TRUE)
#       } else if (choice == 2) {
#         cat("\nWhich field would you like to modify?\n")
#         fields <- c("name", "items", "description", "reference", "waves", "keywords")
#         for (i in seq_along(fields)) {
#           cat(i, ". ", fields[i], "\n", sep="")
#         }
#         field_choice <- as.integer(get_input("Enter the number of the field to modify: "))
#         if (field_choice > 0 && field_choice <= length(fields)) {
#           field <- fields[field_choice]
#           measure <- enter_or_modify_measure(measure)
#         } else {
#           cat("Invalid choice. Please try again.\n")
#         }
#       } else if (choice == 3) {
#         return(enter_or_modify_measure())
#       } else if (choice == 4) {
#         return(FALSE)
#       } else {
#         cat("Invalid choice. Please try again.\n")
#       }
#     }
#   }
#
#   list_measures <- function() {
#     return(names(measure_data))
#   }
#
#   delete_measure <- function(name) {
#     if (name %in% names(measure_data)) {
#       measure_data[[name]] <<- NULL
#       # Sort measures alphabetically after deletion
#       measure_data <<- measure_data[order(names(measure_data))]
#       cat("Measure", name, "deleted successfully.\n")
#     } else {
#       cat("Measure", name, "not found.\n")
#     }
#   }
#
#   create_new_database <- function() {
#     cat("\nCreating a new measures database\n")
#
#     db_name <- get_input("Enter a name for the new database (without .rds extension): ")
#     if (!grepl("\\.rds$", db_name)) {
#       db_name <- paste0(db_name, ".rds")
#     }
#
#     full_path <- file.path(measures_path, db_name)
#     cat("\nThe database will be created at:", full_path, "\n")
#     confirm <- tolower(get_input("Is this correct? (y/n): "))
#
#     if (confirm == "y") {
#       measure_data <<- list()
#       current_file <<- db_name
#       cat("New measures database '", db_name, "' created.\n", sep = "")
#       return(TRUE)
#     } else {
#       cat("Database creation cancelled.\n")
#       return(FALSE)
#     }
#   }
#
#   run_gui <- function() {
#     cat("Welcome to the Boilerplate Measures Manager\n")
#
#     while (TRUE) {
#       cat("\nBoilerplate Measures Manager\n")
#       cat("1. Create new measures database\n")
#       cat("2. Open existing measures database\n")
#       cat("3. List available .rds files\n")
#       cat("4. Quit\n")
#
#       choice <- as.integer(get_input("Enter your choice: "))
#
#       if (choice == 1) {
#         if (create_new_database()) {
#           while (TRUE) {
#             measure <- enter_or_modify_measure()
#             if (review_and_save_measure(measure)) {
#               continue <- tolower(get_input("Would you like to enter another measure? (y/n): ")) == "y"
#               if (!continue) break
#             }
#           }
#         }
#       } else if (choice == 2) {
#         files <- list_rds_files()
#         if (!is.null(files)) {
#           file_choice <- as.integer(get_input("Enter the number of the file you want to open: "))
#           if (file_choice > 0 && file_choice <= length(files)) {
#             file_name <- files[file_choice]
#             load_data(file_name)
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
#         stop("Exiting program", call. = FALSE)
#       } else {
#         cat("Invalid choice. Please try again.\n")
#         next
#       }
#
#       if (choice == 1 || choice == 2) break
#     }
#
#     while (TRUE) {
#       cat("\nMeasures Database Management\n")
#       cat("1. List measures\n")
#       cat("2. Add measure\n")
#       cat("3. Delete measure\n")
#       cat("4. Modify measure\n")
#       cat("5. Save measures data\n")
#       cat("6. Quit\n")
#
#       choice <- as.integer(get_input("Enter your choice: "))
#
#       switch(choice,
#              {
#                measures <- list_measures()
#                if (length(measures) > 0) {
#                  cat("Available measures:\n")
#                  for (i in seq_along(measures)) {
#                    cat(i, ". ", measures[i], "\n", sep="")
#                  }
#                } else {
#                  cat("No measures available.\n")
#                }
#              },
#              {
#                measure <- enter_or_modify_measure()
#                review_and_save_measure(measure)
#              },
#              {
#                measures <- list_measures()
#                if (length(measures) > 0) {
#                  cat("Available measures:\n")
#                  for (i in seq_along(measures)) {
#                    cat(i, ". ", measures[i], "\n", sep="")
#                  }
#                  choice <- as.integer(get_input("Enter the number of the measure to delete: "))
#                  if (choice > 0 && choice <= length(measures)) {
#                    delete_measure(measures[choice])
#                  } else {
#                    cat("Invalid choice. Please try again.\n")
#                  }
#                } else {
#                  cat("No measures available to delete.\n")
#                }
#              },
#              {
#                measures <- list_measures()
#                if (length(measures) > 0) {
#                  cat("Available measures:\n")
#                  for (i in seq_along(measures)) {
#                    cat(i, ". ", measures[i], "\n", sep="")
#                  }
#                  choice <- as.integer(get_input("Enter the number of the measure to modify: "))
#                  if (choice > 0 && choice <= length(measures)) {
#                    measure <- enter_or_modify_measure(measure_data[[measures[choice]]])
#                    review_and_save_measure(measure, is_new = FALSE)
#                  } else {
#                    cat("Invalid choice. Please try again.\n")
#                  }
#                } else {
#                  cat("No measures available to modify.\n")
#                }
#              },
#              {
#                default_name <- if(is.null(current_file)) "measures_data.rds" else current_file
#                cat("\nSave options:\n")
#                cat("1. Use current file name:", default_name, "\n")
#                cat("2. Enter a new file name\n")
#                save_choice <- as.integer(get_input("Enter your choice: "))
#
#                if (save_choice == 1) {
#                  file_name <- default_name
#                } else if (save_choice == 2) {
#                  file_name <- get_input("Enter new file name (including .rds extension): ")
#                } else {
#                  cat("Invalid choice. Using default name.\n")
#                  file_name <- default_name
#                }
#
#                backup <- tolower(get_input("Do you want to create a backup? (y/n): ")) == "y"
#                save_data(file_name, backup)
#              },
#              {
#                stop("Exiting program", call. = FALSE)
#              },
#              {
#                cat("Invalid choice. Please try again.\n")
#              }
#       )
#     }
#   }
#
#   run_gui()
# }

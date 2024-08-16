#' Manage Boilerplate Measures
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
#' It modifies global variables and files on disk, so use with caution.
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
manager_boilerplate_measures <- function(measures_path = NULL) {
  if (is.null(measures_path)) {
    measures_path <- here::here()
  } else {
    measures_path <- here::here(measures_path)
  }

  measure_data <- list()
  current_file <- NULL

  get_input <- function(prompt, allow_empty = FALSE) {
    while (TRUE) {
      input <- readline(paste0(prompt, " (press 'q' to quit): "))
      if (tolower(input) == "q") {
        stop("Exiting program", call. = FALSE)
      }
      if (input != "" || allow_empty) return(input)
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
      measure_data <<- readRDS(file_path)
      current_file <<- file_name
      cat("Data loaded from:", file_path, "\n")
    } else {
      cat("File not found:", file_path, "\n")
      cat("Available files in", measures_path, ":\n")
      list_rds_files()
    }
  }

  save_data <- function(file_name, backup = FALSE) {
    file_path <- file.path(measures_path, file_name)

    if (file.exists(file_path)) {
      cat("Warning: You are about to overwrite an existing file.\n")
      confirm <- tolower(get_input("Do you want to continue? (y/n): "))
      if (confirm != "y") {
        cat("Save operation cancelled.\n")
        return()
      }
    }

    saveRDS(measure_data, file = file_path)
    current_file <<- file_name
    cat("Data saved as:", file_path, "\n")

    if (backup) {
      backup_file <- paste0(tools::file_path_sans_ext(file_name), "_backup.rds")
      backup_path <- file.path(measures_path, backup_file)
      saveRDS(measure_data, file = backup_path)
      cat("Backup saved as:", backup_path, "\n")
    }
  }

  enter_or_modify_measure <- function(existing_measure = NULL) {
    measure <- existing_measure %||% list()
    fields <- c("name", "items", "description", "reference", "waves", "keywords")

    for (field in fields) {
      current_value <- if (is.null(existing_measure)) "" else existing_measure[[field]]

      if (field == "items") {
        if (is.null(existing_measure)) {
          measure[[field]] <- list()
          cat("\nEnter items (press Enter without typing anything to finish):\n")
          cat("Example: \"How often do you have a drink containing alcohol?\"\n")
          item_num <- 1
          while (TRUE) {
            item <- get_input(paste("Item", item_num, "(or press enter to finish):"), allow_empty = TRUE)
            if (item == "") break
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
            cat(i, ". ", current_value[[i]], "\n", sep="")
          }
          modify <- tolower(get_input("Do you want to modify the items? (y/n): ")) == "y"
          if (modify) {
            measure[[field]] <- list()
            cat("Enter new items (press Enter without typing anything to finish):\n")
            item_num <- 1
            while (TRUE) {
              item <- get_input(paste("Item", item_num, "(or press enter to finish):"), allow_empty = TRUE)
              if (item == "") break
              measure[[field]][[item_num]] <- item
              item_num <- item_num + 1
            }
          } else {
            measure[[field]] <- current_value
          }
        }
      } else {
        example <- switch(field,
                          name = "Example: alcohol_frequency",
                          description = "Example: Frequency of alcohol consumption was measured using a single item...",
                          reference = "Example: nzavs2009",
                          waves = "Example: 1-current or 1-15",
                          keywords = "Example: alcohol, frequency, consumption")
        cat("\n", example, "\n", sep = "")
        cat("Current value:", current_value, "\n")
        new_value <- get_input(paste("Enter new", field, "(press enter to keep current):"), allow_empty = TRUE)
        measure[[field]] <- if (new_value == "") current_value else new_value
      }
    }

    return(measure)
  }

  review_and_save_measure <- function(measure, is_new = TRUE) {
    while (TRUE) {
      cat("\nReview your entries:\n")
      print(measure)
      cat("\nWhat would you like to do?\n")
      cat("1. Save measure\n")
      cat("2. Modify a field\n")
      cat("3. Start over\n")
      cat("4. Cancel\n")

      choice <- as.integer(get_input("Enter your choice: "))

      if (choice == 1) {
        if (is_new) {
          measure_data[[measure$name]] <<- measure
        } else {
          # Update the existing measure
          for (field in names(measure)) {
            measure_data[[measure$name]][[field]] <<- measure[[field]]
          }
        }
        # Sort measures alphabetically
        measure_data <<- measure_data[order(names(measure_data))]
        cat("Measure", measure$name, "saved successfully.\n")
        return(TRUE)
      } else if (choice == 2) {
        cat("\nWhich field would you like to modify?\n")
        fields <- c("name", "items", "description", "reference", "waves", "keywords")
        for (i in seq_along(fields)) {
          cat(i, ". ", fields[i], "\n", sep="")
        }
        field_choice <- as.integer(get_input("Enter the number of the field to modify: "))
        if (field_choice > 0 && field_choice <= length(fields)) {
          field <- fields[field_choice]
          measure <- enter_or_modify_measure(measure)
        } else {
          cat("Invalid choice. Please try again.\n")
        }
      } else if (choice == 3) {
        return(enter_or_modify_measure())
      } else if (choice == 4) {
        return(FALSE)
      } else {
        cat("Invalid choice. Please try again.\n")
      }
    }
  }

  list_measures <- function() {
    return(names(measure_data))
  }

  delete_measure <- function(name) {
    if (name %in% names(measure_data)) {
      measure_data[[name]] <<- NULL
      # Sort measures alphabetically after deletion
      measure_data <<- measure_data[order(names(measure_data))]
      cat("Measure", name, "deleted successfully.\n")
    } else {
      cat("Measure", name, "not found.\n")
    }
  }

  create_new_database <- function() {
    cat("\nCreating a new measures database\n")

    db_name <- get_input("Enter a name for the new database (without .rds extension): ")
    if (!grepl("\\.rds$", db_name)) {
      db_name <- paste0(db_name, ".rds")
    }

    full_path <- file.path(measures_path, db_name)
    cat("\nThe database will be created at:", full_path, "\n")
    confirm <- tolower(get_input("Is this correct? (y/n): "))

    if (confirm == "y") {
      measure_data <<- list()
      current_file <<- db_name
      cat("New measures database '", db_name, "' created.\n", sep = "")
      return(TRUE)
    } else {
      cat("Database creation cancelled.\n")
      return(FALSE)
    }
  }

  run_gui <- function() {
    cat("Welcome to the Boilerplate Measures Manager\n")

    while (TRUE) {
      cat("\nBoilerplate Measures Manager\n")
      cat("1. Create new measures database\n")
      cat("2. Open existing measures database\n")
      cat("3. List available .rds files\n")
      cat("4. Quit\n")

      choice <- as.integer(get_input("Enter your choice: "))

      if (choice == 1) {
        if (create_new_database()) {
          while (TRUE) {
            measure <- enter_or_modify_measure()
            if (review_and_save_measure(measure)) {
              continue <- tolower(get_input("Would you like to enter another measure? (y/n): ")) == "y"
              if (!continue) break
            }
          }
        }
      } else if (choice == 2) {
        files <- list_rds_files()
        if (!is.null(files)) {
          file_choice <- as.integer(get_input("Enter the number of the file you want to open: "))
          if (file_choice > 0 && file_choice <= length(files)) {
            file_name <- files[file_choice]
            load_data(file_name)
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
        stop("Exiting program", call. = FALSE)
      } else {
        cat("Invalid choice. Please try again.\n")
        next
      }

      if (choice == 1 || choice == 2) break
    }

    while (TRUE) {
      cat("\nMeasures Database Management\n")
      cat("1. List measures\n")
      cat("2. Add measure\n")
      cat("3. Delete measure\n")
      cat("4. Modify measure\n")
      cat("5. Save measures data\n")
      cat("6. Quit\n")

      choice <- as.integer(get_input("Enter your choice: "))

      switch(choice,
             {
               measures <- list_measures()
               if (length(measures) > 0) {
                 cat("Available measures:\n")
                 for (i in seq_along(measures)) {
                   cat(i, ". ", measures[i], "\n", sep="")
                 }
               } else {
                 cat("No measures available.\n")
               }
             },
             {
               measure <- enter_or_modify_measure()
               review_and_save_measure(measure)
             },
             {
               measures <- list_measures()
               if (length(measures) > 0) {
                 cat("Available measures:\n")
                 for (i in seq_along(measures)) {
                   cat(i, ". ", measures[i], "\n", sep="")
                 }
                 choice <- as.integer(get_input("Enter the number of the measure to delete: "))
                 if (choice > 0 && choice <= length(measures)) {
                   delete_measure(measures[choice])
                 } else {
                   cat("Invalid choice. Please try again.\n")
                 }
               } else {
                 cat("No measures available to delete.\n")
               }
             },
             {
               measures <- list_measures()
               if (length(measures) > 0) {
                 cat("Available measures:\n")
                 for (i in seq_along(measures)) {
                   cat(i, ". ", measures[i], "\n", sep="")
                 }
                 choice <- as.integer(get_input("Enter the number of the measure to modify: "))
                 if (choice > 0 && choice <= length(measures)) {
                   measure <- enter_or_modify_measure(measure_data[[measures[choice]]])
                   review_and_save_measure(measure, is_new = FALSE)
                 } else {
                   cat("Invalid choice. Please try again.\n")
                 }
               } else {
                 cat("No measures available to modify.\n")
               }
             },
             {
               default_name <- if(is.null(current_file)) "measures_data.rds" else current_file
               cat("\nSave options:\n")
               cat("1. Use current file name:", default_name, "\n")
               cat("2. Enter a new file name\n")
               save_choice <- as.integer(get_input("Enter your choice: "))

               if (save_choice == 1) {
                 file_name <- default_name
               } else if (save_choice == 2) {
                 file_name <- get_input("Enter new file name (including .rds extension): ")
               } else {
                 cat("Invalid choice. Using default name.\n")
                 file_name <- default_name
               }

               backup <- tolower(get_input("Do you want to create a backup? (y/n): ")) == "y"
               save_data(file_name, backup)
             },
             {
               stop("Exiting program", call. = FALSE)
             },
             {
               cat("Invalid choice. Please try again.\n")
             }
      )
    }
  }

  run_gui()
}

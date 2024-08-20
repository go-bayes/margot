#' Merge Two Measure Databases
#'
#' This function merges two measure databases, allowing the user to resolve conflicts
#' when the same measure exists in both databases with different content.
#'
#' @param db1 A list representing the first measure database.
#' @param db2 A list representing the second measure database.
#' @param db1_name Character string. The name of the first database (default: "Database 1").
#' @param db2_name Character string. The name of the second database (default: "Database 2").
#'
#' @return A list representing the merged measure database.
#'
#' @details
#' The function iterates through all measures in both databases. When a measure exists
#' in both databases:
#' \itemize{
#'   \item If the entries are identical, it keeps one copy.
#'   \item If the entries differ, it prompts the user to choose which entry to keep.
#' }
#' Measures that exist in only one database are automatically added to the merged database.
#'
#' @examples
#' \dontrun{
#' # Merge two databases with default names
#' merged_db <- margot_merge_databases(test_a, test_b)
#'
#' # Merge two databases with custom names
#' merged_db <- margot_merge_databases(test_a, test_b, "NZAVS 2009", "NZAVS 2020")
#' }
#'
#' @importFrom cli cli_h1 cli_h2 cli_text cli_code cli_progress_bar cli_progress_update
#' @importFrom cli cli_progress_done cli_alert_success cli_alert_info
#'
#' @export
margot_merge_databases <- function(db1, db2, db1_name = "Database 1", db2_name = "Database 2") {
  merged_db <- list()

  # Helper function to get user choice
  get_user_choice <- function(name, db1_entry, db2_entry) {
    cli::cli_h2("Conflict found for measure: {.val {name}}")
    cli::cli_text("Entry from {.strong {db1_name}}:")
    cli::cli_code(capture.output(print(db1_entry)))
    cli::cli_text("Entry from {.strong {db2_name}}:")
    cli::cli_code(capture.output(print(db2_entry)))

    prompt <- cli::cli_text("Which entry do you want to keep? ({.val 1} for {db1_name}, {.val 2} for {db2_name}): ")
    choice <- readline(prompt)
    while (!(choice %in% c("1", "2"))) {
      choice <- readline(cli::cli_text("Invalid input. Please enter {.val 1} or {.val 2}: "))
    }
    return(as.integer(choice))
  }

  # Start merging process
  cli::cli_h1("Starting database merge")
  cli::cli_progress_bar(total = length(unique(c(names(db1), names(db2)))),
                        format = "{cli::pb_spin} Merging databases... [{cli::pb_current}/{cli::pb_total}] [{cli::pb_percent}] [{cli::pb_bar}]")

  # Merge entries from both databases
  all_names <- unique(c(names(db1), names(db2)))
  for (name in all_names) {
    cli::cli_progress_update()

    if (name %in% names(db1) && name %in% names(db2)) {
      # Entry exists in both databases
      if (identical(db1[[name]], db2[[name]])) {
        merged_db[[name]] <- db1[[name]]
        cli::cli_alert_success("Measure {.val {name}} is identical in both databases. Keeping it.")
      } else {
        choice <- get_user_choice(name, db1[[name]], db2[[name]])
        merged_db[[name]] <- if (choice == 1) db1[[name]] else db2[[name]]
        cli::cli_alert_info("Kept entry from {.strong {if(choice == 1) db1_name else db2_name}} for measure {.val {name}}")
      }
    } else if (name %in% names(db1)) {
      # Entry only in database 1
      merged_db[[name]] <- db1[[name]]
      cli::cli_alert_info("Measure {.val {name}} only found in {.strong {db1_name}}. Adding it to merged database.")
    } else {
      # Entry only in database 2
      merged_db[[name]] <- db2[[name]]
      cli::cli_alert_info("Measure {.val {name}} only found in {.strong {db2_name}}. Adding it to merged database.")
    }
  }

  cli::cli_progress_done()
  cli::cli_alert_success("Merge completed. Total measures in merged database: {.val {length(merged_db)}}")

  return(merged_db)
}
# margot_merge_databases <- function(db1, db2, db1_name = "Database 1", db2_name = "Database 2") {
#   merged_db <- list()
#
#   # Helper function to get user choice
#   get_user_choice <- function(name, db1_entry, db2_entry) {
#     cat("\nConflict found for measure:", name, "\n")
#     cat("Entry from", db1_name, ":\n")
#     print(db1_entry)
#     cat("\nEntry from", db2_name, ":\n")
#     print(db2_entry)
#
#     prompt <- sprintf("Which entry do you want to keep? (1 for %s, 2 for %s): ", db1_name, db2_name)
#     choice <- readline(prompt = prompt)
#     while (!(choice %in% c("1", "2"))) {
#       choice <- readline(prompt = "Invalid input. Please enter 1 or 2: ")
#     }
#     return(as.integer(choice))
#   }
#
#   # Merge entries from both databases
#   all_names <- unique(c(names(db1), names(db2)))
#
#   for (name in all_names) {
#     if (name %in% names(db1) && name %in% names(db2)) {
#       # Entry exists in both databases
#       if (identical(db1[[name]], db2[[name]])) {
#         merged_db[[name]] <- db1[[name]]
#         cat("Measure", name, "is identical in both databases. Keeping it.\n")
#       } else {
#         choice <- get_user_choice(name, db1[[name]], db2[[name]])
#         merged_db[[name]] <- if (choice == 1) db1[[name]] else db2[[name]]
#         cat("Kept entry from", if(choice == 1) db1_name else db2_name, "for measure", name, "\n")
#       }
#     } else if (name %in% names(db1)) {
#       # Entry only in database 1
#       merged_db[[name]] <- db1[[name]]
#       cat("Measure", name, "only found in", db1_name, ". Adding it to merged database.\n")
#     } else {
#       # Entry only in database 2
#       merged_db[[name]] <- db2[[name]]
#       cat("Measure", name, "only found in", db2_name, ". Adding it to merged database.\n")
#     }
#   }
#
#   cat("\nMerge completed. Total measures in merged database:", length(merged_db), "\n")
#   return(merged_db)
# }

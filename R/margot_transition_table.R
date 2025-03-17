#' Format a Transition Table
#'
#' Reshapes a long-format transition frequency data frame into a wide-format table with totals,
#' and formats it using markdown. Diagonal elements are bolded to highlight state stability.
#'
#' @param trans_df A data frame with columns \code{from}, \code{to}, and \code{Freq} indicating transitions.
#' @param state_names Optional vector of state names to replace the default numeric state labels.
#'                    If \code{NULL}, state labels are preserved.
#' @param wave_info Optional string specifying the wave transition information to include as a header.
#' @param table_name Optional string specifying the name for the transition table output.
#'                   Default is \code{"transition_table"}.
#'
#' @return A list with components:
#'         \item{table}{The markdown-formatted transition table.}
#'         \item{table_with_header}{The transition table with a header including wave information (if provided).}
#'         \item{wave_info}{The provided wave transition information (if any).}
#'         \item{table_name}{The name provided for the transition table output.}
#'
#' @export
margot_transition_table <- function(data, state_var, id_var, wave_var,
                                    waves = NULL, state_names = NULL,
                                    table_name = "transition_table") {
  if (is.null(waves)) {
    waves <- sort(unique(data[[wave_var]]))
  }
  waves <- sort(waves)

  results <- list(tables = list(), waves = list(), table_name = table_name)

  results$explanation <- paste0(
    "These transition matrices capture shifts in states between consecutive waves. ",
    "Each cell represents the count of individuals transitioning from one state to another. ",
    "The rows correspond to the initial state (From), and the columns correspond to the subsequent ",
    "state (To). **Diagonal entries** (in **bold**) correspond to individuals who remained in ",
    "the same state. **Off-diagonal entries** correspond to individuals who transitioned to a different state.\n\n",
    "A higher number on the diagonal relative to off-diagonal entries indicates greater stability in a state. ",
    "Conversely, higher off-diagonal numbers suggest more frequent shifts between states."
  )

  results$wave_titles <- function() {
    sapply(results$waves, function(pair) {
      paste0("Wave ", pair[1], " → Wave ", pair[2])
    })
  }

  for (i in 1:(length(waves) - 1)) {
    wave1 <- waves[i]
    wave2 <- waves[i + 1]

    wave_pair_data <- data[data[[wave_var]] %in% c(wave1, wave2), ]
    temp_data <- data.frame(
      id = wave_pair_data[[id_var]],
      wave = wave_pair_data[[wave_var]],
      state = wave_pair_data[[state_var]]
    )

    wide_data <- reshape(
      temp_data,
      idvar = "id",
      timevar = "wave",
      direction = "wide"
    )

    from_col <- paste0("state.", wave1)
    to_col <- paste0("state.", wave2)

    valid_rows <- !is.na(wide_data[[from_col]]) & !is.na(wide_data[[to_col]])
    from_states <- wide_data[valid_rows, from_col]
    to_states <- wide_data[valid_rows, to_col]

    transition_data <- data.frame(from = from_states, to = to_states)
    transition_matrix <- table(from = from_states, to = to_states)

    all_states <- sort(unique(c(from_states, to_states)))
    wave_state_names <- if (is.null(state_names)) paste0("State ", all_states) else state_names

    trans_df <- as.data.frame.matrix(transition_matrix)
    trans_df$from <- rownames(trans_df)
    long_trans_df <- tidyr::pivot_longer(trans_df, cols = -from, names_to = "to", values_to = "Freq")

    wave_info <- paste0("Wave ", wave1, " → Wave ", wave2)

    table_result <- transition_table(long_trans_df, wave_state_names,
                                     wave_info = wave_info,
                                     table_name = table_name)

    results$tables[[i]] <- table_result$table
    results$waves[[i]] <- c(wave1, wave2)
  }

  # Updated quarto_code function using the provided table name
  results$quarto_code <- function() {
    code <- paste0("```{r, results='asis'}\ncat(", results$table_name, "$explanation)\n```\n\n")

    for (i in seq_along(results$tables)) {
      wave1 <- results$waves[[i]][1]
      wave2 <- results$waves[[i]][2]

      table_code <- paste0(
        "```{r}\n",
        "#| label: ", results$table_name, "-wave", wave1, "-wave", wave2, "\n",
        "#| tbl-cap: \"Transition Matrix From Wave ", wave1, " to Wave ", wave2, "\"\n",
        results$table_name, "$tables[[", i, "]]\n",
        "```\n\n"
      )
      code <- paste0(code, table_code)
    }

    cat(code)
  }

  class(results) <- c("margot_transitions", class(results))
  return(results)
}

# helper
transition_table <- function(trans_df, state_names = NULL, wave_info = NULL,
                             table_name = "transition_table") {
  required_cols <- c("from", "to", "Freq")
  if (!all(required_cols %in% colnames(trans_df))) {
    stop("trans_df must contain columns: from, to, and Freq")
  }

  trans_wide <- tidyr::pivot_wider(
    trans_df,
    id_cols = "from",
    names_from = "to",
    values_from = "Freq",
    values_fill = 0
  )
  trans_wide$Total <- rowSums(trans_wide[, -1], na.rm = TRUE)
  colnames(trans_wide)[1] <- "From / To"

  if (!is.null(state_names)) {
    all_states <- unique(c(trans_df$from, trans_df$to))
    state_mapping <- setNames(state_names, all_states)
    trans_wide[["From / To"]] <- state_mapping[as.character(trans_wide[["From / To"]])]

    old_cols <- colnames(trans_wide)
    for (i in 2:(length(old_cols) - 1)) {
      old_state <- old_cols[i]
      if (old_state %in% names(state_mapping)) {
        colnames(trans_wide)[i] <- state_mapping[old_state]
      }
    }
  }

  formatted_table <- knitr::kable(trans_wide, format = "markdown")

  for (i in 1:nrow(trans_wide)) {
    from_state <- trans_wide[i, "From / To"]
    to_col_idx <- which(colnames(trans_wide) == from_state)
    if (length(to_col_idx) > 0) {
      value <- trans_wide[i, to_col_idx]
      if (is.numeric(value)) {
        pattern <- paste0("\\|\\s*", value, "\\s*\\|")
        replacement <- paste0("| **", value, "** |")
        formatted_table <- gsub(pattern, replacement, formatted_table)
      }
    }
  }

  if (!is.null(wave_info)) {
    attr(formatted_table, "wave_info") <- wave_info
    wave_header <- paste0("### ", wave_info, "\n\n")
    formatted_table_with_header <- paste0(wave_header, formatted_table)

    return(list(
      table = formatted_table,
      table_with_header = formatted_table_with_header,
      wave_info = wave_info,
      table_name = table_name
    ))
  } else {
    return(list(
      table = formatted_table,
      table_name = table_name
    ))
  }
}

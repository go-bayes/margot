#' Sequential Transition Tables
#'
#' Generates transition tables for sequential waves of data, showing how entities move between
#' states over time. The function produces a series of tables (wave 1 to wave 2, wave 2 to wave 3, etc.)
#' with formatted markdown output highlighting state stability and transitions.
#'
#' @param data A data frame containing the observations.
#' @param state_var The name of the state variable column in `data` as a string.
#'                  This variable tracks the state changes to be analyzed.
#' @param id_var The name of the identifier variable column in `data` as a string.
#'              This variable distinguishes between different subjects or entities.
#' @param wave_var The name of the wave/time variable column in `data` as a string.
#' @param waves Optional vector of wave values to analyze. If NULL, all unique waves in the data are used.
#' @param state_names Optional vector of state names to replace the default state labels.
#'                   If NULL, states will be labeled as "State 1", "State 2", etc.
#'
#' @return A list with the following components:
#'         - `tables`: A list where each element corresponds to a pair of consecutive waves,
#'                    containing the markdown-formatted transition table
#'         - `waves`: A list of wave pairs that were compared
#'         - `explanation`: A single explanation that can be used for all tables
#'
#' @examples
#' \dontrun{
#' df <- read.table(header=TRUE, text="
#' id wave year_measured religion_believe_god
#' 3 0 1 0
#' 3 1 1 1
#' 3 2 1 0
#' 4 0 1 0
#' 4 1 1 1
#' 4 2 1 1
#' 5 0 1 1
#' 5 1 1 0
#' 5 2 1 1")
#'
#' # Get sequential transition tables between all waves
#' transition_tables <- margot_transition_table(df, "religion_believe_god", "id", "wave")
#'
#' # In a standard R session:
#' cat(transition_tables$explanation)
#' cat("\n\n")
#' # Display first transition table (wave 0 to 1)
#' print(transition_tables$tables[[1]])
#'
#' # In a Quarto document:
#' # ```{r, results='asis'}
#' # cat(transition_tables$explanation)
#' # ```
#' #
#' # ```{r}
#' # #| label: tbl-transition-wave0-wave1
#' # #| tbl-cap: "Transition Matrix From Wave 0 to Wave 1"
#' # transition_tables$tables[[1]]
#' # ```
#' }
#'
#' @importFrom dplyr filter arrange
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom knitr kable
#' @export
#'
#' @section Quarto Usage:
#' To use in a Quarto document, you can include the tables with custom captions:
#'
#' ```
#' # Get explanation
#' ```{r, results='asis'}
#' cat(transition_tables$explanation)
#' ```
#'
#' # First transition table
#' ```{r}
#' #| label: tbl-wave0-1
#' #| tbl-cap: !expr paste0("Transitions from Wave ", transition_tables$waves[[1]][1], " to Wave ", transition_tables$waves[[1]][2])
#' transition_tables$tables[[1]]
#' ```
#' ```
#'
#' @export
margot_transition_table <- function(data, state_var, id_var, wave_var, waves = NULL, state_names = NULL) {
  # if waves not specified, use all unique waves in sorted order
  if (is.null(waves)) {
    waves <- sort(unique(data[[wave_var]]))
  }

  # ensure waves are in the correct order
  waves <- sort(waves)

  # initialize list to store results
  results <- list(tables = list(), waves = list())

  # create a single generic explanation
  results$explanation <- paste0(
    "These transition matrices capture shifts in states between consecutive waves. ",
    "Each cell represents the count of individuals transitioning from one state to another. ",
    "The rows correspond to the initial state (From), and the columns correspond to the subsequent ",
    "state (To). **Diagonal entries** (in **bold**) correspond to individuals who remained in ",
    "the same state. **Off-diagonal entries** correspond to individuals who transitioned to a different state.\n\n",
    "A higher number on the diagonal relative to off-diagonal entries indicates greater stability in a state. ",
    "Conversely, higher off-diagonal numbers suggest more frequent shifts between states."
  )

  # add a helper function to generate wave transition titles
  results$wave_titles <- function() {
    sapply(results$waves, function(pair) {
      paste0("Wave ", pair[1], " → Wave ", pair[2])
    })
  }

  # for each consecutive pair of waves
  for (i in 1:(length(waves)-1)) {
    wave1 <- waves[i]
    wave2 <- waves[i+1]

    # subset data for the current pair of waves
    wave_pair_data <- data[data[[wave_var]] %in% c(wave1, wave2), ]

    # create a temporary data frame with just the needed columns
    temp_data <- data.frame(
      id = wave_pair_data[[id_var]],
      wave = wave_pair_data[[wave_var]],
      state = wave_pair_data[[state_var]]
    )

    # create wide format with one row per ID and columns for each wave's state
    wide_data <- reshape(
      temp_data,
      idvar = "id",
      timevar = "wave",
      direction = "wide"
    )

    # create column names based on the actual wave values
    from_col <- paste0("state.", wave1)
    to_col <- paste0("state.", wave2)

    # extract values for transition matrix
    valid_rows <- !is.na(wide_data[[from_col]]) & !is.na(wide_data[[to_col]])
    from_states <- wide_data[valid_rows, from_col]
    to_states <- wide_data[valid_rows, to_col]

    # create the transition matrix
    transition_data <- data.frame(
      from = from_states,
      to = to_states
    )

    # get frequency counts of transitions
    transition_counts <- as.data.frame(table(transition_data))

    # use the transition_table function to format the output
    all_states <- sort(unique(c(from_states, to_states)))

    # create a transition matrix
    transition_matrix <- table(from = from_states, to = to_states)

    # if state names are provided, use them
    wave_state_names <- state_names
    if (is.null(wave_state_names)) {
      wave_state_names <- paste0("State ", all_states)
    }

    # convert transition matrix to long format for transition_table function
    trans_df <- as.data.frame.matrix(transition_matrix)
    trans_df$from <- rownames(trans_df)
    long_trans_df <- tidyr::pivot_longer(trans_df, cols = -from, names_to = "to", values_to = "Freq")

    # include wave transition information in the data passed to transition_table
    wave_info <- paste0("Wave ", wave1, " → Wave ", wave2)

    # generate formatted table using transition_table function
    # pass wave information to the transition_table function
    table_result <- transition_table(long_trans_df, wave_state_names, wave_info = wave_info)

    # store just the table and wave pair
    results$tables[[i]] <- table_result$table
    results$waves[[i]] <- c(wave1, wave2)
  }

  # add a print method to simplify output in interactive sessions
  class(results) <- c("margot_transitions", class(results))

  # add helper method to generate quarto code
  results$quarto_code <- function() {
    code <- "```{r, results='asis'}\ncat(transition_tables$explanation)\n```\n\n"

    for (i in seq_along(results$tables)) {
      wave1 <- results$waves[[i]][1]
      wave2 <- results$waves[[i]][2]

      table_code <- paste0(
        "```{r}\n",
        "#| label: tbl-transition-wave", wave1, "-wave", wave2, "\n",
        "#| tbl-cap: \"Transition Matrix From Wave ", wave1, " to Wave ", wave2, "\"\n",
        "transition_tables$tables[[", i, "]]\n",
        "```\n\n"
      )

      code <- paste0(code, table_code)
    }

    cat(code)
  }

  return(results)
}

# Modified transition_table function to include wave information
transition_table <- function(trans_df, state_names = NULL, wave_info = NULL) {
  # make sure trans_df has the required columns
  required_cols <- c("from", "to", "Freq")
  if (!all(required_cols %in% colnames(trans_df))) {
    stop("trans_df must contain columns: from, to, and Freq")
  }

  # reshape data into a wide format for the transition table
  trans_wide <- tidyr::pivot_wider(
    trans_df,
    id_cols = "from",
    names_from = "to",
    values_from = "Freq",
    values_fill = 0
  )

  # compute row totals
  trans_wide$Total <- rowSums(trans_wide[, -1], na.rm = TRUE)

  # rename "from" column to indicate it's the source state
  colnames(trans_wide)[1] <- "From / To"

  # if state names are provided, replace numeric states with names
  if (!is.null(state_names)) {
    # get unique states from both from and to
    all_states <- unique(c(trans_df$from, trans_df$to))

    # create mapping from states to state names
    state_mapping <- setNames(state_names, all_states)

    # apply mapping to row names (from states)
    trans_wide[["From / To"]] <- state_mapping[as.character(trans_wide[["From / To"]])]

    # apply mapping to column names (to states)
    old_cols <- colnames(trans_wide)
    for (i in 2:(length(old_cols)-1)) {  # skip first (From / To) and last (Total) columns
      old_state <- old_cols[i]
      if (old_state %in% names(state_mapping)) {
        colnames(trans_wide)[i] <- state_mapping[old_state]
      }
    }
  }

  # format the table with knitr::kable
  formatted_table <- knitr::kable(trans_wide, format = "markdown")

  # bold the diagonal elements (where from = to)
  for (i in 1:nrow(trans_wide)) {
    from_state <- trans_wide[i, "From / To"]

    # find the corresponding column index for this state
    to_col_idx <- which(colnames(trans_wide) == from_state)

    if (length(to_col_idx) > 0) {
      # construct pattern to find and replace with bold
      value <- trans_wide[i, to_col_idx]
      if (is.numeric(value)) {
        pattern <- paste0("\\|\\s*", value, "\\s*\\|")
        replacement <- paste0("| **", value, "** |")
        formatted_table <- gsub(pattern, replacement, formatted_table)
      }
    }
  }

  # add wave information as a caption/header if provided
  if (!is.null(wave_info)) {
    # add wave info as an attribute
    attr(formatted_table, "wave_info") <- wave_info

    # create a version of the table with wave info included as header
    wave_header <- paste0("### ", wave_info, "\n\n")
    formatted_table_with_header <- paste0(wave_header, formatted_table)

    return(list(
      table = formatted_table,
      table_with_header = formatted_table_with_header,
      wave_info = wave_info
    ))
  } else {
    return(list(
      table = formatted_table
    ))
  }
}

#' Transition Table
#'
#' Generates a transition table that describes movements and stability between states
#' from one observation to the next. It formats the output as a markdown table, highlighting
#' the number of entities remaining in the same state (diagonal) and those transitioning
#' to different states (off-diagonal).
#'
#' @param data A data frame with columns `from` and `to` indicating the initial and subsequent
#' states of entities, respectively, and a `Freq` column indicating the frequency of transitions.
#' @param state_names Optional; a vector of state names to replace the default state labels.
#' If NULL, states will be labeled as "State 1", "State 2", etc., based on the unique values
#' in `from` and `to` columns.
#'
#' @return A list with two elements: `explanation`, a character string explaining the table,
#' and `table`, a markdown-formatted table of transitions. The diagonal entries (in bold)
#' represent the count of entities that remained in their initial state, while the off-diagonal
#' entries show the count of transitions between different states.
#'
#' @examples
#' \dontrun{
#' df <- read.table(header=TRUE, text="
#' id wave year_measured religion_believe_god
#' 3 0 1 0
#' 3 1 1 1
#' 4 0 1 0
#' 4 1 1 1
#' 5 0 1 1
#' 5 1 1 0")
#'
#' transition_matrix <- create_transition_matrix(df, "religion_believe_god", "id")
#' # Assuming `transition_matrix` is a table with the transition counts between states
#' # First, convert `transition_matrix` to a dataframe suitable for `transition_table`
#' df_transition <- as.data.frame.matrix(transition_matrix)
#' df_transition$from <- rownames(df_transition)
#' long_df_transition <- tidyr::pivot_longer(df_transition, cols = -from, names_to = "to", values_to = "Freq")
#'
#' transition_table_data <- transition_table(long_df_transition)
#' cat(transition_table_data$explanation)
#' cat("\n")
#' print(transition_table_data$table)
#' }
#' @importFrom dplyr mutate arrange
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom knitr kable
#' @export
transition_table <- function(data, state_names = NULL) {
  # ensure the data is a dataframe
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  # check if state names are provided
  if (is.null(state_names)) {
    state_names <- paste0("State ", sort(unique(c(data$from, data$to))))
  }

  # convert the data frame to a wide format and then to characters
  df <- data %>%
    tidyr::pivot_wider(names_from = to, values_from = Freq, values_fill = list(Freq = 0)) |>
    dplyr::mutate(from = factor(from, levels = sort(unique(from)))) %>%
    dplyr::arrange(from) |>
    dplyr::mutate(from = state_names[from]) |>
    setNames(c("From", state_names)) |>
    dplyr::mutate(across(everything(), as.character)) # Convert all columns to character

  # apply bold formatting to the diagonal
  for (i in 1:nrow(df)) {
    df[i, i + 1] <- paste0("**", df[i, i + 1], "**") # Adjust for 'From' being the first column
  }

  # convert to markdown table directly, handling characters
  markdown_table <- knitr::kable(df, format = "markdown", align = 'c', escape = FALSE)

  # explanation
  explanation <- "This transition matrix captures shifts in states across across the treatment intervals. Each cell in the matrix represents the count of individuals transitioning from one state to another. The rows correspond to the treatment at baseline (From), and the columns correspond to the state at the following wave (To). **Diagonal entries** (in **bold**) correspond to the number of individuals who remained in their initial state across both waves. **Off-diagonal entries** correspond to the transitions of individuals from their baseline state to a different state in the treatment wave.
A higher number on the diagonal relative to the off-diagonal entries in the same row indicates greater stability in a state. Conversely, higher off-diagonal numbers suggest more frequent shifts from the baseline state to other states."

  list(explanation = explanation, table = markdown_table)
}

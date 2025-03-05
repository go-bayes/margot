#' Create transition matrix for state transitions
#'
#' To satisify the positivity assumption of causal inference, we must ensure that the exposure occurs in the data. This function computes a transition matrix for a given state variable across subjects, tracking changes between consecutive observations. The function handles both numeric and factor state variables, excluding NA values in the transition count.
#'
#' @param data A data frame containing the observations.
#' @param state_var The name of the state variable column in `data` as a string. This variable tracks the state changes to be analyzed.
#' @param id_var The name of the identifier variable column in `data` as a string. This variable distinguishes between different subjects or entities.
#'
#' @return A matrix indicating the number of transitions between states. The rows represent the initial state ('from'), and the columns represent the subsequent state ('to'). Diagonal entries indicate the number of times the state did not change, while off-diagonal entries indicate transitions from one state to another.
#'
#' @examples
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
#' print(transition_matrix)
#'
#' @note This function explicitly excludes NA values from the transition matrix calculation. It treats numeric state variables by converting them to factors, ensuring a consistent analysis approach for both numeric and factor types.
#' 
#' @export
create_transition_matrix <- function(data, state_var, id_var) {
  # function body as provided
}

# function to generate a square matrix of state transitions, excluding NAs, for numeric or factor state variables
create_transition_matrix <- function(data, state_var, id_var) {
  # ensure data is ordered by id and then time/wave for proper transition tracking
  data <- data[order(data[[id_var]], data$wave), ]

  # calculate lagged states to identify transitions, ensuring alignment within each subject
  data$prev_state <- ave(data[[state_var]], data[[id_var]], FUN = function(x) c(NA, head(x, -1)))

  # remove rows with NA in the current or previous state to avoid misalignment
  valid_rows <- !is.na(data$prev_state) & !is.na(data[[state_var]])
  data <- data[valid_rows, ]

  # convert states to factors if numeric, ensuring all states are represented
  if(is.numeric(data[[state_var]])) {
    all_states <- sort(unique(c(data[[state_var]], data$prev_state)))
    data[[state_var]] <- factor(data[[state_var]], levels = all_states)
    data$prev_state <- factor(data$prev_state, levels = all_states)
  } else {
    # ensure consistency of factor levels for previous and current states
    levels_union <- union(levels(factor(data[[state_var]])), levels(factor(data$prev_state)))
    data[[state_var]] <- factor(data[[state_var]], levels = levels_union)
    data$prev_state <- factor(data$prev_state, levels = levels_union)
  }

  # build the transition matrix
  transition_matrix <- table(from = data$prev_state, to = data[[state_var]])

  return(transition_matrix)
}

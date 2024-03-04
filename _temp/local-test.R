library(testthat)
library(margot)
library(tidyr)
library(dplyr)
library(mice)

# common macros
devtools::install(".")
library(pkgdown)
pkgdown::build_site()


# Defining variables as per your function's documentation
baseline_vars <- c(
  "male",
  "age",
  "eth_cat",
  "partner",
  "agreeableness",
  "conscientiousness",
  "extraversion",
  "honesty_humility",
  "openness",
  "neuroticism",
  "sample_weights"
)

exposure_var <- c("forgiveness")

outcome_vars <- c("alcohol_frequency",
                  "alcohol_intensity",
                  "hlth_bmi",
                  "hours_exercise")

# apply the function
wide_data <-
  margot_wide(df_nz, baseline_vars, exposure_var, outcome_vars)

str(wide_data)
head(wide_data)
naniar::vis_miss(wide_data_2)


baseline_vars <- c(
  "male",
  "age",
  "eth_cat",
  "partner",
  "agreeableness",
  "conscientiousness",
  "extraversion",
  "honesty_humility",
  "openness",
  "neuroticism",
  "sample_weights"
)

exposure_var <- c("forgiveness")

outcome_vars <- c("alcohol_frequency",
                  "alcohol_intensity",
                  "hlth_bmi",
                  "hours_exercise")

wide_data_2 <-
  margot_wide_impute_baseline(df_nz, baseline_vars, exposure_var, outcome_vars)


devtools::document()





library(dplyr)
## Make data for transition table
df_temp <- df_nz |>
   select(id, wave, year_measured, religion_believe_god) |>
  filter(wave %in% c(2018, 2019) & year_measured == 1 & !is.na(religion_believe_god))


id_count <- df_temp |>
  group_by(id) |>
  summarise(n_unique_waves = n_distinct(wave), .groups = 'drop') |>
  filter(n_unique_waves == 2)

valid_ids_1 <- id_count$id

dat_filtered <- df_temp |>
  filter(id %in% valid_ids_1 & wave %in% c(2018, 2019)) |>
  mutate(wave = as.numeric (wave)-1)

head(dat_filtered)
skimr::n_unique(dat_filtered$id)

length(unique(dat_filtered$id))

str(dat_filtered)


library(msm)
library(margot)
out_0 <- create_transition_matrix(dat_filtered, "religion_believe_god", "id")

out <-
  msm::statetable.msm(religion_believe_god, id, data = dat_filtered)

out

str(out)
out_0

transition_table(out_0)

library(dplyr)
library(tidyr)
library(knitr)

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
    pivot_wider(names_from = to, values_from = Freq, values_fill = list(Freq = 0)) %>%
    mutate(from = factor(from, levels = sort(unique(from)))) %>%
    arrange(from) %>%
    mutate(from = state_names[from]) %>%
    setNames(c("From", state_names)) %>%
    mutate(across(everything(), as.character)) # Convert all columns to character

  # apply bold formatting to the diagonal
  for (i in 1:nrow(df)) {
    df[i, i + 1] <- paste0("**", df[i, i + 1], "**") # Adjust for 'From' being the first column
  }

  # convert to markdown table directly, handling characters
  markdown_table <- kable(df, format = "markdown", align = 'c', escape = FALSE)

  # explanation
  explanation <- "The table presents a transition matrix to evaluate shifts in the treatment between the baseline wave and the treatment wave. Entries along the diagonal (in bold) indicate the number of individuals who **stayed** in their initial state. By contrast, the off-diagonal shows the transitions from the initial state (bold) to another state in the following wave (off diagnal). Thus cell located at the intersection of row $i$ and column $j$, where $i \neq j$, gives us the counts of individuals moving from state $i$ to state $j$."

  list(explanation = explanation, table = markdown_table)
}



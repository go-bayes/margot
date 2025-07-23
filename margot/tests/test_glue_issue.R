# test glue issue

library(margot)
library(dplyr)

# test data after group_tab
df <- margot:::group_tab(data.frame(
  ATE = c(0.5),
  `2.5 %` = c(0.2),
  `97.5 %` = c(0.8),
  E_Value = c(2.5),
  E_Val_bound = c(1.8),
  check.names = FALSE,
  row.names = "Outcome1"
), type = "RD")

# filter to get df_f
df_f <- df %>% filter(E_Value > 1, E_Val_bound > 1)

# try the glue operation
effect_col <- "ATE"

result <- df_f %>%
  rowwise() %>%
  mutate(
    lab = glue::glue(
      "{round(.data[[effect_col]], 3)}(",
      "{round(`2.5 %`, 3)},",
      "{round(`97.5 %`, 3)})"
    ),
    text = glue::glue(
      "- {outcome}: {lab}",
      " E-value bound = {E_Val_bound}"
    )
  )

print(result)
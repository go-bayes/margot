library(testthat)
library(dplyr)
library(tidyr)
library(knitr)
library(margot)

test_that("transition_table generates correct markdown table and explanation", {
  # Sample data for testing
  df <- read.table(header=TRUE, text="
  id wave year_measured religion_believe_god
  3 0 1 0
  3 1 1 1
  4 0 1 0
  4 1 1 1
  5 0 1 1
  5 1 1 0")

  transition_matrix <- margot::create_transition_matrix(df, "religion_believe_god", "id")
  # convert the transition matrix to a data frame suitable for transition_table
  df_transition <- as.data.frame.matrix(transition_matrix)
  df_transition$from <- rownames(df_transition)
  long_df_transition <- tidyr::pivot_longer(df_transition, cols = -from, names_to = "to", values_to = "Freq")

  # call transition_table and capture output
  output <- margot::transition_table(long_df_transition)

  # test if the output is a list with two elements
  expect_true(is.list(output))
  expect_equal(length(output), 2)

  # test if the explanation is a non-empty string
  expect_true(is.character(output$explanation))
  expect_true(nchar(output$explanation) > 0)

  # test if the table is a non-empty markdown string
  # correct the check for non-empty markdown table
  total_chars <- sum(nchar(output$table))
  expect_true(total_chars > 0)
})


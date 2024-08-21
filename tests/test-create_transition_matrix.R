library(testthat)
library(margot)
df <- read.table(header = TRUE, text = "
id wave year_measured religion_believe_god
3 0 1 0
3 1 1 1
4 0 1 0
4 1 1 1
5 0 1 1
5 1 1 0")



# test: numeric states are correctly handled
test_that("create_transition_matrix handles numeric states", {
  # use the original df since it already contains numeric states
  result_matrix <- margot::create_transition_matrix(df, "religion_believe_god", "id")
  expect_true(is.matrix(result_matrix))
  expect_equal(dim(result_matrix), c(2, 2))
})

# add more tests for edge cases & etc

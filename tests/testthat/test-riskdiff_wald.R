# test case 1

test_that("check if the function works as expected", {
  evts <- 4
  non_evts <- 6
  control_evts <- 3
  cne <- 8

  expected_output <- (evts / (evts + non_evts)) - (control_evts / (control_evts + cne))
  actual <- riskdiff_wald(matrix(c(evts, control_evts, non_evts, cne), nrow = 2))
  actual_output <- actual$measure[2, 1]

  expect_equal(expected_output, actual_output)
})

# test case 2

test_that("check for error if `y` argument is not NULL", {
  evts <- 4
  non_evts <- 6
  control_evts <- 3
  cne <- 8
  input <- matrix(c(evts, control_evts, non_evts, cne), nrow = 2)

  expect_error(
    riskdiff_wald(
      x = input,
      y = 2,
      conf.level = 0.95,
      rev = c("neither", "rows", "columns", "both"),
      correction = FALSE,
      verbose = FALSE
    ),
    regexp = paste("y argument should be NULL")
  )
})

data(adae)

test_that("interval plot works with expected inputs", {
  out <- interval_plot(
    datain = adae,
    startvar = "ASTDY",
    endvar = "AENDY",
    yvar = "AETERM",
    seriesvar = "AESEV",
    subjectid = "01-701-1302",
    series_color = NA,
    yaxislab = "Reported Term for the Adverse Event"
  )

  complete_stat <- out$rpt_data %>%
    filter(Status == "Complete")

  expect_true(nrow(out$rpt_data) > 0)
  expect_equal(unique(out$rpt_data$USUBJID), "01-701-1302")
  expect_true(all(!is.na(complete_stat$ASTDY)))
  expect_true(all(!is.na(complete_stat$AENDY)))
})

test_that("interval plot works with expected inputs and different subject", {
  out <- interval_plot(
    datain = adae,
    startvar = "ASTDY",
    endvar = "AENDY",
    yvar = "AETERM",
    seriesvar = "AESEV",
    subjectid = "01-701-1275",
    series_color = NA,
    yaxislab = "Reported Term for the Adverse Event"
  )

  complete_stat <- out$rpt_data %>%
    filter(Status == "Complete")

  expect_true(nrow(out$rpt_data) > 0)
  expect_equal(unique(out$rpt_data$USUBJID), "01-701-1275")
  expect_true(all(!is.na(complete_stat$ASTDY)))
  expect_true(all(!is.na(complete_stat$AENDY)))
})

test_that("interval plot works with non default inputs", {
  out <- interval_plot(
    datain = adae,
    startvar = "ASTDY",
    endvar = "AENDY",
    yvar = "AEDECOD",
    seriesvar = "AESEV",
    subjectid = "01-703-1335",
    series_color = NA,
    yaxislab = "Reported Term for the Adverse Event"
  )

  complete_stat <- out$rpt_data %>%
    filter(Status == "Complete")

  expect_true(nrow(out$rpt_data) > 0)
  expect_equal(unique(out$rpt_data$USUBJID), "01-703-1335")
  expect_true("AEDECOD" %in% names(out$rpt_data))
  expect_true(all(!is.na(complete_stat$ASTDY)))
  expect_true(all(!is.na(complete_stat$AENDY)))
})

test_that("interval plot returns empty plot when subject not present", {
  out <- interval_plot(
    datain = adae,
    startvar = "ASTDY",
    endvar = "AENDY",
    yvar = "AETERM",
    seriesvar = "AESEV",
    subjectid = "01-701-13021",
    series_color = NA,
    yaxislab = "Reported Term for the Adverse Event"
  )

  expect_equal(nrow(out$rpt_data), 0)
})

test_that("interval plot works when seriesvar is NA or not present", {
  out <- interval_plot(
    datain = adae,
    startvar = "ASTDY",
    endvar = "AENDY",
    yvar = "AETERM",
    seriesvar = "AESEV1",
    subjectid = "01-701-1302",
    series_color = NA,
    yaxislab = "Reported Term for the Adverse Event"
  )

  expect_equal(unique(out$rpt_data$TOTAL), "All")
})

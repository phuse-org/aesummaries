data(adae)
data(cm)

test_that("multi_interval works with expected inputs", {
  out <- multi_interval(
    datain = list(AE = adae, CM = cm),
    subjectid = "01-701-1302"
  )

  expect_true(nrow(out$rpt_data) > 0)
  expect_equal(unique(out$rpt_data$USUBJID), "01-701-1302")
  expect_equal(out$title, "Events over Study Period for Participant: 01-701-1302\n; Age: 61; Sex: M; Race: WHITE") # nolint
  expect_true(all(c("CMSTDY", "CMENDY", "CMTRT", "CMCLAS") %in% names(out$rpt_data)))
  expect_true(out[["ptly"]][["x"]][["subplot"]])
})

test_that("multi_interval works with expected inputs for subject with no CM data", {
  out <- multi_interval(
    datain = list(AE = adae, CM = cm),
    subjectid = "01-703-1299"
  )

  expect_true(nrow(out$rpt_data) > 0)
  expect_equal(unique(out$rpt_data$USUBJID), "01-703-1299")
  expect_equal(out$title, "Events over Study Period for Participant: 01-703-1299\n; Age: 81; Sex: F; Race: WHITE") # nolint
  expect_equal(out[["ptly"]][["x"]][["data"]][[4]][["text"]], "No data for this participant/period")
  expect_true(out[["ptly"]][["x"]][["subplot"]])
  expect_true(all(is.na(out$rpt_data$CMDECOD)))
})

test_that("multi_interval returns empty plot for subject if AE data set is not present", {
  out <- multi_interval(
    datain = list(CM = cm),
    subjectid = "01-701-1302"
  )

  expect_null(nrow(out$rpt_data))
  expect_null(out[["ptly"]][["x"]][["subplot"]])
  expect_equal(out[["ptly"]][["x"]][["data"]][[1]][["text"]], "No AE data available")
})

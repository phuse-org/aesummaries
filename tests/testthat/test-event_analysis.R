data(event_df)

test_that("Test Case 1: Event Analysis works with expected inputs", {
  goutput <- event_analysis(
    datain = event_df$dsin,
    datain_N = event_df$dout,
    hl_var = "FMQ_NAM",
    hl_val = "ABDOMINAL PAIN",
    hl_scope = "Narrow",
    ll_var = "AEDECOD",
    ll_val = "ABDOMINAL DISCOMFORT",
    ll_scope = "Narrow",
    summary_by = "Events",
    ref_line = 2
  )

  ptly_data <- goutput[["ptly"]][["x"]][["data"]]

  legendgroups <-
    unlist(purrr::compact(purrr::map(
      seq_along(ptly_data),
      function(x) ptly_data[[x]][["legendgroup"]]
    )))

  expect_equal(length(goutput), 6)
  expect_equal(names(goutput), c("ptly", "plot", "rpt_data", "rpt_data1", "title", "footnote"))
  expect_true(nrow(goutput$rpt_data) > 0)
  expect_type(goutput$ptly, "list")
  expect_equal(legendgroups, sort(unique(goutput[["rpt_data"]][["DPTVAL"]])))

  expect_equal(goutput$title, "Event Analysis plot of Adverse Events")
  expect_equal(
    goutput$footnote,
    paste0(
      "* N is the total number of events. \nClassifications of adverse events ",
      "are based on the Medical Dictionary for Regulatory Activities (MedDRA",
      " v21.1). \nFMQ classification is based on FDA FMQ consolidated list. ",
      "\nDashed Horizontal line represents incidence percentage reference line. ",
      "\nTotals for the No. of Participants/Events at a higher level are not ",
      "necessarily the sum of those at the lower levels since a participant ",
      "may report two or more. \nPT - Preferred Term ; FMQ - FDA MedDRA Queries ",
      "\nEvent counts are the sum of individual occurrences within that category."
    )
  )
})


test_that("Test Case 2:  Event Analysis works with expected inputs", {
  goutput <- event_analysis(
    datain = event_df$dsin,
    datain_N = event_df$dout,
    hl_var = "FMQ_NAM",
    hl_val = "abdominal pain",
    hl_scope = "Narrow",
    ll_var = "FMQ_NAM",
    ll_val = "abdominal discomfort",
    ll_scope = "Narrow",
    summary_by = "Events",
    ref_line = 2
  )

  ptly_data <- goutput[["ptly"]][["x"]][["data"]]

  legendgroups <-
    unlist(purrr::compact(purrr::map(
      seq_along(ptly_data),
      function(x) ptly_data[[x]][["legendgroup"]]
    )))

  expect_equal(length(goutput), 6)
  expect_equal(names(goutput), c("ptly", "plot", "rpt_data", "rpt_data1", "title", "footnote"))
  expect_true(nrow(goutput$rpt_data) > 0)
  expect_type(goutput$ptly, "list")
  expect_equal(legendgroups, sort(unique(goutput[["rpt_data"]][["DPTVAL"]])))
  expect_identical(goutput$rpt_data, goutput$rpt_data)
  expect_equal(goutput$title, "Event Analysis plot of Adverse Events")
  expect_equal(
    goutput$footnote,
    paste0(
      "* N is the total number of events. \nClassifications of adverse events ",
      "are based on the Medical Dictionary for Regulatory Activities (MedDRA",
      " v21.1). \nFMQ classification is based on FDA FMQ consolidated list. ",
      "\nDashed Horizontal line represents incidence percentage reference line. ",
      "\nTotals for the No. of Participants/Events at a higher level are not ",
      "necessarily the sum of those at the lower levels since a participant ",
      "may report two or more. \nPT - Preferred Term ; FMQ - FDA MedDRA Queries ",
      "\nEvent counts are the sum of individual occurrences within that category."
    )
  )
})

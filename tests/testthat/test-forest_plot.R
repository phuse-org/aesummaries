data(ae_pre)

risk_dat <- risk_stat(
  datain = ae_pre$dsin,
  d_datain = ae_pre$dout,
  summary_by = "Patients",
  eventVar = "AEDECOD",
  ctrlgrp = "Placebo",
  trtgrp = "Xanomeline High Dose~~Xanomeline Low Dose",
  statistics = "Risk Ratio",
  alpha = 0.05,
  cutoff = 5,
  sort_opt = "Ascending",
  sort_var = "Count"
)
risk_dat1 <- risk_stat(
  datain = ae_pre$dsin,
  d_datain = ae_pre$dout,
  summary_by = "Events",
  eventVar = "AEBODSYS",
  ctrlgrp = "Placebo",
  trtgrp = "Xanomeline High Dose~~Xanomeline Low Dose",
  statistics = "Risk Difference",
  alpha = 0.05,
  cutoff = 5,
  sort_opt = "Ascending",
  sort_var = "Count"
)

test_that("Forest Plot Works with standard inputs", {
  fp_out <- forest_plot(
    datain = risk_dat,
    AE_Filter = "Any",
    review_by = c("AEBODSYS", "AEDECOD"),
    summary_by = "Patients",
    statistics = "Risk Ratio",
    xref = 1,
    pvalcut = 0.05,
    trtbign = "Y",
    scale_trans = "identity"
  )
  # Check that expected 6 outputs are returned:
  expect_equal(names(fp_out), c("ptly", "plot", "drill_plt", "rpt_data", "n", "title", "footnote"))

  # Check type of plotly object returned
  expect_true(fp_out$ptly$x$subplot)
  # Check that the event variable selected (label) is correct:
  expect_equal(
    fp_out$ptly$x$layout$xaxis$title$text,
    "AE Dictionary-Derived Term"
  )
  # Check the chosen statistic as label
  expect_equal(
    fp_out$ptly$x$layout$xaxis3$title$text,
    "Risk Ratio"
  )
  # Check title and footnote
  expect_equal(fp_out$title, "Forest plot for Risk Ratio of Any Adverse Events")

  expect_equal(
    fp_out$footnote,
    paste0(
      "* N is the total number of participants. \nClassifications of adverse ",
      "events are based on the Medical Dictionary for Regulatory Activities (MedDRA v21.1",
      "). \nDashed Vertical line represents risk value reference line. \nTotals for the ",
      "No. of Participants/Events at a higher level are not necessarily the sum of those ",
      "at the lower levels since a participant may report two or more. \nThe number of ",
      "participants reporting at least 1 occurrence of the event specified."
    )
  )
  # Graph data is from input data:
  expect_type(fp_out$rpt_data, "list")
  expect_true(nrow(fp_out$rpt_data) <= nrow(risk_dat))
})

test_that("Forest plot arguments reflect correctly", {
  fp_out1 <- forest_plot(
    datain = risk_dat1,
    AE_Filter = NULL,
    review_by = "AEBODSYS",
    summary_by = "Events",
    statistics = "Risk Difference",
    trtbign = "N",
    scale_trans = "log10"
  )

  # Review term reflects in legend
  expect_equal(
    fp_out1$ptly$x$layout$xaxis$title$text,
    "Body System or Organ Class"
  )

  # Treatment legend does not contain (N = count)
  legendtext <- unlist(purrr::map(
    seq_along(fp_out1$ptly$x$data),
    function(i) fp_out1$ptly$x$data[[i]]$name
  ))
  expect_false(any(str_detect(legendtext[1:3], "\\(N=")))
  expect_equal(legendtext[1:3], unique(risk_dat1$TRTVAR))

  # Check the chosen statistic as label
  expect_equal(
    fp_out1$ptly$x$layout$xaxis3$title$text,
    "Risk Difference"
  )
  # Check title and footnote
  expect_equal(fp_out1$title, "Forest plot for Risk Difference of  Adverse Events")

  expect_equal(
    fp_out1$footnote,
    paste0(
      "* N is the total number of events. \nClassifications of adverse events are ",
      "based on the Medical Dictionary for Regulatory Activities (MedDRA v21.1). ",
      "\nDashed Vertical line represents risk value reference line. \nTotals for ",
      "the No. of Participants/Events at a higher level are not necessarily the ",
      "sum of those at the lower levels since a participant may report two or more.",
      " \nEvent counts are the sum of individual occurrences within that category."
    )
  )
})
## Test with empty data
test_that("Forest Plot works as expected with empty `risk_stat()` output", {
  risk_stat_null <- tibble()
  fp_null <- forest_plot(
    datain = risk_stat_null,
    AE_Filter = "Any",
    review_by = c("AEBODSYS", "AEDECOD"),
    summary_by = "Patients",
    statistics = "Risk Ratio",
    xref = 1,
    pvalcut = 0.05,
    trtbign = "Y",
    scale_trans = "identity"
  )
  expect_equal(length(fp_null), 3)
  expect_equal(names(fp_null), c("ptly", "plot", "rpt_data"))
  expect_identical(risk_stat_null, fp_null$rpt_data)
})

data(ae_pre)

risk_stat_out <- risk_stat(
  datain = ae_pre$dsin,
  d_datain = ae_pre$dout,
  summary_by = "Patients",
  eventVar = "AEDECOD",
  ctrlgrp = "Placebo",
  trtgrp = "Xanomeline High Dose",
  statistics = "Risk Ratio",
  alpha = 0.05,
  cutoff = 2,
  sort_opt = "Ascending",
  sort_var = "Count"
)

test_that("Test Case 1: volcano_plot works with expected inputs", {
  vout <- volcano_plot(
    datain = ae_pre,
    AE_Filter = "Safety",
    statistics_data = risk_stat_out,
    statistics = "Risk Difference",
    treatment1 = "Placebo",
    treatment2 = "Xanomeline Low Dose",
    X_ref = 1,
    summary_by = "Patients",
    pvalue_label = "None",
    treatment1_label = "Control",
    treatment2_label = "Exposure",
    pvalcut = 0.05
  )

  ptly_data <- vout[["ptly"]][["x"]][["data"]]
  legendgroups <-
    unlist(purrr::compact(purrr::map(
      seq_along(ptly_data),
      function(x) ptly_data[[x]][["legendgroup"]]
    )))

  expect_equal(length(vout), 5)
  expect_equal(names(vout), c("ptly", "plot", "rpt_data", "title", "footnote"))
  expect_true(nrow(vout$rpt_data) > 0)
  expect_type(vout$ptly, "list")
  expect_equal(legendgroups, sort(unique(vout[["rpt_data"]][["BYVAR1"]])))
  expect_identical(vout$plot$data, vout$rpt_data)
  expect_equal(vout$title, "Volcano plot for Risk Difference of Safety Adverse Events")
  expect_equal(vout$footnote, "* N is the total number of participants. \nClassifications of adverse events are based on the Medical Dictionary for Regulatory Activities (MedDRA v21.1). \nDashed horizontal line represents p-value of 0.05.\nDotted horizontal line represents FDR adjusted p-value of approximately 0.05 (when applicable). \nDashed Vertical line represents risk value reference line. \nTotals for the No. of Participants/Events at a higher level are not necessarily the sum of those at the lower levels since a participant may report two or more. \nThe number of participants reporting at least 1 occurrence of the event specified.") # nolint
})


test_that("Test Case 2: volcano_plot returns pre processed AE data with
          empty `risk_stat()` output", {
  risk_stat_out_ <- tibble()
  vout <- volcano_plot(
    datain = ae_pre,
    AE_Filter = "Safety",
    statistics_data = risk_stat_out_,
    statistics = "Risk Difference",
    treatment1 = "Placebo",
    treatment2 = "Xanomeline Low Dose",
    X_ref = 1,
    summary_by = "Patients",
    pvalue_label = "None",
    treatment1_label = "Control",
    treatment2_label = "Exposure",
    pvalcut = 0.05
  )

  expect_equal(length(vout), 3)
  expect_equal(names(vout), c("ptly", "plot", "rpt_data"))
  expect_identical(ae_pre, vout$rpt_data)
  expect_identical(ae_pre$dsin, vout$rpt_data$dsin)
})


test_that("Test Case 3: volcano_plot transforms p values correctly based on `pvalue_label`", {
  vout_none <- volcano_plot(
    datain = ae_pre,
    AE_Filter = "Safety",
    statistics_data = risk_stat_out,
    statistics = "Risk Difference",
    treatment1 = "Placebo",
    treatment2 = "Xanomeline Low Dose",
    X_ref = 1,
    summary_by = "Patients",
    pvalue_label = "None",
    treatment1_label = "Control",
    treatment2_label = "Exposure",
    pvalcut = 0.05
  )

  vout_log <- volcano_plot(
    datain = ae_pre,
    AE_Filter = "Safety",
    statistics_data = risk_stat_out,
    statistics = "Risk Difference",
    treatment1 = "Placebo",
    treatment2 = "Xanomeline Low Dose",
    X_ref = 1,
    summary_by = "Patients",
    pvalue_label = "-log10",
    treatment1_label = "Control",
    treatment2_label = "Exposure",
    pvalcut = 0.05
  )

  expect_equal(length(vout_none), length(vout_log))
  expect_equal(names(vout_none), names(vout_log))
  expect_false(identical(
    layer_scales(vout_log$plot)$y$break_positions(),
    layer_scales(vout_none$plot)$y$break_positions()
  ))
})

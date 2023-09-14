data("ae_pre")

ht_dat <- risk_stat(
  datain = ae_pre$dsin,
  d_datain = ae_pre$dout,
  summary_by = "Patients",
  eventVar = "AEBODSYS",
  ctrlgrp = "Placebo",
  trtgrp = "Xanomeline Low Dose",
  statistics = "Risk Ratio",
  alpha = 0.05,
  cutoff = 0,
  sort_opt = "Ascending",
  sort_var = "Count"
)

# Risk and counts for Low Term
lt_dat <- risk_stat(
  datain = ae_pre$dsin,
  d_datain = ae_pre$dout,
  summary_by = "Patients",
  eventVar = "AEDECOD",
  ctrlgrp = "Placebo",
  trtgrp = "Xanomeline Low Dose",
  statistics = "Risk Ratio",
  alpha = 0.05,
  cutoff = 0,
  sort_opt = "Ascending",
  sort_var = "Count"
)


test_that("Test Case 1: Risk table works with expected inputs", {
  ae_out <- adae_r001(
    datain = ae_pre,
    population = "Overall Population",
    AE_Filter = "Any Event",
    riskyn = "Y",
    ctrlgrp = "Placebo",
    trtgrp = "Xanomeline Low Dose",
    ui_statistics = "Risk Ratio",
    ui_lt = "AEDECOD",
    ui_ht = "AEBODSYS",
    ui_cutoff = 0
  )

  # List of outputs returned is correct
  expect_named(ae_out, c("tout", "rpt_data", "title", "footnote"))

  # Class of display table
  expect_true(class(ae_out$tout) == "flextable")

  # Output data check
  rpt <- ae_out$rpt_data

  # Check that treatment pair selected is correct:
  expect_equal(unique(rpt$TRTVAR), c("Placebo", "Xanomeline Low Dose"))
  # In flextable output:
  expect_length(ae_out$tout$header$dataset, 4)

  # Number of terms is same
  expect_equal(nrow(rpt), nrow(lt_dat) + nrow(ht_dat))

  # Value level check:
  rpt_value <- rpt %>% select(DPTVAL, TRTVAR, FREQ, PCT, RISK, RISKCIL, RISKCIU)
  check_value <- bind_rows(ht_dat, lt_dat) %>%
    select(DPTVAL, TRTVAR, FREQ, PCT, RISK, RISKCIL, RISKCIU)
  expect_equal(rpt_value, check_value)

  # Check title and footnote
  expect_equal(
    ae_out$title,
    paste0(
      "Participants With Any Adverse Events by Higher Term and Lower ",
      "Term \nPopulation: Overall Population"
    )
  )
  expect_equal(
    ae_out$footnote,
    paste0(
      "* n is the number of participants with Any adverse events.",
      "\nRisk Ratio is shown between Placebo and Xanomeline Low Dose"
    )
  )
})

test_that("Test Case 2: No Risk table works with expected inputs", {
  ae_out2 <- adae_r001(
    datain = ae_pre,
    population = "Overall Population",
    AE_Filter = NULL,
    riskyn = "N",
    ui_lt = "AEDECOD",
    ui_ht = "AEBODSYS",
    ui_pctdisp = "TRT",
    ui_cutoff = 5
  )

  # List of outputs returned is correct
  expect_named(ae_out2, c("tout", "rpt_data", "title", "footnote"))

  # Class of display table
  expect_true(class(ae_out2$tout) == "flextable")

  rpt2 <- ae_out2$rpt_data

  # Check no risk calculated:
  expect_false("RISK" %in% names(rpt2))
  # Check that all treatments are present with correct counts (denominator when ui_pctdisp="TRT")
  trtcount <- ae_pre$dout %>%
    group_by(TRTVAR) %>%
    summarise(DENOMN = n_distinct(USUBJID)) %>%
    ungroup()
  expect_equal(rpt2 %>% distinct(TRTVAR, DENOMN), trtcount)

  # In flextable output:
  expect_length(ae_out2$tout$header$dataset, 5)

  # Check title and footnote
  expect_equal(
    ae_out2$title,
    "Participants With  Adverse Events by Higher Term and Lower Term \nPopulation: Overall Population" # nolint
  )
  expect_equal(
    ae_out2$footnote,
    "* n is the number of participants with  adverse events."
  )
})

test_that("Test Case 3: Sorting performed correctly", {
  ae_out3 <- adae_r001(
    datain = ae_pre,
    population = "Overall Population",
    AE_Filter = NULL,
    riskyn = "N",
    ui_lt = "AEDECOD",
    ui_ht = "AEBODSYS",
    ui_pctdisp = "TRT",
    ui_cutoff = 5,
    ui_sortopt = "Descending",
    ui_sortvar = "Percent"
  )
  ae_out31 <- adae_r001(
    datain = ae_pre,
    population = "Overall",
    AE_Filter = NULL,
    riskyn = "N",
    ui_lt = "AEDECOD",
    ui_ht = "AEBODSYS",
    ui_pctdisp = "TRT",
    ui_cutoff = 5,
    ui_sortopt = "Alphabetical"
  )


  # Check sorting is in descending order of control percentage
  # First percentage value should be the max value
  expect_equal(
    max(as.numeric(ae_out3$rpt_data$CTRL_P), na.rm = TRUE),
    as.numeric(ae_out3$rpt_data$CTRL_P)[1]
  )


  # Check alphabetical order maintained (high terms as test case)
  rpt31 <- ae_out31$rpt_data %>% filter(DPTVAR == "AEBODSYS")
  expect_equal(rpt31$DPTVAL[1], sort(rpt31$DPTVAL)[1])
})

test_that("Test Case 4: Percentage Denominator variation", {
  ae_out4 <- adae_r001(
    datain = ae_pre,
    population = "Overall Population",
    AE_Filter = NULL,
    riskyn = "N",
    ui_lt = "AEDECOD",
    ui_ht = "AEBODSYS",
    ui_pctdisp = "VAR",
    ui_cutoff = 5
  )
  ae_out41 <- adae_r001(
    datain = ae_pre,
    population = "Overall",
    AE_Filter = NULL,
    riskyn = "N",
    ui_lt = "AEDECOD",
    ui_ht = "AEBODSYS",
    ui_pctdisp = "HT",
    ui_cutoff = 5
  )


  # Check that denominator is total variable count for "VAR"
  expect_equal(unique(ae_out4$rpt_data$DENOMN), length(unique(ae_pre$dout$USUBJID)))

  # For pct=HT: Using one specific
  # Denominator for Low Terms:
  low_denom <- ae_out41$rpt_data %>%
    filter(DPTVARN == 17, DPTVALN != 0) %>%
    distinct(TRTVAR, DENOMN) %>%
    rename(N = DENOMN)
  # Number of distinct subjects in selected high term per treatment
  high_ct <- ae_pre$dout %>%
    filter(AEBODSYS == "MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS") %>%
    group_by(TRTVAR) %>%
    summarise(N = n_distinct(USUBJID)) %>%
    ungroup()

  expect_equal(low_denom, high_ct)

  # testing another term:
  # Denominator for Low Terms:
  low_denom1 <- ae_out41$rpt_data %>%
    filter(DPTVARN == 18, DPTVALN != 0) %>%
    distinct(TRTVAR, DENOMN) %>%
    rename(N = DENOMN)
  # Number of distinct subjects in selected high term per treatment
  high_ct1 <- ae_pre$dout %>%
    filter(AEBODSYS == "RENAL AND URINARY DISORDERS") %>%
    group_by(TRTVAR) %>%
    summarise(N = n_distinct(USUBJID)) %>%
    ungroup()

  expect_equal(low_denom1, high_ct1)
})

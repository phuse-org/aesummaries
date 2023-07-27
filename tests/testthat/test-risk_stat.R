data(ae_pre)
dsin <- ae_pre$dsin
dout <- ae_pre$dout

# testcase 1

test_that("Test Case 1: Check if the function gives expected statistic values", {
  dsin1 <- dsin %>%
    filter(AEDECOD %in% c("NAUSEA", "SINUS BRADYCARDIA")) %>%
    select(USUBJID, TRTA, AEDECOD, AEBODSYS, TRTVAR, BYVAR1)
  dout1 <- dout %>%
    filter(AEDECOD %in% c("NAUSEA", "SINUS BRADYCARDIA")) %>%
    select(USUBJID, TRTA, AEDECOD, AEBODSYS, TRTVAR, BYVAR1)

  denom <- dsin1 %>%
    filter(TRTVAR %in% c("Placebo", "Xanomeline High Dose")) %>%
    group_by(TRTVAR) %>%
    summarise(N = length(unique(USUBJID))) %>%
    ungroup()

  freq <- dsin1 %>%
    filter(TRTVAR %in% c("Placebo", "Xanomeline High Dose")) %>%
    group_by(TRTVAR, AEBODSYS, AEDECOD) %>%
    summarise(n = length(unique(USUBJID))) %>%
    ungroup()

  exp <- left_join(denom, freq, by = "TRTVAR")

  idvar <- c("AEBODSYS", "AEDECOD")
  exp1 <- exp %>%
    mutate(TRTVAR = case_when(
      TRTVAR == "Placebo" ~ "ctrlgrp",
      TRTVAR == "Xanomeline High Dose" ~ "trtgrp"
    )) %>%
    tidyr::pivot_wider(id_cols = any_of(c(idvar)), names_from = TRTVAR, values_from = c(N, n)) %>%
    mutate(
      temp1 = N_ctrlgrp - n_ctrlgrp,
      temp2 = N_trtgrp - n_trtgrp
    )

  mat <- matrix(c(2, 7, 3, 6), nrow = 2)
  risk <- riskratio.wald(mat, conf.level = 1 - 0.05)

  risk_val <- round(risk$measure[2, 1], 3)
  pval <- round(risk$p.value[2, 3], 4)
  cil <- round(risk$measure[2, 2], 2)
  ciu <- round(risk$measure[2, 3], 2)

  expected <- exp %>%
    filter(AEDECOD == "NAUSEA") %>%
    mutate(
      RISK = risk_val,
      PVALUE = pval,
      RISKCIL = cil,
      RISKCIU = ciu,
      PCT = round((n * 100) / N, 2),
      TRTVAR = as.character(TRTVAR)
    )

  risk_s <- risk_stat(
    datain = dsin1,
    d_datain = dout1,
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

  actual <- risk_s %>%
    rename(AEBODSYS = BYVAR1, AEDECOD = DPTVAL, N = TOTAL_N, n = FREQ) %>%
    filter(AEDECOD == "NAUSEA") %>%
    mutate(
      N = as.integer(N),
      n = as.integer(n)
    ) %>%
    select(TRTVAR, N, AEBODSYS, AEDECOD, n, RISK, PVALUE, RISKCIL, RISKCIU, PCT)

  expect_equal(actual$RISK, expected$RISK)
  expect_equal(actual$PVALUE, expected$PVALUE)
  expect_identical(actual, expected)
})

# testcae 2

test_that("Test Case 2: Check if the function works as expected for risk difference", {
  dsin1 <- dsin %>%
    filter(AEDECOD %in% c("NAUSEA", "SINUS BRADYCARDIA")) %>%
    select(USUBJID, TRTA, AEDECOD, AEBODSYS, TRTVAR, BYVAR1)
  dout1 <- dout %>%
    filter(AEDECOD %in% c("NAUSEA", "SINUS BRADYCARDIA")) %>%
    select(USUBJID, TRTA, AEDECOD, AEBODSYS, TRTVAR, BYVAR1)

  denom <- dsin1 %>%
    filter(TRTVAR %in% c("Placebo", "Xanomeline High Dose")) %>%
    group_by(TRTVAR) %>%
    summarise(N = length(unique(USUBJID))) %>%
    ungroup()

  freq <- dsin1 %>%
    filter(TRTVAR %in% c("Placebo", "Xanomeline High Dose")) %>%
    group_by(TRTVAR, AEBODSYS, AEDECOD) %>%
    summarise(n = length(unique(USUBJID))) %>%
    ungroup()

  exp <- left_join(denom, freq, by = "TRTVAR")

  idvar <- c("AEBODSYS", "AEDECOD")
  exp1 <- exp %>%
    mutate(TRTVAR = case_when(
      TRTVAR == "Placebo" ~ "ctrlgrp",
      TRTVAR == "Xanomeline High Dose" ~ "trtgrp"
    )) %>%
    tidyr::pivot_wider(id_cols = any_of(c(idvar)), names_from = TRTVAR, values_from = c(N, n)) %>%
    mutate(
      temp1 = N_ctrlgrp - n_ctrlgrp,
      temp2 = N_trtgrp - n_trtgrp
    )

  mat <- matrix(c(2, 7, 3, 6), nrow = 2)
  risk <- riskdiff_wald(mat, conf.level = 1 - 0.05)

  risk_val <- round(risk$measure[2, 1], 3)
  pval <- round(risk$p.value[2, 3], 4)
  ciu <- round(risk$measure[2, 2], 4)
  cil <- round(risk$measure[2, 3], 4)

  expected <- exp %>%
    filter(AEDECOD == "NAUSEA") %>%
    mutate(
      RISK = risk_val,
      PVALUE = pval,
      TRTVAR = as.character(TRTVAR)
    ) %>%
    arrange(desc(RISK))

  risk_s <- risk_stat(
    datain = dsin1,
    d_datain = dout1,
    summary_by = "Patients",
    eventVar = "AEDECOD",
    ctrlgrp = "Placebo",
    trtgrp = "Xanomeline High Dose",
    statistics = "Risk Difference",
    alpha = 0.05,
    cutoff = 2,
    sort_opt = "Descending",
    sort_var = "RiskValue"
  )

  actual <- risk_s %>%
    rename(AEBODSYS = BYVAR1, AEDECOD = DPTVAL, N = TOTAL_N, n = FREQ) %>%
    filter(AEDECOD == "NAUSEA") %>%
    mutate(
      N = as.integer(N),
      n = as.integer(n)
    ) %>%
    select(TRTVAR, N, AEBODSYS, AEDECOD, n, RISK, PVALUE)

  expect_equal(actual$RISK, expected$RISK)
  expect_equal(actual$PVALUE, expected$PVALUE)
  expect_identical(actual, expected)
})

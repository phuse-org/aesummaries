# test case-1
data("ae_pre")
# mcatstat output will the input for mdisplay
mout <- mcatstat(
  datain = ae_pre$dsin,
  d_datain = ae_pre$dout,
  ui_uniqid = "USUBJID",
  ui_dptvar = "AEDECOD",
  ui_pctdisp = "TRT",
  miss_catyn = "N",
  cum_ctyn = "N",
  total_catyn = "N",
  dptvarn = 1
)
# test case 1
test_that("Test Case 1", {
  actual <- mdisplay(
    datain = mout,
    ui_bylabel = NA,
    ui_catlabel = NA,
    ui_statlabel = NA,
    trtbign = NA,
    extra_df = NULL,
    extra_mergeby = "DPTVAL",
    extra_vars = "",
    colformat = "",
    indent = NULL,
    dptlabel = "  "
  )

  BYVAR <- var_start(mout, "BYVAR")
  BYVARN <- var_start(mout, "BYVARN")
  report <- mout %>%
    select(DPTVAR, TRTVAR, BYVAR, BYVARN, DPTVARN, DPTVALN, DPTVAL, CVALUE, CN) %>%
    mutate(CVALUE = as.character(CVALUE))
  rep <- report %>%
    arrange(across(c("TRTVAR"))) %>%
    tidyr::pivot_wider(
      names_from = TRTVAR,
      values_from = CVALUE,
      values_fill = "-",
      names_sort = FALSE
    ) %>%
    ungroup() %>%
    mutate(across(!BYVAR & !DPTVAR & !DPTVAL & where(is.character), function(x) {
      ifelse(CN == "C", gsub("-", "0", x), x)
    }))
  rep <- rep %>% select(BYVAR, DPTVAR, DPTVAL, everything())
  repdat <- rep %>%
    arrange(across(any_of(c(BYVARN, "DPTVARN", "DPTVALN")))) %>%
    select(-any_of(BYVARN))

  expect_equal(actual$dataf, repdat)
})

# test case 2
test_that("Test Case 2", {
  actual <- mdisplay(
    datain = mout,
    ui_bylabel = "SOC",
    ui_catlabel = "VALUE",
    ui_statlabel = NA,
    trtbign = NA,
    extra_df = NULL,
    extra_mergeby = "DPTVAL",
    extra_vars = "",
    colformat = "",
    indent = NULL,
    dptlabel = "  "
  )

  BYVAR <- var_start(mout, "BYVAR")
  BYVARN <- var_start(mout, "BYVARN")
  report <- mout %>%
    select(DPTVAR, TRTVAR, BYVAR, BYVARN, DPTVARN, DPTVALN, DPTVAL, CVALUE, CN) %>%
    mutate(CVALUE = as.character(CVALUE))
  rep <- report %>%
    arrange(across(c("TRTVAR"))) %>%
    tidyr::pivot_wider(
      names_from = TRTVAR,
      values_from = CVALUE,
      values_fill = "-",
      names_sort = FALSE
    ) %>%
    ungroup() %>%
    mutate(across(!BYVAR & !DPTVAR & !DPTVAL & where(is.character), function(x) {
      ifelse(CN == "C", gsub("-", "0", x), x)
    }))
  rep <- rep %>% select(BYVAR, DPTVAR, DPTVAL, everything())
  repdat <- rep %>%
    arrange(across(any_of(c(BYVARN, "DPTVARN", "DPTVALN")))) %>%
    select(-any_of(BYVARN)) %>%
    rename("SOC" = BYVAR) %>%
    mutate(DPTVAR = "VALUE")

  expect_equal(actual$dataf, repdat)
})

# test case 3
test_that("Test Case 3", {
  actual <- mdisplay(
    datain = mout,
    ui_bylabel = "SOC",
    ui_catlabel = "VALUE",
    ui_statlabel = NA,
    trtbign = ae_pre$bigN,
    extra_df = NULL,
    extra_mergeby = "DPTVAL",
    extra_vars = "",
    colformat = " ",
    indent = "DPTVALN == 1",
    dptlabel = "  "
  )

  BYVAR <- var_start(mout, "BYVAR")
  BYVARN <- var_start(mout, "BYVARN")
  report <- mout %>%
    select(DPTVAR, TRTVAR, BYVAR, BYVARN, DPTVARN, DPTVALN, DPTVAL, CVALUE, CN) %>%
    mutate(CVALUE = as.character(CVALUE))
  rep <- report %>%
    arrange(across(c("TRTVAR"))) %>%
    tidyr::pivot_wider(
      names_from = TRTVAR,
      values_from = CVALUE,
      values_fill = "-",
      names_sort = FALSE
    ) %>%
    ungroup() %>%
    mutate(across(!BYVAR & !DPTVAR & !DPTVAL & where(is.character), function(x) {
      ifelse(CN == "C", gsub("-", "0", x), x)
    }))
  rep <- rep %>% select(BYVAR, DPTVAR, DPTVAL, everything())
  report <- rep %>%
    arrange(across(any_of(c(BYVARN, "DPTVARN", "DPTVALN")))) %>%
    select(-any_of(BYVARN)) %>%
    rename("SOC" = BYVAR) %>%
    mutate(DPTVAR = "VALUE")
  trtbign <- ae_pre$bigN
  n <- paste0("Placebo (N = ", trtbign$BIGN[1], ")")
  repdat <- report %>% rename(
    !!paste0("Placebo (N=", trtbign$BIGN[1], ")", " ") := "Placebo",
    !!paste0("Xanomeline Low Dose (N=", trtbign$BIGN[2], ")", " ") := "Xanomeline Low Dose",
    !!paste0("Xanomeline High Dose (N=", trtbign$BIGN[3], ")", " ") := "Xanomeline High Dose",
    !!paste0("Total (N=", trtbign$BIGN[4], ")", " ") := "Total"
  )

  expect_equal(actual$dataf, repdat)
})

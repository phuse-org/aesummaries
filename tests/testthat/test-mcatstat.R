data(adsl)
data(ae_pre)

# Sample Input 1
ad_entry <- mentry(
  datain = adsl,
  ui_aSubset = NA,
  ui_dSubset = "USUBJID!=''",
  ui_byvar = NA,
  ui_subgrpvar = NA,
  ui_trtvar = "TRT01A",
  ui_trtsort = "TRT01AN",
  ui_addGrpMiss = "N",
  ui_pop_fil = NA
)

# Sample Output created from defaults
ad_sum <- ad_entry$dsin %>%
  group_by(TRTVAR, SEX) %>%
  summarise(FREQ = n_distinct(USUBJID)) %>%
  ungroup() %>%
  group_by(TRTVAR) %>%
  mutate(
    DPTVAL = as.character(SEX), DENOMN = sum(FREQ),
    PCT = format(round(100 * FREQ / DENOMN, 2), nsmall = 2),
    CVALUE = paste0(FREQ, " (", PCT, "%)"), CN = "C",
    DPTVARN = 1, XVAR = DPTVAL,
    DPTVAR = "SEX"
  ) %>%
  ungroup() %>%
  select(-SEX) %>%
  arrange(TRTVAR, DPTVAL)

# sample input 2
ad_entry1 <- mentry(
  datain = adsl,
  ui_aSubset = NA,
  ui_dSubset = "USUBJID!=''",
  ui_byvar = "RACE",
  ui_subgrpvar = "ETHNIC",
  ui_trtvar = "TRT01A",
  ui_trtsort = "TRT01AN",
  ui_addGrpMiss = "N",
  ui_pop_fil = NA
)


test_that("Case 1:mcatstat output with standard inputs", {
  ad_mcat <- mcatstat(
    datain = ad_entry$dsin,
    d_datain = ad_entry$dout,
    ui_dptvar = "SEX",
    ui_pctdisp = "TRT"
  )

  # Check output returned is in expected format
  expect_type(ad_mcat, "list")
  expect_identical(names(ad_mcat), c(
    "DPTVAR", "DPTVAL", "XVAR", "TRTVAR",
    "DENOMN", "DPTVALN", "FREQ", "PCT",
    "CVALUE", "DPTVARN", "CN"
  ))

  # Check values are matched with expected:
  expect_true(all_equal(ad_mcat %>% select(-DPTVALN), ad_sum))
})

test_that("Case 2: Empty input", {
  m_zero <- mcatstat(
    datain = ad_entry$dsin %>% filter(USUBJID == "A"),
    d_datain = ad_entry$dout,
    ui_dptvar = "SEX",
    ui_pctdisp = "TRT"
  )

  expect_identical(m_zero, NULL)
})


test_that("Case 3: Unique ID variation", {
  m_subj <- mcatstat(
    datain = ae_pre$dsin,
    d_datain = ae_pre$dout,
    ui_uniqid = "USUBJID",
    ui_dptvar = "AEDECOD",
    ui_pctdisp = "TRT"
  )
  m_na <- mcatstat(
    datain = ae_pre$dsin,
    d_datain = ae_pre$dout,
    ui_uniqid = NA,
    ui_dptvar = "AEDECOD",
    ui_pctdisp = "TRT"
  )

  # All groups and order except actual count values should be equal
  expect_equal(
    m_subj %>% select(-c(DENOMN, FREQ, PCT, CVALUE)),
    m_na %>% select(-c(DENOMN, FREQ, PCT, CVALUE))
  )

  # Counts are different
  expect_false(setequal(unique(m_subj$DENOMN), unique(m_na$DENOMN)))

  # Check values (for denominator, should also apply to numerator)
  check_na <- ae_pre$dout %>%
    ungroup() %>%
    group_by(TRTVAR) %>%
    summarise(DENOMN = n()) %>%
    ungroup()

  expect_equal(m_na %>% distinct(TRTVAR, DENOMN), check_na)
})

test_that("Case 4: Percentage denominator variation", {
  # Invalid value for ui_pctdisp:
  expect_null(mcatstat(
    datain = ad_entry1$dsin,
    d_datain = ad_entry1$dout,
    ui_dptvar = "SEX",
    ui_pctdisp = "ABC"
  ))

  m_none <- mcatstat(
    datain = ad_entry1$dsin,
    d_datain = ad_entry1$dout,
    ui_dptvar = "SEX",
    ui_pctdisp = "NONE"
  )


  # No percentage columns:
  expect_identical(names(m_none), c(
    "DPTVAR", "DPTVAL", "XVAR", "TRTVAR",
    "SUBGRPVAR1", "SUBGRPVAR1N", "BYVAR1",
    "BYVAR1N", "DPTVALN", "FREQ",
    "CVALUE", "DPTVARN", "CN"
  ))
  expect_equal(m_none$FREQ, m_none$CVALUE)

  # Variable total as denominator
  m_var <- mcatstat(
    datain = ad_entry1$dsin,
    d_datain = ad_entry1$dout,
    ui_dptvar = "SEX",
    ui_pctdisp = "VAR"
  )

  # Entire matrix has same denominator - check value
  expect_length(unique(m_var$DENOMN), 1)
  expect_equal(unique(m_var$DENOMN), length(unique(ad_entry1$dout$USUBJID)))
})

test_that("Case 5: Cumulative Frequency", {
  m_cum <- mcatstat(
    datain = ad_entry$dsin,
    d_datain = ad_entry$dout,
    ui_dptvar = "SEX",
    ui_pctdisp = "NO",
    cum_ctyn = "Y"
  )
  ad_cum <- ad_entry$dsin %>%
    group_by(TRTVAR, SEX) %>%
    summarise(FREQ = n_distinct(USUBJID)) %>%
    mutate(DPTVAL = as.character(SEX), FREQ = cumsum(FREQ)) %>%
    select(-SEX) %>%
    ungroup()

  expect_equal(m_cum %>% select(TRTVAR, FREQ, DPTVAL), ad_cum)
})


test_that("Case 6: Total category count", {
  m_total <- mcatstat(
    datain = ad_entry$dsin,
    d_datain = ad_entry$dout,
    ui_dptvar = "SEX",
    ui_pctdisp = "TRT",
    total_catyn = "Y",
    cum_ctyn = "N"
  )

  expect_true("Total" %in% unique(m_total$DPTVAL))
  # Test single group
  m_pl1 <- ad_sum %>%
    filter(TRTVAR == "Placebo") %>%
    select(FREQ) %>%
    sum()
  m_pl2 <- m_total %>%
    filter(TRTVAR == "Placebo", DPTVAL == "Total") %>%
    pull(FREQ)

  expect_identical(m_pl2, m_pl1)
})

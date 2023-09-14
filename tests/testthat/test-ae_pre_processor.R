# testcase 1:
data(adae)
data(FMQ_Consolidated_List)
test_that("Test Case 1: With standard inputs", {
  actual <- ae_pre_processor(
    datain = adae,
    aeSubset = "AOCCPFL=='Y'",
    aeDenomSubset = "!is.na(ASTDT)",
    ae_filter = "Any Event",
    aeObsPeriod = "Overall Duration",
    aeObsResidual = 0,
    trtvar = "TRTA",
    trtsort = "TRTAN",
    pop_fil = "SAFFL",
    fmq_data = FMQ_Consolidated_List,
    aeEventVar = "AEDECOD",
    aeByVar = "AEBODSYS",
    aeSubGrpVar = NA,
    aeBigN = "N",
    aeGrpVarMiss = "N",
    aeTrtTot = "N",
    aeSubGrpTot = "N"
  )
  date_formats <- c("%d%b%Y", "%Y-%m-%d")

  expected <- adae %>%
    mutate(
      AESTDT = as.Date(ASTDT, tryFormats = date_formats, optional = FALSE),
      AEENDT = as.Date(AENDT, tryFormats = date_formats, optional = FALSE),
      RFSTDTC = as.Date(TRTSDT, tryFormats = date_formats, optional = FALSE),
      RFENDTC = as.Date(TRTEDT, tryFormats = date_formats, optional = FALSE)
    ) %>%
    tidyr::drop_na(RFSTDTC) %>%
    mutate(
      AEDECOD = if_else(!is.na(AESTDT) & is.na(AEDECOD), "Not yet coded", AEDECOD),
      AESTDT = if_else(is.na(AESTDT) & !is.na(AEDECOD), RFSTDTC, AESTDT),
      AESEV = toupper(AESEV)
    )

  mdsin <- mentry(
    datain = expected,
    ui_aSubset = "AOCCPFL=='Y'",
    ui_dSubset = "!is.na(ASTDT)",
    ui_byvar = "AEBODSYS",
    ui_subgrpvar = NA,
    ui_trtvar = "TRTA",
    ui_trtsort = "TRTAN",
    ui_trttotalyn = "N",
    ui_sgtotalyn = "N",
    ui_bign = "N",
    ui_addGrpMiss = "N",
    ui_pop_fil = "SAFFL"
  )
  expect_named(actual, c("dsin", "dout", "bigN"))
  expect_equal(actual$dsin, mdsin$dsin)
  expect_equal(actual$dout, mdsin$dout)
  expect_true(is.na(actual$bigN))
})

# testcase 2:

test_that("Test Case 2: Varying inputs", {
  actual <- ae_pre_processor(
    datain = adae,
    aeSubset = "AOCCPFL=='Y'",
    aeDenomSubset = "!is.na(ASTDT)",
    ae_filter = "Treatment emergent",
    aeObsPeriod = "Other",
    aeObsResidual = 5,
    trtvar = "TRTA",
    trtsort = "TRTAN",
    pop_fil = "SAFFL",
    fmq_data = FMQ_Consolidated_List,
    aeEventVar = "AEDECOD",
    aeByVar = "AEBODSYS",
    aeSubGrpVar = NA,
    aeBigN = "N",
    aeGrpVarMiss = "N",
    aeTrtTot = "Y",
    aeSubGrpTot = "N"
  )
  expect_named(actual, c("dsin", "dout", "bigN"))
  expect_true(is.na(actual$bigN))
  # AE filter applied:
  expect_equal(unique(actual$dsin$TRTEMFL), "Y")
  # Analysis Subset applied:
  expect_equal(unique(actual$dsin$AOCCPFL), "Y")
  # Denom Subset:
  expect_false(any(is.na(actual$dout$ASTDT)))
  # Observation Period:
  expect_true(all(actual$dsin$AESTDT > actual$dsin$RFSTDTC))
  expect_true(all(actual$dsin$AESTDT < (actual$dsin$RFENDTC + 5)))
  # All required variables created in if():
  expect_true(all(c("AESTDT", "AEENDT", "RFSTDTC", "RFENDTC", "AESEV") %in% names(actual$dout)))
  # There are no non-coded terms in default adae:
  expect_false("Not yet coded" %in% unique(actual$dsin$AEDECOD))
  # Treatment
  expect_is(actual$dsin$TRTVAR, "factor")
  # Total Treatment:
  expect_true("Total" %in% unique(actual$dsin$TRTVAR))
  # FMQ is not given as eventvar:
  expect_false("FMQ_NAM" %in% names(actual$dout))
})

# test case 3:
test_that("Test Case 3: FMQ created from Consolidated List", {
  actual <- ae_pre_processor(
    datain = adae,
    aeSubset = "USUBJID != ''",
    aeDenomSubset = "!is.na(ASTDT)",
    ae_filter = c("Mild", "Recovered/Resolved"),
    aeObsPeriod = "Overall Duration",
    trtvar = "TRTA",
    trtsort = "TRTAN",
    pop_fil = "SAFFL",
    fmq_data = FMQ_Consolidated_List,
    aeEventVar = "AEDECOD",
    aeByVar = "FMQ_NAM",
    aeSubGrpVar = NA,
    aeBigN = "Y",
    aeGrpVarMiss = "N",
    aeTrtTot = "N",
    aeSubGrpTot = "N"
  )
  expect_named(actual, c("dsin", "dout", "bigN"))
  expect_is(actual$bigN, "data.frame")
  # AE filter applied:
  expect_equal(toupper(unique(actual$dsin$AESEV)), "MILD")
  expect_equal(toupper(unique(actual$dsin$AEOUT)), "RECOVERED/RESOLVED")
  # FMQ is given as BYVAR
  expect_true("FMQ_NAM" %in% names(actual$dout))
  # Using PT = "Rash" as example:
  Fmq_rash <- FMQ_Consolidated_List %>%
    filter(PT == "Rash") %>%
    mutate(FMQ_NAM = paste0(FMQ, "/", FMQCAT))
  expectedfmq <- paste(unique(Fmq_rash$FMQ_NAM), collapse = "~~")
  actualfmq <- actual$dout %>%
    filter(AEDECOD == "RASH") %>%
    distinct(FMQ_NAM) %>%
    pull()
  expect_equal(actualfmq, expectedfmq)
})
# test case 4
test_that("Test Case 4: Dates Converted As Expected:", {
  actual <- ae_pre_processor(
    datain = adae,
    aeSubset = "USUBJID != ''",
    aeDenomSubset = "!is.na(ASTDT)",
    ae_filter = "Serious",
    aeObsPeriod = "Other",
    aeObsResidual = 5,
    trtvar = "TRTA",
    trtsort = "TRTAN",
    pop_fil = "SAFFL",
    fmq_data = FMQ_Consolidated_List,
    aeEventVar = "AEDECOD",
    aeByVar = "AEBODSYS",
    aeSubGrpVar = NA,
    aeBigN = "N",
    aeGrpVarMiss = "N",
    aeTrtTot = "Y",
    aeSubGrpTot = "N"
  )
  expect_named(actual, c("dsin", "dout", "bigN"))
  # AE filter applied:
  expect_equal(unique(actual$dsin$AESER), "Y")
  # expected class to be 'Date'
  expect_is(actual$dsin$AESTDT, "Date")
  expect_is(actual$dsin$AEENDT, "Date")
  expect_is(actual$dsin$RFSTDTC, "Date")
  expect_is(actual$dsin$RFENDTC, "Date")
})

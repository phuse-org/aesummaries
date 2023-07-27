# testcase 1:

test_that("Test Case 1", {
  data(adae)
  data(FMQ_Consolidated_List)

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

  # expected
  # mentry

  mdsin <- mentry(
    datain = adae,
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

  df <- mdsin$dsin
  date_formats <- c("%d%b%Y", "%Y-%m-%d")

  expected <- df %>%
    mutate(
      AESTDT = as.Date(ASTDT, tryFormats = date_formats, optional = FALSE),
      AEENDT = as.Date(AENDT, tryFormats = date_formats, optional = FALSE),
      RFSTDTC = as.Date(TRTSDT, tryFormats = date_formats, optional = FALSE),
      RFENDTC = as.Date(TRTEDT, tryFormats = date_formats, optional = FALSE)
    ) %>%
    tidyr::drop_na(RFSTDTC) %>%
    mutate(
      AEDECOD = ifelse(!is.na(AESTDT) & is.na(AEDECOD), "Not yet coded", AEDECOD),
      AESTDT = ifelse(is.na(AESTDT) & !is.na(AEDECOD), RFSTDTC, AESTDT),
      AESEV = toupper(AESEV)
    )

  expect_equal(actual$dsin, expected)
})

# testcase 2:

test_that("Test Case 2", {
  data(adae)
  data(FMQ_Consolidated_List)

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

  # expected
  # mentry
  mdsin <- mentry(
    datain = adae,
    ui_aSubset = "AOCCPFL=='Y'",
    ui_dSubset = "!is.na(ASTDT)",
    ui_byvar = "AEBODSYS",
    ui_subgrpvar = NA,
    ui_trtvar = "TRTA",
    ui_trtsort = "TRTAN",
    ui_trttotalyn = "Y",
    ui_sgtotalyn = "N",
    ui_bign = "N",
    ui_addGrpMiss = "N",
    ui_pop_fil = "SAFFL"
  )

  df <- mdsin$dsin
  date_formats <- c("%d%b%Y", "%Y-%m-%d")

  expected <- df %>%
    mutate(
      AESTDT = as.Date(ASTDT, tryFormats = date_formats, optional = FALSE),
      AEENDT = as.Date(AENDT, tryFormats = date_formats, optional = FALSE),
      RFSTDTC = as.Date(TRTSDT, tryFormats = date_formats, optional = FALSE),
      RFENDTC = as.Date(TRTEDT, tryFormats = date_formats, optional = FALSE)
    ) %>%
    tidyr::drop_na(RFSTDTC) %>%
    mutate(
      AEDECOD = ifelse(!is.na(AESTDT) & is.na(AEDECOD), "Not yet coded", AEDECOD),
      AESTDT = ifelse(is.na(AESTDT) & !is.na(AEDECOD), RFSTDTC, AESTDT),
      AESEV = toupper(AESEV)
    ) %>%
    filter(TRTEMFL == "Y") %>%
    filter((AESTDT > RFSTDTC) & (AESTDT < (RFENDTC + 5)))

  expect_equal(actual$dsin, expected)
})

# test case 3:
test_that("Test Case 3", {
  data(adae)
  data(FMQ_Consolidated_List)

  actual <- ae_pre_processor(
    datain = adae,
    aeSubset = "AOCC02FL=='Y'",
    aeDenomSubset = "!is.na(ASTDT)",
    ae_filter = "Recovering/Resolving",
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

  # expected
  # mentry
  mdsin <- mentry(
    datain = adae,
    ui_aSubset = "AOCC02FL=='Y'",
    ui_dSubset = "!is.na(ASTDT)",
    ui_byvar = "AEBODSYS",
    ui_subgrpvar = NA,
    ui_trtvar = "TRTA",
    ui_trtsort = "TRTAN",
    ui_trttotalyn = "Y",
    ui_sgtotalyn = "N",
    ui_bign = "N",
    ui_addGrpMiss = "N",
    ui_pop_fil = "SAFFL"
  )

  df <- mdsin$dsin
  date_formats <- c("%d%b%Y", "%Y-%m-%d")

  expected <- df %>%
    mutate(
      AESTDT = as.Date(ASTDT, tryFormats = date_formats, optional = FALSE),
      AEENDT = as.Date(AENDT, tryFormats = date_formats, optional = FALSE),
      RFSTDTC = as.Date(TRTSDT, tryFormats = date_formats, optional = FALSE),
      RFENDTC = as.Date(TRTEDT, tryFormats = date_formats, optional = FALSE)
    ) %>%
    tidyr::drop_na(RFSTDTC) %>%
    mutate(
      AEDECOD = ifelse(!is.na(AESTDT) & is.na(AEDECOD), "Not yet coded", AEDECOD),
      AESTDT = ifelse(is.na(AESTDT) & !is.na(AEDECOD), RFSTDTC, AESTDT),
      AESEV = toupper(AESEV)
    ) %>%
    filter((AESTDT > RFSTDTC) & (AESTDT < (RFENDTC + 5))) %>%
    filter(AEOUT == toupper("Recovering/Resolving"))

  expect_equal(actual$dsin, expected)
})

# test case 4:
test_that("Test Case 4", {
  data(adae)
  data(FMQ_Consolidated_List)

  input <- adae %>% mutate(
    AESTDT = as.character(ASTDT),
    AEENDT = as.character(AENDT),
    RFSTDTC = as.character(TRTSDT),
    RFENDTC = as.character(TRTEDT)
  )

  actual <- ae_pre_processor(
    datain = input,
    aeSubset = "AOCC02FL=='Y'",
    aeDenomSubset = "!is.na(ASTDT)",
    ae_filter = "Recovering/Resolving",
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

  # expected
  # mentry
  mdsin <- mentry(
    datain = input,
    ui_aSubset = "AOCC02FL=='Y'",
    ui_dSubset = "!is.na(ASTDT)",
    ui_byvar = "AEBODSYS",
    ui_subgrpvar = NA,
    ui_trtvar = "TRTA",
    ui_trtsort = "TRTAN",
    ui_trttotalyn = "Y",
    ui_sgtotalyn = "N",
    ui_bign = "N",
    ui_addGrpMiss = "N",
    ui_pop_fil = "SAFFL"
  )

  df <- mdsin$dsin
  date_formats <- c("%d%b%Y", "%Y-%m-%d")

  expected <- df %>%
    mutate(
      AESTDT = as.Date(AESTDT, tryFormats = date_formats, optional = FALSE),
      AEENDT = as.Date(AEENDT, tryFormats = date_formats, optional = FALSE),
      RFSTDTC = as.Date(RFSTDTC, tryFormats = date_formats, optional = FALSE),
      RFENDTC = as.Date(RFENDTC, tryFormats = date_formats, optional = FALSE)
    ) %>%
    tidyr::drop_na(RFSTDTC) %>%
    mutate(
      AEDECOD = ifelse(!is.na(AESTDT) & is.na(AEDECOD), "Not yet coded", AEDECOD),
      AESTDT = ifelse(is.na(AESTDT) & !is.na(AEDECOD), RFSTDTC, AESTDT),
      AESEV = toupper(AESEV)
    ) %>%
    filter((AESTDT > RFSTDTC) & (AESTDT < (RFENDTC + 5))) %>%
    filter(AEOUT == toupper("Recovering/Resolving"))

  expect_equal(actual$dsin, expected)
})

# test case 5
test_that("Test Case 5", {
  data(adae)
  data(FMQ_Consolidated_List)

  input <- adae %>% mutate(
    AESTDT = as.character(ASTDT),
    AEENDT = as.character(AENDT),
    RFSTDTC = as.character(TRTSDT),
    RFENDTC = as.character(TRTEDT)
  )

  actual <- ae_pre_processor(
    datain = input,
    aeSubset = "AOCC02FL=='Y'",
    aeDenomSubset = "!is.na(ASTDT)",
    ae_filter = "Mild",
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

  # expected
  # mentry
  mdsin <- mentry(
    datain = input,
    ui_aSubset = "AOCC02FL=='Y'",
    ui_dSubset = "!is.na(ASTDT)",
    ui_byvar = "AEBODSYS",
    ui_subgrpvar = NA,
    ui_trtvar = "TRTA",
    ui_trtsort = "TRTAN",
    ui_trttotalyn = "Y",
    ui_sgtotalyn = "N",
    ui_bign = "N",
    ui_addGrpMiss = "N",
    ui_pop_fil = "SAFFL"
  )

  df <- mdsin$dsin
  date_formats <- c("%d%b%Y", "%Y-%m-%d")

  expected <- df %>%
    mutate(
      AESTDT = as.Date(AESTDT, tryFormats = date_formats, optional = FALSE),
      AEENDT = as.Date(AEENDT, tryFormats = date_formats, optional = FALSE),
      RFSTDTC = as.Date(RFSTDTC, tryFormats = date_formats, optional = FALSE),
      RFENDTC = as.Date(RFENDTC, tryFormats = date_formats, optional = FALSE)
    ) %>%
    tidyr::drop_na(RFSTDTC) %>%
    mutate(
      AEDECOD = ifelse(!is.na(AESTDT) & is.na(AEDECOD), "Not yet coded", AEDECOD),
      AESTDT = ifelse(is.na(AESTDT) & !is.na(AEDECOD), RFSTDTC, AESTDT),
      AESEV = toupper(AESEV)
    ) %>%
    filter((AESTDT > RFSTDTC) & (AESTDT < (RFENDTC + 5))) %>%
    filter(AESEV == toupper("Mild"))

  expect_equal(actual$dsin, expected)
})

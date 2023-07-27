data(adsl)

test_that("Test Case:1 mentry works with the inputs given and returns the expected items", {
  data_out <- mentry(
    datain = adsl,
    ui_aSubset = "EFFFL=='Y'",
    ui_dSubset = NA,
    ui_byvar = "SEX",
    ui_subgrpvar = "SITEGR1",
    ui_trtvar = "TRT01A",
    ui_trtsort = "TRT01AN",
    ui_trttotalyn = "N",
    ui_sgtotalyn = "N",
    ui_bign = "Y",
    ui_addGrpMiss = "Y",
    ui_pop_fil = "SAFFL"
  )
  # it returns a list with 3 items
  expect_type(data_out, "list")
  expect_equal(length(data_out), 3)

  # it returns dsin, dout, bign
  expect_equal(names(data_out), c("dsin", "dout", "bign"))

  # testing asubset filter
  expect_equal(unique(data_out$dsin$EFFFL), "Y")

  # testing whether bign has 3 variables -  treatment, subgroup, and bign count
  expect_equal(length(data_out$bign), 3)

  # testing population filter
  expect_equal(unique(data_out$dsin$SAFFL), "Y")

  # testing missing logic in byvar and subgrpvar
  expect_false(unique(data_out$dsin$BYVAR1 == ""))
  expect_false(unique(data_out$dsin$SUBGRPVAR1 == ""))

  # after getting dout, it is filtered with asubset hence checking whether dsin has less rows than
  # dout
  expect_true(nrow(data_out$dsin) < nrow(data_out$dout))
})

test_that("Test Case:2 byvar and byvarn check", {
  data_out <- mentry(
    datain = adsl,
    ui_aSubset = "EFFFL=='Y'",
    ui_dSubset = NA,
    ui_byvar = "SEX,ETHNIC",
    ui_subgrpvar = "SITEGR1,BMIBLGR1",
    ui_trtvar = "TRT01A",
    ui_trtsort = "TRT01AN",
    ui_trttotalyn = "N",
    ui_sgtotalyn = "N",
    ui_bign = "Y",
    ui_addGrpMiss = "N",
    ui_pop_fil = "SAFFL"
  )
  byvars_check <- c(
    "BYVAR1", "BYVAR1N", "BYVAR2", "BYVAR2N", "SUBGRPVAR1",
    "SUBGRPVAR1N", "SUBGRPVAR2", "SUBGRPVAR2N"
  )
  expect_equal(names(data_out$dsin), names(data_out$dout))
  expect_true(isTRUE(all(byvars_check %in% names(data_out$dsin))))
})

test_that("Test Case: 3 bign conditions check when ui_bign = 'N'", {
  data_out <- mentry(
    datain = adsl,
    ui_aSubset = "EFFFL=='Y'",
    ui_dSubset = NA,
    ui_byvar = "SEX,ETHNIC",
    ui_subgrpvar = "SITEGR1,BMIBLGR1",
    ui_trtvar = "TRT01A",
    ui_trtsort = "TRT01AN",
    ui_trttotalyn = "N",
    ui_sgtotalyn = "N",
    ui_bign = "N",
    ui_addGrpMiss = "N",
    ui_pop_fil = "SAFFL"
  )
  expect_equal(length(data_out), 3)
  expect_equal(data_out$bign, NA)
})

test_that("Test Case: 4 bign conditions check when ui_bign = 'Y'", {
  data_out <- mentry(
    datain = adsl,
    ui_aSubset = "EFFFL=='Y'",
    ui_dSubset = NA,
    ui_byvar = "SEX,ETHNIC",
    ui_subgrpvar = NA,
    ui_trtvar = NA,
    ui_trtsort = NA,
    ui_trttotalyn = "N",
    ui_sgtotalyn = "N",
    ui_bign = "Y",
    ui_addGrpMiss = "N",
    ui_pop_fil = "SAFFL"
  )

  expect_equal(data_out$bign, nrow(adsl))
})

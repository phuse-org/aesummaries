# Test Utils functions

data("adae")
data("adsl")
data("ae_pre")

## data_attrib() testing:

test_that("Case 1: data_attrib works as expected", {
  # Labels for adae data:
  adae_lab <- data_attrib(adae)
  expect_is(adae_lab, "data.frame")
  expect_equal(adae_lab$VAR_NAMES, toupper(colnames(adae)))
  expect_equal(
    adae_lab$VAR_LABEL,
    unlist(lapply(names(adae), function(x) attributes(adae[[x]])$label))
  )
})

## reverselog_trans testing
test_that("Case 1: Transformation works with expected input", {
  # Labels for adae data:
  trans2 <- reverselog_trans(2)
  expect_is(trans2, "trans")
  expect_equal(trans2$name, "reverselog-2")
})

## var_start testing

test_that("Case 1: Pattern works with expected input", {
  age_start <- var_start(adsl, "AGE")
  expect_equal(age_start, c("AGE", "AGEGR1", "AGEU"))
})

test_that("Case 2: With or without 'N'", {
  # Labels for adae data:
  byno_n <- var_start(ae_pre$dsin, "BYVAR")
  expect_equal(byno_n, "BYVAR1")
  by_n <- var_start(ae_pre$dsin, "BYVARN")
  expect_equal(by_n, "BYVAR1N")
  agen_start <- var_start(adsl, "AGEGRN")
  expect_equal(agen_start, c("AGEGR1N"))
})

## g_seriescol testing ##

test_that("Case 1: Works with expected input", {
  trt_cols <- g_seriescol(ae_pre$dsin, "red,cyan,forestgreen,black,pink,green", "TRTVAR")

  # Correct number of levels and colors assigned
  expect_equal(unname(trt_cols), c("red", "cyan", "forestgreen", "black"))
  # Names as expected
  expect_named(
    trt_cols,
    c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total")
  )
})

test_that("Case 2: Works with default", {
  trt_na <- g_seriescol(ae_pre$dsin, NA, "TRTVAR")
  # Default colors:

  expect_equal(
    unname(trt_na),
    c("firebrick2", "blue4", "forestgreen", "gold")
  )
  expect_named(
    trt_na,
    c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total")
  )
})

test_that("Case 3: Works with character column", {
  trt_ch <- g_seriescol(ae_pre$dsin, NA, "SEX")
  # Colors:
  expect_equal(
    unname(trt_ch),
    c("firebrick2", "blue4")
  )
  expect_named(
    trt_ch,
    c("F", "M")
  )
})

## g_seriessym ##

test_that("Case 1: Works with expected input", {
  trt_shp <- g_seriessym(
    ae_pre$dsin,
    "triangle,circle,square,asterisk", "TRTVAR"
  )

  # Correct number of levels and colors assigned
  expect_equal(unname(trt_shp), c(2, 1, 0, 8))
  # Names as expected
  expect_named(
    trt_shp,
    c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total")
  )

  # Numeric Input:
  trt_shp1 <- g_seriessym(
    ae_pre$dsin,
    c(1, 21, 23, 3), "TRTVAR"
  )
  expect_equal(unname(trt_shp1), c(1, 21, 23, 3))
  expect_named(
    trt_shp1,
    c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total")
  )
})

test_that("Case 2: Works with default", {
  trt_nashp <- g_seriessym(ae_pre$dsin, NA, "TRTVAR")
  # Default colors:

  expect_equal(
    unname(trt_nashp),
    c(16, 17, 15, 1)
  )
  expect_named(
    trt_nashp,
    c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total")
  )
})

test_that("Case 3: Works with character column", {
  trt_chshp <- g_seriessym(ae_pre$dsin, NA, "SEX")
  # Colors:
  expect_equal(
    unname(trt_chshp),
    c(16, 17)
  )
  expect_named(
    trt_chshp,
    c("F", "M")
  )
})

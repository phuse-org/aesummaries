data("adae")
data("adsl")
# data_read default test
test_that("Case 1: Works with Default", {
  test_adam <- data_read(
    ui_data_source = "Default",
    ui_adam_data = "adae,adsl"
  )
  expect_named(test_adam, c("adam", "adam_attrib"))
  # Adam datasets read
  expect_named(test_adam$adam, c("adae", "adsl"))

  # Read Correctly
  expect_equal(test_adam$adam$adae, adae)
  # Labels read
  expect_equal(test_adam$adam_attrib$adsl$VAR_NAMES, names(adsl))
})

## Source: Local, path = package path

test_that("Case 2: Works with Path", {
  test_adsl <- data_read(
    ui_data_source = "Local",
    ui_adam_data = list(datapath = "testdata/adsl.rda", name = "adsl.rda")
  )
  expect_named(test_adsl, c("adam", "adam_attrib"))
  expect_equal(test_adsl$adam$adsl, adsl)

  expect_equal(test_adsl$adam_attrib$adsl$VAR_NAMES, names(adsl))
})

test_that("Case 3: NULL Path", {
  test_null <- data_read(
    ui_data_source = "Local",
    ui_adam_data = NULL
  )
  expect_null(test_null)
})

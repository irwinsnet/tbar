library(testthat)
library(tbar)


test_that("GetStatus", {
  status <- GetStatus()
  expect_type(status, "list")
  expect_true("current_season" %in% names(status))
  expect_true(length(status) > 5)
})

test_that("GetDistricts", {
  districts <- GetDistricts("2021")
  expect_true(tibble::is_tibble(districts))

})

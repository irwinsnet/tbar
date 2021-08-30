library(testthat)
library(tbar)

testthat("Argument Classification", {
  expect_equal(.ClassifyArg("frc1"), c("{team_key}" = "frc2"))
  expect_equal(1, 2)
}
)

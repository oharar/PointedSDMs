context("SolTin_covariates")

test_that("SolTin_covariates is correct", {
  #  skip_on_cran()
  data("SolTin_covariates")

  expect_is(SolTin_covariates, "data.frame")
  expect_equal(names(SolTin_covariates),  c("X", "Y", "Forest", "NPP", "Altitude"))
  expect_equal(dim(SolTin_covariates),  c(7152, 5))
})

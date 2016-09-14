context("covariates")

test_that("covariates data are OK", {
  #  skip_on_cran()
  data("SolTin_covariates")

# Choice of row 5 is arbitrary
  Row5 <- structure(list(X = -56.375, Y = -26.618063, Forest = 52.49803922, NPP = 8476.479412, Altitude = 91.02156863),
                    .Names = c("X", "Y", "Forest", "NPP", "Altitude"), row.names = 5L, class = "data.frame")
  expect_is(SolTin_covariates, "data.frame")
  # check summary
  expect_equal(dim(SolTin_covariates), c(7152,5))
  expect_equal(names(SolTin_covariates), c("X", "Y", "Forest", "NPP", "Altitude"))
  expect_equal(SolTin_covariates[5,], Row5)
})

context("GetNearestCovariate")

test_that("GetNearestCovariate works correctly", {
  #  skip_on_cran()
  data("SolTin_covariates")
  data("SolTin_ebird")
  NearestCovs <- GetNearestCovariate(points=ebird, covs=covariates)

  expect_is(NearestCovs, "SpatialPointsDataFrame")
  expect_equal(names(NearestCovs),  c("Forest", "NPP", "Altitude"))

  expect_equal(as.numeric(NearestCovs@data[1,]), c(78.43434, 17889.67273, 554.0293), tolerance=1.0e-5)
})

context("AddDistToRangeToSpatialPoints")

test_that("AddDistToRangeToSpatialPoints works correctly", {
  #  skip_on_cran()
  data("SolTin_covariates")
  data("SolTin_ebird")
  load("range.RData")
  load("covariates.RData")
  NearestCovs <- GetNearestCovariate(points=SolTin_ebird, covs=covariates)
  RangeAdded <- AddDistToRangeToSpatialPoints(data = NearestCovs, polynoms = range, scale=FALSE)

  expect_is(RangeAdded, "SpatialPointsDataFrame")
  expect_equal(ncol(RangeAdded@data)-ncol(NearestCovs@data),  1)
  expect_equal(sum(RangeAdded@data$DistToPoly1==0),  303)
})

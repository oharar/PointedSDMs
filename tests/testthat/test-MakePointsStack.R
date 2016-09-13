context("MakePointsStack")

test_that("MakePointsStack works correctly", {
  #  skip_on_cran()
  load("tests/testthat/ebird.RData")
  load("tests/testthat/covariates.RData")
  load("tests/testthat/region.RData")

  Meshpars <- list(cutoff=0.8, max.edge=c(1, 3), offset=c(1,1))
  Mesh <- MakeSpatialRegion(data=NULL, bdry=region,
                            meshpars=Meshpars, proj = CRS("+proj=longlat +ellps=WGS84"))

  stk.eBird <- MakePointsStack(presences=ebird, data=covariates, mesh=Mesh$mesh,
                               polynoms = range, tag='ebird', InclCoords=TRUE)


  expect_is(stk.eBird, "inla.data.stack")
  expect_equal(length(stk.eBird),  3)
  expect_equal(names(stk.eBird),  c("A", "data", "effects"))
  expect_equal(stk.eBird$data$nrow,  length(ebird))
})

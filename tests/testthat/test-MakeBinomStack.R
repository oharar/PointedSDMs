context("MakeBinomStack")

test_that("MakeBinomStack works correctly", {
  #  skip_on_cran()
  load("tests/testthat/parks.RData")
  load("tests/testthat/covariates.RData")
  load("tests/testthat/region.RData")
  load("tests/testthat/range.RData")

  Meshpars <- list(cutoff=0.8, max.edge=c(1, 3), offset=c(1,1))
  Mesh <- MakeSpatialRegion(data=NULL, bdry=region,
                            meshpars=Meshpars, proj = CRS("+proj=longlat +ellps=WGS84"))
  stk.parks <- MakeBinomStack(observs=parks, data=covariates, mesh=Mesh$mesh, presname='Present',
                              polynoms = range, tag='parks', InclCoords=TRUE)

  expect_is(stk.parks, "inla.data.stack")
  expect_equal(length(stk.parks),  3)
  expect_equal(names(stk.parks),  c("A", "data", "effects"))
  expect_equal(stk.parks$data$nrow,  length(parks))
})

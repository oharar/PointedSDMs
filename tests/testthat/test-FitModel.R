context("FitModel")

test_that("FitModel works correctly", {
  #  skip_on_cran()
  load("tests/testthat/ebird.RData")
  load("tests/testthat/parks.RData")
  load("tests/testthat/covariates.RData")
  load("tests/testthat/range.RData")
  load("tests/testthat/region.RData")

  Meshpars <- list(cutoff=0.8, max.edge=c(1, 3), offset=c(1,1))
  Mesh <- MakeSpatialRegion(data=NULL, bdry=region,
                            meshpars=Meshpars, proj = CRS("+proj=longlat +ellps=WGS84"))

  stk.ip <- MakeIntegrationStack(mesh=Mesh$mesh, data=covariates, area=Mesh$w,
                                 tag='ip', InclCoords=TRUE)
  stk.ip.dists <- AddDistToRangeToStack(in.stk=stk.ip, coords=c("X", "Y"),
                                        polynoms = range, scale=FALSE)
  stk.eBird <- MakePointsStack(presences=ebird, data=covariates, mesh=Mesh$mesh,
                               polynoms = range, tag='ebird', InclCoords=TRUE)
  stk.parks <- MakeBinomStack(observs=parks, data=covariates, mesh=Mesh$mesh, presname='Present',
                              polynoms = range, tag='parks', InclCoords=TRUE)


  SolTinModel <- FitModel(stk.eBird, stk.ip.dists, stk.parks, CovNames=NULL, mesh = Mesh$mesh, predictions = FALSE)
  Summ <- summary(SolTinModel)$fixed

  expect_is(SolTinModel, "inla")
# check summary
  expect_equal(nrow(Summ), 9)
  expect_equal(as.vector(Summ[,"mean"]), c(-0.0017, 0.0000, -0.0001, 1.3722, 0.3523, -0.4225, 0.0044, 0.0051, -0.3272),
               tolerance=1e-4)
})

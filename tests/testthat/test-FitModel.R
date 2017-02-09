context("FitModel")

test_that("FitModel works correctly", {
  #  skip_on_cran()
  load("ebird.RData")
  load("parks.RData")
  load("covariates.RData")
  load("range.RData")
  load("region.RData")

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
# test formula
  formula2 <- formula(SolTin.form <- resp ~ 0 + Forest + NPP + Altitude + int.ebird + DistToPoly1 +
    Intercept + X + Y + int.parks)
  SolTinModel2 <- FitModel(stk.eBird, stk.ip.dists, stk.parks, formula = formula2, spat.ind="i",
                           CovNames=NULL, mesh = Mesh$mesh, predictions = FALSE)
  Summ2 <- summary(SolTinModel2)$fixed

  expect_is(SolTinModel, "inla")
# check summary
  expect_equal(nrow(Summ), 9)
  expect_equal(as.vector(Summ[,"mean"]), c(-0.0017, 0, -1e-04, 1.3867, 0.319, -0.3539, 0.0043, 0.0064, -0.294),
               tolerance=1e-4)
  expect_equal(Summ, Summ2, tolerance=1e-4)
})

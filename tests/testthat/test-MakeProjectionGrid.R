context("MakeProjectionGrid")

test_that("MakeProjectionGrid works correctly", {
  #  skip_on_cran()
  load("region.RData")
  load("covariates.RData")

  Meshpars <- list(cutoff=0.8, max.edge=c(1, 3), offset=c(1,1))
  Mesh <- MakeSpatialRegion(data=NULL, bdry=region,
                            meshpars=Meshpars, proj = CRS("+proj=longlat +ellps=WGS84"))
  Nxy.scale <- 1 # use this to change the resolution of the predictions
  Boundary <- Mesh$mesh$loc[Mesh$mesh$segm$int$idx[,2],] # get the boundary of the region
  Nxy <- round(c(diff(range(Boundary[,1])), diff(range(Boundary[,2])))/Nxy.scale)
  stk.pred <- MakeProjectionGrid(nxy=Nxy, mesh=Mesh$mesh, data=covariates,
                                tag='pred', boundary=Boundary)

  expect_is(stk.pred, "list")
  expect_equal(length(stk.pred),  3)
  expect_equal(names(stk.pred),  c("stk", "xy.in", "predcoords"))

  expect_is(stk.pred$stk, "inla.data.stack")
  expect_equal(names(stk.pred$stk),  c("A", "data", "effects"))
  expect_equal(stk.pred$stk$A@Dim, c(446,964))

  expect_is(stk.pred$xy.in, "logical")
  expect_equal(length(stk.pred$xy.in), 930)


  expect_is(stk.pred$predcoords, "matrix")
  expect_equal(dim(stk.pred$predcoords),  c(446, 2))
  expect_equal(colnames(stk.pred$predcoords),  c("X", "Y"))
})

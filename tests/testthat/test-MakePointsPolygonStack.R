context("MakePointsPolygonStack")

test_that("MakePointsPolygonStack works correctly", {
  #  skip_on_cran()
  data("SolTin_covariates")
  load("parks_polygons.RData")
  load("region.RData")
#  load("covariates.RData")

  Meshpars <- list(cutoff=0.8, max.edge=c(1, 3), offset=c(1,1))
  Mesh <- MakeSpatialRegion(data=NULL, bdry=region,
                            meshpars=Meshpars, proj = CRS("+proj=longlat +ellps=WGS84"))

  Parks <- MakePointsPolygonStack(polys=parks.polygons, data=SolTin_covariates,
                                  coords=c("X","Y"), mesh=Mesh$mesh)


  expect_is(Parks, "inla.data.stack")
  expect_equal(Parks$data$nrow, 26)
  expect_equal(Parks$data$data$Ntrials, rep(1, Parks$data$nrow))
})

context("MakeSpatialRegion")

test_that("MakeSpatialRegion works correctly", {
  #  skip_on_cran()
  data("SolTin_polygon")
  Pgon <- Polygons(list(region=Polygon(coords=as.matrix(SolTin_polygon))), ID="region")
  region.polygon <- SpatialPolygons(list(Pgon), proj4string = CRS("+proj=longlat +ellps=WGS84"))

  Meshpars <- list(cutoff=0.8, max.edge=c(1, 3), offset=c(1,1))
  Mesh <- MakeSpatialRegion(data=NULL, bdry=region.polygon,
                            meshpars=Meshpars, proj = CRS("+proj=longlat +ellps=WGS84"))

  expect_equal(names(Mesh),  c("mesh", "spde", "w"))
  expect_is(Mesh$mesh, "inla.mesh")
  expect_is(Mesh$spde, "inla.spde2")
  expect_is(Mesh$spde, "inla.spde")
  expect_is(Mesh$spde, "inla.model.class")
  expect_is(Mesh$w, "numeric")
#  Mesh$mesh
  expect_equal(Mesh$mesh$n,  595)
#  Mesh$spde
  expect_equal(Mesh$spde$n.spde,  Mesh$mesh$n)
#  Mesh$w
  expect_equal(length(Mesh$w),  Mesh$mesh$n)
})

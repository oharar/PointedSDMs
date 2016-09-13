context("AddDistToRangeToStack")

test_that("AddDistToRangeToStack works correctly", {
  #  skip_on_cran()
  load("tests/testthat/region.RData")

  Meshpars <- list(cutoff=0.8, max.edge=c(1, 3), offset=c(1,1))

  Mesh <- MakeSpatialRegion(data=NULL, bdry=region, meshpars=Meshpars,
                            proj = CRS("+proj=longlat +ellps=WGS84"))
  stk.ip <- MakeIntegrationStack(mesh=Mesh$mesh, data=Covariates, area=Mesh$w,
                                 tag='ip', InclCoords=TRUE)
  stk.ip.dists <- AddDistToRangeToStack(in.stk=stk.ip, coords=c("X", "Y"),
                                        polynoms = range, scale=FALSE)


  expect_equal(names(stk.ip.dists),  c("A", "data", "effects"))
  expect_equal(stk.ip.dists$A@Dim, c(595,1190))

  expect_equal(stk.ip.dists$data$nrow,  595)
  expect_equal(sum(is.na(stk.ip.dists$data$data$resp.1)),  0)
  expect_equal(sum(is.na(stk.ip.dists$data$data$resp.2)),  595)
  expect_equal(length(stk.ip.dists$effects$ncol),  8)
  expect_equal(stk.ip.dists$effects$names[[8]], "DistToPoly1")
})

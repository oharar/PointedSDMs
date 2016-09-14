context("MakeIntegrationStack")

test_that("MakeIntegrationStack works correctly", {
  #  skip_on_cran()
  load("region.RData")
  load("covariates.RData")

  Meshpars <- list(cutoff=0.8, max.edge=c(1, 3), offset=c(1,1))
  Mesh <- MakeSpatialRegion(data=NULL, bdry=region,
                            meshpars=Meshpars, proj = CRS("+proj=longlat +ellps=WGS84"))
  stk.ip <- MakeIntegrationStack(mesh=Mesh$mesh, data=covariates, area=Mesh$w,
                                 tag='ip', InclCoords=TRUE)

  expect_is(stk.ip, "inla.data.stack")
  expect_equal(names(stk.ip),  c("A", "data", "effects"))
  expect_equal(stk.ip$A@Dim, c(595,1190))

  expect_equal(stk.ip$data$nrow,  595)
  expect_equal(sum(is.na(stk.ip$data$data$resp.1)),  0)
  expect_equal(sum(is.na(stk.ip$data$data$resp.2)),  595)
  expect_equal(length(is.na(stk.ip$effects$ncol)),  7)
})

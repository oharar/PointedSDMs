context("SolTin_polygon")

test_that("SolTin_polygon is correct", {
  #  skip_on_cran()
  data("SolTin_polygon")

  expect_is(SolTin_polygon, "data.frame")
  expect_equal(names(SolTin_polygon),  c("X", "Y"))
  expect_equal(dim(SolTin_polygon),  c(115, 2))

})

context("polygon")

test_that("polygon data are OK", {
  #  skip_on_cran()
  data("SolTin_polygon")

  # Choice of row 5 is arbitrary
  Row5 <- structure(list(X = -59.7624990079937, Y = -35.1067028718932),
                    .Names = c("X", "Y"), row.names = 5L, class = "data.frame")
  expect_is(SolTin_polygon, "data.frame")
  # check summary
  expect_equal(dim(SolTin_polygon), c(115,2))
  expect_equal(names(SolTin_polygon), c("X", "Y"))
  expect_equal(SolTin_polygon[5,], Row5)
})

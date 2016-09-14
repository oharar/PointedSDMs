context("ebird")

test_that("ebird data are OK", {
  #  skip_on_cran()
  data("SolTin_ebird")

  # Choice of row 5 is arbitrary
  Row5 <- structure(list(X = -47.0212, Y = -24.3836), .Names = c("X", "Y"), row.names = 5L, class = "data.frame")
  expect_is(SolTin_ebird, "data.frame")
  # check summary
  expect_equal(dim(SolTin_ebird), c(315,2))
  expect_equal(names(SolTin_ebird), c("X", "Y"))
  expect_equal(SolTin_ebird[5,], Row5)
})

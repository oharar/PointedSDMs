context("range")

test_that("range data are OK", {
  #  skip_on_cran()
  data("SolTin_range")

  # Choice of row 5 is arbitrary
  Row5 <- structure(list(X = -48.3625001482669, Y = -25.9567037505888),
                    .Names = c("X", "Y"), row.names = 5L, class = "data.frame")
  expect_is(SolTin_range, "data.frame")
  # check summary
  expect_equal(dim(SolTin_range), c(66,2))
  expect_equal(names(SolTin_range), c("X", "Y"))
  expect_equal(SolTin_range[5,], Row5)
})

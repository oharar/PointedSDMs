context("gbif")

test_that("gbif data are OK", {
  #  skip_on_cran()
  data("SolTin_gbif")

  # Choice of row 5 is arbitrary
  Row5 <- structure(list(X = -55.73333, Y = -26.11667),
                    .Names = c("X", "Y"), row.names = 12L, class = "data.frame")
  expect_is(SolTin_gbif, "data.frame")
  # check summary
  expect_equal(dim(SolTin_gbif), c(63,2))
  expect_equal(names(SolTin_gbif), c("X", "Y"))
  expect_equal(SolTin_gbif[5,], Row5)
})

context("parks")

test_that("parks data are OK", {
  #  skip_on_cran()
  data("SolTin_parks")

  # Choice of row 5 is arbitrary
  Row5 <- structure(list(X = -40.625, Y = -19.813369, area = 0.0453750068466116, Present = FALSE),
                    .Names = c("X", "Y", "area", "Present"), row.names = 5L, class = "data.frame")
  expect_is(SolTin_parks, "data.frame")
  # check summary
  expect_equal(dim(SolTin_parks), c(26,4))
  expect_equal(names(SolTin_parks), c("X", "Y", "area", "Present"))
  expect_equal(SolTin_parks[5,], Row5)
})

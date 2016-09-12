context("SolTin_parks")

test_that("SolTin_parks is correct", {
  #  skip_on_cran()
  data("SolTin_parks")

  expect_is(SolTin_parks, "data.frame")
  expect_equal(names(SolTin_parks), c("X", "Y", "area", "Present"))
  expect_equal(dim(SolTin_parks),  c(26, 4))
})

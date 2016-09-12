context("SolTin_range")

test_that("SolTin_range is correct", {
  #  skip_on_cran()
  data("SolTin_range")

  expect_is(SolTin_range, "data.frame")
  expect_equal(names(SolTin_range), c("X", "Y"))
  expect_equal(dim(SolTin_range),  c(66, 2))
})

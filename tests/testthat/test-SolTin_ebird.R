context("SolTin_ebird")

test_that("SolTin_ebird is correct", {
  #  skip_on_cran()
  data("SolTin_ebird")

  expect_is(SolTin_ebird, "data.frame")
  expect_equal(names(SolTin_ebird),  c("X", "Y"))
  expect_equal(dim(SolTin_ebird),  c(315, 2))
})

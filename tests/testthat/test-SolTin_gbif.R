context("SolTin_gbif")

test_that("SolTin_gbif is correct", {
  #  skip_on_cran()
  data("SolTin_gbif")

  expect_is(SolTin_gbif, "data.frame")
  expect_equal(names(SolTin_gbif),  c("X", "Y"))
  expect_equal(dim(SolTin_gbif),  c(63, 2))
})

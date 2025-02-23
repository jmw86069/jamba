
testthat::test_that("asSize", {
   # simple test cases
   testthat::expect_equal(
      asSize(1024),
      "1 kb")
   testthat::expect_equal(
      asSize(1024*1024),
      "1 Mb")
   testthat::expect_equal(
      asSize(1000000),
      "977 kb")
   testthat::expect_equal(
      asSize(1000000, kiloSize=1000, unitType=""),
      "1 M")
   testthat::expect_equal(
      asSize(1000000 * 3.56789, kiloSize=1000, unitType=""),
      "3.57 M")
   testthat::expect_equal(
      asSize(1000000 * 3.56789),
      "3.4 Mb")
   set.seed(123)
   testthat::expect_equal(
      asSize(list(rnorm(1000))),
      "7.91 kb")
   # more arguments
   testthat::expect_equal(
      asSize(1000000 * 3.56789, abbreviateUnits=FALSE),
      "3.4 Megabytes")

})

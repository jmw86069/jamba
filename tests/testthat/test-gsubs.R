

testthat::test_that("gsubs", {
   x <- c("one", "two", "three", "four");
   pattern <- c("one", "four")
   replacement <- c("ONE", "FOUR")
   testthat::expect_equal(
      gsubs(pattern, replacement, x),
      c("ONE", "two", "three", "FOUR"))

   testthat::expect_equal(
      gsubs(pattern, replacement, as.list(x)),
      list("ONE", "two", "three", "FOUR"))
})


testthat::test_that("gsubOrdered", {
   x <- c("one", "two", "three", "four");
   x_factor <- factor(x, levels=x)
   testthat::expect_equal(
      gsubOrdered("two", "TWO", x_factor),
      factor(c("one", "TWO", "three", "four"),
         levels=c("one", "TWO", "three", "four")))

   testthat::expect_equal(
      gsubOrdered("two", "one", x_factor),
      factor(c("one", "one", "three", "four"),
         levels=c("one", "three", "four")))
})

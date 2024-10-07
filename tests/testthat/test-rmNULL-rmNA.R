
x <- list(A=1:6, B=NULL, C=letters[11:16]);
x1 <- x[c(1, 3)];

x2 <- x;
x2[[2]] <- NA;

testthat::test_that("rmNULL", {
   testthat::expect_equal(
      rmNULL(x),
      x1)
   testthat::expect_equal(
      rmNULL(x, nullValue=NA),
      x2)
})


testthat::test_that("rmNA", {
   # set up test data
   x <- c(1:10, NA, 20:25, Inf, -Inf);
   names(x) <- LETTERS[seq_along(x)]
   x5 <- x[c(1:10, 12:17)]
   x6 <- c(x[1:10], -1, x[12:17])
   names(x6) <- names(x)[c(1:17)]
   x7 <- c(x[1:10], -1, x[12:17], 1000, -1000)
   names(x7) <- names(x)[c(1:19)]
   x8 <- x;
   names(x8)[7] <- NA
   x9 <- x8[c(-7, -11, -18, -19)];

   testthat::expect_equal(
      rmNA(x),
      x5)
   testthat::expect_equal(
      rmNA(x, naValue=-1),
      x6)
   testthat::expect_equal(
      rmNA(x, naValue=-1, infiniteValue=1000),
      x7)
   testthat::expect_equal(
      rmNA(x8, rmNAnames=TRUE),
      x9)
})


testthat::test_that("rmInfinite", {
   x <- c(1:10, NA, 20:25, Inf, -Inf);
   names(x) <- LETTERS[seq_along(x)]
   testthat::expect_equal(
      rmInfinite(x),
      x[1:17])
})

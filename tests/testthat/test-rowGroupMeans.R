

testthat::test_that("rowGroupMeans", {
   x <- matrix(1:25, ncol=5);
   rownames(x) <- letters[1:5];
   colnames(x) <- LETTERS[1:5];
   x[, 5] <- x[, 5] + 1;
   groups <- rep(c("group1", "group2"), c(2, 3));

   # median by default
   testthat::expect_equal(
      as.vector(rowGroupMeans(x, groups=groups, includeAttributes=FALSE)),
      c(3.5, 4.5, 5.5, 6.5, 7.5, 16, 17, 18, 19, 20))
   testthat::expect_equal(
      rownames(rowGroupMeans(x, groups=groups, includeAttributes=FALSE)),
      letters[1:5])
   testthat::expect_equal(
      colnames(rowGroupMeans(x, groups=groups, includeAttributes=FALSE)),
      c("group1", "group2"))

   # mean
   testthat::expect_equal(
      round(digits=2,
         as.vector(rowGroupMeans(x, groups=groups,
            useMedian=FALSE, includeAttributes=FALSE))),
      c(3.5, 4.5, 5.5, 6.5, 7.5, 16.33, 17.33, 18.33, 19.33, 20.33))

   # groups with ordered factor levels
   groups_o <- factor(groups, levels=c("group2", "group1"))
   testthat::expect_equal(
      colnames(rowGroupMeans(x, groups=groups_o, includeAttributes=FALSE)),
      c("group2", "group1"))

   # test one NA value in a group
   xNA <- x;
   xNA[1, 3] <- NA;
   testthat::expect_equal(
      as.vector(rowGroupMeans(xNA, groups=groups, includeAttributes=FALSE)),
      c(3.5, 4.5, 5.5, 6.5, 7.5, 19, 17, 18, 19, 20))

   # test one NA value in a group using mean
   testthat::expect_equal(
      round(digits=2,
         as.vector(rowGroupMeans(xNA, groups=groups,
            useMedian=FALSE, includeAttributes=FALSE))),
      c(3.5, 4.5, 5.5, 6.5, 7.5, 19, 17.33, 18.33, 19.33, 20.33))

   # test entire group NA
   xNAg <- x;
   xNAg[1, 3:5] <- NA;
   testthat::expect_equal(
      as.vector(rowGroupMeans(xNAg, groups=groups, includeAttributes=FALSE)),
      c(3.5, 4.5, 5.5, 6.5, 7.5, NaN, 17, 18, 19, 20))

   # entire group NA using mean
   testthat::expect_equal(
      round(digits=2,
         as.vector(rowGroupMeans(xNAg, groups=groups,
            useMedian=FALSE, includeAttributes=FALSE))),
      c(3.5, 4.5, 5.5, 6.5, 7.5, NaN, 17.33, 18.33, 19.33, 20.33))
})


testthat::test_that("rowGroupMeans SparseMatrix", {
   skip_if_not_installed("Matrix")

   x <- matrix(1:25, ncol=5);
   rownames(x) <- letters[1:5];
   colnames(x) <- LETTERS[1:5];
   x[, 5] <- x[, 5] + 1;
   x <- Matrix::Matrix(x, sparse=TRUE);
   groups <- rep(c("group1", "group2"), c(2, 3));

   # median by default
   testthat::expect_equal(
      as.vector(rowGroupMeans(x, groups=groups, includeAttributes=FALSE)),
      c(3.5, 4.5, 5.5, 6.5, 7.5, 16, 17, 18, 19, 20))
   testthat::expect_equal(
      rownames(rowGroupMeans(x, groups=groups, includeAttributes=FALSE)),
      letters[1:5])
   testthat::expect_equal(
      colnames(rowGroupMeans(x, groups=groups, includeAttributes=FALSE)),
      c("group1", "group2"))

   # mean
   testthat::expect_equal(
      round(digits=2,
         as.vector(rowGroupMeans(x, groups=groups,
            useMedian=FALSE, includeAttributes=FALSE))),
      c(3.5, 4.5, 5.5, 6.5, 7.5, 16.33, 17.33, 18.33, 19.33, 20.33))

   # test one NA value in a group
   xNA <- x;
   xNA[1, 3] <- NA;
   testthat::expect_equal(
      as.vector(rowGroupMeans(xNA, groups=groups, includeAttributes=FALSE)),
      c(3.5, 4.5, 5.5, 6.5, 7.5, 19, 17, 18, 19, 20))

   # test one NA value in a group using mean
   testthat::expect_equal(
      round(digits=2,
         as.vector(rowGroupMeans(xNA, groups=groups,
            useMedian=FALSE, includeAttributes=FALSE))),
      c(3.5, 4.5, 5.5, 6.5, 7.5, 19, 17.33, 18.33, 19.33, 20.33))

   # test entire group NA
   xNAg <- x;
   xNAg[1, 3:5] <- NA;
   testthat::expect_equal(
      as.vector(rowGroupMeans(xNAg, groups=groups, includeAttributes=FALSE)),
      c(3.5, 4.5, 5.5, 6.5, 7.5, NaN, 17, 18, 19, 20))

   # entire group NA using mean
   testthat::expect_equal(
      round(digits=2,
         as.vector(rowGroupMeans(xNAg, groups=groups,
            useMedian=FALSE, includeAttributes=FALSE))),
      c(3.5, 4.5, 5.5, 6.5, 7.5, NaN, 17.33, 18.33, 19.33, 20.33))

})

testthat::test_that("rowGroupMeans custom rowStatsFunc", {
   x <- matrix(1:25, ncol=5);
   rownames(x) <- letters[1:5];
   colnames(x) <- LETTERS[1:5];
   x[, 5] <- x[, 5] + 1;
   groups <- rep(c("group1", "group2"), c(2, 3));

   # rowStatsFunc=rowSums
   testthat::expect_equal(
      as.vector(rowGroupMeans(x, groups=groups,
         rowStatsFunc=rowSums, includeAttributes=FALSE)),
      c(7, 9, 11, 13, 15, 49, 52, 55, 58, 61))

   # with ellipses
   testthat::expect_equal(
      as.vector(rowGroupMeans(x, groups=groups,
         dummy=1,
         rowStatsFunc=rowSums, includeAttributes=FALSE)),
      c(7, 9, 11, 13, 15, 49, 52, 55, 58, 61))

   # with ellipses, useMedian=TRUE
   testthat::expect_equal(
      as.vector(rowGroupMeans(x, groups=groups,
         dummy=1, useMedian=TRUE,
         includeAttributes=FALSE)),
      c(3.5, 4.5, 5.5, 6.5, 7.5, 16, 17, 18, 19, 20))

   # with ellipses, useMedian=FALSE
   testthat::expect_equal(
      round(digits=2,
         as.vector(rowGroupMeans(x, groups=groups,
            dummy=1, useMedian=FALSE,
            includeAttributes=FALSE))),
      c(3.5, 4.5, 5.5, 6.5, 7.5, 16.33, 17.33, 18.33, 19.33, 20.33))

})

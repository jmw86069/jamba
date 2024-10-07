

testthat::test_that("rowRmMadOutliers", {
   x <- matrix(1:25, ncol=5);
   rownames(x) <- letters[1:5];
   colnames(x) <- LETTERS[1:5];
   x[, 5] <- x[, 5] + 1;
   x[1, 5] <- 100;
   groups <- rep(c("group1", "group2"), c(2, 3));

   # default outlier detection
   testthat::expect_equal(
      which(is.na(as.vector(rowRmMadOutliers(x)))),
      21)

   # high outlier threshold which does not remove this outlier point
   testthat::expect_equal(
      which(is.na(as.vector(rowRmMadOutliers(x, madFactor=15)))),
      integer(0))

   # rowGroupMeans() with outlier detection
   rowGroupMeans(x, groups=groups, rmOutliers=FALSE, includeAttributes=FALSE)
   rowGroupMeans(x, groups=groups, rmOutliers=TRUE, includeAttributes=FALSE)

   # first establish the value including outlier
   testthat::expect_equal(
      as.vector(rowGroupMeans(x, groups=groups,
         rmOutliers=FALSE, includeAttributes=FALSE)),
      c(3.5, 4.5, 5.5, 6.5, 7.5, 16, 17, 18, 19, 20))

   # remove the outlier
   testthat::expect_equal(
      as.vector(rowGroupMeans(x, groups=groups,
         rmOutliers=TRUE, includeAttributes=FALSE)),
      c(3.5, 4.5, 5.5, 6.5, 7.5, 13.5, 17, 18, 19, 20))

   # mean - first establish the value including outlier
   testthat::expect_equal(
      round(digits=2,
         as.vector(rowGroupMeans(x, groups=groups,
            useMedian=FALSE,
            rmOutliers=FALSE, includeAttributes=FALSE))),
      c(3.5, 4.5, 5.5, 6.5, 7.5, 42.33, 17.33, 18.33, 19.33, 20.33))

   # mean - remove the outlier
   testthat::expect_equal(
      round(digits=2,
         as.vector(rowGroupMeans(x, groups=groups,
            useMedian=FALSE,
            rmOutliers=TRUE, includeAttributes=FALSE))),
      c(3.5, 4.5, 5.5, 6.5, 7.5, 13.5, 17.33, 18.33, 19.33, 20.33))

   # mean - remove the outlier, return the data
   xTest <- x;
   xTest[1, 5] <- NA;
   testthat::expect_equal(
      rowGroupMeans(x, groups=groups,
         useMedian=FALSE, returnType="input",
         rmOutliers=TRUE, includeAttributes=FALSE),
      xTest)

})

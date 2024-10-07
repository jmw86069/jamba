
testthat::test_that("rbindList atomic vectors", {
   # define test data
   L <- list(a=LETTERS[1:4], b=letters[1:3]);
   m <- matrix(ncol=4,
      c(LETTERS[1:4],
         letters[1:3], ""),
      byrow=TRUE)
   rownames(m) <- c("a", "b");
   # second matrix
   L2 <- list(a=LETTERS[1:4], b=NULL, d=letters[1:3]);
   m2 <- matrix(ncol=4,
      data=c(LETTERS[1:4],
         rep("", 4),
         letters[1:3], ""),
      byrow=TRUE)
   rownames(m2) <- c("a", "b", "d");

   # tests
   testthat::expect_equal(
      rbindList(L),
      m)
   testthat::expect_equal(
      rbindList(L2),
      m2[c(1, 3), , drop=FALSE])
   testthat::expect_equal(
      rbindList(L2, nullValue=""),
      m2)
})


testthat::test_that("rbindList list, matrix, data.frame", {
   # define test data
   # define test data
   L <- list(a=LETTERS[1:4], b=letters[1:3]);
   m <- matrix(ncol=4,
      c(LETTERS[1:4],
         letters[1:3], ""),
      byrow=TRUE)
   rownames(m) <- c("a", "b");
   # second matrix
   L2 <- list(a=LETTERS[1:4], b=NULL, d=letters[1:3]);
   m2 <- matrix(ncol=4,
      data=c(LETTERS[1:4],
         rep("", 4),
         letters[1:3], ""),
      byrow=TRUE)
   rownames(m2) <- c("a", "b", "d");
   # data.frame
   mm2 <- rbind(m, m2)
   lmm2 <- rbind(L[[1]], mm2)
   df <- data.frame(m,
      stringsAsFactors=FALSE)
   m3 <- m2;
   colnames(m3) <- colnames(df)
   df2 <- data.frame(rbind(m3, df))

   # tests
   testthat::expect_equal(
      rbindList(list(m, m2)),
      mm2)
   # list, matrix combination
   testthat::expect_equal(
      rbindList(c(list(L[[1]]), list(m, m2))),
      lmm2)
   # matrix, data.frame combination
   testthat::expect_equal(
      rbindList(list(m3, df)),
      df2)
})

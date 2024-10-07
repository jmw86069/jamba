

testthat::test_that("makeNames", {
   exons <- makeNames(rep("exon", 3), suffix="");
   exons1 <- paste0("exon", 1:3)
   E <- makeNames(rep(exons, c(2,3,1)), numberStyle="letters", suffix="");
   E1 <- paste0("exon", c(1, 1, 2, 2, 2, 3), c("a", "b", "a", "b", "c", ""))
   # test vector
   V <- rep(LETTERS[1:3], c(2,3,1));
   # expected output vectors
   V1 <- paste0(V,
      rep(c("_v", ""), c(5, 1)),
      c(1, 2, 1, 2, 3, ""))
   V2 <- paste0(V,
      "_v",
      c(1, 2, 1, 2, 3, 1))
   V3 <- paste0(V,
      c("", "_v")[c(1, 2, 1, 2, 2, 1)],
      c("", 1, "", 1, 2, ""))

   testthat::expect_equal(
      makeNames(V),
      V1)
   testthat::expect_equal(
      makeNames(V, renameOnes=TRUE),
      V2)
   testthat::expect_equal(
      makeNames(V, renameFirst=FALSE),
      V3)

   # fancy nested example

})

testthat::test_that("makeNames exon names", {
   # define test data
   # exons <- makeNames(rep("exon", 3), suffix="");
   exons <- paste0("exon", 1:3)
   exons_expanded <- rep(exons, c(2, 3, 1))
   exons_expected <- paste0(
      exons_expanded,
      c("a", "b", "a", "b", "c", ""))
   # tests
   testthat::expect_equal(
      makeNames(rep("exon", 3), suffix=""),
      exons)
   testthat::expect_equal(
      makeNames(exons_expanded, numberStyle="letters", suffix=""),
      exons_expected)
})


testthat::test_that("nameVector", {
   # test data
   x <- LETTERS[1:5]
   xn <- x;
   names(xn) <- x;
   # expand x with repeated values
   x1 <- x[c(1, 1, 2, 3, 2, 4, 5)];
   x1n <- x1;
   names(x1n) <- paste0(x1,
      c("_v1", "_v2", "_v1", "", "_v2", "", ""))
   x1nn <- names(x1n);
   names(x1nn) <- names(x1n);

   # tests
   testthat::expect_equal(
      nameVector(x),
      xn)
   testthat::expect_equal(
      nameVector(x1),
      x1n)
   testthat::expect_equal(
      nameVectorN(x1n),
      x1nn)
})




testthat::test_that("mixedSortDF character", {
   # define test data
   x <- c("miR-12","miR-1","miR-122","miR-1b", "miR-1a","miR-2");
   # add some arbitrary group information
   g <- rep(c("Air", "Treatment", "Control"), 2);
   # create a data.frame
   df <- data.frame(
      group=g,
      miRNA=x,
      stringsAsFactors=FALSE);
   # expected row orders
   dfo <- c(4, 1, 6, 3, 2, 5);
   dfo21 <- c(2, 5, 4, 6, 1, 3);
   dfo102 <- c(1, 4, 3, 6, 2, 5);
   dfo1m2 <- c(1, 4, 3, 6, 5, 2);

   # LOCALE "C"
   withr::with_locale(new=c("LC_COLLATE"="C"),
   {
      testthat::expect_equal(
         mmixedOrder(df),
         dfo)
      testthat::expect_equal(
         mixedSortDF(df),
         df[dfo, , drop=FALSE])
      testthat::expect_equal(
         mixedSortDF(df, byCols=c(2, 1)),
         df[dfo21, , drop=FALSE])
      testthat::expect_equal(
         mixedSortDF(df, byCols=c(1, 0, 2)),
         df[dfo102, , drop=FALSE])
      testthat::expect_equal(
         mixedSortDF(df, byCols=c("group", "rownames", "miRNA")),
         df[dfo102, , drop=FALSE])
      testthat::expect_equal(
         mixedSortDF(df, byCols=c(1, -2)),
         df[dfo1m2, , drop=FALSE])
   })
})


testthat::test_that("mixedSortDF factor", {
   # define test data
   x <- c("miR-12","miR-1","miR-122","miR-1b", "miR-1a","miR-2");
   # add some arbitrary group information
   g <- rep(c("Air", "Treatment", "Control"), 2);
   # create a data.frame
   df <- data.frame(
      group=g,
      miRNA=x,
      stringsAsFactors=FALSE);
   # test data with factor columns
   dff <- df;
   dff$miRNA <- factor(dff$miRNA,
      levels=df$miRNA[c(5, 4, 2, 6, 1, 3)])
   # expected row orders
   dffo <- c(4, 1, 6, 3, 5, 2)
   dffo21 <- c(5, 4, 2, 6, 1, 3);
   dffo1m2 <- c(1, 4, 3, 6, 2, 5);
   dffo1mr2 <- c(4, 1, 6, 3, 5, 2)
   dffo21hfF <- c(2, 5, 4, 6, 1, 3);

   # LOCALE "C"
   withr::with_locale(new=c("LC_COLLATE"="C"),
   {
      testthat::expect_equal(
         mmixedOrder(dff),
         dffo)
      testthat::expect_equal(
         mixedSortDF(dff),
         dff[dffo, , drop=FALSE])
      testthat::expect_equal(
         mixedSortDF(dff, byCols=c(2, 1)),
         dff[dffo21, , drop=FALSE])
      testthat::expect_equal(
         mixedSortDF(dff, byCols=c(2, 1), honorFactor=FALSE),
         dff[dffo21hfF, , drop=FALSE])
      testthat::expect_equal(
         mixedSortDF(dff, byCols=c(1, -2)),
         dff[dffo1m2, , drop=FALSE])
      testthat::expect_equal(
         mixedSortDF(dff, byCols=c("group", "-miRNA")),
         dff[dffo1m2, , drop=FALSE])
      testthat::expect_equal(
         mixedSortDF(dff, byCols=c("group", "-rownames", "miRNA")),
         dff[dffo1mr2, , drop=FALSE])
   })
})

# test NA values
testthat::test_that("mixedSortDF factor, NA, and blank values", {
   # test data with factor and NA value
   # define test data
   x <- c("miR-12","miR-1","miR-122","miR-1b", "miR-1a","miR-2");
   # add some arbitrary group information
   g <- rep(c("Air", "Treatment", "Control"), 2);
   # create a data.frame
   df <- data.frame(
      group=g,
      miRNA=x,
      stringsAsFactors=FALSE);
   # test data with factor columns
   dff <- df;
   dff$miRNA <- factor(dff$miRNA,
      levels=df$miRNA[c(5, 4, 2, 6, 1, 3)])
   # test NA values
   dffNA <- dff;
   dffNA[3, 2] <- NA;
   # expected row orders
   dffNAo <- c(5, 4, 2, 6, 1, 3)
   dffNAoF <- c(3, 5, 4, 2, 6, 1)

   # LOCALE "C"
   withr::with_locale(new=c("LC_COLLATE"="C"),
   {
      testthat::expect_equal(
         mixedSortDF(dffNA, byCols=c(2, 1)),
         dffNA[dffNAo, , drop=FALSE])
      testthat::expect_equal(
         mixedSortDF(dffNA, byCols=c(2, 1), na.last=FALSE),
         dffNA[dffNAoF, , drop=FALSE])
   })
})

# test blank values
testthat::test_that("mixedSortDF blank values", {
   # test data with factor and NA value
   # define test data
   x <- c("miR-12","miR-1","miR-122","miR-1b", "miR-1a","miR-2");
   # add some arbitrary group information
   g <- rep(c("Air", "Treatment", "Control"), 2);
   # create a data.frame
   df <- data.frame(
      group=g,
      miRNA=x,
      stringsAsFactors=FALSE);
   # test blank values
   dffb <- df;
   dffb[1, 2] <- "";
   dffb[3, 2] <- NA;
   dffb$miRNA <- factor(dffb$miRNA,
      levels=dffb$miRNA[c(5, 4, 2, 6, 1)])
   levels(dffb[,2])
   mixedSortDF(dffb, byCols=c(2, 1))
   dffbo <- c(5, 4, 2, 6, 1, 3)
   dffbo_hfF <- c(1, 2, 5, 4, 6, 3);
   dffbo_hfF_bfF <- c(2, 5, 4, 6, 1, 3);

   # test with blank values
   # LOCALE "C"
   withr::with_locale(new=c("LC_COLLATE"="C"),
   {
      testthat::expect_equal(
         mixedSortDF(dffb, byCols=c(2, 1)),
         dffb[dffbo, , drop=FALSE])
      testthat::expect_equal(
         mixedSortDF(dffb, byCols=c(2, 1), honorFactor=FALSE),
         dffb[dffbo_hfF, , drop=FALSE])
      testthat::expect_equal(
         mixedSortDF(dffb, byCols=c(2, 1),
            honorFactor=FALSE, blanksFirst=FALSE),
         dffb[dffbo_hfF_bfF, , drop=FALSE])
   })
})

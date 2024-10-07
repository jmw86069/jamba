

# test when input is a character vector
testthat::test_that("mixedSort character vectors", {
   # miRNA-inspired test vector
   mirna <- c("miR-12","miR-1","miR-122","miR-1b",
      "miR-1a", "MIR-2", "miR-2", "MIR-2",
      "Cnot9", "Cnot8", "CNOT9", "cnot10");
   # expected order "C"
   mo_c <-    c(10, 11, 9, 12, 2,
      5, 4, 6, 8, 7, 1, 3);
   # expected order "en_US.UTF-8"
   mo_utf8 <- c(10, 9, 11, 12, 2,
      5, 4, 7, 6, 8, 1, 3)

   # LOCALE "C"
   withr::with_locale(new=c("LC_COLLATE"="C"),
   {
      testthat::expect_equal(
         mixedSort(mirna),
         mirna[mo_c])
      testthat::expect_equal(
         mixedOrder(mirna),
         mo_c)
   })
})


# test when input is a factor vector
testthat::test_that("mixedSort factor vectors", {
   # miRNA-inspired test vector
   mirna <- c("miR-12","miR-1","miR-122","miR-1b",
      "miR-1a", "MIR-2", "miR-2", "MIR-2",
      "Cnot9", "Cnot8", "CNOT9", "cnot10");
   # expected order "C"
   mo_c <-    c(10, 11, 9, 12, 2,
      5, 4, 6, 8, 7, 1, 3);
   # expected order with honorFactor=TRUE
   mo_c_hfT <- c(9, 11, 10, 6, 7,
      8, 12, 2, 1, 3, 5, 4)
   # expected order with useCaseTiebreak=FALSE
   mo_c_uctF <- c(10, 9, 11, 12, 2, 5,
      4, 6, 7, 8, 1, 3);
   # expected order with useCaseTiebreak=FALSE,ignore.case=FALSE
   mo_c_uctF_icF <- c(11, 10, 9, 6, 8, 12, 2,
      5, 4, 7, 1, 3);

   # LOCALE "C"
   withr::with_locale(new=c("LC_COLLATE"="C"),
   {
      # convert to factor, levels are locale-dependent
      xf <- factor(mirna)

      # LOCALE "C"
      testthat::expect_equal(
         mixedSort(xf),
         xf[mo_c])
      # do not use case-sensitive tiebreaker
      testthat::expect_equal(
         mixedSort(xf, useCaseTiebreak=FALSE),
         xf[mo_c_uctF])
      # also ignore.case=FALSE
      testthat::expect_equal(
         mixedSort(xf, useCaseTiebreak=FALSE, ignore.case=FALSE),
         xf[mo_c_uctF_icF])
      # confirm honorFactor=FALSE is default
      testthat::expect_equal(
         mixedSort(xf, honorFactor=FALSE),
         xf[mo_c])
      testthat::expect_equal(
         mixedSort(xf, honorFactor=TRUE),
         xf[mo_c_hfT])
      # when honorFactor=TRUE, useCaseTiebreak has no effect
      testthat::expect_equal(
         mixedSort(xf, honorFactor=TRUE, useCaseTiebreak=FALSE),
         xf[mo_c_hfT])
      testthat::expect_equal(
         mixedOrder(xf),
         mo_c)
      testthat::expect_equal(
         mixedOrder(xf, honorFactor=FALSE),
         mo_c)
      testthat::expect_equal(
         mixedOrder(xf, honorFactor=TRUE),
         mo_c_hfT)
   })
})


# test mixedSorts() in list context
testthat::test_that("mixedSorts_factor", {
   # define test list with character vectors
   xl <- list(
      a=c("b1", "z2", "p3", "w4", "n5",
         "e6", "n27", "g28", "l29", "e30", "y1", "b2"),
      b=c("e7", "b8", "h9", "r10",
         "r11", "y12", "f13", "b10"))
   # LOCALE "C"
   withr::with_locale(new=c("LC_COLLATE"="C"),
   {
      # define test list by adding one factor vector
      # factor levels are locale-specific
      xlf <- c(xl,
         list(d=factor(c(xl$a, xl$b))))
      # expected sort order for character vectors
      xls <- list(
         a=xl$a[c(1, 12, 6, 10, 8, 9, 5, 7, 3, 4, 11, 2)],
         b=xl$b[c(2, 8, 1, 7, 3, 4, 5, 6)])

      # expected sort order including factor vector, honorFactor=FALSE
      d_order <- c(1, 12, 14, 20, 6,
         13, 10, 19, 8, 15, 9, 5,
         7, 3, 16, 17,
         4, 11, 18, 2)
      xlfs_char <- c(xls,
         list(d=as.character(
            xlf$d[d_order])))

      # expected sort order including factor vector, honorFactor=TRUE
      d_factor_order <- c(1, 20, 12, 14, 10,
         6, 13, 19, 8, 15, 9, 7,
         5, 3, 16, 17,
         4, 11, 18, 2)
      xlfs_factor <- c(xls,
         list(d=xlf$d[d_factor_order]))

      testthat::expect_equal(
         mixedSorts(xl),
         xls)
      testthat::expect_equal(
         mixedSorts(xlf, honorFactor=FALSE),
         xlfs_char)
      testthat::expect_equal(
         mixedSorts(xlf, honorFactor=TRUE),
         xlfs_factor)
   })
})


testthat::test_that("mixedSorts_NULL", {
   # define test list with character vectors
   testthat::expect_equal(
      mixedSorts(NULL),
      NULL)

   testthat::expect_equal(
      mixedSorts(list(NULL, NULL)),
      list(NULL, NULL))

   testthat::expect_equal(
      mixedSorts(list(A=NULL, B="a", C=NULL)),
      list(A=NULL, B="a", C=NULL))

   testthat::expect_equal(
      mixedSorts(list(NULL, list(NULL, factor("a")), c("b", "a10", "a2"))),
      list(NULL, list(NULL, factor("a")), c("a2", "a10", "b")))
})

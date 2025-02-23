

testthat::test_that("cPaste", {
   L <- list(
      entryA=c("miR-112", "miR-12", "miR-112"),
      entryB=factor(c("A","B","A","B"),
         levels=c("B","A")),
      entryC=factor(c("C","A","B","B","C"),
         levels=c("A","B","C")),
      entryNULL=NULL)
   cPaste_L <- c(
      entryA="miR-112,miR-12,miR-112",
      entryB="A,B,A,B",
      entryC="C,A,B,B,C",
      entryNULL="")
   testthat::expect_equal(
      cPaste(L),
      cPaste_L)
})

testthat::test_that("cPasteU", {
   L <- list(
      entryA=c("miR-112", "miR-12", "miR-112"),
      entryB=factor(c("A","B","A","B"),
         levels=c("B","A")),
      entryC=factor(c("C","A","B","B","C"),
         levels=c("A","B","C")),
      entryNULL=NULL)
   cPasteU_L <- c(
      entryA="miR-112,miR-12",
      entryB="A,B",
      entryC="C,A,B",
      entryNULL="")
   testthat::expect_equal(
      cPasteU(L),
      cPasteU_L)
})

testthat::test_that("cPasteU_nonBioc", {
   L <- list(
      entryA=c("miR-112", "miR-12", "miR-112"),
      entryB=factor(c("A","B","A","B"),
         levels=c("B","A")),
      entryC=factor(c("C","A","B","B","C"),
         levels=c("A","B","C")),
      entryNULL=NULL)
   cPasteU_L <- c(
      entryA="miR-112,miR-12",
      entryB="A,B",
      entryC="C,A,B",
      entryNULL="")
   testthat::expect_equal(
      cPasteU(L, useBioc=FALSE),
      cPasteU_L)
})

testthat::test_that("cPasteU_nonBioc_simpleBioc", {
   L <- list(
      entryA=c("miR-112", "miR-12", "miR-112"),
      entryB=factor(c("A","B","A","B"),
         levels=c("B","A")),
      entryC=factor(c("C","A","B","B","C"),
         levels=c("A","B","C")),
      entryNULL=NULL)
   cPasteU_L <- c(
      entryA="miR-112,miR-12",
      entryB="A,B",
      entryC="C,A,B",
      entryNULL="")
   testthat::expect_equal(
      cPasteU(L, useBioc=FALSE, useSimpleBioc=TRUE),
      cPasteU_L)
})

testthat::test_that("cPasteS", {
   L <- list(
      entryA=c("miR-112", "miR-12", "miR-112"),
      entryB=factor(c("A","B","A","B"),
         levels=c("B","A")),
      entryC=factor(c("C","A","B","B","C"),
         levels=c("A","B","C")),
      entryNULL=NULL)
   cPasteS_L_character <- c(
      entryA="miR-12,miR-112,miR-112",
      entryB="A,A,B,B",
      entryC="A,B,B,C,C",
      entryNULL="")
   cPasteS_L_factor <- c(
      entryA="miR-12,miR-112,miR-112",
      entryB="B,B,A,A",
      entryC="B,B,A,C,C",
      entryNULL="")
   testthat::expect_equal(
      cPasteS(L),
      cPasteS_L_factor)
   testthat::expect_equal(
      cPasteS(L, honorFactor=TRUE),
      cPasteS_L_factor)
   testthat::expect_equal(
      cPasteS(L, honorFactor=FALSE),
      cPasteS_L_character)
})

testthat::test_that("cPasteSU", {
   L <- list(
      entryA=c("miR-112", "miR-12", "miR-112"),
      entryB=factor(c("A","B","A","B"),
         levels=c("B","A")),
      entryC=factor(c("C","A","B","B","C"),
         levels=c("A","B","C")),
      entryNULL=NULL)
   cPasteSU_L_character <- c(
      entryA="miR-12,miR-112",
      entryB="A,B",
      entryC="A,B,C",
      entryNULL="")
   cPasteSU_L_factor <- c(
      entryA="miR-12,miR-112",
      entryB="B,A",
      entryC="B,A,C",
      entryNULL="")
   testthat::expect_equal(
      cPasteSU(L),
      cPasteSU_L_factor)
   testthat::expect_equal(
      cPasteSU(L, honorFactor=TRUE),
      cPasteSU_L_factor)
   testthat::expect_equal(
      cPasteSU(L, honorFactor=FALSE),
      cPasteSU_L_character)
})

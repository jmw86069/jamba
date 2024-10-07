

testthat::test_that("igrepHas", {
   # simple test vectors
   a <- c("data.frame",
      "data_frame",
      "tibble",
      "tbl");
   b <- c(a, NA)
   testthat::expect_equal(
      igrepHas("Data.*Frame", a),
      TRUE)
   testthat::expect_equal(
      igrepHas("matrix", a),
      FALSE)
   testthat::expect_equal(
      igrepHas("tibble", a, minCount=2),
      FALSE)
   testthat::expect_equal(
      igrepHas("tibble|tbl", a, minCount=2),
      TRUE)
   testthat::expect_equal(
      igrepHas("tibble|tbl", b, minCount=2),
      TRUE)
   testthat::expect_equal(
      igrepHas("^$", b, minCount=1, naToBlank=FALSE),
      FALSE)
   testthat::expect_equal(
      igrepHas("^$", b, minCount=1, naToBlank=TRUE),
      TRUE)
})


testthat::test_that("vigrep_vgrep_igrep_provigrep", {
   # test vectors
   V <- paste0(LETTERS[1:5], LETTERS[4:8]);

   testthat::expect_equal(
      vigrep("d", V),
      c("AD", "DG"))
   testthat::expect_equal(
      igrep("d", V),
      c(1, 4))
   testthat::expect_equal(
      vgrep("d", V),
      character(0))
   testthat::expect_equal(
      unvigrep("d", V),
      c("BE", "CF", "EH"))
   testthat::expect_equal(
      unigrep("d", V),
      c(2, 3, 5))
   testthat::expect_equal(
      provigrep(paste0("^", LETTERS[5:1]), V),
      c("EH", "DG", "CF", "BE", "AD"))
   testthat::expect_equal(
      proigrep(paste0("^", LETTERS[5:1]), V),
      c(5, 4, 3, 2, 1))
   testthat::expect_equal(
      provigrep(paste0("^", LETTERS[3:1]), V),
      c("CF", "BE", "AD"))
   testthat::expect_equal(
      provigrep(c(paste0("^", LETTERS[3:1]), "."), V),
      c("CF", "BE", "AD", "DG", "EH"))
})


testthat::test_that("grepls", {
   testthat::expect_equal(
      grepls("^vigrep"),
      list(`package:jamba`=c("vigrep")))
   testthat::expect_equal(
      grepls("vigrep"),
      list(`package:jamba`=c("provigrep", "unvigrep", "vigrep")))
})

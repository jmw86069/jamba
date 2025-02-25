

testthat::test_that("writeOpenxlsx, startRow=1, startCol=1", {
   testthat::skip_if_not_installed("openxlsx")

   x <- data.frame(VARNAME=1:5,
      VARDESC=letters[1:5],
      TYPE=c(0.1, 0.005, 0.01, 0.001, 0.0001),
      UNIQUEKEY=1:5,
      HIGHLIGHT=LETTERS[11:15],
      VALUES1=1:5,
      VALUES2=1:5,
      VALUES3=1:5,
      HIGHLIGHT2=LETTERS[11:15])
   colorSub <- c(VARNAME="firebrick",
      VARDESC="dodgerblue3",
      TYPE="navy",
      UNIQUEKEY="darkorchid4",
      jamba::nameVector(c("gold2", "chocolate3", "hotpink2",
         "mediumpurple2", "steelblue1"),
         letters[1:5]))
   xlsx_tempfile <- tempfile(pattern="jamba_writeOpenxlsx_test",
      fileext=".xlsx")
   writeOpenxlsx(file=xlsx_tempfile,
      x=x,
      colorSub=colorSub,
      intColumns=1, intRule=c(1, 3, 5),
      numColumns=4, numRule=c(1, 3, 5),
      highlightColumns=c(5, 9),
      freezePaneColumn=2,
      pvalueColumns=3,
      startRow=1,
      startCol=1,
      doFilter=FALSE,
      headerRowMultiplier=2,
      colWidths=c(15, 10, 15, 15, 10, 10, 10, 10, 10),
      sheetName="testsheet")

   x_loaded <- readOpenxlsx(xlsx=xlsx_tempfile,
      startRow=1, startCol=1);

   testthat::expect_equal(
      names(x_loaded),
      "testsheet")

   testthat::expect_equal(
      x,
      x_loaded[["testsheet"]])
})


testthat::test_that("writeOpenxlsx, startRow=3, startCol=3", {
   testthat::skip_if_not_installed("openxlsx")

   x <- data.frame(VARNAME=1:5,
      VARDESC=letters[1:5],
      TYPE=c(0.1, 0.005, 0.01, 0.001, 0.0001),
      UNIQUEKEY=1:5,
      HIGHLIGHT=LETTERS[11:15],
      VALUES1=1:5,
      VALUES2=1:5,
      VALUES3=1:5,
      HIGHLIGHT2=LETTERS[11:15])
   colorSub <- c(VARNAME="firebrick",
      VARDESC="dodgerblue3",
      TYPE="navy",
      UNIQUEKEY="darkorchid4",
      jamba::nameVector(c("gold2", "chocolate3", "hotpink2",
         "mediumpurple2", "steelblue1"),
         letters[1:5]))
   xlsx_tempfile <- tempfile(pattern="jamba_writeOpenxlsx_test",
      fileext=".xlsx")
   writeOpenxlsx(file=xlsx_tempfile,
      x=x,
      colorSub=colorSub,
      intColumns=1, intRule=c(1, 3, 5),
      numColumns=4, numRule=c(1, 3, 5),
      highlightColumns=c(5, 9),
      freezePaneColumn=2,
      pvalueColumns=3,
      startRow=3,
      startCol=3,
      doFilter=FALSE,
      headerRowMultiplier=2,
      colWidths=c(15, 10, 15, 15, 10, 10, 10, 10, 10),
      sheetName="testsheet")

   x_loaded <- readOpenxlsx(xlsx=xlsx_tempfile,
      startRow=3, startCol=3);

   testthat::expect_equal(
      names(x_loaded),
      "testsheet")

   testthat::expect_equal(
      x,
      x_loaded[["testsheet"]])

})

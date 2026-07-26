

testthat::test_that("kable_coloring basics", {
   # simple test vectors
   testthat::skip_if_not_installed("kableExtra")

   new_colorSub <- list(
      column_A=nameVector(rainbow(5), LETTERS[1:5]),
      column_B=nameVector(rev(rainbow(5)), LETTERS[1:5]),
      column_C=circlize::colorRamp2(
         colors=c("peachpuff", "lightsalmon", "coral1",
            "firebrick2", "firebrick", "darkred"),
         breaks=0:5 * 1000))
   testdf <- data.frame(column_A=LETTERS[1:5],
      row.names=LETTERS[1:5],
      column_B=LETTERS[1:5],
      column_C=1:5 * 1000)
   kdf1 <- kable_coloring(testdf,
      colorSub=new_colorSub)
   testthat::expect_equal(
      c("kableExtra", "knitr_kable"),
      class(kdf1))
   kdf1str <- unlist(strsplit(as.character(kdf1), "\n"));
   # confirm presence of "<span style"
   testthat::expect_equal(
      any(grep("<span style", kdf1str)),
      TRUE)
   # confirm no presence of "&lt;span style"
   testthat::expect_equal(
      any(grep("&lt;span style", kdf1str)),
      FALSE)
   # confirm "1,000" uses
   # background-color: rgba(255, 160, 122, 255)
   rgb1000 <- gsub(".$", "[0-9]",
      col2rgb(new_colorSub$column_C(1000))[, 1]);
   grep_pattern <- paste0("^.*<span style.*[; ](background-color:[ ]*",
      "rgba[([0-9]+,[ ]*[0-9]+,[ ]*[0-9]+,[ ]*[0-9]+[)].*>[ ]*1[,]*000[ ]*</span>).*")
   kdf1match <- vigrep(grep_pattern, kdf1str)
   if (length(kdf1match) == 1) {
      test_pattern <- paste0("<span style.*[; ]background-color:[ ]*",
         "rgba[(]", rgb1000[1],
         ",[ ]*", rgb1000[2],
         ",[ ]*", rgb1000[3],
         ".*>[ ]*1[,]*000[ ]*</span>")
      testthat::expect_equal(
         any(grep(test_pattern, ignore.case=TRUE, kdf1match)),
         TRUE)
   }
})



# test when input is an R color or hex color
testthat::test_that("getColorRamp with multiple color input", {
   test_color1 <- c("red4");
   expected_color1 <- c("#FCFCFCFF", "#C46666FF", "#9E2424FF",
      "#810F0FFF", "#650000FF")
   testthat::expect_equal(
      getColorRamp(test_color1, n=5, alpha=TRUE),
      expected_color1)
   testthat::expect_equal(
      getColorRamp(test_color1, n=5, alpha=FALSE),
      substr(expected_color1, 1, 7))
   # trimRamp
   expected_color1_trim10 <- c("#ECD1D1FF", "#B84949FF", "#9A2121FF",
      "#7F0E0EFF", "#650000FF")
   testthat::expect_equal(
      getColorRamp(test_color1, n=5, alpha=TRUE, trimRamp=c(1, 0)),
      expected_color1_trim10)

   # hex input
   # - edge case where R-4.5.0 on MacOS returns "#3434ECFF" not "#3333ECFF"
   test_color2 <- c("#0000FF");
   expected_color2 <- c("#FCFCFCFF",
      "#7575FDFF",
      "#3434ECFF",
      "#1616D1FF",
      "#0000B6FF")
   fix3 <- function(x)gsub("#3.3.", "#3.3.", x)
   testthat::expect_equal(
      fix3(getColorRamp(test_color2, n=5, alpha=TRUE)),
      fix3(expected_color2))
   testthat::expect_equal(
      fix3(getColorRamp(test_color2, n=5, alpha=FALSE)),
      fix3(substr(expected_color2, 1, 7)))
   # trimRamp
   expected_color2_trim10 <- c("#D5D5FCFF", "#5B5BFDFF", "#2F2FE8FF",
      "#1414CFFF", "#0000B6FF")
   testthat::expect_equal(
      getColorRamp(test_color2, n=5, alpha=TRUE, trimRamp=c(1, 0)),
      expected_color2_trim10)
})

# test when input is a vector of colors
testthat::test_that("getColorRamp with multiple color input", {
   test_colors <- c("red4", "#0000FF");
   expected_colors <- c("#8B0000FF", "#68003FFF", "#45007FFF",
      "#2100BFFF", "#0000FFFF")
   testthat::expect_equal(
      getColorRamp(test_colors, n=5, alpha=TRUE),
      expected_colors)
   testthat::expect_equal(
      getColorRamp(test_colors, n=5, alpha=FALSE),
      substr(expected_colors, 1, 7))
})

# test when input is a known color ramp name
# col2str <- function(x){cat(cPaste(paste0('"', x, '"'), sep=", "))}
testthat::test_that("getColorRamp with named color ramp input", {
   test_color_ramp1 <- c("RdBu");
   expected_color_ramp1 <- c("#67001FFF", "#E48267FF", "#F7F7F7FF",
      "#69ABCFFF", "#053061FF")
   testthat::expect_equal(
      getColorRamp(test_color_ramp1, n=5, alpha=TRUE),
      expected_color_ramp1)
   # trimRamp
   expected_color_ramp1_trim11 <- c("#9C1127FF", "#EF9B7AFF", "#F7F7F7FF",
      "#86BDDAFF", "#195696FF")
   testthat::expect_equal(
      getColorRamp(test_color_ramp1, n=5, alpha=TRUE, trimRamp=c(1, 1)),
      expected_color_ramp1_trim11)

   # reversed color ramp
   test_color_ramp2 <- c("RdBu_r");
   expected_color_ramp2 <- c("#053061FF", "#6AABD0FF", "#F7F7F7FF",
      "#E48166FF", "#67001FFF")
   testthat::expect_equal(
      getColorRamp(test_color_ramp2, n=5, alpha=TRUE),
      expected_color_ramp2)
   # trimRamp
   expected_color_ramp2_trim11 <- c("#195696FF", "#86BDDAFF", "#F7F7F7FF",
      "#EF9B7AFF", "#9C1127FF")
   testthat::expect_equal(
      getColorRamp(test_color_ramp2, n=5, alpha=TRUE, trimRamp=c(1, 1)),
      expected_color_ramp2_trim11)
})

# test color function input
testthat::test_that("getColorRamp color function(n) input", {
   # color function(n) format
   color_function <- colorspace::qualitative_hcl;
   expected_n7 <- c("#E16A86", "#C18500", "#799D00",
      "#00AB6E", "#00A9BE", "#6C8EE6", "#D169D0")
   getColorRamp(color_function, n=7, gradientN=15)
   testthat::expect_equal(
      getColorRamp(color_function, n=7, gradientN=7, alpha=FALSE),
      expected_n7)

   # expect string function name and function to be equivalent
   testthat::expect_equal(
      getColorRamp("colorspace::rainbow_hcl", n=5),
      getColorRamp(colorspace::rainbow_hcl, n=5))
   # expect string function name and function to be equivalent
   testthat::expect_equal(
      getColorRamp("colorspace::rainbow_hcl", n=NULL)(5),
      getColorRamp(colorspace::rainbow_hcl, n=NULL)(5))
})

# test circlize::colorRamp2() format
testthat::test_that("getColorRamp circlize::colorRamp2() function input", {
   # circlize::colorRamp2() format
   circlize_function <- function(x=NULL, return_rgb=FALSE, max_value=1) {
      # no actual work here
      NULL;
   }
   attr(circlize_function, "colors") <- t(col2rgb(
      c("dodgerblue", "red3")) / 255);
   attr(circlize_function, "breaks") <- c(-1, 0, 1);
   expected_n5 <- c("#1E90FFFF", "#496BBFFF",
      "#75487FFF", "#A1233EFF", "#CD0000FF")
   testthat::expect_equal(
      getColorRamp(circlize_function, n=5),
      expected_n5)
})

# test color function output
# test when input is a vector of colors
testthat::test_that("getColorRamp color function output", {
   test_colors <- c("red4", "#0000FF");
   expected_colors <- c("#8B0000FF", "#68003FFF", "#45007FFF",
      "#2100BFFF", "#0000FFFF")
   testthat::expect_equal(
      class(getColorRamp(test_colors, n=NULL, alpha=TRUE)),
      "function")
   testthat::expect_equal(
      getColorRamp(test_colors, n=NULL, alpha=TRUE)(5),
      expected_colors)
})

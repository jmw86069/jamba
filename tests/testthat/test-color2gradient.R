

testthat::test_that("color2gradient using dex", {
   test_color1 <- c("red4", "gold1");
   expected_color_dex1 <- c(
      "#C55959FF", "#A83939FF", "#8B1F1FFF", "#6F0C0CFF", "#520000FF",
      "#FFE34BFF", "#ECCF34FF", "#D9BC20FF", "#C7AA0FFF", "#B49800FF")
   names(expected_color_dex1) <- makeNames(rep(test_color1, each=5));
   testthat::expect_equal(
      color2gradient(test_color1, n=5, alpha=TRUE),
      expected_color_dex1)

   expected_color_dex2 <- c(
      "#CF6D6DFF", "#AD4444FF", "#8B2525FF", "#6A0E0EFF", "#490000FF",
      "#FFE65EFF", "#E8CD40FF", "#D0B626FF", "#B99E11FF", "#A18800FF")
   names(expected_color_dex2) <- makeNames(rep(test_color1, each=5));
   testthat::expect_equal(
      color2gradient(test_color1, n=5, dex=2, alpha=TRUE),
      expected_color_dex2)

   expected_color_dex0.5 <- c(
      "#BB4646FF", "#A32E2EFF", "#8B1A1AFF", "#740B0BFF", "#5C0000FF",
      "#FFE03BFF", "#F0D12AFF", "#E2C21AFF", "#D3B40CFF", "#C4A500FF")
   names(expected_color_dex0.5) <- makeNames(rep(test_color1, each=5));
   testthat::expect_equal(
      color2gradient(test_color1, n=5, dex=0.5, alpha=TRUE),
      expected_color_dex0.5)

   expected_color_dex_n5 <- c(
      "#AB2A2AFF", "#9B1D1DFF", "#8B1111FF", "#7B0808FF", "#6C0000FF",
      "#FFDD24FF", "#F6D41AFF", "#EDCB11FF", "#E4C208FF", "#DBB900FF")
   names(expected_color_dex_n5) <- makeNames(rep(test_color1, each=5));
   testthat::expect_equal(
      color2gradient(test_color1, n=5, dex=-5, alpha=TRUE),
      expected_color_dex_n5)
})



testthat::test_that("groupedAxis grouped", {
   test_vector <- rep(c("DMSO", "Etop"), c(2, 3))
   expected_df <- data.frame(check.names=FALSE,
      stringsAsFactors=FALSE,
      axis_at=c(0.8, 1.5, 2.2, 2.8, 4.0, 5.2),
      axis_ticks=c(TRUE, FALSE, TRUE),
      axis_labels=c("", "DMSO", "", "", "Etop", ""),
      axis_side=1,
      axis_group=c(1, 1, 1, 2, 2, 2))
   testthat::expect_equal(
      groupedAxis(side=1,
         do_plot=FALSE,
         group_style="grouped",
         x=test_vector),
      expected_df)
})

testthat::test_that("groupedAxis partial", {
   test_vector2 <- rep(c("DMSO", "Etop"), c(1, 3))
   expected_df2 <- data.frame(check.names=FALSE,
      stringsAsFactors=FALSE,
      axis_at=c(1.0, 1.0, 1.8, 3.0, 4.2),
      axis_ticks=c(TRUE, FALSE, TRUE, FALSE, TRUE),
      axis_labels=c("", "DMSO", "", "Etop", ""),
      axis_side=1,
      axis_group=c(1, 1, 2, 2, 2))
   testthat::expect_equal(
      groupedAxis(side=1,
         do_plot=FALSE,
         group_style="partial",
         x=test_vector2),
      expected_df2)
})

testthat::test_that("groupedAxis centered", {
   test_vector3 <- rep(c("DMSO", "Etop"), c(2, 3))
   expected_df3 <- data.frame(check.names=FALSE,
      stringsAsFactors=FALSE,
      axis_at=c(1.5, 4.0),
      axis_ticks=c(TRUE, TRUE),
      axis_labels=c("DMSO", "Etop"),
      axis_side=1,
      axis_group=c(1, 1))
   testthat::expect_equal(
      groupedAxis(side=1,
         do_plot=FALSE,
         group_style="centered",
         x=test_vector3),
      expected_df3)
})

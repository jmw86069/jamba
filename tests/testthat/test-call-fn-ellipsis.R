
testthat::test_that("call_fn_ellipsis", {
   test_fn <- function(x, y, ...) {
      x + y
   }

   wrapper <- function(x=1, ...) {
      call_fn_ellipsis(test_fn,
         x=x,
         ...)
   }
   testthat::expect_equal(
      suppressWarnings(wrapper(x=1, y=3)),
      4)

   wrapper_dupe_arg <- function(x=1, ...) {
      call_fn_ellipsis(test_fn,
         ...,
         x=x,
         y=2)
   }
   # uses '...' as priority
   testthat::expect_equal(
      suppressWarnings(wrapper_dupe_arg(x=1, y=3)),
      4)
   testthat::expect_equal(
      suppressWarnings(wrapper_dupe_arg(x=1)),
      3)

   # uses internal y=2 by priority
   wrapper_dupe_arg_rev <- function(x=1, ...) {
      call_fn_ellipsis(test_fn,
         x=x,
         y=2,
         ...)
   }
   testthat::expect_equal(
      suppressWarnings(wrapper_dupe_arg_rev(x=1, y=5)),
      3)
})

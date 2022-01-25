
#' Compare two functions line by line using diff
#'
#' Compare two functions line by line using diff
#'
#' This function is a utility wrapper around linux commandline
#' tool `diff`, used to compare the text representation of two
#' R functions.
#'
#' @family jam practical functions
#'
#' @return None. This function is called for its output, which
#'    can be captured with `utils::capture.output()`.
#'
#' @param f1 `function` expected to be passed by function name.
#' @param f2 `function` expected to be passed by function name.
#' @param width `integer` number of columns to allow in output.
#' @param wide `logical` indicating whether to use wide output
#'    from `diff`, which prints the two functions side by side.
#' @param trim_whitespace `logical` indicating whether to trim leading
#'    and trailing whitespace on each function line to avoid differences
#'    defined only by the indentation of text.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' diff_functions(base::grepl, base::agrepl)
#'
#' @export
diff_functions <- function
(f1,
 f2,
 width=NULL,
 wide=TRUE,
 trim_whitespace=FALSE,
 verbose=FALSE,
 ...)
{
   if (length(width) == 0) {
      default_width <- as.numeric(Sys.getenv("COLUMNS"));
      if (is.na(default_width)) {
         default_width <- 80;
      }
      width <- getOption("width", default_width);
   }
   f1_name <- gsub("[^-a-zA-Z0-9_]", ".",
      deparse(substitute(f1)));
   f2_name <- gsub("[^-a-zA-Z0-9_]", ".",
      deparse(substitute(f2)));

   f1_file <- file.path(tempdir(),
      paste0(f1_name, ".R"));
   f2_file <- file.path(tempdir(),
      paste0(f2_name, ".R"));

   jamba::printDebug("f1_name: ",
      f1_name);
   jamba::printDebug("f1_file: ",
      f1_file);

   jamba::printDebug("f2_name: ",
      f2_name);
   jamba::printDebug("f2_file: ",
      f2_file);

   f1_text <- paste0(capture.output(
      print(f1)), "\n");
   f2_text <- paste0(capture.output(
      print(f2)), "\n");

   if (trim_whitespace) {
      f1_text <- gsub("^[ \t]+|[ \t]+$",
         "",
         f1_text);
      f2_text <- gsub("^[ \t]+|[ \t]+$",
         "",
         f2_text);
   }

   cat(f1_text,
      file=f1_file);
   cat(f2_text,
      file=f2_file);

   diff_cmd <- paste("diff",
      "-W", width,
      "-y",
      f1_file,
      f2_file);
   print(diff_cmd);
   x <- system(diff_cmd);
   unlink(f1_file);
   unlink(f2_file);
   invisible(x);
}


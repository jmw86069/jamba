
#' Extend kableExtra colorization of 'Rmarkdown' tables
#'
#' Extend kableExtra colorization of 'Rmarkdown' tables
#'
#' This function extends the `kableExtra` package, and is only
#' available for use if the `kableExtra` package is installed. It is
#' intended to allow specific color assignment of elements in a
#' data.frame, but otherwise uses the `kableExtra` functions to
#' apply those colors.
#'
#' The use case is to provide colorized HTML output for 'Rmarkdown',
#' it has not been tested with other `format` output.
#'
#' The argument `colorSub` accepts:
#'
#' * `character` vector input where
#' names should match column values
#' * `function` that accepts column values and returns a `character`
#' vector of colors of equal length
#' * `list` input where names should match `colnames(df)`, and where
#' each list element should contain either a `character` vector, or
#' `function` as described above.
#'
#' @returns object with class `c("kableExtra", "knitr_kable")` by default
#'    when `return_type="kable"`, suitable to render inside an 'Rmarkdown'
#'    or HTML context. Or returns `data.frame` when `return_type="data.frame"`.
#'
#' @param df `data.frame` input. Note that `kable` input is not supported.
#' @param colorSub one of the following inputs:
#'    * `character` vector of R colors, whose names match entries
#'    in the `data.frame` which are given these assigned colors
#'    * `function` that takes column values as input, and returns
#'    a `character` vector with one color per value, using `NA`
#'    or `NULL` to indicate `"transparent"`
#'    * `list` whose names match `colnames(df)`, where each entry
#'    contains either `character` or `function` option as described
#'    above. A `character` vector should be named by values expected
#'    in each column. A `function` should take column values as input,
#'    and return a `character` vector with same length of R colors.
#' @param background_as_tile `logical` default `TRUE`, whether the
#'    cell background color will appear as a rounded tile (`TRUE`)
#'    or a rectangle (`FALSE`).
#'    Either way, the color does not fill the entire whitespace
#'    of the table cell, but only around the text itself.
#' @param color_cells `logical` indicating whether to color individual cells,
#'    default `TRUE`. This may be `FALSE` when also applying `row_color_by`,
#'    so the entire row will be colorized.
#' @param row_color_by `character` vector with one or more `colnames`,
#'    indicating how to colorize entire rows of a table.
#'    When one column is defined, colors in `colorSub` are used as normal.
#'    When multiple columns are defined, values from each column are
#'    concatenated using `sep` delimiter. Then resulting values are
#'    compared with `colorSub`.
#' @param sep `character` delimiter used to combine values in multiple
#'    columns when `row_color_by` is supplied and contains multiple
#'    `colnames`. The delimited character strings are compared to
#'    `colorSub` to assign colors.
#' @param border_left,border_right,extra_css `character` values optionally
#'    passed to `kableExtra::column_spec()` as a convenient way to apply
#'    borders for each column (`border_left`, `border_right`) or enable
#'    or disable word-wrapping by column. Some helpful examples:
#'    * `border_left=FALSE`: disables left border
#'    * `border_left="1px solid #DDDDDD"`: light gray 1 pixel left border
#'    * `border_right=FALSE`: disables right border
#'    * `border_right="1px solid #DDDDDD"`: light gray 1 pixel right border
#'    * `extra_css=NULL`: disables word-wrap
#'    * `extra_css="whitespace: nowrap;"`: enables text word-wrap
#'    * when all options above contain only `FALSE` or `NULL`, then
#'    `kableExtra::column_spec()` is not applied.
#' @param format `character` passed to `knitr::kable()`, default `"html"`
#'    which is the intended format for most scenarios.
#'    It can be set to `NULL` to enable auto-detection of the format.
#' @param format.args `list` of arguments passed to `base::format()`
#'    intended mainly for `numeric` columns.
#' @param row.names `logical` indicating whether to include `rownames(df)`.
#'    When `row.names=NA` the default is to display rownames if they
#'    are not `NULL` and not equal to `1:nrow(df)`.
#' @param align `character` passed to `kableExtra::kable()` to define
#'    alignment of each column.
#' @param return_type `character` string indicating the type of data
#'    to return.
#'    * `return_type="kable"`: (default) returns object with class
#'    `"kableExtra", "knitr_kable"` suitable for downstream processing.
#'    * `return_type="data.frame"`: returns a `data.frame` whose cells
#'    contain HTML markup with corresponding colors defined.
#' @param verbose boolean indicating whether to print verbose output.
#' @param ... additional arguments are passed to `kableExtra::kable()`
#'    which allows the usual customizations on the initial call.
#'
#' @examplesIf all(check_pkg_installed(c("circlize", "kableExtra")))
#' expt_df <- data.frame(
#'    Sample_ID="",
#'    Treatment=rep(c("Vehicle", "Dex"), each=6),
#'    Genotype=rep(c("Wildtype", "Knockout"), each=3),
#'    Rep=paste0("rep", c(1:3)))
#' expt_df$Sample_ID <- pasteByRow(expt_df[, 2:4])
#'
#' # define colors
#' colorSub <- c(Vehicle="palegoldenrod",
#'    Dex="navy",
#'    Wildtype="gold",
#'    Knockout="firebrick",
#'    nameVector(
#'       color2gradient("grey48", n=3, dex=10),
#'       rep("rep", 3),
#'       suffix=""),
#'    nameVector(
#'       color2gradient(n=3,
#'          c("goldenrod1", "indianred3", "royalblue3", "darkorchid4")),
#'       expt_df$Sample_ID))
#' kbl <- kable_coloring(
#'    expt_df,
#'    caption="Experiment design table showing categorical color assignment.",
#'    colorSub)
#' # Note that the HTML table is rendered in 'Rmarkdown', not pkgdown
#' kbl
#'
#' # return_type="data.frame" is a data.frame with HTML contents
#' kdf3 <- kable_coloring(
#'    return_type="data.frame",
#'    df=expt_df,
#'    colorSub=colorSub)
#' kdf3;
#'
#' @family jam color functions
#' @family jam practical functions
#'
#' @export
kable_coloring <- function
(df,
 colorSub=NULL,
 background_as_tile=TRUE,
 color_cells=TRUE,
 row_color_by=NULL,
 sep="_",
 border_left="1px solid #DDDDDD",
 border_right=FALSE,
 extra_css="white-space: nowrap;",
 format="html",
 format.args=list(
    trim=TRUE,
    big.mark=","),
 row.names=NA,
 align=NULL,
 return_type=c("kable",
    "data.frame"),
 verbose=FALSE,
 ...)
{
   ## Purpose is to enhance kableExtra by automating some manual steps
   ## required to colorize text fields in a data.frame
   ##
   ## The input is expected to be a data.frame
   if (!requireNamespace("kableExtra", quietly=TRUE)) {
      stop("kable_coloring() requires the kableExtra package.");
   }
   return_type <- match.arg(return_type);

   # keep a non-kable copy for now
   df1 <- df;
   if (all(is.na(row.names))) {
      if (length(rownames(df)) > 0 &&
            !all(rownames(df) == seq_len(nrow(df)))) {
         row.names <- TRUE;
      } else {
         row.names <- FALSE;
      }
   }
   if (length(align) == 0 ||
         all(is.na(align)) ||
         max(nchar(align), na.rm=TRUE) == 0) {
      align <- sapply(seq_len(ncol(df1)), function(i){
         if (is.numeric(df[[i]])) {
            "r"
         } else {
            "l"
         }
      })
   } else {
      if (max(nchar(align)) == 1) {
         align <- rep(align,
            length.out=ncol(df));
      }
   }

   if (TRUE %in% color_cells) {
      for (i in colnames(df)) {
         if (length(colorSub) > 0 && is.list(colorSub)) {
            # colorSub as color_list, named by colnames(df)
            # if ("color_sub" %in% names(attributes(colorSub))) {}
            if (i %in% names(colorSub)) {
               if (verbose) {
                  printDebug("kable_coloring(): ",
                     "colorizing column:",
                     i,
                     " using colorSub[[i]].");
               }
               icolorSub <- colorSub[[i]];
               if (is.function(icolorSub)) {
                  value_colors <- icolorSub(df[[i]])
               } else {
                  value_colors <- icolorSub[as.character(df[[i]])]
               }
               column_values <- df[[i]];
               if (is.numeric(column_values) && length(format.args) > 0) {
                  column_values <- do.call(base::format,
                     c(list(x=column_values),
                        format.args));
               }
               df[[i]] <- kableExtra::cell_spec(
                  x=column_values,
                  background_as_tile=background_as_tile,
                  color=setTextContrastColor(
                     rmNA(value_colors,
                        naValue="transparent")
                  ),
                  background=rmNA(value_colors,
                     naValue="transparent"))
            }
         } else if (any(as.character(df[[i]]) %in% names(colorSub))) {
            # colorSub as named character vector
            if (verbose) {
               printDebug("kable_coloring(): ",
                  "colorizing column:",
                  i);
            }
            df[[i]] <- kableExtra::cell_spec(df[[i]],
               background_as_tile=background_as_tile,
               color=setTextContrastColor(
                  rmNA(colorSub[as.character(df[[i]])],
                     naValue="transparent")
               ),
               background=rmNA(colorSub[as.character(df[[i]])],
                  naValue="transparent"))
         } else {
            if (verbose) {
               printDebug("kable_coloring(): ",
                  "No values to colorize in column:",
                  i,
                  fgText=c("orange","red"));
            }
         }
      }
   }

   ## Convert to kable
   if (return_type %in% "kable" || length(row_color_by) > 0) {
      df <- kableExtra::kable_styling(
         kableExtra::kable(x=df,
            escape=FALSE,
            format=format,
            format.args=format.args,
            align=align,
            row.names=row.names,
            ...))
   }

   ## Optionally color rows using values in a column
   if (length(row_color_by) > 0) {
      if (verbose) {
         printDebug("kable_coloring(): ",
            "row_color_by:",
            row_color_by);
      }
      row_color_by_value <- NULL;
      row_colors <- NULL;
      if (length(row_color_by) == 1) {
         # one column defines the colors used, most common usage
         if (length(colorSub) > 0) {
            if (is.numeric(row_color_by)) {
               row_color_by <- colnames(df1)[row_color_by];
            }
            row_color_by_value <- df1[[as.character(row_color_by)]]
            if (is.list(colorSub)) {
               # colorSub is a list named by colnames(df)
               icolorSub <- colorSub[[row_color_by]];
               if (is.function(icolorSub)) {
                  row_colors <- icolorSub(row_color_by_value);
               } else {
                  row_colors <- icolorSub[row_color_by_value];
               }
            } else {
               # colorSub is a named character vector, or single color function
               if (is.function(colorSub)) {
                  row_colors <- colorSub(row_color_by_value);
               } else {
                  row_colors <- colorSub[row_color_by_value];
               }
            }
         } else {
            # colorSub is empty, do nothing
         }
      } else {
         # two or more columns define the colors used
         row_color_by_value <- as.character(
            pasteByRow(df1[, row_color_by, drop=FALSE],
               sep=sep));
         if (length(colorSub) > 0) {
            if (is.list(colorSub)) {
               # look for attributes "color_sub"
               if ("color_sub" %in% names(attributes(colorSub))) {
                  color_sub <- attr(colorSub, "color_sub");
               } else {
                  color_sub <- unlist(unname(
                     lapply(colorSub, function(icolorSub){
                        if (is.function(icolorSub)) {
                           return(character(0))
                        }
                        icolorSub
                     })))
               }
               row_colors <- rmNA(naValue="transparent",
                  color_sub[row_color_by_value])
            } else {
               # colorSub is a named character vector, or single color function
               if (is.function(colorSub)) {
                  row_colors <- colorSub(row_color_by_value);
               } else {
                  row_colors <- colorSub[row_color_by_value];
               }
            }
         }
      }
      if (verbose) {
         printDebug("kable_coloring(): ",
            "row_color_by_value:",
            row_color_by_value);
         printDebug("kable_coloring(): ",
            "row_colors:",
            row_colors);
      }
      row_colors_valid <- unique(rmNA(row_colors));
      for (row_color in row_colors_valid) {
         which_rows <- which(row_colors %in% row_color);
         if (verbose) {
            printDebug("kable_coloring(): ",
               "colorizing rows:",
               which_rows,
               row_color,
               fgText=c("orange",
                  "lightblue",
                  "orange",
                  row_color))
         }
         df <- kableExtra::row_spec(
            kable_input=df,
            row=which_rows,
            background=row_color,
            color=setTextContrastColor(row_color));
      }
   }

   # optional column_spec
   do_column_spec <- (
      length(extra_css) > 0 ||
      (length(border_left) > 0 && !FALSE %in% border_left) ||
         (length(border_right) > 0 && !FALSE %in% border_right))
   if ("kable" %in% return_type &&
         !"latex" %in% format &&
         TRUE %in% do_column_spec) {
      if (length(border_left) == 0) {
         border_left <- FALSE;
      }
      if (length(border_right) == 0) {
         border_right <- FALSE;
      }
      which_columns <- seq_len(ncol(df1) + 1 * (TRUE %in% row.names));
      df <- kableExtra::column_spec(
         kable_input=df,
         column=which_columns,
         border_left=border_left,
         border_right=border_right,
         extra_css=extra_css);
   }

   df;
}

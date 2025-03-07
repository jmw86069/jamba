
#' print colorized output to R console
#'
#' print colorized output to R console
#'
#' This function prints colorized output to the R console, with some
#' rules for colorizing the output to help visually distinguish items.
#'
#' The main intent is to use this function to print pretty debug messages,
#' because color helps identify.
#'
#' By default, output has the following configurable properties:
#'
#' * each line begins with a comment, controlled by default
#' `comment=getOption("jam.comment", TRUE)` which by default uses `"##"`,
#' but which can be defined to use a different prefix, or `FALSE`
#' for no prefix at all.
#' * each line includes time and date stamp controlled by
#' `timeStamp=getOption("jam.timeStamp", TRUE)` which by default includes the
#' current time and date.
#' * each line formats `numeric` values, controlled by
#' `formatNumbers=getOption("jam.formatNumbers", TRUE)`, which determines
#' whether to apply arguments `big.mark` and `small.mark` to make numeric
#' values more readable.
#' * each entry in `...` is printed with its own foreground color `fgText`,
#' background color `bgText`, with a slight lighter/darker dithering effect
#' to add minor visual distinction for multiple values.
#' * Values in each `vector` are concatenated by `sep=","` by default.
#' * Each `list` is concatenated by `collapse=""` by default.
#'
#' Additional convenience rules:
#' * For convenience, when the last `...` argument is a `character` vector
#' of colors, it is assumed to be `fgText`.
#' * When the only entry in `...` is a `character` vector of R colors,
#' the names are printed using the color vector for `fgText`, or if no
#' names exist the colors are printed using the color vector for `fgText`.
#' * For `printDebugI()` or `invert=TRUE`, colors typically assigned to
#' `fgText` are instead assigned to `bgText`.
#' * For very specific color assignments, `fgText` and/or `bgText` can be
#' defined as a `list` of `character` vectors of R colors, in which case
#' the `list` overall is recycled to the length `...` to be printed,
#' and within each vector of `...` printed the corresponding color vector
#' is recycled to the length of that vector.
#'
#' For use inside 'Rmarkdown' `.Rmd` documents, current recommendation is
#' to define the R output with `results='asis'` like this:
#'
#' ```
#' \`\`\`{r block_name, results='asis'}
#' # some R code here
#' \`\`\`
#' ```
#'
#' Then define a global option to turn off the comment prefix in
#' `printDebug()`: `options("jam.comment"=FALSE)`
#'
#' For colorized text, it may require `"html_output"` rendering of the
#' `.Rmd` 'Rmarkdown' file, as well as this option to enable HTML formatting
#' by `printDebug()`: `options("jam.htmlOut"=TRUE)`.
#'
#' @param ... `character`, `factor`, `numeric` or compatible atomic vectors
#'    to be printed to the R console. These arguments are recognized as
#'    any un-named argument, or any argument whose name does not match the
#'    named arguments below.
#' @param fgText one of two formats to define the foreground color for
#'    elements in `...` being printed. Each element is colored in order,
#'    and when multiple vector values are contained in one `...` element,
#'    the color defined in `fgText` is extended.
#'    The input types recognized:
#'    * `NULL` when no color is defined, one of two outputs:
#'       1. When all values in `...` represent colors, these colors are
#'       used to colorize the output text. When `names()` are present
#'       they are used as the text labels in place of the vector value.
#'       2. When not all values in `...` represent colors, the default
#'       color set is used: `c("darkorange1", "dodgerblue")`.
#'       3. To disable option 1 above, define a specific value for `fgText`,
#'       such as `fgText=c("darkorange1", "dodgerblue")`.
#'
#'    * `vector` of R compatible colors, recycled to the length of `...`.
#'    When any element of `...` is a vector with multiple values, the
#'    corresponding color in `fgText` is shaded slightly lighter and
#'    darker, then recycled to the vector length, so that adjacent values
#'    have slightly different color.
#'    This behavior is controlled by default argument `splitComments=TRUE`.
#'    * `list` of vectors of R compatible colors, recycled to the length
#'    of `...`, then applied to each element in `...` in order. When only
#'    one color is defined, and multiple values are present in the
#'    corresponding `list` element, the color is shaded slightly lighter
#'    and darker, then recycled to the vector length, as described above.
#'    This behavior is controlled by default argument `splitComments=TRUE`.
#'    When multiple colors are defined for the `list` element, these
#'    values are recycled to the vector length.
#'    * **Note**: When `invert=TRUE` the values for `fgText` and `bgText` are
#'    reversed, and if the resulting `fgText` is `NULL` then its color
#'    is defined by `setTextContrastColor()` in order to define a contrasting
#'    text color.
#' @param fgDefault `character` defaults to
#'    `getOption("jam.fgDefault", c("darkorange1", "dodgerblue"))`, and
#'    is used when colors are not defined by `fgText` or by the
#'    input `...` values.
#' @param bgText `vector` of R colors, or `list` of vectors, used to define
#'    the background color, using the same approach described for `fgText`.
#'    Note that `NULL` or `NA` defines the absence of any background color,
#'    which is default. When `invert=TRUE`, which is default for
#'    `printDebugI()`, the values for `fgText` and `bgText` are reversed.
#' @param fgTime `character` R color to colorize the time
#' @param timeStamp `logical` whether to include a time stamp in output
#' @param comment `logical` whether to prefix output with '## ' as a comment,
#'    or `character` string used as a prefix.
#' @param formatNumbers `logical` whether to format numbers using
#'    `format()` which controls the number of digits displayed, and is
#'    default. When `formatNumbers=FALSE` sometimes `numeric` values
#'    that contain `integers` may be represented as `14.0000000001`.
#' @param trim,digits,nsmall,justify,big.mark,small.mark,zero.print,width
#'    arguments passed to `format()`.
#' @param doColor `logical` or `NULL` indicating whether to colorize output.
#'    When `doColor` is `NULL`, if the `"crayon"` package is available,
#'    and if crayon detects color is permitted, color is enabled.
#' @param splitComments `logical` whether to color each element independently
#'    without light-dark alternating pattern. The intensity of the
#'    adjustment is controlled by `dex` passed to `color2gradient()`.
#' @param collapse `character` collapse string used to separate list items,
#'    by default "" so text separation is expected in the input data.
#' @param sep `character` separator used to separate vector elements, when
#'    a list items contains a vector.
#' @param doReset `logical` or `NULL`, indicating whether to apply
#'    `crayon::reset()` to the delimiter `sep`. When `doReset=TRUE` the
#'    style on the delimiter is forced to reset, using `crayon::reset()`,
#'    or to remove pre-existing style with `crayon::strip_style()`. When
#'    `doReset=NULL` and `sep` contains ANSI escape characters, they are
#'    left as-is; when `doReset=NULL` and `sep` does not contain ANSI escape
#'    characters, `sep` becomes `crayon::reset(sep)` which forces the style
#'    to be reset between printed values.
#' @param detectColors `logical` whether to detect and potentially try to
#'    correct console color capabilities.
#' @param dex `numeric` passed to `color2gradient()` to split a color
#'    into a lighter,darker alternating pattern. Until version 0.0.83.900,
#'    this process used `gradientWtFactor=1` and was not adjustable.
#'    Note that when `splitComments=TRUE` the input values in `...`
#'    are flattened to a single vector, and colors in `fgText` are
#'    applied directly without adjustment.
#' @param darkFactor,sFactor `numeric` arguments deprecated.
#' @param Crange,Lrange `numeric` range of chroma and luminance values
#'    between 0 and 100. When NULL, default values are assigned
#'    by `setCLranges()`. The intent is to restrict the range relative
#'    to the console background color, also controlled by `lightMode`.
#' @param lightMode `logical` or NULL, indicating whether the text
#'    background color is light, where `lightMode=TRUE` indicates the
#'    background is white or light enough to require darker text,
#'    imposing a maximum brightness for colors displayed.
#'    When `NULL` it calls `checkLightMode()`, which uses:
#'    * `getOption("jam.lightMode")` if defined
#'    * otherwise attempts to detect whether the session is running inside
#'    RStudio, by checking for environmental variable `"RSTUDIO"`,
#'    under the assumption that default RStudio uses a light background,
#'    therefore `lightMode=TRUE`.
#'    * if steps above fail, it uses `lightMode=FALSE`.
#'    * to force a specific lightMode for all uses, use options:
#'    `options(jam.lightMode=TRUE)` or `options(jam.lightMode=FALSE)`.
#' @param removeNA `logical` whether to remove NA values and not print to
#'    the console.
#' @param replaceNULL `character` or NULL, optionally replace NULL elements
#'    with non-NULL character value, otherwise NULL elements are ignored.
#' @param adjustRgb `numeric` value adjustment used during the conversion of
#'    RGB colors to ANSI colors, which is inherently lossy. If not defined,
#'    it uses the default returned by `setCLranges()` which itself uses
#'    `getOption("jam.adjustRgb")` with default=0. In order to boost
#'    color contrast, an alternate value of -0.1 is suggested.
#' @param byLine `logical` whether to delimit lists by line instead of
#'    using collapse to combine them onto one line.
#' @param verbose `logical` whether to print verbose output
#' @param indent `character` optional characters used as a prefix to indent
#'    output. When `numeric` it is rounded to integer, then this many
#'    character spaces `" "` are concatenated together to define the
#'    indent width. Note that the `indent` text is not colorized.
#' @param keepNA `logical`, default TRUE, whether to keep and print NA values.
#' @param file argument passed to `cat()` to send output to a file or
#'    compatible output of `cat()`.
#' @param append `logical` whether to append output, passed to `cat()`
#'    when `file` is defined.
#' @param invert `logical` indicating whether foreground and background
#'    colors should be switched, as is default for `printDebugI()`.
#'    Note when the resulting `fgText` is `NULL`, its color is defined
#'    by `setTextContrastColor()` to define a contrasting text color
#'    relative to the background color in `bgText`.
#' @param htmlOut `logical` indicating whether to print HTML span
#'    output, using format
#'    `<span style="color:fg;background-color:bg">text</span>`.
#'    This argument is not yet implemented, more testing is required
#'    to determine the best mechanism to use for things like 'Rmarkdown'
#'    rendering, and R-shiny app rendering.
#'
#' @returns `NULL` invisibly, this function is called for the side effect
#'    of printing output using `cat()`.
#'
#' @family jam practical functions
#'
#' @examples
#' printDebug("Testing ", "default ", "printDebug().");
#' printDebug("List of vectors:", c("one", "two", "three"));
#'
#' # By default, there is no space between separate elements in `...`
#' printDebug("List of vectors:", c("one", "two", "three"),
#'    c("four", "five", "six"));
#' # To add a space " " between elements, use collapse
#' printDebug("List of vectors:", c("one", "two", "three"),
#'    c("four", "five", "six"), collapse=" ");
#'
#' # slightly different style, one entry per line, indented:
#' printDebug("List of vectors:", c("one", "two", "three"),
#'    c("four", "five", "six"), collapse="\n   ");
#'
#' # when a vector entirely contains recognized colors,
#' # the colors are used in the output
#' printDebug(c("red", "blue", "yellow"));
#'
#' # When the vector contains colors, the names are used as the label
#' color_vector <- jamba::nameVector(c("red", "blue", "green","orange"),
#'    c("group_A", "group_B", "group_C", "group_D"));
#' printDebug(color_vector);
#'
#' # Remember the sister function that inverses the colors
#' printDebugI(color_vector);
#'
#' printDebug(1:10, fgText="blue", dex=2);
#' printDebug(1:10, bgText="blue", dex=2);
#' printDebug(1:10, fgText="orange", dex=2);
#'
#' @export
printDebug <- function
(...,
 fgText=NULL,
 fgDefault=getOption("jam.fgDefault", c("darkorange1", "dodgerblue")),
 bgText=NULL,
 fgTime=getOption("jam.fgTime", "cyan2"),
 timeStamp=getOption("jam.timeStamp", TRUE),
 comment=getOption("jam.comment", !htmlOut),
 formatNumbers=getOption("jam.formatNumbers", TRUE),
 trim=getOption("jam.trim", TRUE),
 digits=getOption("jam.digits"),
 nsmall=getOption("jam.nsmall", 0L),
 justify="left",
 big.mark=getOption("jam.big.mark", ","),
 small.mark=getOption("jam.small.mark", "."),
 zero.print=NULL,
 width=NULL,
 doColor=getOption("jam.doColor"),
 splitComments=FALSE,
 collapse=getOption("jam.collapse", ""),
 sep=getOption("jam.sep", ","),
 doReset=NULL,
 detectColors=TRUE,
 dex=2,
 darkFactor=c(1, 1.5),
 sFactor=c(1, 1.5),
 lightMode=checkLightMode(),
 Crange=getOption("jam.Crange"),
 Lrange=getOption("jam.Lrange"),
 removeNA=FALSE,
 replaceNULL=NULL,
 adjustRgb=getOption("jam.adjustRgb"),
 byLine=FALSE,
 verbose=FALSE,
 indent="",
 keepNA=TRUE,
 file=getOption("jam.file", ""),
 append=getOption("jam.append", TRUE),
 invert=getOption("jam.invert", FALSE),
 htmlOut=getOption("jam.htmlOut", FALSE))
{
   ## Purpose is to wrapper a print() function with optional time-date stamp
   ## suppressPackageStartupMessages(testit())
   ##
   ## The messages are received as lists of vectors, and colors are matched
   ## to each list element, with vector elements colored using alternative
   ## light-dark versions of each color.
   ##
   ## The following command will print one,two,three in alternating light-dark
   ## orange,  and print four,five,six in alternating light-dark lightblue.
   ## printDebug(c("one", "two", "three"), c("four", "five"), fgText=c("orange", "lightblue"))
   ##
   ## splitComments=TRUE will split each vector and apply the colors without the
   ## alternating light-dark coloring.
   ##
   ## byLine=TRUE will delimit lists one-per-line, instead of using collapse=""
   ##
   ## replaceNULL will change any NULL entries to this value, suggested to be "NULL"
   ## to make NULL entries visible
   ##
   # validate comment argument
   if (length(comment) == 0) {
      comment <- FALSE;
   } else {
      comment <- head(comment, 1)
      if (is.logical(comment)) {
         if (TRUE %in% comment) {
            comment <- "## ";
         } else {
            comment <- "";
         }
      } else {
         comment <- as.character(comment)
      }
   }
   if (byLine) {
      collapse <- "\n";
   }
   if (is.numeric(indent)) {
      indent <- paste0(rep(" ", length.out=floor(head(indent, 1))),
         collapse="");
   }

   ## Determine the type of coloration we can use
   ## by setting up a conditional array to capture various combinations
   ## of 3 options:
   ## - availability of the package crayon
   ## - availability of the package xterm256
   ## - the given preference doColor value (1=xterm256, 2=crayon, 0=no color, everything else is pickem)
   if (TRUE %in% htmlOut) {
      doColor <- 1;
   } else {
      hasCrayon <- check_pkg_installed("crayon")
      if (is.null(doColor)) {
         if (hasCrayon %in% "TRUE") {
            doColor <- 2;
         } else {
            doColor <- 0;
         }
      }
   }

   ## Check lightMode, whether the background color is light or not
   if (length(lightMode) == 0 && length(Crange) > 0 && length(Lrange) > 0) {
      # use them as-is
      if (length(adjustRgb) == 0) {
         adjustRgb <- CLranges$adjustRgb;
      }
   } else {
      CLranges <- setCLranges(lightMode=lightMode,
         Crange=Crange,
         Lrange=Lrange,
         adjustRgb=adjustRgb,
         setOptions="FALSE");
      adjustRgb <- CLranges$adjustRgb;
      Crange <- CLranges$Crange;
      Lrange <- CLranges$Lrange;
   }

   if (length(darkFactor) <= 1) {
      darkFactor <- c(1, darkFactor);
   }
   if (length(sFactor) <= 1) {
      sFactor <- c(1, sFactor);
   }

   ## Convert list(...) into something usable here
   xList <- list(...);

   ## Determine if the color values have been defined
   if (length(fgText) == 0) {
      fgTextBase <- tail(rmNULL(xList), 1);
      if (length(names(fgTextBase)) > 0) {
         fgTextBaseIsColor <- FALSE;
      } else {
         if (igrepHas("list", class(fgTextBase[[1]]))) {
            fgTextBase <- unlist(fgTextBase, recursive=FALSE);
         }
         if (!is.list(fgTextBase[[1]])) {
            # it may contain colors and NA, but must contain at least one color
            fgTextBaseIsColor <- all(sapply(fgTextBase, function(i){
               icolor <- isColor(i)
               (length(rmNA(i)) > 0 && all(icolor[!is.na(i)]))
            }));
         } else {
            fgTextBaseIsColor <- FALSE;
         }
      }
      if (fgTextBaseIsColor) {
         fgText <- fgTextBase;
         fgText <- unlist(fgText, recursive=FALSE);
         xList <- head(rmNULL(xList), -1);
      } else {
         if (length(bgText) == 0) {
            fgText <- fgDefault;
         } else {
            fgText <- c(NA);
         }
      }
   }

   if (length(xList) == 0) {
      if (verbose) {
         printDebug("printDebug(): ",
            "input recognzied as color vector.");
      }
      if (!igrepHas("list", class(fgText))) {
         fgText <- list(fgText);
      }
      xList <- lapply(fgText, function(i){
         if (is.null(names(i))) {
            i;
         } else {
            names(i);
         }
      });

      ## This style prints the names in the color as defined
      printDebug(xList[[1]],
         fgText=fgText,
         verbose=verbose,
         indent=paste0(indent, "   "),
         bgText=bgText,
         fgTime=fgTime,
         timeStamp=timeStamp,
         comment=comment,
         formatNumbers=formatNumbers,
         trim=trim,
         digits=digits,
         nsmall=nsmall,
         justify=justify,
         big.mark=big.mark,
         small.mark=small.mark,
         zero.print=zero.print,
         width=width,
         doColor=doColor,
         splitComments=splitComments,
         collapse=collapse,
         sep=sep,
         doReset=doReset,
         detectColors=detectColors,
         dex=dex,
         lightMode=NULL,
         Lrange=Lrange,
         Crange=Crange,
         removeNA=removeNA,
         replaceNULL=replaceNULL,
         adjustRgb=adjustRgb,
         byLine=byLine,
         invert=invert,
         htmlOut=htmlOut);
      return(invisible(NULL));
   } else {

      ## Optionally split the list into a single vector
      if (splitComments) {
         xList <- as.list(unlist(rmNULL(xList, replaceNULL="NULL")));
      }
      ## Optionally remove NA values
      if (removeNA) {
         xList <- lapply(xList, function(i){
            i[is.na(i)] <- "";
            i;
         })
      }
      ## Optionally apply crayon::reset() to delimiter sep
      if (length(doReset) == 0) {
         if (!crayon::has_style(sep) && doColor == 2) {
            if (verbose) {
               printDebug("printDebug(): ",
                  c("applied ", "sep <- crayon::reset(sep)"),
                  sep="");
            }
            sep <- crayon::reset(sep);
         }
      } else if (doReset && doColor == 2) {
         if (!crayon::has_style(sep)) {
            sep <- crayon::reset(sep);
            if (verbose) {
               printDebug("printDebug(): ",
                  c("applied ", "sep <- crayon::reset(sep)"),
                  sep="");
            }
         } else {
            sep <- crayon::reset(crayon::strip_style(sep));
            if (verbose) {
               printDebug("printDebug(): ",
                  c("applied ", "sep <- crayon::reset(crayon::strip_style(sep))"),
                  sep="");
            }
         }
      }
      ## Optionally replace NULL with "NULL"
      xList <- rmNULL(xList, replaceNULL="NULL");

      ## Attempt to unlist each list element
      xList <- unnestList(xList);

      ## Extend fgText and bgText to the length of xList
      if (doColor) {
         if (!igrepHas("list", class(fgText))) {
            fgText <- as.list(fgText);
         }
         if (length(fgText) > 0) {
            fgText <- rep(fgText, length.out=length(xList));
         }
         if (length(bgText) > 0) {
            bgText <- rep(bgText, length.out=length(xList));
         }
      }

      xListSlength <- lengths(xList);
      if (doColor > 0 && any(xListSlength >= 1)) {
         if (verbose) {
            printDebug("printDebug(): ",
               "Applying colors within list elements.")
         }
         xListMulti <- which(xListSlength > 1);
         fgText <- lapply(seq_along(fgText), function(i){
            iColor <- fgText[[i]];
            iColor_na <- is.na(iColor);
            if (all(iColor_na)) {
               iColor <- rep(iColor,
                  length.out=xListSlength[i]);
            } else if (length(iColor) == 1) {
               ## If the color is dark, make the off-color lighter,
               ## if the color is bright, make the off-color darker
               iL <- col2hcl(iColor)["L",];
               if (head(dex, 1) > 0) {
                  iColor <- rep(
                     color_dither(iColor,
                        min_contrast=1.1 + dex/10),
                     length.out=xListSlength[i]);
               } else {
                  iColor <- rep(iColor,
                     length.out=xListSlength[i]);
               }
               if (any(iColor_na)) {
                  iColor[iColor_na] <- NA;
               }
            } else {
               iColor <- rep(iColor,
                  length.out=xListSlength[i]);
            }
            iColor;
         });
         if (length(bgText) > 0) {
            bgText <- lapply(seq_along(bgText), function(i){
               iColor <- bgText[[i]];
               iColor_na <- is.na(iColor);
               if (all(iColor_na)) {
                  iColor <- rep(iColor,
                     length.out=xListSlength[i]);
               } else if (length(iColor) == 1) {
                  iL <- col2hcl(iColor)["L",];
                  if (head(dex, 1) > 0) {
                     iColor <- rep(
                        color_dither(iColor,
                           min_contrast=1.1 + dex / 10),
                        length.out=xListSlength[i]);
                  } else {
                     iColor <- rep(iColor,
                        length.out=xListSlength[i]);
                  }
                  if (any(iColor_na)) {
                     iColor[iColor_na] <- NA;
                  }
               } else {
                  iColor <- rep(iColor,
                     length.out=xListSlength[i]);
               }
               iColor;
            });
         }
      }
      fgText <- unlist(fgText);
      bgText <- unlist(bgText);
      if (invert) {
         if (verbose) {
            printDebug("printDebug(): ",
               c("Inverting ", "fgText", " and ", "bgText"),
               sep="");
         }
         fgText1 <- fgText;
         fgText <- bgText;
         bgText <- fgText1;
      }

      if (verbose) {
         printDebug("printDebug(): ",
            "xList (before):");
         print(xList);
      }
      x <- unlist(lapply(xList, function(i){
         if (verbose) {
            printDebug("printDebug(): ",
               "i:");
            print(i);
         }
         if (formatNumbers &&
               igrepHas("numeric|float|integer|long|double", class(i))) {
            i <- format(i,
               trim=trim,
               digits=digits,
               nsmall=nsmall,
               justify=justify,
               big.mark=big.mark,
               small.mark=small.mark,
               zero.print=zero.print,
               width=width);
         } else {
            i <- as.character(i);
         }
         ## trim duplicated delimiters
         ## (under evaluation since it bugs out when the delimiter is ANSI-colored)
         if (length(i) > 1 && nchar(sep) > 0) {
            i[-length(i)] <- paste0(i[-length(i)], sep);
         }
         ## apply final color reset
         if (doColor == 2) {
            i[length(i)] <- paste0(i[length(i)], crayon::reset(""));
         }
         i;
      }));
      if (verbose) {
         printDebug("printDebug(): ",
            "x (after):");
         print(x);
      }

      ## Convert "transparent" to "grey" for compatibility with crayon
      fgAlpha <- col2alpha(fgText);
      if (any(fgAlpha <= 0)) {
         fgText[fgAlpha <= 0] <- "#777777";
      }
      bgAlpha <- col2alpha(bgText);
      if (any(bgAlpha <= 0)) {
         #bgText[bgAlpha <= 0] <- "#777777";
         ## use NA which makes bgText color transparent
         bgText[bgAlpha <= 0] <- NA;
      }

      if (doColor == 2) {
         ################### crayon output
         if (verbose) {
            printDebug("printDebug(): ",
               "Using crayon package colorization.");
         }
         if (crayon::num_colors() < 256) {
            ## If crayon detects 8-color terminal,
            ## make one attempt at 256-color terminal
            Sys.setenv(TERM="xterm-256color");
            crayon::num_colors(256);
         }
         if (timeStamp) {
            timeStampValue <- paste(c("(", crayon::make_style("bold")(
               make_styles(style=fgTime,
                  bg_style=NA,
                  bg=FALSE,
                  adjustRgb=adjustRgb,
                  Lrange=Lrange,
                  Crange=Crange,
                  setOptions="FALSE",
                  verbose=verbose>1,
                  text=format(Sys.time(), "%H:%M:%S"))
            ), ") ", format(Sys.time(), "%d%b%Y"), ": "), collapse="");
         } else {
            timeStampValue <- "";
         }

         printValue <- paste(sapply(seq_along(x), function(ix){
            xStr <- x[ix];
            if (igrepHas("factor", class(xStr))) {
               xStr <- as.character(xStr);
            }
            new_string <- make_styles(
               style=fgText[ix],
               bg_style=bgText[ix],
               bg=FALSE,
               text=xStr,
               verbose=verbose>1,
               Lrange=Lrange,
               Crange=Crange,
               setOptions="FALSE",
               adjustRgb=adjustRgb);
            new_string;
         }), collapse=collapse);

         printString <- c(timeStampValue,
            indent,
            printValue);
         if (length(comment) > 0 && nchar(comment) > 0) {
            printString <- c(comment, printString);
         }
         cat(printString, "\n",
            file=file,
            append=append);
      } else if (doColor == 1) {
         ################### HTML span output
         if (verbose) {
            printDebug("printDebug(): ",
               "Using html span colorization.");
         }
         if (timeStamp) {
            timeStampValue <- paste(c("(",
               make_html_styles(style=fgTime,
                  bg_style=NA,
                  bg=FALSE,
                  lightMode=lightMode,
                  Lrange=Lrange,
                  Crange=Crange,
                  setOptions="FALSE",
                  verbose=verbose>1,
                  text=format(Sys.time(), "%H:%M:%S")),
               ") ", format(Sys.time(), "%d%b%Y"), ": "), collapse="");
         } else {
            timeStampValue <- "";
         }
         printValue <- paste(sapply(seq_along(x), function(ix){
            xStr <- x[ix];
            if (igrepHas("factor", class(xStr))) {
               xStr <- as.character(xStr);
            }
            new_string <- make_html_styles(
               style=fgText[ix],
               bg_style=bgText[ix],
               bg=FALSE,
               text=xStr,
               verbose=verbose>1,
               lightMode=lightMode,
               Lrange=Lrange,
               Crange=Crange,
               setOptions="FALSE",
               adjustRgb=adjustRgb);
            new_string;
         }), collapse=collapse);

         printString <- c(timeStampValue,
            indent,
            printValue);
         if (length(comment) > 0 && nchar(comment) > 0) {
            printString <- c(comment, printString);
         }
         # Note addition of <br> for newline
         cat(
            paste0(
               cPaste(printString, sep=""),
               "<br/>\n"),
            file=file,
            append=append);
      } else {
         ################### no colorized output
         if (verbose) {
            printDebug("printDebug(): ",
               "Using no colorization.");
         }
         if (timeStamp) {
            timeStampValue <- as.character(format(Sys.time(),
               "(%H:%M:%S) %d%b%Y: "));
         } else {
            timeStampValue <- "";
         }
         printValue <- paste(sapply(seq_along(x), function(ix){
            xStr <- x[ix];
            if (igrepHas("factor", class(xStr))) {
               xStr <- as.character(xStr);
            }
            new_string <- xStr;
            new_string;
         }), collapse=collapse);
         printString <- c(timeStampValue,
            indent,
            printValue);
         if (length(comment) > 0 && nchar(comment) > 0) {
            printString <- c(comment, printString);
         }
         cat(printString, "\n",
            file=file,
            append=append);
      }
   }
   invisible(NULL);
}

#' print colorized output to R console, inverted
#'
#' print colorized output to R console, inverted
#'
#' This function prints colorized output to the R console, using the
#' same logic as `printDebug` except by default the color is inverted
#' so the default `fgText` colors are applied to the background.
#'
#' @family jam practical functions
#'
#' @returns `NULL` invisibly, this function is called for the side effect
#'    of printing output using `cat()`.
#'
#' @rdname printDebug
#'
#' @export
printDebugI <- function
(...,
 invert=TRUE)
{
   printDebug(...,
      invert=invert);
}


#' print colorized output to HTML
#'
#' print colorized output to HTML
#'
#' This function prints colorized output in HTML form, using the
#' same logic as `printDebug()` except by default the output is HTML.
#' The intended use is for 'Rmarkdown' with chunk option `results='asis'`,
#' which causes the HTML code to be interpreted directly as HTML.
#'
#' This function internally calls `printDebug()` which then calls
#' `make_html_styles()`. The text is surrounded by `<span color='#FFFFFF'>`
#' HTML formatting.
#'
#' @family jam practical functions
#'
#' @returns `NULL` invisibly, this function is called for the side effect
#'    of printing output using `cat()`.
#'
#' @rdname printDebug
#'
#' @export
printDebugHtml <- function
(...,
 htmlOut=TRUE,
 comment=FALSE)
{
   printDebug(...,
      htmlOut=htmlOut,
      comment=comment);
}

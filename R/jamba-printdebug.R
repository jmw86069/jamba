
#' print colorized output to R console
#'
#' print colorized output to R console
#'
#' This function prints colorized output to the R console, with some
#' rules for colorizing the output to help visually distinguish items.
#' Its output also by default begins with comment '#' characters, a
#' datetimestamp, so it the output is copied back into the R console it
#' will not cause a new command to be run.
#'
#' The colorization uses a vector or list of colors for fgText (foreground)
#' and bgText (background.), applied to each item in '...'. When an item in
#' '...' is a vector, each vector element is colored alternating light
#' and dark from that base color, to give visual indication of each element.
#' The next item in '...' receives the next color from fgText, and so on.
#' Colors in fgText are recycled to the length of '...'
#'
#' @param ... text to be printed to the R console.
#' @param fgText vector of R compatible colors, or a list of vectors of
#'    R compatible colors, to define the foreground colors. In the latter
#'    case, each vector is applied to each list item from '...'
#' @param bgText vector of R compatible colors, or a list of vectors,
#'    to define the background color.
#' @param fgTime character R color to colorize the time
#' @param timeStamp logical whether to include a time stamp in output
#' @param comment logical whether to prefix output with '##' as a comment
#' @param formatNumbers logical whether to format numbers using
#'    \code{\link[base]{format}} which controls the number of digits displayed.
#' @param trim,digits,nsmall,justify,big.mark,small.mark,zero.print,width
#'    parameters sent to the \code{\link[base]{format}} function.
#' @param doColor NULL or logical indicating whether to colorize output. If
#'    NULL it detects whether the crayon package is available and console
#'    color is enabled.
#' @param splitComments logical whether to color each element independently
#'    without light-dark alternating pattern.
#' @param collapse character collapse string used to separate list items,
#'    by default "" so text separation is expected in the input data.
#' @param sep character separator used to separate vector elements, when
#'    a list items contains a vector.
#' @param doReset logical or `NULL`, indicating whether to apply
#'    `crayon::reset()` to the delimiter `sep`. When `doReset=TRUE` the
#'    style on the delimiter is forced to reset, using `crayon::reset()`,
#'    or to remove pre-existing style with `crayon::strip_style()`. When
#'    `doReset=NULL` and `sep` contains ANSI escape characters, they are
#'    left as-is; when `doReset=NULL` and `sep` does not contain ANSI escape
#'    characters, `sep` becomes `crayon::reset(sep)` which forces the style
#'    to be reset between printed values.
#' @param detectColors logical whether to detect and potentially try to
#'    correct console color capabilities.
#' @param darkFactor numeric darkness to apply to alternative vector values
#'    when using alternating light-dark color shading.
#' @param sFactor numeric color saturation to apply to alternative vector
#'    values when using alternating light-dark color shading.
#' @param Crange numeric range of chroma values, ranging
#'    between 0 and 100. When NULL, default values will be
#'    assigned to Crange by `setCLranges()`.
#' @param Lrange numeric range of luminance values, ranging
#'    between 0 and 100. When NULL, default values will be
#'    assigned to Lrange by `setCLranges()`.
#' @param lightMode boolean or NULL, indicating whether the text background
#'    color is light, thus imposing a maximum brightness for colors displayed.
#'    It use lightMode if defined by the function caller, otherwise it will
#'    use options("jam.lightMode") if defined, lastly it will attempt to detect
#'    whether running inside Rstudio by checking the environment variable
#'    "RSTUDIO", and if so it will assign lightMode TRUE.
#' @param removeNA logical whether to remove NA values and not print to
#'    the console.
#' @param replaceNULL character or NULL, optionally replace NULL elements
#'    with non-NULL character value.
#' @param adjustRgb numeric value adjustment used during the conversion of
#'    RGB colors to ANSI colors, which is inherently lossy. If not defined,
#'    it uses the default returned by `setCLranges()` which itself uses
#'    \code{getOption("jam.adjustRgb")} with default=0. In order to boost
#'    color contrast, an alternate value of -0.1 is suggested.
#' @param byLine logical whether to delimit lists by line instead of
#'    using collapse to combine them onto one line.
#' @param verbose logical whether to print verbose output
#' @param indent character optional characters used as a prefix to indent
#'    output.
#' @param file passed to \code{cat}, to allow sending output to
#'    a specified file.
#' @param append logical whether to append output, relevant only when
#'    \code{file} specifies a filename.
#' @param invert logical indicating whether foreground and background
#'    colors should be switched.
#' @param htmlOut logical indicating whether to print HTML span
#'    output, using format
#'    `<span style="color:fg;background-color:bg">text</span>`.
#'    This argument is not yet implemented, more testing is required
#'    to determine the best mechanism to use for things like RMarkdown
#'    rendering, and R-shiny app rendering.
#'
#' @return This function is called for the by-product of printing
#'    debug output, it returns `invisible(NULL)`, no output.
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
#' @export
printDebug <- function
(...,
 fgText=NULL,#c("orange", "lightblue"),
 bgText=NULL,
 fgTime="cyan",
 timeStamp=TRUE,
 comment=TRUE,
 formatNumbers=TRUE,
 trim=TRUE,
 digits=NULL,
 nsmall=0L,
 justify="left",
 big.mark="",
 small.mark="",
 zero.print=NULL,
 width=NULL,
 doColor=NULL,
 splitComments=FALSE,
 collapse="",
 sep=",",
 doReset=NULL,
 detectColors=TRUE,
 darkFactor=c(1,1.5),
 sFactor=c(1,1.5),
 lightMode=NULL,
 Crange=getOption("jam.Crange"),
 Lrange=getOption("jam.Lrange"),
 removeNA=FALSE,
 replaceNULL=NULL,
 adjustRgb=getOption("jam.adjustRgb"),
 byLine=FALSE,
 verbose=FALSE,
 indent="",
 keepNA=TRUE,
 file="",
 append=TRUE,
 invert=FALSE,
 htmlOut=FALSE,
 x)
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
   if (byLine) {
      collapse <- "\n";
   }

   ## Determine the type of coloration we can use
   ## by setting up a conditional array to capture various combinations
   ## of 3 options:
   ## - availability of the package crayon
   ## - availability of the package xterm256
   ## - the given preference doColor value (1=xterm256, 2=crayon, 0=no color, everything else is pickem)
   if (htmlOut) {
      doColor <- 1;
   } else {
      hasCrayon <- as.character(suppressWarnings(suppressPackageStartupMessages(require(crayon))));
      if (is.null(doColor)) {
         if (hasCrayon %in% "TRUE") {
            doColor <- 2;
         } else {
            doColor <- 0;
         }
      }
   }

   ## Check lightMode, whether the background color is light or not
   CLranges <- setCLranges(lightMode=lightMode,
      Crange=Crange,
      Lrange=Lrange,
      adjustRgb=adjustRgb);
   if (length(adjustRgb) == 0) {
      adjustRgb <- CLranges$adjustRgb;
   }
   Crange <- CLranges$Crange;
   Lrange <- CLranges$Lrange;

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
      if (igrepHas("list", class(fgTextBase[[1]]))) {
         fgTextBase <- unlist(fgTextBase, recursive=FALSE);
      }
      fgTextBaseIsColor <- sapply(fgTextBase, function(i){
         all(rmNA(isColor(i)));
      });
      if (fgTextBaseIsColor) {
         fgText <- fgTextBase;
         fgText <- unlist(fgText, recursive=FALSE);
         xList <- head(rmNULL(xList), -1);
      } else {
         if (length(bgText) == 0) {
            fgText <- c("darkorange1", "dodgerblue");
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
         darkFactor=darkFactor,
         sFactor=sFactor,
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
         if (!crayon::has_style(sep)) {
            if (verbose) {
               printDebug("printDebug(): ",
                  c("applied ", "sep <- crayon::reset(sep)"),
                  sep="");
            }
            sep <- crayon::reset(sep);
         }
      } else if (doReset) {
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
               if (1 == 2) {
                  if (iL < 70) {
                     useDarkFactor <- darkFactor * -1;
                  } else {
                     useDarkFactor <- darkFactor;
                  }
                  iColor <- rep(
                     makeColorDarker(darkFactor=useDarkFactor,
                        sFactor=c(sFactor),
                        iColor,
                        keepNA=keepNA),
                     length.out=xListSlength[i]);
               } else {
                  if (iL < 70) {
                     ik <- 1;
                  } else if (iL > 90) {
                     ik <- 4;
                  } else {
                     ik <- 3;
                  }
                  iColor <- rep(
                     c(iColor,
                        color2gradient(iColor, n=4, gradientWtFactor=1)[ik]),
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
               if (length(iColor) == 1) {
                  iColor <- rep(
                     makeColorDarker(darkFactor=c(darkFactor),
                        sFactor=c(sFactor),
                        iColor,
                        keepNA=keepNA),
                     length.out=xListSlength[i]);
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
         if (doColor) {
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
            timeStampValue <- paste(c("(", make_style("bold")(
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

         printString <- c(timeStampValue, printValue);
         if (comment) {
            printString <- c("## ", printString);
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
                  Lrange=Lrange,
                  Crange=Crange,
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
               Lrange=Lrange,
               Crange=Crange,
               adjustRgb=adjustRgb);
            new_string;
         }), collapse=collapse);

         printString <- c(timeStampValue, printValue);
         if (comment) {
            printString <- c("## ", printString);
         }
         cat(printString, "\n",
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
         printString <- c(timeStampValue, paste(x, collapse=collapse));
         if (comment) {
            printString <- c("#  ", printString);
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
#' @export
printDebugI <- function
(...,
 invert=TRUE)
{
   printDebug(...,
      invert=invert);
}


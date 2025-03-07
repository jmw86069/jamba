#'


#' prefix integers with leading zeros
#'
#' prefix integers with leading zeros
#'
#' The purpose of this function is to pad integer numbers so they contain
#' a consistent number of digits, which is helpful when sorting values
#' as character strings.
#'
#' @returns `character` vector of length(x).
#'
#' @family jam string functions
#'
#' @param x `integer`, `numeric`, or `character` vector. In reality, only
#'    `nchar(x)` is required to determine padding.
#' @param padCharacter `character` with nchar(padCharacter)==1, used to pad
#'    each digit as a prefix.
#' @param useNchar `NULL` or `integer` number of digits used, or if the maximum
#'    `nchar(x)` is higher, that number of digits is used. Note `useNchar` is
#'    mostly useful when all numbers are less than 10, but the desired output
#'    is to have a fixed number of digits 2 or higher.
#' @param ... additional parameters are ignored.
#'
#' @examples
#' padInteger(c(1, 10, 20, 300, 5000))
#'
#' @export
padInteger <- function
(x,
 padCharacter="0",
 useNchar=NULL,
 ...)
{
   ## Purpose is to pad integer numbers so they contain the same number of
   ## characters.  This function is optimized to be (hopefully) faster than
   ## padString().
   if (length(x) == 0) {
      return(x);
   }
   maxNchar <- max(c(useNchar, nchar(x)));
   if (maxNchar == 0) {
      return("");
   }
   x1 <- paste(paste(rep(padCharacter, maxNchar), collapse=""), x, sep="");
   x2 <- substr(x1, nchar(x1)-maxNchar+1, nchar(x1));
   return(x2);
}

#' pad a character string to a fixed length
#'
#' pad a character string to a fixed length
#'
#' @returns `character` vector of length(x)
#'
#' @family jam string functions
#'
#' @param x `character` vector
#' @param stringLength `integer` length for the resulting character strings
#'    in `x`. By default, all strings are padded to the length of the
#'    longest entry, however stringLength can be defined to impose strict
#'    number of characters for all entries.
#' @param padCharacter `character` string with nchar=1 used for padding.
#' @param justify `character` string with "left", "right", "center" to indicate
#'    alignment of the resulting text string.
#' @param ... additional parameters are ignored.
#'
#' @examples
#' padString(c("one","two","three"));
#' padString(c("one","two","three","four"), padCharacter="_", justify="center");
#'
#' @export
padString <- function
(x,
 stringLength=max(nchar(x)),
 padCharacter=" ",
 justify="left",
 ...)
{
   ## Purpose is to add padding characters (spaces) to force a string to
   ## be a certain fixed number of characters wide
   ## Note: It will also crop strings to this same length in case the
   ## given string(s) are too long.
   if (length(stringLength) > 1) {
      x <- substring(x, 1, stringLength);
   } else {
      x <- substr(x, 1, stringLength);
   }
   if (justify == "right") {
      x <- sapply(x, function(i){
         paste(c(rep(padCharacter, stringLength-nchar(i)), i), collapse="");
      })
   } else if (justify == "center") {
      x <- sapply(x, function(i){
         n1 <- ceiling((stringLength-nchar(i))/2);
         n2 <- floor((stringLength-nchar(i))/2);
         paste(c(rep(padCharacter, n1),
            i,
            rep(padCharacter, n2)), collapse="");
      })
   } else {
      ## Everything else gets left-justified
      x <- sapply(x, function(i){
         paste(c(i, rep(padCharacter, stringLength-nchar(i))), collapse="");
      })
   }
   return(x);
}

#' convert date to age in days
#'
#' convert date to age in days
#'
#' @family jam date functions
#'
#' @returns integer value with the number of calendar days before the
#'    current date, or the `nowDate` if supplied.
#'
#' @param testDate `character` date recognized by `asDate()`,
#'    representing the test date.
#' @param nowDate `character` date recognized by `asDate()`,
#'    representing the reference date, by default the current day.
#' @param units `character` indicating the units, as used by
#'    `difftime()`.
#' @param ... additional parameters are ignored.
#'
#' @examples
#' dateToDaysOld("23aug2007")
#'
#' @export
dateToDaysOld <- function
(testDate,
 nowDate=Sys.Date(),
 units="days",
 ...)
{
   ## Purpose is to report the number of days old something is,
   ## using the asDate() format "DDmmmYYYY" like "03may1997"
   as.integer(difftime(nowDate, asDate(testDate), units=units));
}

#' convert date DDmmmYYYY to Date
#'
#' convert date DDmmmYYYY to Date
#'
#' This function converts a text date string to Date object, mainly to
#' allow date-related math operations, for example \code{\link[base]{difftime}}.
#'
#' @family jam date functions
#'
#' @returns Date object
#'
#' @param getDateValues `character` date, in format recognized by dateFormat
#' @param dateFormat `character` string representing the recognized date
#'    format, by default `"DDmmmYYYY"`, which recognizes `"23aug2007"`.
#' @param ... additional parameters are ignored.
#'
#' @examples
#' asDate(getDate());
#'
#' @export
asDate <- function
(getDateValues,
 dateFormat="%d%b%Y",
 ...)
{
   ## Purpose is to convert getDate() formatted values "21jan2012"
   ## into proper R Date objects for sorting purposes
   ##
   ## dateFormat is the output format
   newDates <- as.Date(getDateValues, dateFormat);
   names(newDates) <- getDateValues;
   return(newDates);
}

#' get simple date string
#'
#' get simple date string in the format DDmonYYYY such as 17jul2018.
#'
#' Gets the current date in a simplified text string. Use
#' `asDate()` to convert back to Date object.
#'
#' @returns `character` vector with simplified date string
#'
#' @family jam date functions
#'
#' @param t current time in an appropriate class such as `"POSIXct"`
#'    or `"POSIXt"`. The default is output of `Sys.time()`.
#' @param trim `logical` whether to trim the output of `format()`
#'    in the event that multiple values are sent for argument `t`.
#' @param dateFormat `character` string representing the recognized date
#'    format, by default `"DDmmmYYYY"`, which recognizes `"23aug2007"`.
#' @param ... additional parameters sent to `format()`.
#'
#' @examples
#' getDate();
#'
#' @export
getDate <- function
(t=Sys.time(),
 trim=TRUE,
 dateFormat="%d%b%Y",
 ...)
{
   ## Purpose is to define a data in the format
   ## 05may2011 (DDmmmYYYY)
   tolower(
      format(t,
         dateFormat,
         trim=trim,
         ...));
}

#' set R prompt with project name and R version
#'
#' set R prompt with project name and R version
#'
#' This function sets a simple, colorized R prompt with useful
#' information:
#' * `projectName`
#' * R version, major and minor included
#' * Process ID (PID)
#'
#' The prompt is defined in `options("prompt")`.
#'
#' ## Where Am I?
#'
#' It is useful for the question: "What version of R?"
#' In rare cases, multiple R versions can be active at once (!), see
#' the `rig` package for this exciting capability.
#'
#' ## What Am I Doing?
#'
#' The core question addressed is : "What am I working on?"
#' The project name is especially useful when working
#' with multiple active R sessions.
#'
#' ## How Do I Stop This Thing?
#'
#' It may also be useful for the question "How do I stop this thing",
#' by returning the Process ID to be used to kill a long-running process
#' without fear of killing the **wrong** long-running process.
#'
#' ## Can It Have Color?
#'
#' Then of course, meeting the above requirements, at least make it pretty.
#'
#' ## Word-Wrap Gone Awry
#'
#' A color-encoded prompt may sometimes interfere
#' with word-wrapping on the R console.
#' A long line may wrap prematurely
#' before reaching the right edge of the screen.
#' There are two frequent causes of this issue:
#'
#' 1. `options("width")` is sometimes defined too narrow for the
#' screen. When resizing the console, this option should be updated,
#' and sometimes this update fails. To fix, either resize the window
#' briefly again, or define `options("width")` manually.
#' (Or debug the reason that this option is not being updated.)
#' 2. The terminal `locale` is sometimes mismatched with the terminal,
#' usually caused by a layer of terminal emulation which is not
#' compatible with ANSI color codes, or ANSI escape codes.
#'
#'    * Some examples: 'PuTTY' on 'Windows', GNU 'screen', 'tmux'.
#'    * Check `Sys.env("LC_ALL")`. The most common results are
#'    `"C"` for generic C-type output, or a Unicode/UTF-8 locale such as
#'    `"en_US.UTF-8"` ('enUS' is English-USA in this context).
#'    In general, Unicode/UTF-8 is recommended, with benefit that
#'    it more readily displays other Unicode characters.
#'    However, sometimes the terminal environment (PuTTY or iTerm)
#'    is expecting one locale, but is receiving another. Either
#'    switching the terminal expected locale, or the R console locale,
#'    may resolve the mismatch.
#'
#' R uses 'readline' for unix-like systems by default, and
#' issues related to using color prompt are handled at that level.
#'
#' The 'readline' library allows escaping ANSI color characters so they
#' do not contribute to the estimated line width, and these codes are
#' used in `setPrompt()`.
#'
#' The final workaround is `useColor=FALSE`, but that would be a sad
#' outcome.
#'
#' @returns `list` named `"prompt"`, suitable to use in `options()`
#'    with the recommended prompt.
#'    When `updateOptions=FALSE` use: `options(setPrompt("projectName"))`
#'
#' @family jam practical functions
#'
#' @param projectName `character` string, default "unnamed", used as
#'    a label to represent the project work.
#' @param useColor `logical` whether to define a color prompt if the
#'    `crayon` package is installed.
#' @param projectColor,bracketColor,Rcolors,PIDcolor,promptColor `character`
#'    colors used when `useColor==TRUE` and the `crayon` package
#'    is installed:
#'    * `projectColor` colors the project name;
#'    * `bracketColor` colors the curly brackets around the project;
#'    * `Rcolors` can be a vector of 3 colors, colorizing "R",
#'    the "-" divider, and the R version;
#'    * `PIDcolor` colors the PID when `usePid=TRUE`; and
#'    * `promptColor` colors the `">"` at the end of the prompt.
#' @param usePid `logical` whether to include the process ID in the prompt.
#'    Including the PID is helpful for the rare occasion when a process is
#'    hung and needs to be stopped directly.
#' @param resetPrompt `logical` whether to revert all changes to the prompt
#'    back to the default R prompt, that is, no color and no projectName.
#' @param addEscape `logical` or `NULL` indicating whether to wrap color
#'    encoding ANSI inside additional escape sequences. This change is
#'    helpful for linux-based (readline-based) R consoles, by telling
#'    the console not to count ANSI color control characters as visible
#'    characters when determining word wrapping on the console. Note
#'    that RStudio does not work well with this setting.
#'    If you find that the word-wrap is incorrect in the R console, try
#'    `addEscape=TRUE`. Apparently most versions of RStudio will already
#'    adjust (and prevent) colorizing the prompt during editing, presumably
#'    to sidestep the problem of calculating the correct character length.
#'    By default when `addEscape` is `NULL`, it checks whether environmental
#'    variable `RSTUDIO` equals `"1"` (running inside RStudio) then sets
#'    `addEscape=FALSE`; otherwise it defines `addEscape=TRUE`.
#'    In most cases for commandline prompts, `addEscape=TRUE` is helpful
#'    and not problematic.
#' @param updateOptions `logical` whether to update the user `options()`
#'    with `options(prompt="...")`, default TRUE.
#' @param verbose `logical` whether to print verbose output.
#' @param debug `logical` indicating whether to print the ANSI control
#'    character output for the full prompt, for visual review.
#' @param ... additional parameters are passed to `make_styles()` which is
#'    only relevant with the argument `useColor=TRUE`.
#'
#' @examples
#' setPrompt("jamba")
#'
#' setPrompt("jamba", projectColor="purple");
#'
#' setPrompt("jamba", usePid=FALSE);
#'
#' setPrompt(resetPrompt=TRUE);
#'
#' @export
setPrompt <- function
(projectName="unnamed",
 useColor=TRUE,
 projectColor="yellow",
 bracketColor="white",
 Rcolors=c("white","white","white"),
 PIDcolor=NA,
 promptColor="white",
 usePid=TRUE,
 resetPrompt=FALSE,
 addEscape=NULL,
 updateOptions=TRUE,
 debug=FALSE,
 verbose=FALSE,
 ...)
{
   ## Set the R command prompt to display the current R project name
   ##
   ## usePid will include the parent process PID in the prompt, which
   ## can be helpful when an R session hangs, but you have multiple active
   ## R sessions, and might otherwise not be able to tell which R session
   ## is problematic.
   ##
   ## if projectName is not supplied, try .GlobalEnv
   if (length(projectName) > 1) {
      projectName <- cPaste(projectName, sep="-");
   }
   if (length(useColor) > 0 &&
         useColor &&
         check_pkg_installed("crayon")) {
      useColor <- 1;
   } else {
      useColor <- 0;
   }
   promptValue <- "> ";
   projectColor <- rep(c(projectColor, "white"), length.out=1);
   bracketColor <- rep(c(bracketColor, "white"), length.out=1);
   Rcolors <- rep(c(rep(c(Rcolors), length.out=3), "white"), length.out=3);
   PIDcolor <- rep(c(PIDcolor, NA), length.out=1);
   promptColor <- rep(c(promptColor, "white"), length.out=1);
   if (verbose) {
      printDebug("setPrompt(): ",
         "useColor:",
         useColor);
   }
   if (resetPrompt) {
      if (verbose) {
         printDebug("setPrompt(): ",
            "Resetting basic prompt for R.");
      }
      addEscape <- FALSE;
      promptValue <- "> ";
   } else if (useColor == 1) {
      ## use crayon
      if (!usePid) {
         promptValue <- paste(
            make_styles(
               style=c(bracketColor,
                  projectColor,
                  bracketColor,
                  NA,
                  Rcolors,
                  promptColor),
               c("{",
                  projectName,
                  "}",
                  "-",
                  "R",
                  "-",
                  paste0(R.version[c("major", "minor")], collapse="."),
                  "> "),
               verbose=verbose,
               ...
            ),
            collapse="");
      } else {
         promptValue <- paste(
            make_styles(
               style=c(bracketColor,
                  projectColor,
                  bracketColor,
                  NA,
                  Rcolors,
                  NA,
                  PIDcolor,
                  promptColor),
               c("{",
                  projectName,
                  "}",
                  "-",
                  "R",
                  "-",
                  paste0(R.version[c("major", "minor")], collapse="."),
                  "_",
                  Sys.getpid(),
                  "> "),
               verbose=verbose,
               ...
            ),
            collapse="");
      }
   } else {
      if (verbose) {
         printDebug("Setting non-colorized prompt for R.");
      }
      if (!usePid) {
         promptValue <- paste0("{",
            projectName,
            "}",
            "-R-",
            paste(R.version[c("major", "minor")], collapse="."),
            "> ");
      } else {
         promptValue <- paste0("{",
            projectName,
            "}",
            "-R-",
            paste(R.version[c("major", "minor")], collapse="."),
            "_",
            Sys.getpid(),
            "> ");
      }
   }
   ## optionally add escape sequences
   if (length(addEscape) == 0) {
      if (Sys.getenv("RSTUDIO") %in% "1") {
         addEscape <- FALSE;
      } else {
         addEscape <- TRUE;
      }
   }
   if (useColor && addEscape) {
      promptValue <- gsub("(\033[[0-9;]+[nm]{0,1})",
         "\001\\1\002",
         promptValue);
   }
   if (verbose && debug) {
      cat("promptValue:\n");
      print(promptValue);
   }

   if (verbose) {
      cat("setPrompt() defined promptValue: '", promptValue, "'\n\n");
   }
   if (TRUE %in% updateOptions) {
      options("prompt"=promptValue);
   }
   invisible(list(prompt=promptValue));
}


#' Paste data.frame rows into character vector
#'
#' Paste data.frame rows into a character vector, optionally removing
#' empty fields in order to avoid delimiters being duplicated.
#'
#' This function is intended to paste `data.frame` (or `matrix`, or `tibble`)
#' values for each row of data.
#' It differs from using `apply(x, 2, paste)`:
#'
#' * it handles factors without converting to integer factor level numbers.
#' * it also by default removes blank or empty fields, preventing the delimiter
#' from being included multiple times, per the `condenseBlanks` argument.
#' * it is notably faster than apply, by means of running `paste()` on
#' each column of data, making the output vectorized, and scaling rather
#' well for large `data.frame` objects.
#'
#' The output can also include name:value pairs, which can make the output
#' data more self-describing in some circumstances. That said, the most basic
#' usefulness of this function is to create row labels.
#'
#' @returns `character` vector of length `nrow(x)`.
#'
#' @family jam string functions
#'
#' @param x `data.frame` or comparable object such as `matrix` or `tibble`.
#' @param sep `character` string separator to use between columns.
#' @param na.rm `logical` whether to remove NA values, or include them
#'    as `"NA"` strings.
#' @param condenseBlanks `logical` whether to condense blank or empty values
#'    without including an extra delimiter between columns.
#' @param includeNames `logical` whether to include the colname delimited
#'    prior to the value, using sepName as the delimiter.
#' @param sepName `character` string relevant when `includeNames=TRUE`,
#'    this value becomes the delimiter between name:value.
#' @param blankGrep `character` string used as regular expression pattern in
#'    `grep()` to recognize blank entries;
#'    by default any field containing no text, or only whitespace,
#'    is considered a blank entry.
#' @param verbose `logical` whether to print verbose output.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' # create an example data.frame
#' a1 <- c("red","blue")[c(1,1,2)];
#' b1 <- c("yellow","orange")[c(1,2,2)];
#' d1 <- c("purple","green")[c(1,2,2)];
#' df2 <- data.frame(a=a1, b=b1, d=d1);
#' df2;
#'
#' # the basic output
#' pasteByRow(df2);
#'
#' # Now remove an entry to show the empty field is skipped
#' df2[3,3] <- "";
#' pasteByRow(df2);
#'
#' # the output tends to make good rownames
#' rownames(df2) <- pasteByRow(df2);
#'
#' # since the data.frame contains colors, we display using
#' # imageByColors()
#' withr::with_par(list("mar"=c(5,10,4,2)), {
#' imageByColors(df2, cellnote=df2);
#' })
#'
#' @export
pasteByRow <- function
(x,
 sep="_",
 na.rm=TRUE,
 condenseBlanks=TRUE,
 includeNames=FALSE,
 sepName=":",
 blankGrep="^[ ]*$",
 verbose=FALSE,
 ...)
{
   ## Purpose is to paste values in each column using paste(),
   ## intending to be faster than apply(x, 1, paste), while also
   ## allowing some logic about how to handle missing values.
   ##
   ## x can be a data.frame, tibble, matrix, or DataFrame.
   ##
   ## Converts factor columns to character to prevent inadvertent
   ## use of integers which are pointers to factor levels.
   ##
   ## condenseBlanks=TRUE will remove blank entries so there are no two
   ## delimiters back to back
   ##
   ## includeNames=TRUE will include the colnames along with values,
   ## typically useful for generating name:value pairs. When includeNames=TRUE
   ## the sepName is used to delimit name:value pairs. The name:value pair
   ## is then delimited using sep.
   ##
   ## To use this function to create rownames or vector names, it is often
   ## helpful to run makeNames() to ensure names are unique, e.g.
   ## rownames(x) <- makeNames(pasteByRow(x))
   ##
   ## TODO: revisit how to handle na.rm=FALSE, where keeping "NA" would
   ## be beneficial.
   ##
   sep <- head(sep, 1);
   if (length(ncol(x)) == 0 || ncol(x) == 0) {
      return(x);
   }

   ## Convert matrix to data.frame, so we can use consistent [[x]] syntax
   ## Note: consider avoiding this converstion in order to save memory,
   ## potentially helpful for objects with large number of rows
   if (igrepHas("matrix", class(x))) {
      x <- as.data.frame(x);
   }

   ## Convert factor columns to character
   for (iCol in seq_len(ncol(x))) {
      if (igrepHas("factor", class(x[,iCol]))) {
         x[,iCol] <- as.character(x[[iCol]]);
      }
   }

   ## Note: the use of [[1]] requires data.frame or tibble, and
   ## can no longer use matrix class directly
   getColVals <- function(x, i, includeNames, na.rm, sepName) {
      xVals <- x[[i]];
      isNa <- (is.na(xVals));
      if (any(isNa)) {
         if (na.rm) {
            xVals[isNa] <- "";
         } else {
            xVals[isNa] <- "NA";
         }
      }

      if (condenseBlanks) {
         isBlank <- grep(blankGrep, xVals);
      }
      if (includeNames) {
         xVals <- paste0(colnames(x)[i], sepName, xVals);
         if (condenseBlanks && length(isBlank) > 0) {
            xVals[isBlank] <- "";
         }
      } else {
         if (condenseBlanks && length(isBlank) > 0) {
            xVals[isBlank] <- "";
         }
      }
      xVals;
   }
   xVals <- getColVals(x, 1, includeNames, na.rm, sepName);

   if (ncol(x) > 1) {
      for (i1 in 2:ncol(x)) {
         xVals1 <- getColVals(x, i1, includeNames, na.rm, sepName);
         if (condenseBlanks) {
            isBlank1 <- (is.na(xVals1) | grepl(blankGrep, xVals1));
            isBlank <- (is.na(xVals) | grepl(blankGrep, xVals));
            sepV <- ifelse(isBlank | isBlank1, "", sep);
         } else {
            sepV <- sep;
         }
         xVals <- paste0(xVals, sepV, xVals1);
      }
   }

   if (!is.null(rownames(x))) {
      names(xVals) <- rownames(x);
   }
   return(xVals);
}


#' break a vector into groups
#'
#' breaks a vector into groups
#'
#' This function takes a vector of values, determines "chunks" of identical
#' values, from which it defines where breaks occur. It assumes the input
#' vector is ordered in the way it will be displayed, with some labels
#' being duplicated consecutively. This function defines the breakpoints
#' where the labels change, and returns the ideal position to put a single
#' label to represent a duplicated consecutive set of labels.
#'
#' It can return fractional coordinates, for example when a label represents
#' two consecutive items, the fractional coordinate can be used to place the
#' label between the two items.
#'
#' This function is useful for things like adding labels to
#' `imageDefault()` color image map of sample groupings, where
#' it may be ideal to label only unique elements in a contiguous set.
#'
#' @returns `list` with the following named elements:
#'    * `"breakPoints"`: The mid-point coordinate between each break.
#'    These midpoints would be good for drawing dividing lines for
#'    example.
#'    * `"labelPoints"`: The ideal point to place a label to represent
#'    the group.
#'    * `"newLabels"`: A vector of labels the same length as the input
#'    data, except using blank values except where a label should
#'    be drawn. This output is good for text display.
#'    * `"useLabels"`: The unique set of labels, without blanks,
#'    corresponding to the coordinates supplied by labelPoints.
#'    * `"breakLengths"`: The integer size of each set of labels.
#'
#' @family jam string functions
#'
#' @param x `character` vector of labels
#' @param labels `character` vector of custom labels to represent the items
#'    in x
#' @param returnFractions `logical` whether to return fractional coordinates
#'    for labels that should be positioned between two labels
#' @param ... additional parameters are ignored.
#'
#' @examples
#' b <- rep(LETTERS[c(1:5, 1)], c(2,3,5,4,3,4));
#' bb <- breaksByVector(b);
#' # Example showing how labels can be minimized inside a data.frame
#' data.frame(b,
#'    newLabels=bb$newLabels);
#'
#' # Example showing how to reposition text labels
#' # so duplicated labels are displayed in the middle
#' # of each group
#' bb2 <- breaksByVector(b, returnFractions=TRUE);
#' ylabs <- c("minimal labels", "all labels");
#' withr::with_par(adjustAxisLabelMargins(ylabs, 2), {
#'    withr::local_par(adjustAxisLabelMargins(bb2$useLabels, 1))
#'    nullPlot(xlim=range(seq_along(b)), ylim=c(0,3),
#'       doBoxes=FALSE, doUsrBox=TRUE);
#'    graphics::axis(2, las=2, at=c(1,2), ylabs);
#'    graphics::text(y=2, x=seq_along(b), b);
#'    graphics::text(y=1, x=bb2$labelPoints, bb2$useLabels);
#'
#' ## Print axis labels in the center of each group
#' graphics::axis(3,
#'    las=2,
#'    at=bb2$labelPoints,
#'    labels=bb2$useLabels);
#'
#' ## indicate each region
#' for (i in seq_along(bb2$breakPoints)) {
#'    graphics::axis(1,
#'       at=c(c(0, bb2$breakPoints)[i]+0.8, bb2$breakPoints[i]+0.2),
#'       labels=c("", ""));
#' }
#' ## place the label centered in each region without adding tick marks
#' graphics::axis(1,
#'    las=2,
#'    tick=FALSE,
#'    at=bb2$labelPoints,
#'    labels=bb2$useLabels);
#' ## abline to indicate the boundaries, if needed
#' graphics::abline(v=c(0, bb2$breakPoints) + 0.5,
#'    lty="dashed",
#'    col="blue");
#'
#' })
#' # The same process is used by imageByColors()
#'
#' @export
breaksByVector <- function
(x,
 labels=NULL,
 returnFractions=FALSE,
 ...)
{
   ## Purpose is to take a vector of values, and determine the "chunks" of
   ## identical values, so we can define where the breaks occur.
   ## labels is expected to have length equal to the number of Rle chunks.
   ##
   ## breakPoints = the coordinate start for each break
   ## labelPoints = the midpoint coordinate between each break
   ## newLabels = vector of labels, where everything is empty except at the
   ##    midpoint of each cluster, where labels are applied.
   ##
   ## returnFractions=TRUE will place a label between cells, in case there are an
   ## even number of duplicated entries, it will return the coordinate halfway
   ## between the middle.
   ##
   ## Use the base::rle() function instead of IRanges::Rle()
   if (any(c("factor", "ordered") %in% class(x))) {
      x <- nameVector(as.character(x),
         names(x),
         makeNamesFunc=c);
   }
   xRle <- rle(x);

   ## Commented out syntax used by Bioconductor Rle
   #breakPoints <- cumsum(runLength(xRle));
   xLengths <- xRle$lengths;
   breakPoints <- cumsum(xRle$lengths);
   useLabels <- xRle$values;
   names(breakPoints) <- useLabels;

   labelPoints <- 0.5 + (c(0, head(breakPoints, -1)) + breakPoints) / 2;
   if (!returnFractions) {
      labelPoints <- trunc(labelPoints);
   }
   names(labelPoints) <- names(breakPoints);
   if (!is.null(labels) && length(labels) == length(breakPoints)) {
      newLabels <- rep("", length(x));
      newLabels[labelPoints] <- labels;
   } else {
      newLabels <- rep("", length(x));
      #newLabels[labelPoints] <- runValue(xRle);
      newLabels[labelPoints] <- xRle$values;
   }
   if (!is.null(names(x))) {
      newLabels <- nameVector(newLabels, names(x));
   }

   # calculate chunk sizes

   list(breakPoints=breakPoints,
      labelPoints=labelPoints,
      newLabels=newLabels,
      useLabels=useLabels,
      breakLengths=xLengths);
}

#' Draw grouped axis labels
#'
#' Draw grouped axis labels given a character vector.
#'
#' This function extends `breaksByVector()` specifically for
#' axis labels. It is intended where character labels are spaced
#' at integer steps, and some labels are expected to be repeated.
#'
#' @family jam plot functions
#'
#' @returns `data.frame` invisibly, which contains the relevant axis
#'    coordinates, labels, and whether the coordinate should
#'    appear with a tick mark.
#'
#' @examples
#' withr::with_par(list("mar"=c(4,4,6,6)), {
#' b <- rep(LETTERS[1:5], c(2,3,5,4,3));
#' b2 <- c(b[1:2], makeNames(b[3:5]), b[6:16]);
#' nullPlot(doBoxes=FALSE,
#'    doUsrBox=TRUE,
#'    xlim=c(0,18),
#'    ylim=c(0,18));
#'
#' groupedAxis(1, b);
#' groupedAxis(2, b, group_style="grouped");
#' groupedAxis(2, b, group_style="centered");
#' groupedAxis(3, b2, do_abline=TRUE);
#' groupedAxis(4, b2, group_style="grouped");
#' graphics::mtext(side=1, "group_style='partial_grouped'", line=2, las=0);
#' graphics::mtext(side=2, "group_style='grouped'", line=2, las=0);
#' graphics::mtext(side=3, "group_style='partial_grouped'", line=2, las=0);
#' graphics::mtext(side=4, "group_style='grouped'", line=2, las=0);
#' })
#'
#' @param side `integer` indicating the axis side, passed to `graphics::axis()`.
#'    1=bottom, 2=left, 3=top, 4=right.
#' @param x `character` vector of axis labels
#' @param group_style `character` string indicating the style of label:
#'    * `"partial_grouped"` - uses square bracket to bound 2+ repeated entries,
#'    and single line tick mark for non-repeated entries.
#'    * `"grouped"` - uses square bracket to bound each set of repeated entries
#'    including non-repeated entries.
#'    * `"centered"` - only labels the center of each group of repeated entries
#'    with no bracket bounding the entries.
#' @param las `integer` indicating whether labels should be perpendicular,
#'    see `graphics::par("las")`.
#' @param returnFractions `logical` passed to `breaksByVector()` to calculate
#'    label positions. Set `returnFractions=FALSE` and all labels will only
#'    appear at integer locations on the axis.
#' @param nudge `numeric` adjustment for labels away from the plot border.
#' @param do_abline `logical` indicating whether to draw `graphics::abline()` lines
#'    inside the plot to indicate the exact breakpoints between each group
#'    of labels.
#' @param abline_lty line type compatible with `graphics::par("lty")`, used when
#'    `do_abline=TRUE`.
#' @param abline_col `character` color used when `do_abline=TRUE`.
#' @param do_plot `logical` whether to plot the resulting axis,
#'    as an option to suppress the output and do something else
#'    with the `data.frame` of coordinates returned by this function.
#' @param ... additional arguments are passed to `breaksByVector()`, and/or to
#'    `graphics::axis()`.
#'
#' @export
groupedAxis <- function
(side=1,
 x,
 group_style=c("partial_grouped",
    "grouped",
    "centered"),
 las=2,
 returnFractions=TRUE,
 nudge=0.2,
 do_abline=FALSE,
 abline_lty="solid",
 abline_col="grey40",
 do_plot=TRUE,
 ...)
{
   ## Purpose is to provide a convenient wrapper for breaksByVector()
   ## used to draw grouped axis labels
   sides <- intersect(c(1,2,3,4), side);
   group_style <- match.arg(group_style);

   ## Call breaksByVector()
   bb2 <- breaksByVector(x=x,
      returnFractions=returnFractions,
      ...);

   ## Print axis labels in the center of each group
   for (side in sides) {
      if ("centered" %in% group_style) {
         axis_df <- tryCatch({
            data.frame(check.names=FALSE,
               stringsAsFactors=FALSE,
               axis_at=bb2$labelPoints,
               axis_ticks=TRUE,
               axis_labels=bb2$useLabels,
               axis_side=side,
               axis_group=1)
         }, error=function(e){
            data.frame(check.names=FALSE,
               stringsAsFactors=FALSE,
               axis_at=1,
               axis_ticks=TRUE,
               axis_labels="",
               axis_side=1,
               axis_group=1)[0, , drop=FALSE]
         })
         if (TRUE %in% do_plot) {
            graphics::axis(side=side,
               las=las,
               at=bb2$labelPoints,
               labels=bb2$useLabels,
               ...);
         }
      } else if (any(c("partial_grouped", "grouped") %in% group_style)) {
         ## indicate each region
         axis_dfs <- lapply(seq_along(bb2$breakPoints), function(i){
            x1 <- c(0, bb2$breakPoints)[i] + 1;
            x2 <- bb2$breakPoints[i];
            if (x1 == x2 && "partial_grouped" %in% group_style) {
               axis_df <- data.frame(check.names=FALSE,
                  stringsAsFactors=FALSE,
                  axis_at=x1,
                  axis_ticks=TRUE,
                  axis_labels="",
                  axis_side=side,
                  axis_group=i)
               if (TRUE %in% do_plot) {
                  graphics::axis(side=side,
                     at=x1,
                     labels=c(""),
                     ...);
               }
            } else {
               axis_df <- data.frame(check.names=FALSE,
                  stringsAsFactors=FALSE,
                  axis_at=c(
                     x1 - nudge,
                     x2 + nudge),
                  axis_ticks=TRUE,
                  axis_labels=c("",
                     ""),
                  axis_side=side,
                  axis_group=i)
               if (TRUE %in% do_plot) {
                  graphics::axis(side=side,
                     at=c(
                        x1 - nudge,
                        x2 + nudge),
                     labels=c("", ""),
                     ...);
               }
            }
            axis_df
         })
         axis_df1 <- tryCatch({
            rbindList(axis_dfs);
         }, error=function(e){
            data.frame(check.names=FALSE,
               stringsAsFactors=FALSE,
               axis_at=1,
               axis_ticks=TRUE,
               axis_labels="",
               axis_side=side,
               axis_group=1)[0, , drop=FALSE]
         })

         ## place the label centered in each region without adding tick marks
         axis_df2 <- tryCatch({
            data.frame(check.names=FALSE,
               stringsAsFactors=FALSE,
               axis_at=bb2$labelPoints,
               axis_ticks=FALSE,
               axis_labels=bb2$useLabels,
               axis_side=side,
               axis_group=seq_along(bb2$useLabels))
         }, error=function(e){
            data.frame(check.names=FALSE,
               stringsAsFactors=FALSE,
               axis_at=1,
               axis_labels="",
               axis_side=side,
               axis_group=1)[0, , drop=FALSE]
         })
         axis_df <- mixedSortDF(
            rbind(axis_df1, axis_df2),
            byCols=c("axis_group", "axis_at"))

         if (TRUE %in% do_plot) {
            graphics::axis(side=side,
               las=las,
               tick=FALSE,
               at=bb2$labelPoints,
               labels=bb2$useLabels,
               ...);
         }
      }
   }

   ## abline to indicate the boundaries, if needed
   if (TRUE %in% do_abline && TRUE %in% do_plot) {
      if (any(c(1,3) %in% sides)) {
         graphics::abline(v=c(0, bb2$breakPoints) + 0.5,
            lty=abline_lty,
            col=abline_col,
            ...);
      }
      if (any(c(2,4) %in% sides)) {
         graphics::abline(h=c(0, bb2$breakPoints) + 0.5,
            lty=abline_lty,
            col=abline_col,
            ...);
      }
   }
   if (nrow(axis_df) > 0) {
      rownames(axis_df) <- NULL
   }
   return(invisible(axis_df));
}


#' convert column number to 'Excel' column name
#'
#' convert column number to 'Excel' column name
#'
#' The purpose is to convert an `integer` column number into a valid 'Excel'
#' column name, using `LETTERS` starting at A.
#' This function implements an arbitrary number of digits, which may or
#' may not be compatible with each version of 'Excel'.  18,278 columns
#' would be the maximum for three digits, "A" through "ZZZ".
#'
#' This function is useful when referencing 'Excel' columns via another
#' interface such as via openxlsx. It is also used by `makeNames()`
#' when the `numberStyle="letters"`, in order to provide letter suffix values.
#'
#' One can somewhat manipulate the allowed column names via the `useLetters`
#' argument, which by default uses the entire 26-letter Western alphabet.
#'
#' @returns `character` vector with length(x)
#'
#' @family jam practical functions
#'
#' @param x `integer` vector
#' @param useLetters `character` vector of single-digit characters to use as
#'    digits in the resulting column name. Note that these characters can
#'    be of almost any length, with any content.
#' @param zeroVal `character` single-digit to be used whenever `x==0`, or as a
#'    prefix for negative values. In theory there should be no negative
#'    input values, but this basic mechanism is used to handle the possibility.
#' @param ... Additional arguments are ignored.
#'
#' @examples
#' colNum2excelName(1:30)
#'
#' @export
colNum2excelName <- function
(x,
 useLetters=LETTERS,
 zeroVal="a",
 ...)
{
   ## Purpose is to convert a numerical column number into Excel name.
   ## This function implements an arbitrary number of digits, which may or
   ## may not be compatible with each version of Excel.  18,278 columns
   ## would be the maximum for three digits, "A" through "ZZZ"
   ##
   ## Custom function which returns the remainder except instead
   ## of zero, it uses the maximum. This function is vectorized
   xSign <- sign(x);
   x <- abs(x);
   subRemainder <- function(x, base=26, ...)
   {
      sub1 <- x %% base;
      ifelse(sub1 == 0, base, sub1);
   }

   useBase <- length(useLetters);

   ## We must add a blank entry at the end, so paste() is not
   ## allowed to omit NULL entries in vectorized mode.
   useLetters <- c(useLetters[1:useBase], "");

   ## Start with the last digit and work inward
   sub1 <- subRemainder(x, base=useBase);
   sub1vals <- useLetters[sub1];
   main1 <- as.integer((x-1) / useBase);
   while (any(main1 > useBase)) {
      lg1 <- which(main1 > useBase);
      sub2vals <- useLetters[subRemainder(main1[lg1], base=useBase)];
      sub1vals[lg1] <- paste0(sub2vals, sub1vals[lg1]);
      main1[lg1] <- as.integer((main1[lg1]-1) / useBase);
   }
   ## We must change zero to the last entry which is ""
   main1[main1 == 0] <- (useBase + 1);
   main1vals <- useLetters[main1];
   excelColName <- paste0(main1vals, sub1vals);
   names(excelColName) <- names(x);

   ## Values of zero are set to zeroVal, by default "a"
   if (any(x %in% 0)) {
      excelColName[x %in% 0] <- zeroVal;
   }
   ## Negative values are prefixed with zeroVal, by default "a"
   if (any(xSign %in% "-1")) {
      xNegative <- which(xSign %in% "-1");
      excelColName[xNegative] <- paste0(zeroVal, excelColName[xNegative]);
   }
   return(excelColName);
}

#' Decide plot panel rows, columns for graphics::par(mfrow)
#'
#' Decide plot panel rows, columns for graphics::par(mfrow)
#'
#' This function returns the recommended rows and columns of panels
#' to be used in `graphics::par("mfrow")` with R base plotting. It attempts
#' to use the device size and plot aspect ratio to keep panels roughly
#' square. For example, a short-wide device would have more columns of panels
#' than rows; a tall-thin device would have more rows than columns.
#'
#' The `doTest=TRUE` argument will create `n` number of
#' panels with the recommended layout, as a visual example.
#'
#' Note this function calls `getPlotAspect()`,
#' therefore if no plot device is currently open,
#' the call to `graphics::par()` will open a new graphics device.
#'
#' @returns `numeric` vector length=2, with the recommended number of plot
#'    rows and columns, respectively. It is intended to be used directly
#'    in this form: `graphics::par("mfrow"=decideMfrow(n=5))`
#'
#' @family jam plot functions
#'
#' @param n `integer` number of plot panels
#' @param method `character` string indicating the type of layout to favor.
#'    \describe{
#'       \item{"aspect"}{uses the device size and aspect ratio of the plot to try
#'          to maintain roughly square plot panels.}
#'       \item{"wide"}{tries to keep the columns and rows similar, erring on
#'          the side of more columns than rows.}
#'       \item{"tall"}{tries to keep the columns and rows similar, erring on
#'          the side of more rows than columns.}
#'    }
#' @param doTest `logical` whether to provide a visual test. Note that
#'    `n` is required as the number of plot panels requested.
#' @param xyratio `numeric` default 1, with the desired target x-to-y ratio.
#'    For example, to have plots slightly wider (x width) than tall
#'    (y height), use `xyratio=1.3`. The observed device aspect ratio
#'    is divided by `xyratio` to determine the target aspect ratio
#'    of plot panels.
#' @param trimExtra `logical` default TRUE, whether to trim blank rows or
#'    columns in the expected layout when it would be entirely blank.
#'    For example, `n=4` may produce `c(3, 2)` output to meet the
#'    desired aspect ratio, however with `trimExtra=TRUE` it would
#'    be reduced to `c(2, 2)` to minimize unused whitespace.
#' @param ... additional parameters are ignored.
#'
#' @examples
#' # display a test visualization showing 6 panels
#' withr::with_par(list("mar"=c(2, 2, 2, 2)), {
#' decideMfrow(n=6, doTest=TRUE);
#' })
#'
#' # use a custom target xyratio of plot panels
#' withr::with_par(list("mar"=c(2, 2, 2, 2)), {
#' decideMfrow(n=3, xyratio=3, doTest=TRUE);
#' })
#'
#' # a manual demonstration creating 6 panels
#' n <- 6;
#' withr::with_par(list(
#'    "mar"=c(2, 2, 2, 2),
#'    "mfrow"=decideMfrow(n)), {
#' for(i in seq_len(n)){
#'    nullPlot(plotAreaTitle=paste("Plot", i));
#' }
#' })
#'
#' @export
decideMfrow <- function
(n,
 method=c("aspect", "wide", "tall"),
 doTest=FALSE,
 xyratio=1,
 trimExtra=TRUE,
 ...)
{
   ## Purpose is to decide how to arrange plots so that panels are roughly
   ## square.
   dinAspect <- getPlotAspect(type="device") / xyratio;
   n1 <- (sqrt(n/dinAspect));
   n2 <- (sqrt(n*dinAspect));
   n1diff <- abs(round(n1) - n1);
   n2diff <- abs(round(n2) - n2);
   if (n1diff < n2diff) {
      n1 <- round(n1);
      n2 <- ceiling(n/n1);
   } else {
      n2 <- round(n2);
      n1 <- ceiling(n/n2);
   }

   ## Trim blank panels by row,column
   if (trimExtra) {
      nblank <- (prod(n1, n2) - n);
      if (nblank > 0) {
         if (nblank >= n2) {
            n1 <- ceiling(n / n2)
         } else if (nblank >= n1) {
            n2 <- ceiling(n / n1)
         }
      }
   }

   ## Optionally provide a visual test
   if (doTest) {
      withr::local_par("mfrow"=c(n1, n2));
      for(i in seq_len(n)){
         nullPlot(plotAreaTitle=paste("Plot", i));
      }
   }
   c(n1, n2);
}

#' Get aspect ratio for coordinates, plot, or device
#'
#' Get aspect ratio for coordinates, plot, or device
#'
#' @returns `numeric` plot aspect ratio for a plot device, of the requested
#' type, see the `type` argument.
#'
#' @family jam plot functions
#'
#' @param type `character` type of aspect ratio to calculate.
#'    \describe{
#'       \item{"coords"}{calculates plot coordinate aspect ratio, which
#'          is helpful for creating proper circular shapes, for example,
#'          where the x-axis and y-axis ranges are very different. Note
#'          that this calculation does also correct for margin sizes.}
#'       \item{"plot"}{calculates plot aspect ratio, based upon the
#'          actual size of the plot, independent of the numeric coordinate
#'          range of the plot. This aspect ratio reflects the relative
#'          visual height and width of the plot area, ignoring margins.}
#'       \item{"device"}{calculates plot aspect ratio, based upon the
#'          complete graphical device, i.e. the full space including all
#'          panels, margins, and plot areas.}
#'    }
#' @param parUsr,parPin,parDin `numeric` values equivalent to their
#'    respective `graphics::par()` output, from `graphics::par("usr")`,
#'    `graphics::par("pin")`, and `graphics::par("din")`. Values can be
#'    supplied directly, which among other things, prevents opening a
#'    graphical device if one is not already opened. Any call to
#'    `graphics::par()` will otherwise cause a graphic device to be opened,
#'    which may not be desired on a headless R server.
#' @param ... additional parameters are ignored.
#'
#' @examples
#' withr::with_par(list("mfrow"=c(2, 4), "mar"=c(1, 1, 1, 1)), {
#' for (i in 1:8) {
#'    nullPlot(plotAreaTitle=paste("Plot", i), xlim=c(1,100), ylim=c(1,10),
#'       doMargins=FALSE);
#'    graphics::axis(1, las=2);
#'    graphics::axis(2, las=2);
#' }
#' # device aspect inside the 2x4 layout
#' getPlotAspect("plot");
#' })
#' # device aspect outside the 2x4 layout
#' getPlotAspect("plot");
#'
#' @export
getPlotAspect <- function
(type=c("coords", "plot", "device"),
 parUsr=graphics::par("usr"),
 parPin=graphics::par("pin"),
 parDin=graphics::par("din"),
 ...)
{
   ## Purpose is to get the plot aspect ratio, given an open
   ## plot window, taking into account both the plot region
   ## and the size of the displayed plot device.
   ## The end result is a ratio of the x-axis to y-axis
   ## actual visual width.
   ##
   ## type=="coords" uses the x- and y-axis coordinates, along with
   ## the axis ranges, and plot window size, to determine the coordinate
   ## aspect ratio. This ratio is useful when creating a perfect square.
   ##
   ## type=="plot" uses the plot dimensions in inches, to calculate the
   ## aspect ratio of the plot pane itself.
   ##
   ## type=="device" uses only the device dimensions to determine the
   ## aspect ratio of the device itself, independent of the size of any
   ## plot panels inside the device.
   type <- match.arg(type);

   parDinAspect <- parDin[1] / parDin[2];
   if (type %in% "device") {
      return(parDinAspect);
   }

   parPinAspect <- parPin[1] / parPin[2];
   if (type %in% "plot") {
      return(parPinAspect);
   }

   plotWidth <- diff(parUsr[1:2]);
   plotHeight <- diff(parUsr[3:4]);
   ## plot device aspect ratio as width:height
   plotUsrAspect <- plotHeight / plotWidth;
   plotAspect <- plotUsrAspect * parPinAspect;
   plotAspect;
}

#' frequency of entries, ordered by frequency
#'
#' frequency of entries, ordered by frequency
#'
#' This function mimics output from `table()` with two key
#' differences. It sorts the results by decreasing frequency, and optionally
#' filters results for a minimum frequency. It is effective when checking
#' for duplicate values, and ordering them by the number of occurrences.
#'
#' This function is useful when working with large vectors of gene
#' identifiers, where it is not always obvious whether genes are replicated
#' in a particular technological assay. Transcript microarrays for example,
#' can contain many replicated genes, but often only a handful of genes are
#' highly replicated, while the rest are present only once or twice on the
#' array.
#'
#' @returns `integer` vector of counts, named by the unique input
#'    values in `x`.
#'
#' @family jam string functions
#'
#' @param x `character`, `numeric`, `factor` vector input to use when
#'    calculating frequencies.
#' @param doSort `logical` whether to sort results decreasing by frequency.
#' @param minCount optional `integer` minimum frequency, any results with
#'    fewer counts observed will be omitted from results.
#' @param maxCount optional `integer` maximum frequency for returned results.
#' @param nameSortFunc `function` used to sort results after sorting by
#'    frequency. For example, one might use `mixedSort()`. If
#'    `nameSortFunc=NULL` then no name sort will be applied.
#' @param ... additional parameters are ignored.
#'
#' @examples
#' testVector <- rep(c("one", "two", "three", "four"), c(1:4));
#' tcount(testVector);
#' tcount(testVector, minCount=2);
#'
#' @export
tcount <- function
(x,
 minCount=NULL,
 doSort=TRUE,
 maxCount=NULL,
 nameSortFunc=sort,
 ...)
{
   ## Purpose is similar to table(), except this is just a quick way to return counts of each element,
   ## sorted by the counts in decreasing order. tcount(x)[1] is a quick way to see if any element is
   ## present more than once.
   ##
   ## minCount will filter results to those having at least that high a count

   ## Note we detect factor class in reverse, since ordered factors have two class values
   ## and would otherwise fail to be detected if we use class(x) %in% "factor"
   if (c("factor") %in% class(x)) {
      x <- as.character(x);
   }
   #x1 <- tapply(x, x, length);
   x1 <- table(x);
   x1 <- nameVector(as.vector(x1), names(x1), makeNamesFunc=c);

   ## Filter before sort, for potential speed gain
   if (!is.null(minCount)) {
      x1 <- x1[x1 >= minCount];
   }
   if (!is.null(maxCount)) {
      x1 <- x1[x1 <= maxCount];
   }

   if (doSort) {
      if (!is.null(nameSortFunc)) {
         x1 <- x1[match(nameSortFunc(names(x1)), names(x1))];
      }
      x1 <- sort(x1, decreasing=TRUE);
   }
   return(x1);
}


#' vectorized make_styles for crayon output
#'
#' vectorized make_styles for crayon output
#'
#' This function is essentially a vectorized version of
#' `crayon::make_style()` in order to style a vector of
#' character strings with a vector of foreground and background styles.
#'
#' @returns `character` vector with the same length as `text` input vector,
#' where entries are surrounded by the relevant encoding consistent with
#' the `style` defined at input. In short, a character vector as input,
#' a colorized character vector as output.
#'
#' @family jam internal functions
#'
#' @param style `character` vector of one or more styles. When `NULL` or `NA`,
#'    no style is applied, except when `bg_style` is supplied
#'    and is neither `NA` nor `NULL`, in which case entries with
#'    a `bg_style` and no `style` will use `setTextContrastColor()`
#'    to define a contrasting `style`.
#' @param text `character` vector (or coerced to `character`) of one or
#'    more values,.
#' @param bg `logical` indicating whether the `style` should be
#'    applied to the background instead of foreground. This argument
#'    is ignored when `bg_style` is supplied.
#' @param bg_style `NULL` or a `character` vector of one or more background
#'    styles. When this argument is not NULL, it applies both the foreground
#'    `style` and background `bg_style` together, and therefore ignores
#'    `Crange` and `Lrange` settings.
#' @param colors `integer`, default NULL, number of colors for console output,
#'    when NULL it calls `crayon::num_colors()` to detect console
#'    capabilities.
#' @param grey `logical`, default FALSE, whether to use greyscale.
#' @param Cgrey `numeric` chroma (C) value, which defines grey colors at or
#'    below this chroma. Any colors at or below the grey cutoff will have
#'    use ANSI greyscale coloring. To disable, set `Cgrey=-1`.
#' @param lightMode `logical` indicating whether the background color
#'    is light (TRUE is bright), or dark (FALSE is dark.) By default
#'    it calls `checkLightMode()` which queries `getOption("lightMode")`.
#' @param Crange `numeric` range of chroma values, ranging
#'    between 0 and 100. When NULL, default values will be
#'    assigned to Crange. When supplied, range(Crange) is used.
#' @param Lrange `numeric` range of luminance values, ranging
#'    between 0 and 100. When NULL, default values will be
#'    assigned to Lrange. When supplied, range(Lrange) is used.
#' @param adjustRgb `numeric` value adjustment used during the conversion of
#'    RGB colors to ANSI colors, which is inherently lossy. If not defined,
#'    it uses the default returned by `setCLranges()` which itself uses
#'    `getOption("jam.adjustRgb")` with default=0. In order to boost
#'    color contrast, an alternate value of -0.1 is suggested.
#' @param adjustPower `numeric` adjustment power factor
#' @param fixYellow `logical` indicating whether to "fix" the darkening of
#'    yellow, which otherwise turns to green. Instead, since JAM can,
#'    JAM will make the yellow slightly more golden before darkening. This
#'    change only affects color hues between 80 and 90. This argument is
#'    passed to `applyCLrange()`.
#' @param colorTransparent `character` color used to substitute for
#'    "transparent" which a valid R color, but not a valid color for
#'    the crayon package.
#' @param alphaPower `numeric` value, used to adjust the RGB values for alpha
#'    values less than 255, by raising the ratio to 1/alphaPower, which takes
#'    the ratio of square roots.  alphaPower=100 for minimal adjustment.
#' @param setOptions `character` or `logical` whether to update
#'    `Crange` and `Lrange` options during the subsequent call to
#'    `setCLranges()`. By default,
#'    * `"ifnull"` will update only options which were previously `NULL`;
#'    * `"FALSE"` prevents modifying the global options;
#'    * `"TRUE"` will update these options with the current values.
#' @param verbose `logical` indicating whether to print verbose output
#' @param ... additional parameters are ignored
#'
#' @examples
#' cat(make_styles(style=c("red", "yellow"), text=c("one ", "two")), "\n")
#'
#' @export
make_styles <- function
(style=NULL,
 text,
 bg=FALSE,
 bg_style=NULL,
 grey=FALSE,
 colors=NULL,
 Cgrey=getOption("jam.Cgrey", 5),
 lightMode=NULL,
 Crange=getOption("jam.Crange"),
 Lrange=getOption("jam.Lrange"),
 adjustRgb=getOption("jam.adjustRgb"),
 adjustPower=1.5,
 fixYellow=TRUE,
 colorTransparent="grey45",
 alphaPower=2,
 setOptions=c("ifnull","FALSE","TRUE"),
 verbose=FALSE,
 ...)
{
   ## Purpose is to wrapper make_style for a vector of styles
   ## and text.  By default make_style() only accepts one style
   ## and returns a function, not the text value
   ##
   ## style is repeated to match the length of the vector text,
   ## however text is not repeated to match the length of style.
   ##
   ## This function also accepts NA as input, returning the input
   ## text with no modification, intended for vectorized operations.
   ##
   ## checkSat=TRUE will check whether the color is extremely low saturation,
   ## in which case, grey=TRUE is automatically set.
   ##
   ## adjustRgb=TRUE will pre-adjust the RGB values, which in crayon do
   ## a conversion to the ANSI 6-bit color scale, which tends to round
   ## values up, losing subtle differences because colors tend to be too bright.
   ##
   ## Lastly, if text has names, they are preserved in the output.
   ##
   ## subTransparent is used to replace the valid R color "transparent"
   ## with something compatible with crayon::make_style()
   ## subTransparent=NA will perform no styling on transparent entries,
   ## thus using the ANSI default text color
   ##
   ## alphaPower=2 is used to adjust the RGB values for alpha values
   ## less than 255, by raising the ratio to 1/alphaPower, which takes the
   ## ratio of square roots.  alphaPower=100 for minimal adjustment.
   ##
   if (length(text) == 0) {
      return(text);
   }
   if (!requireNamespace("crayon", quietly=TRUE)) {
      ## If crayon is not available, return text without style. So sad.
      return(text);
   }
   if (length(setOptions) == 0) {
      setOptions <- "ifnull";
   } else {
      setOptions <- as.character(setOptions);
   }

   if (length(colors) == 0) {
      colors <- crayon::num_colors()
   }
   if (length(Cgrey) == 0) {
      Cgrey <- -1;
   }
   if (length(colorTransparent) == 0) {
      colorTransparent <- NA;
   }

   ## Determine Crange, Lrange, adjustRgb
   CLranges <- setCLranges(lightMode=lightMode,
      Crange=Crange,
      Lrange=Lrange,
      Cgrey=Cgrey,
      setOptions=setOptions,
      ...);
   if (length(adjustRgb) == 0) {
      adjustRgb <- CLranges$adjustRgb;
   }
   Crange <- CLranges$Crange;
   Lrange <- CLranges$Lrange;
   if (verbose) {
      print(paste0("make_styles(): ",
         "Crange:",
         paste(Crange, collapse=","),
         ", Lrange:",
         paste(Lrange, collapse=","),
         ", adjustRgb:",
         adjustRgb,
         ", fixYellow:",
         fixYellow));
   }

   if (length(fixYellow) == 0) {
      fixYellow <- FALSE;
   }
   fixYellow <- rep(fixYellow, length.out=length(text));

   ## Process style
   if (length(style) > 0 && igrepHas("matrix", class(style))) {
      if (verbose) {
         print(paste0("make_styles(): ",
            "Handling argument style as matrix."))
      }
      if (!all(c("red", "green", "blue") %in% rownames(style))) {
         stop("When style is a matrix it must contain rownames 'red', 'green', and 'blue'.");
      }
      styleNA <- (is.na(style["red",]) |
            is.na(style["green",]) |
            is.na(style["blue",]));
      ## Convert to color vector to apply CL range, then back to rgb
      styleV <- rgb2col(style);
      styleV <- rep(styleV, length.out=length(text));
      styleNA <- rep(styleNA, length.out=length(text));
   } else {
      if (verbose) {
         print(paste0("make_styles(): ",
            "Handling argument style as vector."))
      }
      if (length(style) == 0 || length(unlist(style)) == 0) {
         style <- NA;
      }
      style <- rep(style, length.out=length(text));
      styleNA <- is.na(style);
      styleV <- style;
   }

   ## Process bg_style
   if (length(bg_style) > 0 && igrepHas("matrix", class(bg_style))) {
      if (verbose) {
         print(paste0("make_styles(): ",
            "Handling argument bg_style as matrix."))
      }
      if (!all(c("red", "green", "blue") %in% rownames(bg_style))) {
         stop("When bg_style is a matrix it must contain rownames 'red', 'green', and 'blue'.");
      }
      styleNA <- (is.na(bg_style["red",]) |
            is.na(bg_style["green",]) |
            is.na(bg_style["blue",]));
      ## Convert to color vector to apply CL range, then back to rgb
      bg_styleV <- rgb2col(bg_style);
      bg_styleV <- rep(bg_styleV, length.out=length(text));
      bg_styleNA <- rep(bg_styleNA, length.out=length(text));
   } else {
      if (verbose) {
         print(paste0("make_styles(): ",
            "Handling argument bg_style as vector."))
      }
      if (length(bg_style) == 0 || length(unlist(bg_style)) == 0) {
         bg_style <- NA;
      }
      bg_style <- rep(bg_style, length.out=length(text));
      bg_styleNA <- is.na(bg_style);
      bg_styleV <- bg_style;
   }
   if (length(bg) == 0) {
      bg <- FALSE;
   }
   bg <- rmNA(naValue=FALSE,
      rep(bg, length.out=length(text)));

   ## Optionally apply fixYellow() to bg
   if (any(fixYellow & !bg_styleNA)) {
      ## fixYellow
      bg_style[!bg_styleNA & fixYellow] <- fixYellow(bg_style[!bg_styleNA & fixYellow],
         ...);
   }

   if (verbose) {
      print(paste0("styleV (before):", cPaste(styleV)));
      print(styleV);
      print(paste0("bg_styleV (before):", cPaste(bg_styleV)));
   }
   ## Apply Crange, Lrange only when bg_style is NA
   if (any(bg_styleNA & !styleNA)) {
      if (verbose) {
         print(paste0("make_styles(): ",
            "applyCLrange()"));
      }
      styleV[bg_styleNA & !styleNA] <- applyCLrange(styleV[bg_styleNA & !styleNA],
         Lrange=Lrange,
         Crange=Crange,
         Cgrey=Cgrey,
         fixYellow=fixYellow,
         verbose=verbose,
         ...);
   }
   ## Check for any colors too close to white, they
   ## cause a bug where any foreground color is always
   ## white if one color is ANSI white \033[38;5;255
   ## The workaround is to make them 98% white
   style_white <- grepl("^#F[89ABCDEF]F[89ABCDEF]F[89ABCDEF]|^white$", styleV);
   if (any(style_white)) {
      if (verbose) {
         print(paste0("make_styles(): ",
            "fixing style_white"));
      }
      styleV[style_white] <- "#F7F7F7";
   }

   ## Optionally apply fixYellow() to styleV
   if (any(fixYellow & !styleNA)) {
      ## fixYellow
      styleV[!styleNA & fixYellow] <- fixYellow(styleV[!styleNA & fixYellow],
         ...);
   }

   if (verbose) {
      print(paste0("styleV (after):",
         paste(styleV, collapse=",")));
      print(paste0("bg_styleV (after):",
         paste(bg_styleV, collapse=",")));
   }
   ## Convert to rgb
   style <- grDevices::col2rgb(styleV, alpha=TRUE);
   if (any(styleNA)) {
      style[,styleNA] <- NA;
   }
   bg_style <- grDevices::col2rgb(bg_styleV, alpha=TRUE);
   if (any(bg_styleNA)) {
      bg_style[,bg_styleNA] <- NA;
   }
   if (verbose) {
      print(paste0("make_styles(): ",
         "style:"));
      print(style);
      print(paste0("make_styles(): ",
         "bg_style:"));
      print(bg_style);
   }

   ## Apply alpha and check for transparent colors
   if ("alpha" %in% rownames(style) &&
       any(rmNA(style["alpha",], naValue=255) < 255)) {
      if (verbose) {
         print(paste0("make_styles(): ",
            "applying alpha."));
      }
      alphaFactor <- (style["alpha",])^(1/alphaPower)/(255)^(1/alphaPower);
      style[c("red","green","blue"),] <- style[c("red","green","blue"),] *
         rep(alphaFactor, each=3);
      isTransparent <- (style["alpha",] == 0);
   } else {
      isTransparent <- rep(FALSE, length.out=length(text));
   }
   ## Remove transparency
   style <- style[c("red","green","blue"),,drop=FALSE];

   ## Adjust RGB
   if (adjustRgb != 0 && any(!styleNA)) {
      if (verbose) {
         print(paste0("make_styles(): ",
            "applying adjustRgb"));
      }
      if (!is.na(adjustPower) && !is.null(adjustPower)) {
         ## This method uses square root transform
         style1 <- round((style^adjustPower)/(255^adjustPower)*6 + adjustRgb);
      } else {
         ## This method shifts color brightness down slightly
         style1 <- round(style/255*6 + adjustRgb);
      }
      style[!is.na(style)] <- style1[!is.na(style)] * 255/6;
      style[!is.na(style) & style < 1] <- 1;
      style[!is.na(style) & style > 255] <- 255;
      if (verbose) {
         print(paste0("make_styles(): ",
            "style (post-adjustRgb):"));
         print(style);
      }
   }

   ## Check color saturation for greyscale colors
   if (any(!styleNA)) {
      if (verbose) {
         print(paste0("make_styles(): ",
            "determining grey colors"));
      }
      Cvals <- rep(100, length(styleV));
      Cvals[!styleNA] <- col2hcl(styleV[!styleNA])["C",];
      isCgrey <- (Cvals <= Cgrey);
   } else {
      isCgrey <- rep(FALSE, length.out=length(text));
   }

   ## Apply each style to each text entry
   iVals <- sapply(seq_along(text), function(i){
      iText <- text[i];
      iStyle <- style[,i,drop=FALSE];
      ibgStyle <- bg_style[,i,drop=FALSE];
      iGrey <- isCgrey[i];
      if (styleNA[i] && bg_styleNA[i]) {
         if (verbose) {
            print(paste0("make_styles(): ",
               "No style applied to text:",
               iText));
         }
         iText;
      } else if (styleNA[i] && !bg_styleNA[i]) {
         ## Combine bg with contrasting fg color
         bg_contrast <- setTextContrastColor(rgb2col(ibgStyle),
            useGrey=5);
         if (verbose) {
            print(paste0("make_styles(): ",
               "bg style and contrasting fg style applied to text:",
               iText));
            print(paste0("iText:", iText,
               ", colors:", colors,
               ", bg_contrast:", bg_contrast,
               ", rgb2col(ibgStyle):", rgb2col(ibgStyle)));
         }
         crayon::make_style(rgb2col(ibgStyle),
            bg=TRUE,
            colors=colors)(
               crayon::make_style(bg_contrast,
                  bg=FALSE,
                  colors=colors)(iText)
            );
      } else if (!styleNA[i] && !bg_styleNA[i]) {
         ## Combine bg with fg color
         if (verbose) {
            print(paste0("make_styles(): ",
               "bg style and fg style applied to text:",
               iText));
         }
         crayon::make_style(rgb2col(ibgStyle),
            bg=TRUE,
            colors=colors)(
               crayon::make_style(rgb2col(iStyle),
                  bg=FALSE,
                  colors=colors)(iText)
            );
      } else {
         if (verbose) {
            print(paste0("make_styles(): ",
               "fg style applied to text:",
               iText));
         }
         if (isTransparent[i]) {
            iStyle <- grDevices::col2rgb(colorTransparent, alpha=TRUE);
         }
         if (verbose) {
            print(paste0("make_styles(): ",
               "iStyle:"));
            print(iStyle);
            print(paste0("make_styles(): ",
               "bg:", bg));
            print(paste0("make_styles(): ",
               "colors:", colors));
            print(paste0("make_styles(): ",
               "iGrey:", iGrey));
         }
         crayon::make_style(rgb2col(iStyle),
            bg=bg[i],
            colors=colors,
            grey=iGrey)(iText);
      }
   });
   if (length(names(text)) > 0) {
      names(iVals) <- names(text);
   }
   attr(iVals, "color") <- rgb2col(style);
   attr(iVals, "bg_color") <- rgb2col(bg_style);
   iVals;
}



#' Show R function arguments jam-style
#'
#' Show R function arguments jam-style
#'
#' This function displays R function arguments, organized with one argument
#' per line, and colorized using the `crayon` package if
#' installed.
#'
#' Output is nicely spaced to help visual alignment of argument names
#' and argument values.
#'
#' Output can be filtered by `character` pattern. For example the
#' function `ComplexHeatmap::Heatmap()` is amazing, and offers numerous
#' arguments. To find arguments relevant to dendrograms, use `"dend"`:
#'
#' `jargs(ComplexHeatmap::Heatmap, "dend")`
#'
#' NOTE: This function has edge case issues displaying complex function
#' argument values such as nested lists and custom functions.
#' In that case the argument name is printed as usual, and the argument value
#' is displayed as a partial snippet of the default argument value.
#'
#' Generic functions very often contain no useful parameters,
#' making it difficult to discover required
#' parameters without reading the function documentation from the proper
#' dispatched function and calling package. In that case,
#' try using `jargs(functionname.default)` for example compare:
#'
#' `jargs(barplot)`
#'
#' to:
#'
#' `jargs(barplot.default)`
#'
#' @family jam practical functions
#'
#' @param x `function` or character name of a function.
#' @param grepString `NULL`, `logical`, or `character` grep regular expression
#'    pattern used to filter function arguments by name. Very useful to
#'    search a function for arguments with a substring `"row"`.
#'    * If `logical`, it is assumed to be sortVars, and indicates whether
#'    to sort the parameter names.
#'    * if `character` it will subset the function arguments by name matching
#'    this regular expression pattern.
#' @param sortVars `logical` whether to sort the function parameter names.
#'    * `sortVars=FALSE` returns arguments in the order they appear in the
#'    function definition.
#'    * `sortVars=TRUE` returns arguments sorted alphabetically.
#' @param useMessage `logical` default TRUE, whether to print output
#'    using `message()`, otherwise text is returned invisibly to be
#'    displayed separately.
#' @param asList `logical` default TRUE, display one entry per line or
#'    display results as a `data.frame`.
#' @param useColor `logical` whether to display results in color, if the crayon
#'    package is available, and terminal console is capable.
#' @param lightMode `logical` or `NULL`, indicating whether the text background
#'    color is light, thus imposing a maximum brightness for colors displayed.
#'    It use lightMode if defined by the function caller, otherwise it will
#'    use `getOption("jam.lightMode")` if defined, lastly it will attempt to
#'    detect whether running inside Rstudio by checking the environment variable
#'    "RSTUDIO", and if so it will assume lightMode==TRUE.
#' @param Crange `numeric` range of chroma values, ranging
#'    between 0 and 100. When NULL, default values will be
#'    assigned to Crange by `setCLranges()`.
#' @param Lrange `numeric` range of luminance values, ranging
#'    between 0 and 100. When NULL, default values will be
#'    assigned to Lrange by `setCLranges()`.
#' @param adjustRgb `numeric` value adjustment used during the conversion of
#'    RGB colors to ANSI colors, which is inherently lossy. If not defined,
#'    it uses the default returned by `setCLranges()` which itself uses
#'    \code{getOption("jam.adjustRgb")} with default=0. In order to boost
#'    color contrast, an alternate value of -0.1 is suggested.
#' @param useCollapseBase `character` string used to combine multiple parameter
#'    values.
#' @param verbose `logical` whether to print verbose output.
#' @param debug `integer` value, greater than 0 will cause debug-type verbose
#'    output, useful because parameters are hard!
#' @param ... Additional arguments are installed.
#'
#' @returns `NULL` this function called for the byproduct of printing
#'    its output.
#'
#' @examples
#' args(jargs)
#' jargs(jargs)
#'
#' # retrieve parameters involving notes from imageByColors
#' jargs(imageByColors, "note")
#'
#' @export
jargs <- function
(x,
 grepString=NULL,
 sortVars=FALSE,
 useMessage=TRUE,
 asList=TRUE,
 useColor=TRUE,
 lightMode=NULL,
 Crange=getOption("jam.Crange"),
 Lrange=getOption("jam.Lrange"),
 adjustRgb=getOption("jam.adjustRgb"),
 useCollapseBase=", ",
 verbose=FALSE,
 debug=0,
 ...)
{
   ## Purpose is to clean up the args() output to my personal preferences
   ##
   ## grepString is used to subset the arguments by name
   ##
   ## sortVars=TRUE by default, sorts the argument names, except that
   ## the '...' argument is placed last for legibility
   ##
   ## asList=TRUE uses display similar to default args()
   ## asList=FALSE uses data.frame colsHead() display
   ##
   ## useColor=TRUE because why not, right?

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
         ...);
      adjustRgb <- CLranges$adjustRgb;
      Crange <- CLranges$Crange;
      Lrange <- CLranges$Lrange;
   }

   if (useColor) {
      if (requireNamespace("crayon", quietly=TRUE)) {
         useCrayon <- TRUE;
      } else {
         if (verbose) {
            printDebug("jargs(): ",
               "Turned color off since the ",
               "crayon",
               " package is not available.");
         }
         useColor <- FALSE;
      }
   } else {
      useCrayon <- FALSE;
   }

   if (length(grepString) > 0 && grepString %in% c(FALSE)) {
      grepString <- setdiff(grepString, FALSE);
      sortVars <- FALSE;
   }

   ## Get function arguments
   argsText <- formals(x);
   if (length(grepString) > 0) {
      argsText <- argsText[vigrep(grepString, names(argsText))];
      if (length(argsText) == 0) {
         if (verbose) {
            printDebug("jargs(): ",
               "No arguments matched the grepString.",
               fgText="yellow");
         }
         invisible(NULL);
      }
   }

   if (length(argsText) > 0 && asList) {
      varLen <- nameVector(nchar(names(argsText)), names(argsText));
      indent <- paste(rep(" ", max(varLen)+1), collapse="");
      x1 <- as.vector(gsub("=$", "", sapply(names(argsText), function(i){
         if (verbose) {
            printDebug("jargs(): ",
               "i:",
               i);
         }
         col1 <- "mediumpurple2";
         col2 <- "mediumaquamarine";
         deText <- handleArgsText(argsText[[i]],
            col1=col1,
            col2=col2,
            indent=indent,
            lightMode=NULL,
            adjustRgb=adjustRgb,
            Crange=Crange,
            Lrange=Lrange,
            useCollapseBase=useCollapseBase,
            useColor=useCrayon,
            debug=debug,
            verbose=verbose);

         aText <- paste(i, paste(deText, collapse=" "), sep=" = ");
         aText;
      })));
      names(x1) <- names(argsText);
      if (sortVars && length(x1) > 1) {
         x2 <- mixedSort(x1);
      } else {
         x2 <- x1;
      }
      varLen <- varLen[names(x2)];
      varLen1 <- sapply(x2, function(i){
         nchar(strsplit(i, "=")[[1]])[1]
      });
      x3 <- sapply(1:length(x2), function(i){
         paste0(
            c(rep(" ", max(varLen) - varLen[i]),
               x2[i]),
            collapse="");
      });
      x3string <- paste0(paste(x3, collapse=",\n"), "\n");
      if (TRUE %in% useMessage) {
         message(x3string);
      }
      return(invisible(x3string));
   } else {
      argsList <- lapply(argsText, deparse);
      if (sortVars) {
         argsList <- argsList[mixedSort(names(argsList))];
      }
      argsDF <- list2df(argsList);
      argsDF;
   }
}

#' Handle function arguments as text
#'
#' Handles a list or list of lists, converting to human-readable text format
#'
#' This function is a rare non-exported function intended to be called by
#' `jargs()`, but separated in order to help isolate the logical
#' steps required.
#'
#' @family jam internal functions
#'
#' @inherit jargs
#' @param argTextA object passed by `jargs()` when iteratively parsing
#'    function argument values.
#' @param name `character` name of the argument.
#' @param col1,col2,colT,colF,colNULL `character` colors used as defaults
#'    for first and second arguments, TRUE, FALSE, NULL, respectively.
#' @param indent `character` string used as a prefix in output to help
#'    apply text indent.
#' @param useCollapseList `character` string inserted between multiple values
#'    to split list entries across multiple lines.
#' @param useCollapseBase `character` string used to separate multiple
#'    values in a vector which is not split across multiple lines.
#' @param level `integer` indicating the level of depth in iterative parsing.
#' @param ... Additional arguments are ignored.
#'
#' @returns `character` vector including ANSI coloring when available.
#'
#' @examples
#' cat(paste0(handleArgsText(formals(graphics::hist.default)), "\n"), sep="")
#'
#' @export
handleArgsText <- function
(argTextA,
 name="",
 col1="mediumpurple2",
 col2="mediumaquamarine",
 colT="dodgerblue3",
 colF="red1",
 colNULL="grey60",
 lightMode=NULL,
 Crange=getOption("jam.Crange"),
 Lrange=getOption("jam.Lrange"),
 adjustRgb=getOption("jam.adjustRgb"),
 indent="",
 useCollapseList=",\n      ",
 useCollapseBase=", ",
 level=1,
 debug=0,
 useColor=TRUE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to take input in the form of formals(functionname)
   ## and return a formatted string suitable for printing to the R
   ## console.
   ##
   #argTextA <- argsTextA[[i]];
   if (level == 20) {
      useCollapseBase <- useCollapseList;
   }
   if ("name" %in% class(argTextA)) {
      deTextA <- deparse(argTextA);
   } else {
      deTextA <- deparse(argTextA[[1]]);
   }

   # version 0.0.88.900: assume input already resolved Crange,Lrange,adjustRgb
   # ## Check lightMode, whether the background color is light or not
   # CLranges <- setCLranges(lightMode=lightMode,
   #    Crange=Crange,
   #    Lrange=Lrange,
   #    ...);
   # if (length(adjustRgb) == 0) {
   #    adjustRgb <- CLranges$adjustRgb;
   # }
   # Lrange <- CLranges$Lrange;
   # Crange <- CLranges$Crange;

   if (verbose) {
      printDebug(indent, "", "handleArgsText(): ",
         "class(argTextA):",
         class(argTextA),
         Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
   }
   if (debug > 0 && verbose) {
      printDebug(indent, "", "=== handleArgsText():\n   ",
         "name:\n      ",
         name, "\n   ", "",
         "as.character(argTextA):\n      ",
         as.character(argTextA), "\n   ", "",
         "deTextA:\n      ",
         deTextA,
         sep="\n      ",
         Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
   }
   if (any(c("pairlist","call") %in% class(argTextA))) {
      ##
      ## Class is "call"
      ##
      ## Multi-value entry
      whichMid <- tail(seq_along(argTextA), -1);
      whichEnds <- setdiff(seq_along(argTextA), whichMid);
      firstArg <- deparse(argTextA[[whichEnds]]);
      firstArgName <- names(argTextA)[whichEnds];

      ##
      if (firstArg %in% "function") {
         ##
         ##################################
         ## Handle functions
         if (verbose) {
            printDebug(indent, "", "handleArgsText(): ",
               "firstArg is 'function'",
               ", whichEnds=", whichEnds,
               ", whichMid=", whichMid,
               ", argTextA:",
               fgText=c("mediumaquamarine","yellow"),
               Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
            print(argTextA);
            for (i1 in seq_along(argTextA)) {
               printDebug(indent, "", "handleArgsText(): ",
                  "argTextA[[", i1, "]]",
                  Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
               print(argTextA[[i1]]);
            }
         }
         ## Parse functions differently than other entries
         whichMid <- head(tail(seq_along(argTextA), -1), -1);
         whichEnds <- setdiff(seq_along(argTextA), whichMid);
         fnArgs <- argTextA[[head(whichMid,1)]];
         fnArgsNames <- names(fnArgs);
         fnArgsText <- handleArgsText(fnArgs,
            name="",
            col1=col1,
            col2=col2,
            indent=paste0(indent, "   "),
            useCollapseBase=useCollapseBase,
            Crange=Crange,
            Lrange=Lrange,
            adjustRgb=adjustRgb,
            level=level+1,
            useColor=useColor,
            debug=1,
            verbose=verbose);

         fnBody <- deparse(argTextA[[3]]);
         if (length(fnBody) > 1) {
            fnBody[2:length(fnBody)] <- paste0(indent,
               "   ",
               fnBody[2:length(fnBody)]);
         }
         fnBodyText <- paste0(fnBody, collapse="\n");

         if (useColor) {
            deTextA <- paste0(
               make_styles(
                  text=c("function(",
                     fnArgsText,
                     ")",
                     fnBodyText),
                  style=c(col1, col2),
                  adjustRgb=adjustRgb,
                  Lrange=Lrange,
                  Crange=Crange,
                  setOptions="FALSE"),
               collapse=" ");
         } else {
            deTextA <- paste(
               "function(",
               fnArgsText,
               ")",
               fnBodyText,
               collapse=" ");
         }
      } else {
         ##
         ##################################
         ## Handle non-functions
         if (verbose) {
            printDebug(indent, "", "handleArgsText(): ",
               "pairlist firstArg:",
               Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
            print(firstArg);
            printDebug(indent, "", "handleArgsText(): ",
               "name: '",
               name,
               "'",
               Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
            printDebug(indent, "", "handleArgsText(): ",
               "whichMid:",
               whichMid,
               ", length(whichMid):",
               length(whichMid),
               Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
         }
         if ("pairlist" %in% class(argTextA) &&
               length(whichMid) > 0 &&
               all(isColor(as.character(argTextA[whichMid])))) {
            ############################################
            ## Handle pairlist, all values are colors
            if (verbose) {
               printDebug(indent, "", "handleArgsText(): ",
                  "'c1' && all(isColor)",
                  ", calling handleArgsText()",
                  fgText=c("orange", "aquamarine1"),
                  Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
            }
            argTextA[whichMid] <- sapply(whichMid, function(j1){
               j <- argTextA[[j1]];
               jName <- names(argTextA)[j1];
               handleArgsText(j,
                  name=jName,
                  col1=as.character(j),
                  col2=as.character(j),
                  indent=paste0(indent, "   "),
                  useCollapseBase=useCollapseBase,
                  adjustRgb=adjustRgb,
                  Crange=Crange,
                  Lrange=Lrange,
                  level=level+1,
                  useColor=useColor,
                  debug=debug,
                  verbose=verbose);
            });
         } else if ("pairlist" %in% class(argTextA) &&
               length(whichMid) > 0 &&
               all(is.numeric(argTextA[whichMid]))) {
            ############################################
            ## Handle pairlist, all values are numeric
            if (verbose) {
               printDebug(indent, "", "handleArgsText(): ",
                  "'c1' && all(is.numeric)",
                  ", calling handleArgsText()",
                  fgText=c("orange", "aquamarine1"),
                  Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
            }
            # if (requireNamespace("colorjam", quietly=TRUE)) {
            #    arg_colors <- colorjam::vals2colorLevels(argTextA[whichMid],
            #       divergent=TRUE,
            #       lens=50,
            #       col=getColorRamp("RdBu_r", trimRamp=c(3, 3)));
            # } else {
               arg_colors <- rep(c("skyblue","mediumslateblue"),
                  length.out=length(argTextA[whichMid]));
            # }
            names(arg_colors) <- as.character(whichMid);
            argTextA[whichMid] <- sapply(whichMid, function(j1){
               j <- argTextA[[j1]];
               jName <- names(argTextA)[j1];
               handleArgsText(j,
                  name=jName,
                  col1=arg_colors[as.character(j1)],
                  col2=arg_colors[as.character(j1)],
                  indent=paste0(indent, "   "),
                  useCollapseBase=useCollapseBase,
                  adjustRgb=adjustRgb,
                  Crange=Crange,
                  Lrange=Lrange,
                  level=level+1,
                  useColor=useColor,
                  debug=debug,
                  verbose=verbose);
            });
         } else {
            ############################################
            ## Handle pairlist, values are not colors
            if (firstArg %in% "list") {
               useCollapseBase <- useCollapseList;
            } else {
               useCollapseBase <- ", ";
            }
            if (verbose) {
               printDebug(indent, "", "handleArgsText(): ",
                  "firstArg is not 'function', !all(isColor)",
                  ", calling handleArgsText()",
                  fgText=c("lightgreen","orange"),
                  Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
            }
            if ("pairlist" %in% class(argTextA)) {
               whichMid <- seq_along(argTextA);
               whichEnds <- setdiff(whichEnds, whichMid);
               if (verbose) {
                  printDebug(indent, "", "handleArgsText(): ",
                     "-- pairlist, setting whichMid <- seq_along(argTextA):",
                     whichMid,
                     Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
               }
            }
            if (verbose) {
               printDebug(indent, "", "handleArgsText(): ",
                  "Iterating argTextA[whichMid], ",
                  "whichMid=", whichMid,
                  ", whichEnds=", whichEnds,
                  Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
            }
            argTextA[whichMid] <- sapply(whichMid, function(j1){
               jName <- names(argTextA)[j1];
               if (debug > 0 && verbose) {
                  printDebug(indent, "calling handleArgsText(): ",
                     "whichMid[j1], j1=",
                     j1,
                     ", jName:",
                     jName,
                     fgText=c("lightsalmon", "yellow"),
                     Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
               }
               if ("pairlist" %in% class(argTextA)) {
                  j <- argTextA[j1];
               } else {
                  j <- argTextA[[j1]];
               }
               handleArgsText(argTextA=j,
                  name=jName,
                  col1=col2,
                  col2=col1,
                  indent=paste0(indent, "   "),
                  useCollapseBase=useCollapseBase,
                  level=level+1,
                  useColor=useColor,
                  adjustRgb=adjustRgb,
                  Crange=Crange,
                  Lrange=Lrange,
                  debug=debug,
                  verbose=verbose);
            });
         }
         if (length(whichEnds) > 0) {
            if (useColor) {
               argTextA[whichEnds] <- make_styles(text=firstArg,
                  style=col1,
                  adjustRgb=adjustRgb,
                  Lrange=Lrange,
                  Crange=Crange,
                  setOptions="FALSE");
            } else {
               argTextA[whichEnds] <- firstArg;
            }
         }
         if (igrepHas("[a-zA-Z]", firstArg)) {
            ## Format: function("value1", "value2")
            if (verbose) {
               printDebug(indent, "", "handleArgsText(): ",
                  "Format: function('value1', 'value2')",
                  ", name:", name,
                  fgText=c("purple1", "yellow"),
                  Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
               printDebug(indent, "", "handleArgsText(): ",
                  "Collapsing params into multiple lines with ",
                  "useCollapseBase",
                  " then some indention",
                  fgText=c("lightblue3","orange1"),
                  Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
            }
            if (igrepHas("[\n]", useCollapseBase)) {
               useCollapse <- paste0(useCollapseBase, indent);
            } else {
               useCollapse <- useCollapseBase;
            }
            deTextA <- paste0(
               ifelse(nchar(name)>0,
                  paste0(name, "="),
                  ""),
               argTextA[whichEnds],
               "(",
               paste(argTextA[whichMid], collapse=useCollapse),
               ")",
               collapse=" ");
         } else if (length(argTextA[whichMid]) == 2) {
            ## Format: "value1" || "value2"
            if (verbose) {
               printDebug(indent, "", "handleArgsText(): ",
                  "Format: 'value1' || 'value2'",
                  ", name:",
                  name,
                  fgText=c("lightpink1","lightsalmon1"),
                  Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
               printDebug("whichEnds:", whichEnds,
                  ", whichMid:", whichMid);
               for (i1 in seq_along(argTextA)) {
                  printDebug("argTextA[[", i1, "]]");
                  print(argTextA[[i1]]);
               }
            }
            if ("[" %in% as.character(argTextA[whichEnds]) ||
               (useColor &&
                "[" %in% crayon::strip_style(argTextA[whichEnds]))) {
               ## Special case where "[" must also be closed
               deTextA <- paste(argTextA[head(whichMid,1)],
                  argTextA[whichEnds],
                  argTextA[tail(whichMid,1)],
                  gsub("([^\033]|^)[[]",
                     "\\1]",
                     argTextA[whichEnds]),
                  sep=" ");
            } else {
               deTextA <- paste(argTextA[head(whichMid,1)],
                  argTextA[whichEnds],
                  argTextA[tail(whichMid,1)],
                  sep=" ");
            }
         } else {
            if (verbose) {
               printDebug(indent, "", "handleArgsText(): ",
                  "Format: generic",
                  ", name:",
                  name,
                  fgText=c("lightskyblue","lightpink"),
                  Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
            }
            if (useColor && length(name) > 0) {
               argTextA <- make_styles(text=paste0(name,
                  ifelse(nchar(name)>0,"=",""),
                  as.character(argTextA)),
                  style=col2,
                  adjustRgb=adjustRgb,
                  Lrange=Lrange,
                  Crange=Crange,
                  setOptions="FALSE");
            } else {
               argTextA <- paste0(name,
                  ifelse(nchar(name)>0,"=",""),
                  as.character(argTextA),
                  collapse="");
            }
            deTextA <- argTextA;
         }
      }
   } else if (length(deTextA) > 1 &&
         igrepHas("^[a-zA-Z0-9]+[(]", deTextA[1])) {
      ##
      ## Class is not "call" but is multi-entry
      ##
      ## !class(argTextA) %in% "call"
      ## Multi-value entry
      whichMid <- head(tail(seq_along(deTextA), -1), -1);
      whichEnds <- setdiff(seq_along(deTextA), whichMid);
      if (useColor) {
         deTextA[whichMid] <- make_styles(
            text=as.character(deTextA[whichMid]),
            style=col2,
            adjustRgb=adjustRgb,
            Lrange=Lrange,
            Crange=Crange,
            setOptions="FALSE");
         deTextA[whichEnds] <- make_styles(
            text=as.character(deTextA[whichEnds]),
            style=col1,
            adjustRgb=adjustRgb,
            Lrange=Lrange,
            Crange=Crange,
            setOptions="FALSE");
      } else {
         deTextA[whichMid] <- as.character(deTextA[whichMid]);
         deTextA[whichEnds] <- as.character(deTextA[whichEnds]);
      }
      # 0.0.106.900 - 'i' was replaced with 'name' since 'i' does not exist
      # aText <- paste(i, paste(deTextA, collapse=" "), sep=" = ");
      aText <- paste(name, paste(deTextA, collapse=" "), sep=" = ");
   } else if ("logical" %in% class(argTextA)) {
      ##
      ## Class is logical, we colorize TRUE and FALSE
      ##
      if (useColor) {
         if (igrepHas("FALSE", deTextA)) {
            if (length(name) > 0 && nchar(name) > 0) {
               ## Value has a name, so print "name=FALSE"
               deTextA <- paste0(
                  make_styles(text=c(name, "=", as.character(deTextA)),
                     style=c(col1, NA, colF),
                     adjustRgb=adjustRgb,
                     Lrange=Lrange,
                     Crange=Crange,
                     setOptions="FALSE"),
                  collapse="");
            } else {
               ## Value has no name, so print "FALSE"
               deTextA <- make_styles(
                  text=as.character(deTextA),
                     style=colF,
                     adjustRgb=adjustRgb,
                     Lrange=Lrange,
                     Crange=Crange,
                     setOptions="FALSE");
            }
         } else {
            if (length(name) > 0 && nchar(name) > 0) {
               ## Value has a name, so print "name=TRUE"
               deTextA <- paste0(
                  make_styles(text=c(name, "=", as.character(deTextA)),
                     style=colT,
                     adjustRgb=adjustRgb,
                     Lrange=Lrange,
                     Crange=Crange,
                     setOptions="FALSE"),
                  collapse="");
            } else {
               ## Value has no name, so print "TRUE"
               deTextA <- make_styles(text=as.character(deTextA),
                  style=colT,
                  adjustRgb=adjustRgb,
                  Lrange=Lrange,
                  Crange=Crange,
                  setOptions="FALSE");
            }
         }
      } else {
         ## no colorization of logical string
         if (length(name) > 0 && nchar(name) > 0) {
            ## Value has a name, so print "name=TRUE"
            deTextA <- paste0(c(name, "=", as.character(deTextA)),
               collapse="");
         } else {
            ## Value has no name, so print "TRUE"
            deTextA <- as.character(deTextA);
         }
      }
   } else {
      ##
      ## Class is not "call", not "logical"
      ##
      if (verbose) {
         printDebug(indent, "", "handleArgsText(): ",
            "class(argTextA):", class(argTextA),
            ", deTextA (before make_styles):",
            deTextA,
            fgText=c("lightsteelblue","lightsalmon2"),
            Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
      }
      if (length(argTextA) > 0 &&
            useColor &&
            all(isColor(rmNA(naValue="grey35", as.character(argTextA))))) {
         ##
         ## Handle parameter values which are all colors, by using those
         ## colors for the output text color
         if (length(name) > 0 && nchar(name) > 0) {
            if (verbose) {
               printDebug(indent, "", "handleArgsText(): ",
                  "  named ", "  colored", " parameter",
                  fgText=c("lightpink2","lightblue3"),
                  Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
            }
            deTextA <- paste0(
               make_styles(text=c(name, "=", as.character(deTextA)),
                  style=c(col1, NA,
                     rmNA(naValue="grey35", as.character(argTextA))),
                  adjustRgb=adjustRgb,
                  Lrange=Lrange,
                  Crange=Crange,
                  setOptions="FALSE"),
               collapse="");
         } else {
            if (verbose) {
               printDebug(indent, "", "handleArgsText(): ",
                  "unnamed ", "  colored", " parameter",
                  fgText=c("lightpink2","lightblue3"),
                  Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
            }
            deTextA <- paste0(
               make_styles(text=as.character(deTextA),
                  style=as.character(argTextA),
                  adjustRgb=adjustRgb,
                  Lrange=Lrange,
                  Crange=Crange,
                  setOptions="FALSE"),
               collapse="");
         }
      } else {
         ##
         ## Parameter values are not colors, so we use default colors here
         if (length(name) > 0 && nchar(name) > 0) {
            if (verbose) {
               printDebug(indent, "", "handleArgsText(): ",
                  "  named ", "uncolored", " parameter",
                  fgText=c("lightpink2","lightblue3"),
                  Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
            }
            if (useColor && "NULL" %in% class(argTextA)) {
               ## For NULL we color using colNULL
               deTextA <- paste0(
                  make_styles(text=c(name, "=", as.character(deTextA)),
                     style=c(col2, NA, colNULL),
                     adjustRgb=adjustRgb,
                     Lrange=Lrange,
                     Crange=Crange,
                     setOptions="FALSE"),
                  collapse="");
            } else if (length(as.character(deTextA)) > 0 &&
                  nchar(as.character(deTextA)) > 0) {
               if (useColor) {
                  deTextA <- paste0(
                     make_styles(text=c(name, "=", as.character(deTextA)),
                        style=c(col2, NA, col1),
                        adjustRgb=adjustRgb,
                        Lrange=Lrange,
                        Crange=Crange,
                        setOptions="FALSE"),
                     collapse="");
               } else {
                  deTextA <- paste0(
                     c(name,
                        "=",
                        as.character(deTextA)),
                     collapse=" ");
               }
            } else {
               ## No parameter value, just the name
               ## as used for mandatory function arguments
               if (useColor) {
                  deTextA <- paste0(
                     make_styles(text=c(name),
                        style=c(col2),
                        adjustRgb=adjustRgb,
                        Lrange=Lrange,
                        Crange=Crange,
                        setOptions="FALSE"),
                     collapse="");
               } else {
                  deTextA <- paste0(c(name),
                     collapse=" ");
               }
            }
         } else {
            if (verbose) {
               printDebug(indent, "", "handleArgsText(): ",
                  "unnamed ", "uncolored", " parameter",
                  fgText=c("lightpink2","lightblue3"),
                  Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
            }
            if (useColor) {
               if ("NULL" %in% class(argTextA)) {
                  deTextA <- paste0(
                     make_styles(text=as.character(deTextA),
                        style=colNULL,
                        adjustRgb=adjustRgb,
                        Lrange=Lrange,
                        Crange=Crange,
                        setOptions="FALSE"),
                     collapse="");
               } else {
                  deTextA <- paste0(
                     make_styles(text=as.character(deTextA),
                        style=col1,
                        adjustRgb=adjustRgb,
                        Lrange=Lrange,
                        Crange=Crange,
                        setOptions="FALSE"),
                     collapse="");
               }
            } else {
               deTextA <- paste0(as.character(deTextA),
                  collapse="");
            }
         }
      }
      if (debug > 0 && verbose) {
         printDebug(indent, "", "handleArgsText(): ",
            "deTextA (after make_styles):", deTextA,
            fgText=c("lightskyblue3","lightseagreen"),
            Crange=Crange,Lrange=Lrange,adjustRgb=adjustRgb);
      }
   }
   return(deTextA);
}

#' Apply noise floor and ceiling to numeric vector
#'
#' Apply noise floor and ceiling to numeric vector
#'
#' A noise floor is useful when detected numeric values are sometimes below
#' a clear noise threshold, and where some downstream ratio may be calculated
#' using these values. Applying a noise floor ensures the ratios and not
#' artificially higher, especially in cases where the values involved are
#' least reliable. This procedure is expected to produce more conservative
#' and appropriate ratios in that scenario.
#'
#' A ceiling is similar, values above the ceiling are set to the ceiling,
#' which is practical when values above a certain threshold are conceptually
#' similar to those at the threshold. One clear example is plotting
#' `-log10(Pvalue)` when the range of P-values might approach 1e-1000.
#' In this case, setting a ceiling of 50 conceptually equates P-values
#' below 1e-50, while also restricting the axis range of a plot.
#'
#' The ability to set values at the floor to a different value, using
#' `newValue` different from `minimum`, is intended to allow separation
#' of numeric values from the floor for illustrative purposes.
#'
#' @returns `numeric` vector or `matrix`, matching the input type `x`
#'    where numeric values are fixed to the `minimum` and `ceiling`
#'    values as defined by `newValue` and `newCeiling`, respectively.
#'
#' @family jam numeric functions
#'
#' @param x `numeric` vector or matrix
#' @param minimum `numeric` floor value
#' @param newValue `numeric`, by default the same as the floor value. Sometimes
#'    it can be useful to define a different value, one example is to define
#'    values as `NA`, or another distinct number away from the floor.
#' @param adjustNA `logical` whether to change `NA` values to the `newValue.`
#' @param ceiling `numeric` value, optionally a ceiling. If defined, then values
#'    above the ceiling value are set to `newCeiling.`
#' @param newCeiling `numeric` value when ceiling is defined, values above the
#'    ceiling are set to this `numeric` value.
#' @param ... additional parameters are ignored.
#'
#' @examples
#' # start with some random data
#' n <- 2000;
#' x1 <- stats::rnorm(n);
#' y1 <- stats::rnorm(n);
#'
#' # apply noise floor and ceiling
#' x2 <- noiseFloor(x1, minimum=-2, ceiling=2);
#' y2 <- noiseFloor(y1, minimum=-2, ceiling=2);
#'
#' # apply noise floor and ceiling with custom replacement values
#' xm <- cbind(x=x1, y=y1);
#' xm3 <- noiseFloor(xm,
#'    minimum=-2, newValue=-3,
#'    ceiling=2, newCeiling=3);
#'
#' withr::with_par(list("mfrow"=c(2,2)), {
#' plotSmoothScatter(x1, y1);
#' plotSmoothScatter(x2, y2);
#' plotSmoothScatter(xm3);
#' })
#'
#' @export
noiseFloor <- function
(x,
 minimum=0,
 newValue=minimum,
 adjustNA=FALSE,
 ceiling=NULL,
 newCeiling=ceiling,
 ...)
{
   ## Purpose is to apply a noise floor, that is, to set all values
   ## to be at least 'minimum' amount.
   ## This function performs no scaling or normalization.
   if (length(x) == 0) {
      return(x);
   }
   if (length(minimum) > 0) {
      if (adjustNA) {
         x[is.na(x) | (!is.na(x) & x < minimum)] <- newValue;
      } else {
         x[!is.na(x) & x < minimum] <- newValue;
      }
   }
   if (length(ceiling) > 0) {
      x[!is.na(x) & x > ceiling] <- newCeiling;
   }
   return(x);
}

#' Convert radians to degrees
#'
#' Convert radians to degrees
#'
#' This function simply converts radians which range from zero to pi*2,
#' into degrees which range from 0 to 360.
#'
#' @param x `numeric` vector, expected to be radian values between zero
#'    and pi*2.
#' @param ... other parameters are ignored.
#'
#' @family jam numeric functions
#'
#' @returns `numeric` vector after coverting radians to degrees.
#'
#' @examples
#' rad2deg(c(pi*2, pi/2))
#'
#' @export
rad2deg <- function
(x, ...)
{
   ## Purpose is to convert radians to degrees, using pi/2 as 90 degrees
   x * (180 / pi);
}

#' Convert degrees to radians
#'
#' Convert degrees to radians
#'
#' This function simply converts degrees which range from 0 to 360,
#' into radians which range from zero to pi*2.
#'
#' @param x `numeric` vector, expected to be degree values between zero
#'    and 360.
#' @param ... other parameters are ignored.
#'
#' @family jam numeric functions
#'
#' @returns `numeric` vector after coverting degrees to radians.
#'
#' @examples
#' deg2rad(rad2deg(c(pi*2, pi/2)))/pi;
#'
#' @export
deg2rad <- function
(x, ...)
{
   ## Purpose is to convert degrees to radians, using pi/2 as 90 degrees
   x * (pi / 180);
}

#' print dimensions of list object elements
#'
#' @description
#' `sdim()`  prints the name and dimensions of `list` object elements,
#' such as a `list` of `data.frame`
#'
#' `ssdim()` prints the name and dimensions of nested elements of `list`
#' objects, for example a `list` of `list` objects that each contain
#' other objects.
#'
#' `sdima()` prints the name and dimensions of object `attributes(x)`.
#' It is useful for summarizing the `attributes()` of an object.
#'
#' `ssdima()` prints the name and dimensions of nested elements of `list`
#' object `attributes()`, for example a `list` of `list` objects that each
#' contain other objects. It is useful for comparing attributes across `list`
#' elements.
#'
#' This function prints the dimensions of a list of objects, usually a `list`
#' of `data.frame` objects, but extended to handle more complicated lists,
#' including even S4 object `methods::slotNames()`.
#'
#' Over time, more object types will be made compatible with this function.
#' Currently, `igraph` objects will print the number of nodes and edges, but
#' requires the igraph package to be installed.
#'
#' @param x one of several recognized object classes:
#'    * an S3 object inheriting from class `"list"`, including a nested list of
#'    lists or simple list
#'    * an S3 atomic object, which returns only the length
#'    * a single multi-dimensional object such as `data.frame`, `matrix`,
#'    `array`, `tibble`, or similar, which returns only its dimensions.
#'    * an `S4` object in which case it used `methods::slotNames(x)`
#'    to traverse the object structure
#'    * an `"environment"` object, in which case `ls(envir=x)` is
#'    used to traverse the object structure.
#'    * When the object is `S4` that inherits `"List"` from the
#'    `S4Vectors` package, it will attempt to use the proper subset
#'    functions from `S4Vectors` via `names(x)`, but that process only works
#'    properly if the `S4Vectors` package is previously loaded,
#'    otherwise it reverts to using `methods::slotNames(x)`.
#' @param includeClass `logical` indicating whether to print the class of
#'    each element in the input `x` object. Note that for S4 objects,
#'    each element will be the object returned for each of
#'    `methods::slotNames(x)`.
#' @param doFormat `logical` indicating whether to format the dimensions using
#'    \code{format(...,big.mark=",")}, which is mainly useful for extremely
#'    large dimensions. This parameter should probably become more broadly
#'    useful and respectful for different locales.
#' @param big.mark `character` value used when `doFormat=TRUE`, used in the
#'    call to `format(...,big.mark)`.
#' @param verbose `logical` whether to print verbose output
#' @param ... additional parameters are ignored.
#'
#' @returns `data.frame` where each row indicates the dimensions of
#'    each element in the input list. When `includeClass` is `TRUE` it
#'    will include a column `class` which indicates the class of each
#'    list element. When the input list contains arrays with more than
#'    two dimensions, the first two dimensions are named `"rows"` and
#'    `"columns"` with additional dimensions named `"dim3"` and so on.
#'    Any list element with fewer than that many dimensions will only have
#'    values populated to the relevant dimensions, for example a character
#'    vector will only populate the length.
#'
#' @family jam list functions
#'
#' @examples
#' L <- list(LETTERS=LETTERS,
#'    letters=letters,
#'    lettersDF=data.frame(LETTERS, letters));
#' sdim(L)
#'
#' LL <- list(L=L, A=list(1:10))
#' sdim(LL)
#' ssdim(LL)
#'
#' m <- matrix(1:9,
#'    ncol=3,
#'    dimnames=list(
#'       Rows=letters[1:3],
#'       Columns=LETTERS[1:3]));
#' sdima(m);
#' ssdima(m);
#'
#' @export
sdim <- function
(x,
 includeClass=TRUE,
 doFormat=FALSE,
 big.mark=",",
 verbose=FALSE,
 ...)
{
   ## Purpose is to take a list of data.frames and return the dim() of each.
   ##
   ## It does try to work with other object types, returning the length for
   ## most which are not data.frame, matrix, tibble.
   ##
   ## includeClass is logical value indicating whether to return the class
   ## of each object in its own column, which provides a useful summary
   ## of the list elements.
   ##
   ## If the object is not "list" class, and has slotNames(x), then
   ## they will be used instead
   ##

   getDim <- function
   (i,
    doFormat=FALSE,
    includeClass=FALSE,
    big.mark=",",
    ...)
   {
      ## Simply a wrapper function
      iClass <- class(i);

      ## igraph objects return the number of vertices and edges
      if (igrepHas("igraph", iClass)) {
         if (!check_pkg_installed("igraph")) {
            stop("The igraph package is required to describe igraph objects.");
         }
         iDim <- c(igraph::vcount(i),
            igraph::ecount(i));
      } else {
         iDim <- tryCatch({
            dim(i);
         }, error=function(e){
            length(i);
         });
         if (length(iDim) == 0) {
            iDim <- length(i);
         }
      }
      names(iDim) <- head(
         c("rows",
            "cols",
            makeNames(renameOnes=TRUE, startN=3, suffix="",
               tail(rep("dim", length.out=length(iDim)), -2))),
         length(iDim));
      if (doFormat) {
         iDim <- format(iDim,
            big.mark=big.mark,
            trim=TRUE,
            ...);
      }
      iDim <- as.list(iDim);
      if (includeClass) {
         iClass <- nameVector(class(i),
            "class",
            renameFirst=FALSE,
            startN=2);
         iDim <- c(as.list(iClass),
            iDim);
      }
      data.frame(iDim);
   }

   ## Special case for S4 objects with only one slotName ".Data"
   if (isS4(x)) {
      sn1 <- methods::slotNames(x);
      xn1 <- names(x);
      if (".Data" %in% sn1 && length(sn1) == 1) {
         if (verbose) {
            printDebug("sdim(): ",
               "Coercing S4 class using ",
               "list(x@.Data)");
         }
         xl <- methods::slot(x, ".Data");
         ## If there are no names, and the names(x) are the
         ## proper length, assign names(x) to the list we create.
         if (length(names(xl)) == 0 && length(xl) == length(xn1)) {
            names(xl) <- xn1;
         }
         x <- xl;
         rm(xl);
      }
   }

   ## Iterate each element and determine the dimensions
   #if (!igrepHas("list|tbl|tibble|data.frame|matrix", class(x)) &&
   #      isS4(x)) {
   x_is_table <- igrepHas("tbl|tibble|data.frame|matrix|data.table|dataframe",
      class(x));
   if (isS4(x) && !x_is_table) {
      ## Wrap in tryCatch() in case the supporting object package
      ## is not installed, otherwise just use slotNames(x).
      is_List <- tryCatch({
         inherits(x, "List") && length(x[[1]]) >= 0;
      }, error=function(e){
         FALSE;
      });
      if (is_List) {
         sn1 <- nameVector(seq_along(x), names(x));
         if (verbose) {
            printDebug("sdim(): ",
               "Coercing S4 List using ",
               "names(x)");
         }
      } else {
         sn1 <- nameVector(methods::slotNames(x));
         if (verbose) {
            printDebug("sdim(): ",
               "Coercing S4 class using ",
               "slotNames(x)");
         }
      }
      sdL <- lapply(sn1, function(sni){
         if (is_List) {
            i <- x[[sni]];
         } else {
            i <- methods::slot(x, sni);
         }
         iDim <- getDim(i,
            doFormat=doFormat,
            includeClass=includeClass,
            big.mark=big.mark,
            ...);
         iDim;
      });
   } else if (is.environment(x)) {
      ## handle environment by using ls(x) which maintains order,
      ## instead of names(x) which is in random order
      sdL <- lapply(nameVector(ls(x)), function(i){
         iDim <- getDim(
            get(i, envir=x),
            doFormat=doFormat,
            includeClass=includeClass,
            big.mark=big.mark,
            ...);
         iDim;
      });
   } else {
      ## If it is a vector and not an inherited form of list
      if ((is.atomic(x) && !is.recursive(x)) ||
            x_is_table) {
         if (verbose) {
            printDebug("sdim(): ",
               "Coercing class(x) from ",
               class(x),
               " to list(x)");
            printDebug("x_is_table:", x_is_table);
            printDebug("is.vector(x):", is.vector(x));
            printDebug("igrepHas('list', class(x)):",
               igrepHas("list", class(x)));
            printDebug("is.list(x):", is.list(x));
         }
         x <- list(x);
      }
      if (verbose) {
         printDebug("sdim(): ",
            "length(x):",
            length(x));
      }
      sdL <- lapply(x, function(i){
         iDim <- getDim(i,
            doFormat=doFormat,
            includeClass=includeClass,
            big.mark=big.mark,
            ...);
         iDim;
      });
   }
   if (includeClass) {
      sdLnames <- unique(unlist(lapply(sdL, names)));
      sdLnamesC <- vigrep("^class", sdLnames);
      sdLnamesD <- unvigrep("^class", sdLnames);
      sdLnames <- c(sdLnamesD, sdLnamesC)
      sdL <- lapply(sdL, function(i){
         df2 <- data.frame(lapply(nameVector(sdLnames), function(i)NA));
         df2[,colnames(i)] <- i;
         df2;
      });
   }
   sd1 <- rbindList(sdL);
   sd1$rows <- rmNA(naValue="", sd1$rows);
   sd1$cols <- rmNA(naValue="", sd1$cols);
   return(sd1);
}

#' @rdname sdim
#'
#' @returns `data.frame` which
#'    describes the dimensions of the objects in
#'    `attributes(x)`.
#'
#' @export
sdima <- function
(x,
 includeClass=TRUE,
 doFormat=FALSE,
 big.mark=",",
 verbose=FALSE,
 ...)
{
   #
   sdim(attributes(x),
      includeClass=includeClass,
      doFormat=doFormat,
      big.mark=big.mark,
      verbose=verbose,
      ...);
}

#' @rdname sdim
#'
#' @returns `list` of `data.frame` each of which
#'    describes the dimensions of the objects in
#'    `attributes(x)`.
#'
#' @export
ssdima <- function
(x,
 includeClass=TRUE,
 doFormat=FALSE,
 big.mark=",",
 verbose=FALSE,
 ...)
{
   #
   ssdim(attributes(x),
      includeClass=includeClass,
      doFormat=doFormat,
      big.mark=big.mark,
      verbose=verbose,
      ...);
}

#' @rdname sdim
#'
#' @returns `list` of `data.frame`, each row indicates the dimensions of
#'    each element in the input list.
#'    When `includeClass` is `TRUE` it
#'    will include a column `class` which indicates the class of each
#'    list element.
#'    When the input `list` contains arrays with more than
#'    two dimensions, the first two dimensions are named `"rows"` and
#'    `"columns"` with additional dimensions named `"dim3"` and so on.
#'    Any `list` element with fewer than that many dimensions will only have
#'    values populated to the relevant dimensions, for example a character
#'    vector will only populate the length.
#'
#' @export
ssdim <- function
(x,
 includeClass=TRUE,
 doFormat=FALSE,
 big.mark=",",
 verbose=FALSE,
 ...)
{
   ## Purpose is to run sdim() on a list of lists of data.frames

   ## Special case for S4 objects with only one slotName ".Data"
   if (isS4(x)) {
      sn1 <- methods::slotNames(x);
      xn1 <- names(x);
      if (".Data" %in% sn1 && length(sn1) == 1) {
         if (verbose) {
            printDebug("ssdim(): ",
               "Coercing S4 class using ",
               "list(x@.Data)");
         }
         xl <- methods::slot(x, ".Data");
         ## If there are no names, and the names(x) are the
         ## proper length, assign names(x) to the list we create.
         if (length(names(xl)) == 0 && length(xl) == length(xn1)) {
            names(xl) <- xn1;
         }
         x <- xl;
      }
   }

   ## S4 objects are handled as a list using slotNames(x)
   if (isS4(x)) {
      # For S4 objects we iterate slotNames(x)
      if (verbose) {
         printDebug("ssdim(): ",
            "Handling S4 object type.");
      }
      ## Wrap in tryCatch() in case the supporting object package
      ## is not installed, otherwise just use slotNames(x).
      is_List <- tryCatch({
         inherits(x, "List") && length(x[[1]]) >= 0;
      }, error=function(e){
         FALSE;
      })
      if (is_List) {
         ## List uses names(x)
         lapply(nameVector(seq_along(x), names(x)), function(iName){
            if (verbose) {
               printDebug("ssdim(): ",
                  "S4 List item:",
                  iName,
                  ", List name:",
                  names(x)[iName]);
            }
            sdim(x[[iName]],
               includeClass=includeClass,
               doFormat=doFormat,
               big.mark=big.mark,
               verbose=verbose);
         });
      } else {
         lapply(nameVector(methods::slotNames(x)), function(iName){
            if (verbose) {
               printDebug("ssdim(): ",
                  "slotName iName:",
                  iName);
            }
            sdim(methods::slot(x, iName),
               includeClass=includeClass,
               doFormat=doFormat,
               big.mark=big.mark,
               verbose=verbose);
         });
      }
   } else if (!any(c("list", "environment") %in% unlist(sclass(x)))) {
      ## No recognizable list structure
      if (is.vector(x) ||
            igrepHas("data.*frame|tibble|matrix|ranges$", class(x))) {
         if (verbose) {
            printDebug("ssdim(): ",
               "Handling vector/data.frame/tibble/ranges.");
         }
         sdim(x);
      } else if (length(names(x)) > 0) {
         if (verbose) {
            printDebug("ssdim(): ",
               "Handling S3 object type using names(x).");
         }
         lapply(nameVector(names(x)), function(i){
            sdim(x[[i]]);
         });
      } else if (length(x) > 1) {
         if (verbose) {
            printDebug("ssdim(): ",
               "Handling S3 object type without names.");
         }
         lapply(seq_along(x), function(i){
            sdim(x[[i]]);
         });
      } else {
         if (verbose) {
            printDebug("ssdim(): ",
               "Falling back to sdim(x).");
         }
         sdim(x);
      }
   } else {
      if (verbose) {
         printDebug("ssdim(): ",
            "Handling list object type.");
      }
      lapply(x, function(iDFL){
         sdim(iDFL,
            includeClass=includeClass,
            doFormat=doFormat,
            big.mark=big.mark,
            verbose=verbose);
      });
   }
}

#' return the classes of a list of objects
#'
#' return the classes of a list of objects
#'
#' This function takes a `list` and returns the classes for each
#' object in the list. In the event an object class has multiple values,
#' the returned object is a list, otherwise is a vector.
#' If `x` is an S4 object, then `methods::slotNames(x)` is used, and
#' the class is returned for each S4 slot.
#'
#' When `x` is a `data.frame`, `data.table`, `tibble`, or similar
#' `DataFrame` table-like object, the class of each column is returned.
#'
#' For the special case where `x` is an S4 object with one slotName
#' `".Data"`, the values in `x@.Data` are coerced to a `list`. One
#' example of this case is with `limma::MArrayLM-class`.
#'
#' When `x` is a matrix, the class of each column is returned for
#' consistency, even though the class of each column should be identical.
#'
#' For more more information about a list-like object, including
#' the lengths/dimensions of the elements, see `sdim()` or `ssdim()`.
#'
#' @returns `character` vector with the class of each list element, or
#' column name, depending upon the input `class(x)`.
#'
#' @param x an S3 object inheriting from class `list`, or an S4 object.
#' @param ... additional parameters are ignored.
#'
#' @family jam list functions
#'
#' @examples
#' sclass(list(LETTERS=LETTERS, letters=letters));
#'
#' sclass(data.frame(B=letters[1:10], C=2:11))
#'
#' @export
sclass <- function
(x,
 ...)
{
   ## Purpose is to take a list of objects and return the class() of each.
   ##
   ## If the object is not "list" class, and has slotNames(x), then
   ## they will be used instead
   if (isS4(x)) {
      sn1 <- methods::slotNames(x);
      xn1 <- names(x);
      if (".Data" %in% sn1 && length(sn1) == 1) {
         ## Special case of x@.Data containing a list
         xl <- methods::slot(x, ".Data");
         if (length(xl) == length(xn1)) {
            names(xl) <- xn1;
         }
         x <- xl;
      } else {
         sd1 <- sapply(sn1, function(sni){
            class(methods::slot(x, sni));
         });
      }
   }
   if (igrepHas("matrix", class(x))) {
      sd1 <- sapply(nameVector(colnames(x)), function(i){
         class(x[,i])
      });
   } else if (!isS4(x)) {
      sd1 <- sapply(x, function(i){
         class(i);
      });
   }
   sd1;
}

#' Scale a numeric vector from 0 to 1
#'
#' Scale a numeric vector from 0 to 1
#'
#' This function is intended as a quick way to scale numeric values
#' between 0 and 1, however other ranges can be defined as needed.
#'
#' NA values are ignored and will remain NA in the output. To handle
#' NA values, use the `rmNA()` function, which can optionally replace
#' NA with a fixed numeric value.
#'
#' The parameters `low` and `high` are used optionally to provide a
#' fixed range of values expected for `x`, which is useful for
#' consistent scaling of `x`. Specifically, if `x` may be a
#' vector of numeric values ranging from 0 and 100, you would
#' define `low=0` and `high=100` so that `x` will be consistently
#' scaled regardless what actual range is represented by `x`.
#'
#' Note that when `x` contains only one value, and `low` and `high`
#' are not defined, then `x` will be scaled based upon the
#' argument `singletMethod`. For example, if you provide `x=2`
#' and want to scale `x` values to between 0 and 10... `x` can
#' either be the `mean` value `5`; the `min`imum value `0`; or
#' the `max`imum value `10`.
#'
#' However, if `low` or `high` are defined, then x will be scaled
#' relative to that range.
#'
#' @param x `numeric` vector.
#' @param from the minimum `numeric` value to re-scale the input numeric vector.
#' @param to the maximum `numeric` value to re-scale the input numeric vector.
#' @param low `numeric` value defining the low end of the input numeric range,
#'    intended when input values might not contain the entire numeric
#'    range to be re-scaled.
#' @param high `numeric` value defining the high end of the input numeric range,
#'    intended when input values might not contain the entire numeric
#'    range to be re-scaled.
#' @param naValue optional `numeric` value used to replace `NA`, usually by
#'    replacing `NA` with zero.
#' @param singletMethod `character` value describing how to handle singlet
#'    input values, for example how to scale the number 5 by itself.
#'    * "mean" then it uses the average of `from` and `to`,
#'    * "min" uses the `from` value, and
#'    * "max" uses the `to` value.
#' @param ... additional parameters are ignored.
#'
#' @family jam numeric functions
#'
#' @returns `numeric` vector after applying the transformations.
#'
#' @examples
#' # Notice the first value 1 is re-scaled to 0
#' normScale(1:11);
#'
#' # Scale values from 0 to 10
#' normScale(1:11, from=0, to=10);
#'
#' # Here the low value is defined as 0
#' normScale(1:10, low=0);
#'
#' normScale(c(10,20,40,30), from=50, to=65);
#'
#' @export
normScale <- function
(x,
 from=0,
 to=1,
 low=min(x, na.rm=TRUE),
 high=max(x, na.rm=TRUE),
 naValue=NA,
 singletMethod=c("mean","min","max"),
 ...)
{
   ## Purpose is to scale values between 0 and 1
   ## or optionally into a given range.
   ## naValue is used to change any NA values to this number.
   ##
   ## Note data can be scale to a custom range, using from and to.
   ##
   ## Note data can also be scaled to a range, using fixed reference points,
   ## using low and high.  For example, you may want values at zero to stay zero,
   ## even if there are negative values.
   ##
   ## If only one value is supplied, then singletMethod is used:
   ## singletMethod="mean" the resulting value is the mean(c(from,to))
   ## singletMethod="max" the resulting value is the max(c(from,to))
   ## singletMethod="min" the resulting value is the min(c(from,to))
   ##
   ## You can therefore scale data using fixed points to a new fixed range,
   ## For example you may want a value of 100 to be scaled to 5, and values
   ## at zero to remain zero, with negative values also being scale accordingly.
   ##    normScale(x, from=0, to=5, low=0, high=100);
   ##
   #x <- ((x - min(x, na.rm=na.rm)) / diff(range(x, na.rm=na.rm)) * diff(c(from,to))) +  from;
   ##
   ## If given low and high, and they are different (meaning there is
   ## more than one input value), then we honor that range. This way
   ## we can receive a single value, along with a defined low and high,
   ## and process it consistently

   # process NA input
   if (all(is.na(x))) {
      return(NA);
   }

   if (length(low) > 0 && length(high) > 0 && low == high) {
      singletMethod <- match.arg(singletMethod);
      if (singletMethod %in% "mean") {
         x[!is.na(x)] <- mean(c(from, to));
      } else if (singletMethod %in% "max") {
         x[!is.na(x)] <- max(c(from, to));
      } else if (singletMethod %in% "max") {
         x[!is.na(x)] <- min(c(from, to));
      }
   } else {
      x <- ((x - low) / diff(range(c(low,high))) * diff(c(from,to))) +  from;
      if (!is.na(naValue) && any(is.na(x))) {
         x[is.na(x)] <- naValue;
      }
   }
   return(x);
}

#' Warp a vector of numeric values relative to zero
#'
#' Warp a vector of numeric values relative to zero
#'
#' This function warps numeric values using a log curve
#' transformation, such that values are either more compressed
#' near zero, or more compressed near the maximum values.
#' For example, a vector of integers from -10 to 10 would be warped
#' so the intervals near zero were smaller than 1, and intervals
#' farthest from zero are greater than 1.
#'
#' The main driver for this function was the desire to compress
#' divergent color scales used in heatmaps, in order to enhance
#' smaller magnitude numeric values. Existing color ramps map the
#' color gradient in a linear manner relative to the numeric range,
#' which can cause extreme values to dominate the color scale.
#' Further, a linear application of colors is not always appropriate.
#'
#' @param x `numeric` vector
#' @param lens `numeric` value which defines the lens factor,
#'    where `lens > 0` will compress values near zero, and
#'    `lens < 0` will expand values near zero and compress
#'    values near the maximum value. If `lens == 0` the
#'    numeric values are not changed.
#' @param baseline `numeric` value describing the baseline, for example
#'    when the central value is non-zero. The baseline is subtracted
#'    from `x`, the warp is applied, then the baseline is added to
#'    the result.
#' @param xCeiling `numeric` maximum value used for the color warp range,
#'    useful for consistency. When `xCeiling` is not supplied, the
#'    maximum difference from `baseline` is used. When `xCeiling` is
#'    defined, and `baseline` is non-zero, the effective value used
#'    is `(xCeiling - baseline)`.
#' @param ... additional arguments are ignored.
#'
#' @returns `numeric` vector after applying the warp function.
#'
#' @family jam numeric functions
#'
#' @examples
#' x <- c(-10:10);
#' xPlus10 <- warpAroundZero(x, lens=10);
#' xMinus10 <- warpAroundZero(x, lens=-10);
#'
#' plot(x=x, y=xPlus10, type="b", pch=20, col="dodgerblue",
#'    main="Comparison of lens=+10 to lens=-10");
#' graphics::points(x=x, y=xMinus10, type="b", pch=18, col="orangered");
#' graphics::abline(h=0, v=0, col="grey", lty="dashed", a=0, b=1);
#' graphics::legend("topleft",
#'    legend=c("lens=+10", "lens=-10"),
#'    col=c("dodgerblue","orangered"),
#'    pch=c(20,18),
#'    lty="solid",
#'    bg="white");
#'
#' # example showing the effect of a baseline=5
#' xPlus10b5 <- warpAroundZero(x, lens=10, baseline=5);
#' xMinus10b5 <- warpAroundZero(x, lens=-10, baseline=5);
#' plot(x=x, y=xPlus10b5, type="b", pch=20, col="dodgerblue",
#'    main="Comparison of lens=+10 to lens=-10",
#'    ylim=c(-10,15),
#'    sub="baseline=+5");
#' graphics::points(x=x, y=xMinus10b5, type="b", pch=18, col="orangered");
#' graphics::abline(h=5, v=5, col="grey", lty="dashed", a=0, b=1);
#' graphics::legend("topleft",
#'    legend=c("lens=+10", "lens=-10"),
#'    col=c("dodgerblue","orangered"),
#'    pch=c(20,18),
#'    lty="solid",
#'    bg="white");
#'
#' @export
warpAroundZero <- function
(x,
 lens=5,
 baseline=0,
 xCeiling=NULL,
   ...)
{
   ## Purpose is to take a vector representing points on
   ## a line with slope=0, and warp the line using log2
   ## transformation, symmetric around zero.
   if (lens == 0) {
      return(x);
   }
   if (length(baseline) == 1 && is.numeric(baseline)) {
      x <- x - baseline;
   } else {
      baseline <- 0;
   }
   nColors <- 50;
   if (length(xCeiling) == 0) {
      xCeiling <- max(abs(x), na.rm=TRUE);
   } else {
      xCeiling <- max(abs(xCeiling) - baseline);
   }
   if (any(abs(x) > abs(xCeiling))) {
      xOob <- (abs(x) > abs(xCeiling));
      x[xOob] <- abs(x[xOob]) * sign(x[xOob]);
   }
   x1 <- seq(from=-1, to=1, length.out=nColors);
   y1 <- normScale(
      log2(1+abs(x1)*abs(lens))*sign(x1),
      from=-1,
      to=1);
   if (lens > 0) {
      y1 <- stats::approx(x=y1, y=x1, xout=x1)$y;
   }

   stats::approx(x=x1 * (xCeiling),
      y=y1 * (xCeiling),
      xout=x)$y + baseline;
}

#' lengths for recursive lists
#'
#' lengths for recursive lists
#'
#' This function takes a list as input, and returns the length
#' of each list element after running `base::unlist()`.
#'
#' @returns `integer` value, vector, or list:
#' * When `doSum is NULL` (default) it returns an `integer` vector
#' with length `length(x)` and names `names(x)`,
#' whose values are the total number of elements in each item in
#' `x` after running `base::unlist()`.
#' * When `doSum=="TRUE"`, it returns the single `integer` length of
#' all elements in `x`.
#' * When `doSum=="FALSE"`, it returns the full structure of `x` with the
#' `integer` length of each element.
#'
#' The parameter `doSum` is intended for internal use, during
#' recursive calls of `rlengths()` to itself. When `doSum is NULL` or
#' `TRUE`, recursive calls to `rlengths()` set `doSum=TRUE`.
#'
#' @family jam list functions
#'
#' @param x `list` or vector
#' @param doSum `logical` indicating whether to return the overall sum
#'    of lengths. When `NULL` it will return the aggregate length of
#'    each list element in `x`. When `FALSE` it will return the same
#'    list structure of x, with the length of each. When `TRUE` it will
#'    return the total length of all elements in `x` as one value.
#' @param ... additional parameters are ignored
#'
#' @examples
#' x <- list(
#'    A=list(
#'       A1=nameVector(1:3, letters[1:3]),
#'       A2=list(
#'          A1a=nameVector(4:7, letters[4:7]),
#'          A1b=nameVector(11:14, letters[11:14]))),
#'    B=list(B1=nameVector(1:9, letters[1:9]),
#'       B2=nameVector(20:25, letters[20:25])));
#' # default lengths(x) shows length=2 for A and B
#' lengths(x)
#' # rlengths(x) shows the total length of A and B
#' rlengths(x)
#'
#' @export
rlengths <- function
(x,
 doSum=NULL,
 ...)
{
   ## Purpose is to provide recursive lengths() for nested lists
   ##
   # x <- list(A=list(A1=nameVector(1:3, letters[1:3]), A2=nameVector(4:7, letters[4:7])),
   #    B=list(B1=nameVector(1:9, letters[1:9]), B2=nameVector(20:25, letters[20:25])))
   rl <- lapply(x, function(i){
      if (length(doSum) == 0) {
         doSum <- TRUE;
      }
      ## Note: the algorithm behaves unexpectedly with different exotic
      ## classes embedded inside a list
      if (!igrepHas("function", class(i)) &&
            (igrepHas("list", class(i)) ||
                  any(lengths(i) > 1))) {
         rlengths(i, doSum=doSum, ...);
      } else {
         length(i)
      }
   });
   if (length(doSum) > 0) {
      if (doSum) {
         rl <- do.call(sum, rl);
      }
   } else {
      rl <- unlist(rl);
   }
   return(rl);
}

#' Search for objects in the environment
#'
#' Search for objects in the environment
#'
#' This function searches the active R environment for an object name
#' using `vigrep()` (value, case-insensitive grep).
#' It is helpful when trying to find an object using a
#' substring, for example `grepls("statshits")`.
#'
#' @param x `character` string used as a grep pattern
#' @param where `character` string compatible with `base::ls()` or if
#'    installed, `AnnotationDbi::ls()`. A special value `"all"` will
#'    search all environments on the search path `base::search()`
#'    in order.
#' @param ignore.case `logical` indicating whether the pattern match
#'    is case-insensitive.
#' @param searchNames `logical` indicating whether names should also
#'    be searched, which is only relevant for `AnnDb` objects,
#'    for example `org.Mm.egSYMBOL2EG` from the `org.Mm.eg.db`
#'    Bioconductor package.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional parameters are ignored.
#'
#' @returns `character` vector of matching object names, or if
#'    `where="all"` it returns a named list
#'    whose names indicate the search environment name, and whose
#'    entries are matching object names within each environment.
#'
#' @family jam grep functions
#'
#' @examples
#' # Find all objects named "grep", which should find
#' # base grep() and jamba::vigrep() among other results.
#' grepls("grep");
#'
#' # Find objects in the local environment
#' allStatsHits <- c(1:12);
#' someStatsHits <- c(1:3);
#' grepls("statshits");
#' # shortcut way to search only the .GlobalEnv, the active local environment
#' grepls("statshits", 1);
#'
#' # return objects with "raw" in the name
#' grepls("raw");
#'
#' # Require "Raw" to be case-sensitive
#' grepls("Raw", ignore.case=FALSE)
#'
#' @export
grepls <- function
(x,
 where="all",
 ignore.case=TRUE,
 searchNames=TRUE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to search for an object name in a variety of places
   ##
   ## where can be a package, in format "package:jamba"
   ## where can be "all" which uses everything in the search path `search()`
   ##
   ## where can be an AnnDb environment, for example
   ## grepls("Actb", org.Mm.egSYMBOL2EG)
   ##
   ## searchNames=TRUE only affects AnnDB objects, which are converted
   ## to list first, then searched by value and by name.
   ## grepls("Actb", org.Mm.egSYMBOL2EG, searchNames=FALSE)
   ##
   if (igrepHas("character", class(where)) &&
         "all" %in% where) {
      if (verbose) {
         printDebug("grepls(): ",
            "Searching ",
            '"all"');
      }
      searchL <- lapply(nameVector(search()), function(i){
         ls(i);
      });
      # 0.0.93.900: improve grep() on list with regular expressions
      searchLuse <- searchL[sapply(searchL, function(i){
         igrepHas(x, i)
      })]
      # searchLuse <- searchL[igrep(x,
      #    searchL,
      #    ignore.case=ignore.case)];

      lapply(searchLuse, function(i){
         vigrep(x,
            i,
            ignore.case=ignore.case);
      });
   } else {
      if (verbose) {
         printDebug("grepls(): ",
            "class(where):",
            class(where));
      }
      if (searchNames && igrepHas("anndb", class(where))) {
         ## AnnDb objects are converted to lists, which lets us
         ## search the values and names together
         if (verbose) {
            printDebug("grepls(): ",
               "Converted AnnDb to list.")
         }
         lsVal <- as.list(where);
      } else {
         lsVal <- ls(where);
         #vigrep(x, lsVal);
      }
      c(lsVal[igrep(x,
         lsVal,
         ignore.case=ignore.case)],
         lsVal[igrep(x,
            names(lsVal),
            ignore.case=ignore.case)]);
   }
}

#' Return the newest file from a vector of files
#'
#' Return the newest file from a vector of files
#'
#' This function returns the newest file, defined by the most
#' recently modified time obtained from `base::file.info()`.
#'
#' @param x `character` vector of files, specifying file path where
#'    required.
#' @param timecol `character` value from the output of `base::file.info()`
#'    indicating the time column used to order files. By default `"mtime"`
#'    refers to the time the file was last modified.
#' @param n `integer` number of files to return, in order of the most
#'    recent to the least recent. By default `n=1` returns only the one
#'    newest file.
#' @param ... additional parameters are ignored.
#'
#' @family jam practical functions
#'
#' @examples
#' newestFile(list.files());
#'
#' @returns `character` vector `length=1` of the most recently modified file
#' from the input vector `x`. Note that any files not found are removed,
#' using `base::file.exists()`, which means invalid symlinks will be ignored.
#'
#' @export
newestFile <- function
(x,
 timecol="mtime",
 n=1,
 ...)
{
   ## This function takes a vector of one or more files and returns
   ## the one file most recently modified.
   ##
   ## Files not found are removed from the input.
   x <- x[file.exists(x)];
   if (length(x) <= 1) {
      return(x);
   }
   iFO <- file.info(x);
   iFOorder <- rev(order(iFO[[timecol]]));
   return(head(x[iFOorder], n));
}

#' Vectorized isFALSE
#'
#' Vectorized isFALSE
#'
#' This function applies three criteria to an input vector, to
#' determine if each entry in the vector is FALSE:
#'
#' 1. It must be class `logical`.
#' 2. It must not be `NA`.
#' 3. It must evaluate as `FALSE`.
#'
#' @param x vector
#' @param ... additional arguments are ignored
#'
#' @family jam practical functions
#'
#' @returns `logical` vector with length matching `x`.
#'
#' @examples
#' isFALSEV(c(TRUE, FALSE, NA, TRUE))
#'
#' @export
isFALSEV <- function
(x,
 ...)
{
   ## Purpose is to supply vectorized version of base::isFALSE()
   (is.logical(x) & !is.na(x) & !x)
}

#' Vectorized isTRUE
#'
#' Vectorized isTRUE
#'
#' This function applies three criteria to an input vector, to
#' determine if each entry in the vector is TRUE:
#'
#' 1. It must be class `logical`.
#' 2. It must not be `NA`.
#' 3. It must evaluate as `TRUE`.
#'
#' @param x vector
#' @param ... additional arguments are ignored
#'
#' @family jam practical functions
#'
#' @returns `logical` vector with length matching `x`.
#'
#' @examples
#' isTRUEV(c(TRUE, FALSE, NA, TRUE))
#'
#' @export
isTRUEV <- function
(x,
   ...)
{
   ## Purpose is to supply vectorized version of base::isFALSE()
   (is.logical(x) & !is.na(x) & x)
}

#' Paste data.frame rows into an ordered factor
#'
#' Paste data.frame rows into an ordered factor
#'
#' This function is an extension to `jamba::pasteByRow()` which
#' pastes rows from a `data.frame` into a character vector. This
#' function defines factor levels by running `jamba::mixedSortDF(unique(x))`
#' and calling `jamba::pasteByRow()` on the result. Therefore the
#' original order of the input `x` is maintained while the factor
#' levels are based upon the appropriate column-based sort.
#'
#' Note that the `...` additional arguments are
#' passed to `jamba::mixedSortDF()` to customize the column-based
#' sort order, used to define factor levels. A good way to test the
#' order of factors is to run `jamba::mixedSortDF(unique(x))` with
#' appropriate arguments, and confirm the rows are ordered as expected.
#'
#' Note also that `jamba::mixedSortDF()` uses `jamba::mixedSort()`
#' which itself performs alphanumeric sort in order to keep
#' values in proper numeric order where possible.
#'
#' @param x `data.frame`
#' @param sep `character` separator to use between columns
#' @param na.rm `logical` whether to remove NA values, or include them as "NA"
#' @param condenseBlanks `logical` whether to condense blank or empty values
#'    without including an extra delimiter between columns.
#' @param includeNames `logical` whether to include the colname delimited
#'    prior to the value, using sepName as the delimiter.
#' @param keepOrder `logical` indicating whether non-factor columns
#'    should order factor levels based upon the existing order of
#'    unique items. This option is intended for `data.frame` whose
#'    columns are already sorted in proper order, but where columns
#'    are not `factor` with appropriate factor levels. Note that
#'    even when `keepOrder=TRUE` all existing `factor` columns will
#'    honor the order of factor levels already present in those
#'    columns.
#' @param byCols `integer` or `character` passed to `mixedSortDF()`.
#'    This argument defines the order of columns sorted by `mixedSortDF()`,
#'    and does not affect the order of columns pasted. Columns are
#'    always pasted in the same order they appear in `x`. This argument
#'    `byCols` was previously passed via `...` but is added here
#'    to make this connection more direct.
#' @param na.last `logical` passed to `base::factor()` to determine whether
#'    `NA` values are first or last in factor level order.
#' @param ... additional arguments are passed to `jamba::pasteByRow()`,
#'    and to `jamba::mixedSortDF()`.
#'
#' @family jam string functions
#'
#' @returns `factor` vector whose levels are defined by existing
#'    factor levels, then by sorted values.
#'
#' @examples
#' f <- LETTERS;
#' df <- data.frame(A=f[rep(1:3, each=2)],
#'    B=c(NA, f[3]),
#'    C=c(NA, NA, f[2]))
#' df
#'
#' # note that output is consistent with mixedSortDF()
#' jamba::mixedSortDF(df)
#' jamba::pasteByRowOrdered(df)
#'
#' jamba::mixedSortDF(df, na.last=FALSE)
#' jamba::pasteByRowOrdered(df, na.last=FALSE)
#'
#' jamba::mixedSortDF(df, byCols=c(3, 2, 1))
#' jamba::pasteByRowOrdered(df, byCols=c(3, 2, 1))
#'
#' df1 <- data.frame(group=rep(c("Control", "ABC1"), each=6),
#'    time=rep(c("Hour2", "Hour10"), each=3),
#'    rep=paste0("Rep", 1:3))
#' # default will sort each column alphanumerically
#' pasteByRowOrdered(df1)
#'
#' # keepOrder=TRUE will honor existing order of character columns
#' pasteByRowOrdered(df1, keepOrder=TRUE)
#'
#' @export
pasteByRowOrdered <- function
(x,
 sep="_",
 na.rm=TRUE,
 condenseBlanks=TRUE,
 includeNames=FALSE,
 keepOrder=FALSE,
 byCols=seq_len(ncol(x)),
 na.last=TRUE,
 ...)
{
   ## Purpose is to enhance pasteByRow() except maintain ordering of factors
   ## where applicable, or define the output as a factor with ordered levels,
   ## using mixedSortDF() which does alphanumeric ordering while maintaining
   ## pre-existing factor ordering.
   xstr <- pasteByRow(x,
      sep=sep,
      na.rm=na.rm,
      condenseBlanks=condenseBlanks,
      includeNames=includeNames,
      ...);

   # optionally convert non-factor columns to factor with levels
   # based upon the order of existing unique values
   if (length(keepOrder) > 0 && any(keepOrder)) {
      reorder_cols <- grepl("character", ignore.case=TRUE, cPaste(sclass(x)));
      if (any(reorder_cols)) {
         for (i in which(reorder_cols)) {
            x[[i]] <- factor(x[[i]],
               levels=unique(x[[i]]));
         }
      }
   }

   # calculate level order by the same logic,
   # except sort columns with mixedSortDF()
   xlevels <- pasteByRow(
      mixedSortDF(unique(x),
         na.last=na.last,
         byCols=byCols,
         ...),
      sep=sep,
      na.rm=na.rm,
      condenseBlanks=condenseBlanks,
      includeNames=includeNames,
      ...);
   factor(xstr,
      levels=xlevels);
}

#' Merge list of data.frames retaining all rows
#'
#' Merge list of data.frames retaining all rows
#'
#' This function is a wrapper around `base::merge.data.frame()`
#' except that it allows more than two data.frame objects,
#' and applies default arguments `all.x=TRUE` and `all.y=TRUE`
#' for each merge operation to ensure that all rows are kept.
#'
#' @family jam list functions
#'
#' @returns `data.frame` after iterative calls to `base::merge.data.frame()`.
#'
#' @param ... arguments are handled as described:
#'    * named arguments are passed through to `base::merge.data.frame()`,
#'    with the exception of `all.x` and `all.y` which are both defined
#'    `all.x=TRUE` and `all.y=TRUE`.
#'    and all other arguments are assumed
#'    to be `data.frame` or equivalent, and are merged in order they
#'    appear as arguments. The order of these `data.frame` objects
#'    should not affect the output content, but will affect the row
#'    and column order of the resulting `data.frame`.
#'
#' @examples
#' df1 <- data.frame(City=c("New York", "Los Angeles", "San Francisco"),
#'    State=c("New York", "California", "California"))
#' df2 <- data.frame(Team=c("Yankees", "Mets", "Giants", "Dodgers"),
#'    City=c("New York", "New York", "San Francisco", "Los Angeles"))
#' df3 <- data.frame(State=c("New York", "California"),
#'    `State Population`=c(39.24e9, 8.468e9),
#'    check.names=FALSE)
#' mergeAllXY(df1, df3, df2)
#'
#' df4 <- data.frame(check.names=FALSE,
#'    CellLine=rep(c("ul3", "dH1A", "dH1B"), each=2),
#'    Treatment=c("Vehicle", "Dex"))
#' df4$CellLine <- factor(df4$CellLine,
#'    levels=c("ul3", "dH1A", "dH1B"))
#' df4$Treatment <- factor(df4$Treatment,
#'    levels=c("Vehicle", "Dex"))
#' df5 <- data.frame(
#'    Treatment=rep(c("Vehicle", "Dex"), each=3),
#'    Time=c("0h", "12h", "24h"))
#' df6 <- data.frame(check.names=FALSE,
#'    CellLine=c("ul3", "dH1A", "dH1B"),
#'    Type=c("Control", "KO", "KO"))
#' mergeAllXY(df4, df5, df6)
#'
#' # note the factor order is maintained
#' mergeAllXY(df4, df5, df6)$CellLine
#' mergeAllXY(df4, df5)$Treatment
#'
#' # merge "all" can append rows to a data.frame
#' df4b <- data.frame(check.names=FALSE,
#'    CellLine=rep("dH1C", 2),
#'    Treatment=c("Vehicle", "Dex"))
#' mergeAllXY(df4, df4b)
#'
#' # factor order is maintained, new levels are appended
#' mergeAllXY(df4, df4b)$CellLine
#'
#' # merge proceeds except shows missing data
#' mergeAllXY(df4, df4b, df5, df6)
#'
#' # note that appending rows is tricky, the following is incorrect
#' df6b <- data.frame(check.names=FALSE,
#'    CellLine="dH1C",
#'    Type="KO")
#' mergeAllXY(df4, df4b, df5, df6, df6b)
#'
#' # but it can be resolved by merging df6 and df6b
#' mergeAllXY(df4, df4b, df5, mergeAllXY(df6, df6b))
#'
#' # it may be easier to recognize by sorting with mixedSortDF()
#' mixedSortDF(honorFactor=TRUE,
#'    mergeAllXY(df4, df4b, df5, mergeAllXY(df6, df6b)))
#'
#' # again, factor order is maintained
#' mergeAllXY(df4, df4b, df5, sort=FALSE, mergeAllXY(df6, df6b))$CellLine
#'
#' # the result can be sorted properly
#' mixedSortDF(honorFactor=TRUE,
#'    mergeAllXY(df4, df4b, df5, mergeAllXY(df6, df6b)))
#'
#' @export
mergeAllXY <- function
(...)
{
   ## Purpose is a simple wrapper to merge(..., all.x=TRUE, all.y=TRUE);
   ##
   ## But detect whether 'x' and 'y' are defined, and if not, then define them
   inList <- list(...);

   ## name inList for any un-named entries
   if (is.null(names(inList))) {
      names(inList) <- makeNames(rep("x", length(inList)));
   } else {
      names(inList) <- makeNames(names(inList));
   }

   ## Filter out parameters for the merge.data.frame() function
   mergeArgs <- unvigrep("^(x|y|[.]{3})$",
      names(formals(base::merge.data.frame)));
   inListArgs <- inList[names(inList) %in% mergeArgs];
   inList <- inList[!names(inList) %in% mergeArgs];

   ## Now un-nest the list of entries remaining, so they're all non-list entities
   inList <- unnestList(inList);

   ## Filter for only data.frame or matrix like objects
   inListClass <- sapply(inList, function(i){
      class(i);
   });
   inList <- inList[igrep("data.*frame|matrix", inListClass)];
   inListClass <- sapply(inList, function(i){
      class(i);
   });
   ## Convert anything not a data.frame into such an object (ha!)
   if (any(!inListClass %in% c("data.frame"))) {
      whichNonDF <- which(!inListClass %in% c("data.frame"));
      inList[whichNonDF] <- lapply(inList[whichNonDF], function(i){
         newDF <- as.data.frame(i);
         rownames(newDF) <- rownames(i);
         colnames(newDF) <- colnames(i);
         newDF;
      })
   }

   ## Accept list of data.frames and run iterative merge on them
   ## Usually seen as one '...' element which is a list of data.frames
   if (length(inList) == 1) {
      return(inList[[1]]);
   } else if (length(inList) == 200) {
      ## If two list elements, we just call merge() once using x and y like normal
      x <- inList[[1]];
      y <- inList[[2]];

      if (length(inListArgs) > 0) {
         x <- do.call(merge,
            c(alist(x=x,
               y=y,
               all.x=TRUE,
               all.y=TRUE),
               inListArgs));
      } else {
         x <- merge(x,
            y,
            all.x=TRUE,
            all.y=TRUE);
      }
      return(x);
   } else if (length(inList) >= 2) {
      ## Run iterative merge() methods
      x <- inList[[1]];
      for (i in 2:length(inList)) {
         y <- inList[[i]];
         ## If we have merge.data.frame() arguments to pass,
         ## use do.call() so we can separately pass those function arguments
         if (length(inListArgs) > 0) {
            x <- do.call(merge,
               c(alist(x=x, y=y, all.x=TRUE, all.y=TRUE), inListArgs));
         } else {
            x <- merge(x, y, all.x=TRUE, all.y=TRUE);
         }
      }
      return(x);
   } else {
      stop("Could not match input to expected types, e.g. list of data.frames or matrices.");
   }
}

#' Un-nest a nested list into a simple list
#'
#' Un-nest a nested list into a simple list
#'
#' This function inspects a list, and unlists each entry
#' resulting in a simple list of non-list entries as a result.
#' Sometimes when concatenating lists together, one list gets
#' added as a list-of-lists. This function resolves that problem
#' by providing one flat list.
#'
#' @returns `list` that has been flattened so that it contains
#'    no `list` elements. Note that it may contain some list-like
#'    objects such as `data.frame`, defined by `stopClasses`.
#'
#' @family jam list functions
#'
#' @param x `list` potentially containing nested lists.
#' @param addNames `logical` indicating whether to add names to
#'    the list elements when names are not already present. When
#'    `addNames=TRUE` and no names are present `unnamedBase` is
#'    used to define names.
#' @param unnamedBase `character` value used as a base for naming any
#'    un-named lists, using the format `makeNamesFunc(rep(unnamedBase, n))`.
#' @param parentName `character` with optional prefix, used as parent name,
#'    default is NULL.
#' @param sep `character` delimiter used between nested list names.
#' @param makeNamesFunc `function` that takes a character vector and returns
#'    non-duplicated character vector of equal length. By default it
#'    uses `jamba::makeNames()`.
#' @param stopClasses `vector` of classes that should not be un-nested,
#'    useful in case some classes inherit list properties.
#' @param extraStopClasses `vector` of additional values for `stopClasses`,
#'    created mostly to show that `options("jam.stopClasses")` can be
#'    used to define `stopClasses`, for example when this function
#'    is called but where arguments cannot be conveniently passed
#'    through the calling function.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' L <- list(A=letters[1:10],
#'    B=list(C=LETTERS[3:9], D=letters[4:11]),
#'    E=list(F=list(G=LETTERS[3:9], D=letters[4:11])));
#' L;
#'
#' # inspect the data using str()
#' str(L);
#'
#' unnestList(L);
#'
#' # optionally change the delimiter
#' unnestList(L, sep="|");
#'
#' # example with nested lists of data.frame objects
#' df1 <- data.frame(a=1:2, b=letters[3:4]);
#' DFL <- list(A=df1,
#'    B=list(C=df1, D=df1),
#'    E=list(F=list(G=df1, D=df1)));
#' str(DFL);
#' unnestList(DFL);
#' str(unnestList(DFL));
#'
#' # packageVersion() returns class "package_version"
#' # where is.list(packageVersion("base")) is TRUE,
#' # but it cannot ever be subsetted as a list with x[[1]],
#' # and thus it breaks this function
#' identical(is.list(packageVersion("base")), is.list(packageVersion("base"))[[1]])
#' unnestList(lapply(nameVector(c("base","graphics")), packageVersion))
#'
#' @export
unnestList <- function
(x,
 addNames=FALSE,
 unnamedBase="x",
 parentName=NULL,
 sep=".",
   makeNamesFunc=makeNames,
 stopClasses=c("dendrogram",
    "data.frame",
    "matrix",
    "package_version",
    "tbl",
    "data.table"),
 extraStopClasses=getOption("jam.stopClasses"),
 ...)
{
   ## Purpose is to take a list of lists, and un-nest them
   ## into a list with depth=1, but all the non-list elements
   ## contained within it
   newList <- list();

   stopClasses <- unique(c(stopClasses, extraStopClasses));
   if (any(class(x) %in% stopClasses)) {
      return(x);
   }

   ## Create default names if they don't exist already
   if (addNames) {
      x_names <- names(x);
      if (length(x_names) == 0) {
         names(x) <- rep(unnamedBase, length(x));
      }
      emptyNamesX <- which(names(x) %in% c("", NA));
      if (any(emptyNamesX)) {
         names(x)[emptyNamesX] <- rep(unnamedBase, length(emptyNamesX));
      }
   }

   ## add prefix if it were specified
   if (length(parentName) > 0) {
      names(x) <- paste(parentName, names(x), sep=sep);
   }

   ## Make sure names are unique
   if (length(names(x)) > 0) {
      names(x) <- makeNamesFunc(names(x), ...);
   }

   ## Iterate each list until we hit a non-list entry
   if (inherits(x, "list") || is.list(x)) {
      jvals <- seq_along(x);
      for (j in jvals) {
         i <- x[[j]];
         ## If we reached a list, and if the class isn't something
         ## we want to allow (e.g. dendrogram) then unnest one layer deeper
         ##
         ## Note the use of tryCatch() which returns the original
         ## object upon error, which should catch infinite recursion.
         if (inherits(i, "list") || is.list(i) && !any(class(i) %in% stopClasses)) {
            if (1 == 2) {
               i <- unnestList(x=i,
                  addNames=addNames,
                  unnamedBase=unnamedBase,
                  stopClasses=stopClasses,
                  parentName=j,
                  sep=sep,
                  makeNamesFunc=makeNamesFunc,
                  ...);
            } else {
               i <- tryCatch({
                  unnestList(x=i,
                     addNames=addNames,
                     unnamedBase=unnamedBase,
                     stopClasses=stopClasses,
                     parentName=names(x[j]),
                     sep=sep,
                     makeNamesFunc=makeNamesFunc,
                     ...);
               }, error=function(e){
                  structure("error", class="try-error", condition=e);
               });
               if ("try-error" %in% class(i)) {
                  return(x);
                  i <- list(x[[j]]);
               }
            }
         } else {
            i <- list(i);
            if (length(names(x[j])) > 0 && !names(x[j]) %in% c("", NA)) {
               names(i) <- names(x[j]);
            }
         }
         newList <- c(newList, i);
      };
   } else {
      newList <- x;
   }
   newList;
}

#' log2 transformation with directionality
#'
#' log2 transformation with directionality
#'
#' This function applies a log2 transformation but maintains
#' the sign of the input data, allowing for log2 transformation
#' of negative values.
#'
#' The method applies an offset to the absolute value `abs(x)`,
#' in order to handle values between zero and 1, then applies
#' log2 transformation, then multiplies by the original sign
#' from `sign(x)`.
#'
#' The argument `offset` is used to adjust values, for example
#' `offset=1` will apply log2 transformation `log2(1 + x)`,
#' except using the absolute value of `x`. This method allows
#' for positive and negative input data to contain values
#' between 0 and 1, and between -1 and 0.
#'
#' This function could be described as applying
#' a log2 transformation of the "magnitude" of values in `x`,
#' while maintaining the positive or negative directionality.
#'
#' If any `abs(x)` are less than `offset` this function will
#' raise an error.
#'
#' @returns numeric vector of log-transformed magnitudes.
#'
#' @param x `numeric` vector
#' @param offset `numeric` value added to the absolute values
#'    of `x` prior to applying the log transformation.
#' @param base `numeric` value indicating the logarithmic base,
#'    by default `2` in order to apply `base::log2()`.
#' @param ... additional arguments are ignored.
#'
#' @family jam practical functions
#'
#' @examples
#' x <- c(-100:100)/10;
#' log2signed(x);
#' plot(x=x, y=log2signed(x), xlab="x", ylab="log2signed(x)")
#'
#' @export
log2signed <- function
(x,
 offset=1,
 base=2,
 ...)
{
   ## Purpose is to transform numeric data using log2 transformation
   ## but where negative values are kept negative by log2-transforming
   ## the absolute value, then multiplying by the original sign.
   if (length(x) == 0) {
      return(x);
   }
   if (offset < 1 && any(abs(x) < 1)) {
      stop(
         paste0(
            "Values in abs(x) less than offset ",
            offset,
            " cannot be transformed without losing direction.")
      );
   }

   ## Determine the sign(x)
   x_sign <- sign(x);
   ## For now, do not convert sign 0 to sign 1.
   #x_sign <- ifelse(x_sign == 0, 1, x_sign);

   if (length(base) == 0 || all(unique(base) == 2)) {
      return(log2(abs(x) + offset) * x_sign);
   }
   # Note: the conversion to different log base
   #log2(abs(x) + offset) * x_sign  / log2(base);
   log(abs(x) + offset, base=base) * x_sign;
}

#' exponentiate log2 values with directionality
#'
#' exponentiate log2 values with directionality
#'
#' This function is the reciprocal to `log2signed()`.
#'
#' It #' exponentiates the absolute values of `x`,
#' then subtracts the `offset`, then multiplies results
#' by the `sign(x)`.
#'
#' The `offset` is typically used to maintain
#' directionality of values during log transformation by
#' requiring all absolute values to be `1` or larger, thus
#' by default `offset=1`.
#'
#' @returns numeric vector of exponentiated values.
#'
#' @param x `numeric` vector
#' @param offset `numeric` subtracted from exponentiated values
#'    prior to multiplying by the `sign(x)`.
#' @param base `numeric` value indicating the logarithmic base used.
#'    For example `base=2` indicates values were transformed using
#'    `log2()`.
#' @param ... additional arguments are ignored.
#'
#' @family jam practical functions
#'
#' @examples
#' x <- c(-100:100)/10;
#' z <- log2signed(x);
#' #plot(x=x, y=z, xlab="x", ylab="log2signed(x)")
#' plot(x=x, y=exp2signed(z), xlab="x", ylab="exp2signed(log2signed(x))")
#' plot(x=z, y=exp2signed(z), xlab="log2signed(x)", ylab="exp2signed(log2signed(x))")
#'
#' @export
exp2signed <- function
(x,
 offset=1,
 base=2,
 ...)
{
   ## Purpose is to apply the appropriate reciprocal to log2signed()
   ## Determine the sign(x)
   x_sign <- sign(x);

   ## Exponentiate
   if (length(base) == 0 || all(unique(base) == 2)) {
      (2^abs(x) - offset) * x_sign;
   } else {
      # Note: the equivalent of the reciprocal of converting log base
      # (2^(abs(x) * log2(base)) - offset) * x_sign;
      # or in terms of e:
      # (exp(abs(x) + log(base)) - offset) * x_sign;
      (base^abs(x) - offset) * x_sign;
   }
}

#' Apply head() across each element in a list of vectors
#'
#' Apply head() across each element in a list of vectors
#'
#' Note that this function currently only operates on a list
#' of vectors. This function is notably faster than
#' `lapply(x, head, n)` because it operates on the entire
#' vector in one step.
#'
#' Also the input `n` can be a vector so that each element in
#' the list has a specific number of items returned.
#'
#' @returns `list` with at most `n` elements per vector.
#'
#' @family jam list functions
#'
#' @param x `list` of atomic vectors, assumed to be the same
#'    atomic type.
#' @param n `integer` maximum number of items to include from
#'    each element in the list `x`. When `n` contains multiple
#'    values, they are recycled to `length(x)` and applied to each
#'    list element in order.
#' @param ... additional arguments are passed to `utils::head()`.
#'
#' @examples
#' l <- list(a=1:10, b=2:5, c=NULL, d=1:100);
#' heads(l, 1);
#'
#' heads(l, 2);
#'
#' heads(l, n=c(2, 1, 3, 5))
#'
#' @export
heads <- function
(x,
 n=6,
 ...)
{
   if (!is.list(x)) {
      stop("Input must be a list.");
   }
   if (length(x) == 0) {
      return(x)
   }
   if (length(x) == 1) {
      x[[1]] <- head(x[[1]], n=head(n, 1), ...);
      return(x);
   }
   if (!is.atomic(x[[1]]) || !is.atomic(x[[(length(x))]])) {
      stop("Input must be a list of atomic vectors.");
   }
   if (length(names(x)) == 0) {
      xnames <- seq_along(x)
   } else {
      xnames <- factor(names(x), levels=names(x));
   }
   xidx <- rep(xnames, lengths(x));
   xlen <- unlist(unname(lapply(split(xidx, xidx), seq_along)));

   # Optionally expand n to be applied to each list element in order
   if (length(n) > 1 && length(unique(n)) > 1) {
      n <- rep(n, length.out=length(x))
      n <- rep(n, lengths(x));
   }

   xkeep <- (xlen <= n);
   xfull <- unlist(unname(x));
   xnew <- split(xfull[xkeep], xidx[xkeep]);
   if (length(names(x)) == 0) {
      names(xnew) <- NULL;
   }
   xnew
}

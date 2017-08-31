#'
#' jamba: Jam Base Methods
#'
#' The jamba package contains several jam base functions
#' which are re-usable for routine R analysis work, and are
#' important dependencies for other Jam R packages.
#'
#' The goal will be to
#' maintain these methods as lightweight as possible, so
#' their inclusion in an analysis workflow will not incur
#' a noticeable burden.
#'
#' @section miscellaneous helper functions:
#'    fileInfo,
#'    padInteger,
#'    padString,
#'    dateToDaysOld,
#'    asDate,
#'    getDate,
#'    setPrompt,
#'    pasteByRow,
#'    breaksByVector,
#'    printDebug,
#'    rbindList,
#'    colNum2excelName,
#'    decideMfrow,
#'    getPlotAspect,
#'    tcount,
#'    make_styles,
#'    noiseFloor
#'
#' @section plot functions:
#'    plotSmoothScatter,
#'    smoothScatterJam,
#'    imageDefault,
#'    nullPlot,
#'    usrBox,
#'    imageDefault,
#'    imageByColors
#'
#' @section string functions:
#'    igrepHas,
#'    vigrep,
#'    vgrep,
#'    igrep,
#'    unigrep,
#'    unvigrep,
#'    provigrep,
#'    makeNames,
#'    nameVector,
#'    nameVectorN,
#'    rmNULL,
#'    rmNA,
#'    rmInfinite
#'
#' @section color functions:
#'    setTextContrastColor,
#'    col2hcl,
#'    col2alpha,
#'    alpha2col,
#'    hsv2col,
#'    rgb2col,
#'    makeColorDarker,
#'    getColorRamp,
#'    isColor
#'
#' @docType package
#' @name jamba
NULL

#' file information in data.frame format
#'
#' file information in data.frame format
#'
#' This function is a minor extension to \code{\link{file.info}} in that it
#' adds the filename as a proper colname, and "size" which contains a text
#' file size.
#'
#' @return data.frame with file information, including "filename" and "size"
#' as additional colnames as compared to \code{\link{file.info}} output.
#'
#' @param fileList character vector with one or more file paths.
#'
#' @export
fileInfo <- function
(fileList, #doColsHead=TRUE,
 ...)
{
   ## Purpose is to wrapper file.info() so it returns a pretty tabular summary for one or more files
   ## One cool pattern to follow is to list files contained within a package:
   ## colsHead(file.info(list.files(path=find.package("mirna20cdf"), full.names=TRUE)))
   fileList <- path.expand(fileList);
   fi1 <- file.info(fileList, ...);
   fi1[,"size"] <- asSize(fi1[,"size"]);
   fi1 <- data.frame(check.names=FALSE, stringsAsFactors=FALSE,
      "filename"=rownames(fi1), fi1);
   hiCols <- igrep("filename|size", colnames(fi1));
   maxFileWidth <- max(nchar(fi1[,1]));

   ## Left-justify the text by right-padding with spaces before calling colsHead()
   ## making it easier to read
   fi1[,1] <- as.character(sapply(fi1[,1], function(i){
      paste(c(i, rep(" ", maxFileWidth-nchar(i))), collapse="");
   }));
   fi1[,1] <- padString(fi1[,1], justify="right");
   #if (doColsHead) {
   #   colsHead(fi1, rows=10, maxRows=100, hiCols=hiCols);
   #}
   return(fi1);
}

#' prefix integers with leading zeros
#'
#' prefix integers with leading zeros
#'
#' The purpose of this function is to pad integer numbers so they contain
#' a consistent number of digits, which is helpful when sorting values
#' as character strings.
#'
#' @return character vector of length(x).
#'
#' @param x input integer, numeric, or character vector. In reality, only
#'    nchar(x) is used to determine padding.
#' @param padCharacter character with nchar(padCharacter)==1, used to pad
#'    each digit as a prefix.
#' @param useNchar NULL or integer number of digits used, or if the maximum
#'    nchar(x) is higher, that number of digits is used. Note useNchar is
#'    mostly useful when all numbers are less than 10, but the desired output
#'    is to have a fixed number of digits 2 or higher.
#' @param ... additional parameters are ignored.
#'
#' @export
padInteger <- function
(x, padCharacter="0", useNchar=NULL,
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
#' @return character vector of length(x)
#'
#' @param x character vector
#' @param stringLength integer length for the resulting character strings
#'    in \code{x}. By default, all strings are padded to the length of the
#'    longest entry, however stringLength can be defined to impose strict
#'    number of characters for all entries.
#' @param padCharacter single character used for padding.
#' @param justify character value with "left", "right", "center" to indicate
#'    alignment of the resulting text string.
#' @param ... additional parameters are ignored.
#'
#' @examples
#' padString(c("one","two","three"));
#' padString(c("one","two","three","four"), padCharacter="_", justify="center");
#'
#' @export
padString <- function
(x, stringLength=max(nchar(x)), padCharacter=" ",
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
#' @param testDate character date recognized by \code{\link{asDate}},
#'    representing the test date.
#' @param nowDate character date recognized by \code{\link{asDate}},
#'    representing the reference date, by default the current day.
#' @param units character indicating the units, as used by
#'    \code{\link{difftime}}.
#' @param ... additional parameters are ignored.
#'
#' @examples
#' dateToDaysOld("23aug2007")
#'
#' @export
dateToDaysOld <- function
(testDate, nowDate=Sys.Date(), units="days",
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
#' allow date-related math operations, for example \code{\link{difftime}}.
#'
#' @return Date object
#' @param getDateValues character date, in format recognized by dateFormat
#' @param dateFormat character string representing the recognized date format,
#'    by default DDmmmYYYY, for example 23aug2007.
#' @param ... additional parameters are ignored.
#'
#' @examples
#' asDate(getDate());
#'
#' @export
asDate <- function
(getDateValues, dateFormat="%d%b%Y",
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
#' get simple date string
#'
#' Gets the current date in a simplified text string. Use
#' \code{\link{asDate}} to convert back to Date object.
#'
#' @return character vector with simplified date string
#'
#' @param t current time, by default the output of \code{\link{Sys.time}}.
#' @param trim logical whether to trim the output of \code{\link{format}} in
#'    the event that multiple values are sent for \code{t}.
#' @param ... additional parameters sent to \code{\link{format}}
#'
#' @examples
#' getDate();
#'
#' @export
getDate <- function(t=Sys.time(),trim=TRUE,...)
{
   ## Purpose is to define a data in the format
   ## 05may2011 (DDmmmYYYY)
   tolower(format(t, "%d%b%Y",trim=trim,...));
}

#' set R prompt with project name and R version
#'
#' set R prompt with project name and R version
#'
#' This function sets the R prompt including project name, the R
#' version, and the process ID. It is intended to be useful by
#' reinforcing the active project for an R session, particularly when
#' there may be multiple R sessions active. The R version can be useful
#' when running R on different machines, to reinforce which version of
#' R is active on the given machine. The process ID is mainly helpful in
#' the event an R process spins out of control, and it would be useful
#' to know definitively which exact process ID is stuck, so that it can
#' be killed without affecting other R sessions inadvertently.
#'
#' The prompt is defined in \code{options("prompt")}.
#'
#' Note that in some cases, the color encoding of the prompt interferes
#' with word wrapping, the symptom is that when typing text into the R console
#' a long line will begin to word wrap prematurely, before the text reaches
#' the edge of the screen. There are two frequent causes of this issue:
#'
#' \describe{
#'    \item{options("width")}{is sometimes defined too narrow for the
#'       screen, which can happen when resizing the console, or when
#'       accessing an R session via GNU screen, or tmux, and the environment
#'       variable has not been propagated to the terminal window. Usually
#'       this issue is resolved by defining \code{options("width")} manually,
#'       or by simply resizing the terminal window, which may trigger the
#'       appropriate environment variable updates.}
#'    \item{The locale}{can sometimes be mismatched with the terminal window,
#'       usually caused by some terminal emulation layer which is not
#'       properly detecting the compatibility of the server. It may happen
#'       for example, when using PuTTY on Windows, or when using GNU screen or
#'       tmux on linux or Mac OSX. To troubleshoot, check
#'       \code{Sys.env("LC_ALL")} which may be "C" or another locale such as
#'       "en_US.UTF-8". Note that switching locale may have the effect of
#'       correcting the word wrap, but may adversely affect display of
#'       non-standard unicode characters.}
#' }
#'
#' In any event, R uses readline for unix-like systems by default, and
#' issues related to using color prompt are handled at that level. For example,
#' in some Mac OSX consoles, there are alternate color escape sequences which
#' are used to tell readline to ignore an escape sequence when it counts the
#' number of characters being displayed by the prompt.
#'
#' @return invisible character string representing the prompt used.
#' @param projectName character string representing the active project.
#' @param useColor logical whether to define a color prompt if the
#'    \code{\link{crayon}} package is installed.
#' @param projectColor,bracketColor colors used when useColor==TRUE and'
#'    the \code{\link{crayon}} package is installed.
#' @param usePid logical whether to include the process ID in the prompt.
#' @param resetPrompt logical whether to revert all changes to the prompt
#'    back to the default R prompt, that is, no color and no projectName.
#' @param verbose logical whether to print verbose output
#' @param ... additional parameters are ignored.
#'
#' @examples
#' setPrompt("jamba", projectColor="purple");
#' setPrompt("jamba", usePid=FALSE);
#'
#' @export
setPrompt <- function
(projectName=get("projectName", envir=.GlobalEnv),
 useColor=TRUE,
 projectColor="yellow",
 bracketColor=c("white", "cyan"),
 usePid=TRUE,
 resetPrompt=FALSE,
 verbose=FALSE,
 ...)
{
   ## Set the R command prompt to display the current R project name
   ##
   ## usePid will include the parent process PID in the prompt, which
   ## can be helpful when an R session hangs, but you have multiple active
   ## R sessions, and might otherwise not be able to tell which R session
   ## is problematic.
   if (useColor %in% c(TRUE, 1) &&
       suppressPackageStartupMessages(require(crayon))) {
      useColor <- 1;
   } else {
      useColor <- 0;
   }
   promptValue <- "> ";
   bracketColor <- head(bracketColor, 1);
   if (verbose) {
      printDebug("useColor:", useColor);
   }
   if (resetPrompt) {
      if (verbose) {
         printDebug("Resetting basic prompt for R.");
      }
      options("prompt"="> ");
   } else if (useColor == 1) {
      ## use crayon
      if (!usePid) {
         promptValue <- paste(make_styles(style=c(head(bracketColor,1),
            projectColor, head(bracketColor,1),
            "white", NA, NA, "white"),
               c("{", projectName, "}",
                  "-R", "-",
                  paste0(R.version[c("major", "minor")], collapse="."),
                  "> ")),
            collapse="");
      } else {
         promptValue <- paste(make_styles(style=c(head(bracketColor,1),
            projectColor, head(bracketColor,1),
            "white", NA, NA, NA, NA, "white"),
               c("{", projectName, "}",
                  "-R", "-",
                  paste0(R.version[c("major", "minor")], collapse="."),
                  "_", Sys.getpid(),
                  "> ")),
            collapse="");
      }
      if (verbose) {
         cat("promptValue: '", promptValue, "'\n\n");
      }
      options("prompt"=promptValue);
   } else {
      if (verbose) {
         printDebug("Setting non-colorized prompt for R.");
      }
      if (!usePid) {
         promptValue <- paste0("{", projectName, "}",
            "-R-", paste(R.version[c("major", "minor")], collapse="."),
            "> ");
      } else {
         promptValue <- paste0("{", projectName, "}",
            "-R-", paste(R.version[c("major", "minor")], collapse="."),
            "_", Sys.getpid(),
            "> ");
      }
      options("prompt"=promptValue);
   }
   invisible(promptValue);
}

#' convert numeric value to size
#'
#' convert numeric value to size 
#'
#' This function converts numeric input to computer size in bytes, using
#' the most appropriate human-friendly units as possible. For example, it
#' will use kilobytes (kb), megabytes (Mb), gigabytes (Gb), etc. as
#' necessary.
#'
#' This function by default assumes 1,024 bytes per kilobyte, but can be
#' made to use 1000, for example in order to represent genome base size,
#' which is represented in kilobases (kb), megabases (Mb), gigabases (Gb),
#' etc. Simply define unitType="bases" in this scenario.
#'
#' @return character vector representing human-friendly sizes.
#'
#' @param x numeric vector
#' @param humanFriendly logical, currently only TRUE is accepted, whether to
#'    include human-friendly units to the output.
#' @param digits integer number of digits used by \code{\link{format}} when
#'    formatting the number to create a character string
#' @param abbreviateUnits logical whether to print abbreviated units, for
#'    example using k, M, G, T, P instead of kilo, mega, Giga, Tera, Peta,
#'    respectively.
#' @param unitType character string indicating the base unit of measure,
#'    by default "bytes". Note that trailing "s" is removed when the number
#'    is singular.
#' @param unitAbbrev character string indicating an abbreviated base unit,
#'    by default it uses the first character from \code{unitType}.
#' @param kiloSize numeric number of base units when converting from one
#'    base unit, to one "kilo" base unit. For file sizes, this value is 1024,
#'    but for other purposes this value may be 1000.
#' @param ... other parameters passed to \code{\link{format}}.
#'
#' @examples
#' asSize(c(1, 10,2010,22000,52200))
#' #> "1 byte"   "10 bytes" "2 kb"     "21 kb"    "51 kb"
#'
#' @export
asSize <- function
(x, humanFriendly=TRUE, digits=3,
 abbreviateUnits=TRUE,
 unitType="bytes", unitAbbrev=gsub("^(.).*$", "\\1", unitType),
 kiloSize=1024,
 ...)
{
   ## Prints a numerical value as if it were a computer object size
   ##
   ## unitType is typically "byte" but can be any relevant unit, e.g.
   ## unitType="" and kiloSize=1000 will convert to user-friendly
   ## numerical values, so 4500 becomes 4.5k
   if (is.null(unitType)) {
      unitType <- "";
   }
   if (is.null(unitAbbrev)) {
      unitAbbrev <- gsub("^(.).*$", "\\1", unitType);
   }

   if (abbreviateUnits) {
      sizeUnits <- c(unitType, paste0(c("k", "M", "G", "T", "P"),
         unitAbbrev));
      sizeUnitsX <- nameVector(c(0, 1, 2, 3, 4, 5), sizeUnits);
   } else {
      sizeUnits <- paste0(c("", "kilo", "Mega", "Giga", "Tera", "Peta"),
         unitType);
   }
   if (class(x) %in% c("object_size")) {
      x <- as.integer(x);
   }
   xUnits <- rep("", length(x));
   xValues <- x;
   ## Iterate through large to small values, progressively dividing out
   ## orders of magnitude until the result fits within the range available
   for (i in names(rev(sizeUnitsX))) {
      whichX <- (!is.na(x) &
                 xUnits %in% "" &
                 x >= kiloSize^sizeUnitsX[i]);
      xUnits[whichX] <- i;
      xValues[whichX] <- x[whichX] / kiloSize^sizeUnitsX[i];
   }
   ## If we have zeros, and we have a unit defined for zero, we use that
   ## to describe the zeros, e.g. "0 bytes"
   if (any(sizeUnitsX == 0) && any(!is.na(xValues) & xValues == 0)) {
      xUnits[!is.na(xValues) & xValues == 0] <- names(which(sizeUnitsX == 0));
   }
   ## If we have values==1, remove trailing 's', a common pet peeve of mine
   xOnes <- which(!is.na(xValues) & xValues == 1);
   if (length(xOnes) > 0) {
      xUnits[xOnes] <- gsub("([^s])s$", "\\1", xUnits[xOnes]);
   }

   newX <- paste(format(trim=TRUE, digits=2, xValues, ...), xUnits);
   newX[is.na(x)] <- NA;

   return(newX);
}

#' Paste data.frame rows into character vector
#'
#' Paste data.frame rows into a character vector, optionally removing
#' empty fields in order to avoid delimiters being duplicated.
#'
#' This function is intended to paste data.frame (or matrix, or tibble) values
#' for each row of data. It differs from using apply(x, 2, paste) in that it
#' handles factors without converting to integer factor level numbers. It
#' also by default removes blank or empty fields, preventing the delimiter
#' from being included multiple times, per the condenseBlanks parameter.
#' Lastly, it is notably faster than apply, by means of running paste() on
#' each column of data, making the output vectorized, and scaling rather
#' well for large data.frames.
#'
#' The output can also include name:value pairs, which can make the output
#' data more self-describing in some circumstances. That said, the most basic
#' usefulness of this function is to create row labels.
#'
#' @return character vector of length \code{nrow(x)}.
#'
#' @param x data.frame, matrix, or tibble
#' @param sep character separator to use between columns
#' @param na.rm logical whether to remove NA values, or include them as "NA"
#' @param condenseBlanks logical whether to condense blank or empty values
#'    without including an extra delimiter between columns.
#' @param includeNames logical whether to include the colname delimited
#'    prior to the value, using sepName as the delimiter.
#' @param sepName character, only relevant when includeNames=TRUE, this value
#'    becomes the delimiter.
#' @param blankGrep character grep string used to recognize blank entries,
#'    by default any field containing no text, or only spaces, is considered
#'    a blank entry.
#' @param verbose logical whether to print verbose output.
#'
#' @examples
#' a1 <- c("red","blue")[c(1,1,2)];
#' b1 <- c("yellow","orange")[c(1,2,2)];
#' d1 <- c("purple","green")[c(1,2,2)];
#' df2 <- data.frame(a=a1, b=b1, d=d1);
#' pasteByRow(df2);
#'
#' @export
pasteByRow <- function
(x, sep="_", na.rm=TRUE, condenseBlanks=TRUE,
 includeNames=FALSE, sepName=":",
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
            isBlank <- (is.na(xVals1) | grepl("^[ ]*$", xVals1));
            sepV <- ifelse(isBlank, "", sep);
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
#' \link{\code{imageDefault}} color image map of sample groupings, where
#' it may be ideal to label only unique elements in a contiguous set.
#'
#' @return
#' list
#'    \describe{
#'       \item{"breakPoints"}{The mid-point coordinate between each break.
#'          These midpoints would be good for drawing dividing lines for
#'          example.}
#'       \item{"labelPoints"}{The ideal point to place a label to represent
#'          the group.}
#'       \item{"newLabels"}{A vector of labels the same length as the input
#'          data, except using blank values except where a label should
#'          be drawn. This output is good for text display.}
#'       \item{"useLabels"}{The unique set of labels, without blanks,
#'          corresponding to the coordinates supplied by labelPoints.}
#'    }
#'
#' @param x vector of labels
#' @param labels character vector of custom labels to represent the items
#'    in x
#' @param returnFractions logical whether to return fractional coordinates
#'    for labels that should be positioned between two labels
#' @param ... additional parameters are ignored.
#'
#' @examples
#' b <- rep(LETTERS[1:5], c(2,3,5,4));
#' bb <- breaksByVector(b);
#' data.frame(b, bb$newLabels);
#'
#' @export
breaksByVector <- function
(x, labels=NULL, returnFractions=FALSE,
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
   if (any(class(x) %in% c("factor", "ordered"))) {
      x <- nameVector(as.character(x), names(x));
   }
   xRle <- rle(x);

   ## Commented out syntax used by Bioconductor Rle
   #breakPoints <- cumsum(runLength(xRle));
   breakPoints <- cumsum(xRle$lengths);

   labelPoints <- 0.5 + (c(0, head(breakPoints, -1)) + breakPoints) / 2;
   if (!returnFractions) {
      labelPoints <- trunc(labelPoints);
   }
   useLabels <- xRle$values;
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
   list(breakPoints=breakPoints,
        labelPoints=labelPoints,
        newLabels=newLabels,
        useLabels=useLabels);
}

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
#'    \link{\code{format}} which controls the number of digits displayed.
#' @param trim,digits,nsmall,justify,big.mark,small.mark,zero.print,width
#'    parameters sent to the \link{\code{format}} function.
#' @param doColor NULL or logical indicating whether to colorize output. If
#'    NULL it detects whether the crayon package is available and console
#'    color is enabled.
#' @param splitComments logical whether to color each element independently
#'    without light-dark alternating pattern.
#' @param collapse character collapse string used to separate list items,
#'    by default "" so text separation is expected in the input data.
#' @param sep character separator used to separate vector elements, when
#'    a list items contains a vector.
#' @param detectColors logical whether to detect and potentially try to
#'    correct console color capabilities.
#' @param darkFactor numeric darkness to apply to alternative vector values
#'    when using alternating light-dark color shading.
#' @param sFactor numeric color saturation to apply to alternative vector
#'    values when using alternating light-dark color shading.
#' @param removeNA logical whether to remove NA values and not print to
#'    the console.
#' @param replaceNULL character or NULL, optionally replace NULL elements
#'    with non-NULL character value.
#' @param adjustRgb logical sent to \code{\link{make_styles}}, whether to
#'    adjust RGB to prevent loss of detail with low color brightness.
#' @param byLine logical whether to delimit lists by line instead of
#'    using collapse to combine them onto one line.
#' @param verbose logical whether to print verbose output
#' @param indent character optional characters used as a prefix to indent
#'    output.
#'
#' @examples
#' printDebug("Testing ", "default ", "printDebug().");
#' printDebug("List of vectors:", c("one", "two", "three"));
#' printDebug("List of vectors:", c("one", "two", "three"),
#'    c("four", "five", "six"), collapse=" ");
#'
#' # slightly different style, one entry per line, indented:
#' printDebug("List of vectors:", c("one", "two", "three"),
#'    c("four", "five", "six"), collapse="\n   ");
#'
#' @export
printDebug <- function
(..., fgText=NULL,#c("orange", "lightblue"),
 bgText=NULL, fgTime="cyan", timeStamp=TRUE, comment=TRUE,
 formatNumbers=TRUE, trim=TRUE, digits=NULL, nsmall=0L, justify="left",
 big.mark="", small.mark="", zero.print=NULL, width=NULL,
 doColor=NULL, splitComments=FALSE, collapse="", sep=",", detectColors=TRUE,
 darkFactor=c(1,1.5),#-1.4,1.2),
 sFactor=c(1,1.5),#c(-1.3,1.2),
 removeNA=FALSE, replaceNULL=NULL,
 adjustRgb=-0.1,
 byLine=FALSE, verbose=FALSE, indent="", keepNA=TRUE,
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

   ## Deal with the type of coloration we can use
   ## by setting up a conditional array to capture various combinations
   ## of 3 options:
   ## - availability of the package crayon
   ## - availability of the package xterm256
   ## - the given preference doColor value (1=xterm256, 2=crayon, 0=no color, everything else is pickem)
   hasCrayon <- as.character(suppressPackageStartupMessages(require(crayon)));
   if (is.null(doColor)) {
      if (hasCrayon %in% "TRUE") {
         doColor <- 2;
      } else {
         doColor <- 0;
      }
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
   if (is.null(fgText)) {
      fgTextBase <- tail(rmNULL(xList), 1);
      #print("fgTextBase:");
      #print(fgTextBase);
      if (igrepHas("list", class(fgTextBase[[1]]))) {
         fgTextBase <- unlist(fgTextBase, recursive=FALSE);
      }
      fgTextBaseIsColor <- sapply(fgTextBase, function(i){
         all(isColor(i));
      });
      if (fgTextBaseIsColor) {
         fgText <- fgTextBase;
         #if (igrepHas("list", class(fgText[[1]]))) {
            fgText <- unlist(fgText, recursive=FALSE);
         #}
         xList <- head(rmNULL(xList), -1);
      } else {
         fgText <- c("lightgoldenrod1", "lightblue");
      }
   }

   if (length(xList) == 0) {
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
      printDebug(xList[[1]], fgText=fgText,
         verbose=verbose, indent=paste0(indent, "   "),
         bgText=bgText, fgTime=fgTime, timeStamp=timeStamp, comment=comment,
         formatNumbers=formatNumbers, trim=trim, digits=digits, nsmall=nsmall,
         justify=justify, big.mark=big.mark, small.mark=small.mark,
         zero.print=zero.print, width=width, doColor=doColor,
         splitComments=splitComments, collapse=collapse, sep=sep,
         detectColors=detectColors, darkFactor=darkFactor, sFactor=sFactor,
         removeNA=removeNA, replaceNULL=replaceNULL, adjustRgb=adjustRgb,
         byLine=byLine);
      invisible(NULL);
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
      ## Optionally replace NULL with "NULL"
      xList <- rmNULL(xList, replaceNULL="NULL");

      ## Extend fgText and bgText to the length of xList
      if (!igrepHas("list", class(fgText))) {
         fgText <- as.list(fgText);
      }
      if (length(fgText) > 0) {
         fgText <- rep(fgText, length.out=length(xList));
      }
      if (length(bgText) > 0) {
         bgText <- rep(bgText, length.out=length(xList));
      }

      xListSlength <- lengths(xList);
      if (any(xListSlength >= 1)) {
         xListMulti <- which(xListSlength > 1);
         fgText <- lapply(seq_along(fgText), function(i){
            iColor <- fgText[[i]];
            if (length(iColor) == 1) {
               ## If the color is dark, make the off-color lighter,
               ## if the color is bright, make the off-color darker
               if (col2hcl(iColor)["L",] < 70) {
                  useDarkFactor <- darkFactor * -1;
               } else {
                  useDarkFactor <- darkFactor;
               }
               iColor <- rep(makeColorDarker(darkFactor=useDarkFactor,
                     sFactor=c(sFactor), iColor, keepNA=keepNA),
                  length.out=xListSlength[i]);
            } else {
               iColor <- rep(makeColorDarker(darkFactor=head(darkFactor,1),
                     sFactor=head(sFactor,1), iColor, keepNA=keepNA),
                  length.out=xListSlength[i]);
            }
            iColor;
         });
         if (length(bgText) >- 0) {
            bgText <- lapply(seq_along(bgText), function(i){
               iColor <- bgText[[i]];
               if (length(iColor) == 1) {
                  iColor <- rep(makeColorDarker(darkFactor=c(darkFactor),
                        sFactor=c(sFactor), iColor, keepNA=keepNA),
                     length.out=xListSlength[i]);
               } else {
                  iColor <- rep(makeColorDarker(darkFactor=head(darkFactor,1),
                        sFactor=head(sFactor,1), iColor, keepNA=keepNA),
                     length.out=xListSlength[i]);
               }
               iColor;
            });
         }
      }
      fgText <- unlist(fgText);
      bgText <- unlist(bgText);

      x <- unlist(lapply(xList, function(i){
         if (formatNumbers &&
            igrepHas("numeric|float|integer|long|double", class(i))) {
            i <- format(i, trim=trim, digits=digits, nsmall=nsmall,
               justify=justify, big.mark=big.mark, small.mark=small.mark,
               zero.print=zero.print, width=width);
         }
         i <- as.character(i);
         if (length(i) > 1) {
            if (nchar(sep) > 0) {
               i[-length(i)] <- gsub(paste0("[", sep, "]*$"), sep,
                  i[-length(i)]);
            }
         }
         i;
      }));

      ## Convert "transparent" to "grey" for compatibility with crayon
      fgText <- sapply(fgText, function(ix){
         ix[ix %in% "transparent" | grepl("^[#]......00$", ix)] <- "#777777";
         ix;
      });
      if (!is.null(bgText)) {
         bgText <- lapply(bgText, function(ix){
            ix[ix %in% "transparent" | grepl("^[#]......00$", ix)] <- "#777777";
            ix;
         });
      }

      if (doColor == 2) {
         if (verbose) {
            printDebug("Using crayon package colorization.");
         }
         if (8 == crayon::num_colors()) {
            ## If crayon detects 8-color terminal,
            ## make one attempt at 256-color terminal
            Sys.setenv(TERM="xterm-256color");
            crayon::num_colors(256);
         }
         if (timeStamp) {
            timeStampValue <- paste(c("(", make_style("bold")(
                  make_styles(style=fgTime, adjustRgb=adjustRgb,
                     text=format(Sys.time(), "%H:%M:%S"))
               ), ") ", format(Sys.time(), "%d%b%Y"), ": "), collapse="");
         } else {
            timeStampValue <- "";
         }

         if (!is.null(bgText)) {
            printValue <- paste(sapply(seq_along(x), function(ix){
               xStr <- x[ix];
               if (igrepHas("factor", class(xStr))) {
                  xStr <- as.character(xStr);
               }
               if (!is.na(fgText[ix])) {
                  if (is.na(bgText[[ix]])) {
                     make_styles(style=fgText[ix], text=xStr,
                        adjustRgb=adjustRgb);
                  } else {
                     make_styles(style=bgText[[ix]], bg=TRUE,
                        adjustRgb=adjustRgb,
                        text=make_styles(style=fgText[ix], adjustRgb=adjustRgb,
                           text=xStr));
                  }
               } else if (!is.na(bgText[[ix]])) {
                   make_styles(style=bgText[[ix]], bg=TRUE, text=xStr,
                     adjustRgb=adjustRgb);
               } else {
                  xStr;
               }
            }), collapse=collapse);
         } else {
            printValue <- paste(sapply(seq_along(x), function(ix){
               xStr <- x[ix];
               if (igrepHas("factor", class(xStr))) {
                  xStr <- as.character(xStr);
               }
               if (!is.na(fgText[ix])) {
                  make_styles(style=fgText[ix], text=xStr,
                     adjustRgb=adjustRgb);
               } else {
                  xStr;
               }
            }), collapse=collapse);
         }
         printString <- c(timeStampValue, printValue);
         if (comment) {
            printString <- c("## ", printString);
         }
         cat(printString, "\n");
      } else {
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
         cat(printString, "\n");
      }
   }
}

#' convert column number to Excel column name
#'
#' convert column number to Excel column name
#'
#' The purpose is to convert a numerical column number into a valid Excel 
#' column name, using LETTERS starting at A.
#' This function implements an arbitrary number of digits, which may or
#' may not be compatible with each version of Excel.  18,278 columns
#' would be the maximum for three digits, "A" through "ZZZ".
#'
#' This function is useful when referencing Excel columns via another
#' interface such as via openxlsx. It is also used by \code{\link{makeNames}}
#' when the numberStyle="letters", in order to provide letter suffix values.
#'
#' One can somewhat manipulate the allowed column names via the useLetters
#' parameter, which by default uses the entire 26-letter Western alphabet.
#'
#' @return character vector with length(x)
#'
#' @param x integer vector
#' @param useLetters character vector of single-digit characters to use as
#'    digits in the resulting column name. Note that these characters can
#'    be of almost any length, with any content.
#' @param zeroVal character single-digit to be used whenever x==0, or as a
#'    prefix for negative values. In theory there should be no negative
#'    input values, but this basic mechanism is used to handle the possibility.
#'
#' @examples
#' colNum2excelName(1:30)
#'
#' @export
colNum2excelName <- function
(x, useLetters=LETTERS, zeroVal="a",
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

#' Decide plot panel rows, columns for par(mfrow)
#'
#' Decide plot panel rows, columns for par(mfrow)
#'
#' @export
decideMfrow <- function
(n, method=c("aspect", "wide", "tall"),
 ...)
{
   ## Purpose is to decide how to arrange plots so that panels are roughly
   ## square.
   dinAspect <- getPlotAspect(type="device");
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
   c(n1, n2);
}

#' Get aspect ratio for coordinates, plot, or device
#'
#' Get aspect ratio for coordinates, plot, or device
#'
#' @export
getPlotAspect <- function
(type=c("coords", "plot", "device"),
 parUsr=par("usr"), parPin=par("pin"), parDin=par("din"),
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
#' @export
tcount <- function
(x, doSort=TRUE, minCount=NULL, maxCount=NULL, nameSortFunc=sort,
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
#' \code{\link[crayon]{make_style}} in order to style a vector of
#' character strings with a vector of foreground and background styles.
#'
#' @param style vector of one or more styles
#' @param text vector of one or more character values
#' @param bg NULL or a vector of one or more background styles
#' @param colors integer number of colors allowed for console output
#' @param checkSet logical whether to check if the color is extremely
#'    low saturation, in which case grey==TRUE is set.
#' @param setCutoff numeric cutoff for color saturation when checkSat==TRUE.
#' @param adjustRgb numeric value when converting colors to RGB format
#'    by crayon, which tends to round values up, losing subtle differences
#'    seen because colors are often too bright.
#' @param adjustPower numeric adjustment power factor
#' @param lRange lightness range, default c(0,1)
#' @param sRange saturation range, default c(0,1)
#' @param subTransparent color used to substitute for "transparent" which
#'    a valid R color, but not a valid color for the crayon package.
#' @param alphaPower numeric value, used to adjust the RGB values for alpha
#'    values less than 255, by raising the ratio to 1/alphaPower, which takes
#'    the ratio of square roots.  alphaPower=100 for minimal adjustment.
#' @param verbose logical whether to print verbose output
#' @param ... additional parameters are ignored
#'
#' @export
make_styles <- function
(style, text, bg=FALSE, grey=FALSE, colors=num_colors(),
 checkSat=TRUE, satCutoff=0.01, adjustRgb=0, adjustPower=1.5,
 lRange=c(0,1), sRange=c(0,1), subTransparent="grey45",
 alphaPower=2,
 verbose=FALSE, ...)
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
   if (!suppressPackageStartupMessages(require(crayon))) {
      ## If crayon is not available, return text without style. So sad.
      return(text);
   }

   if (igrepHas("matrix", style)) {
      style <- style[,rep(1:ncol(style), length.out=length(text)),drop=FALSE];
   } else {
      style <- rep(style, length.out=length(text));
      styleNA <- is.na(style);
      style <- col2rgb(style, alpha=TRUE);
      if (any(styleNA)) {
         style[,styleNA] <- NA;
      }
   }
   ## Apply alpha
   if ("alpha" %in% rownames(style) &&
       any(rmNA(style["alpha",], naValue=0) < 255)) {
      alphaFactor <- (style["alpha",])^(1/alphaPower)/(255)^(1/alphaPower);
      style[c("red","green","blue"),] <- style[c("red","green","blue"),] *
         rep(alphaFactor, each=3);
   }
   style <- style[c("red","green","blue"),,drop=FALSE];
   if (verbose) {
      print("style:");
      print(style);
   }

   ## Optionally define an allowed range of lightness for
   ## visibility of text
   if (!all(c(0,1) %in% lRange)) {
      if (is.null(colnames(style))) {
         colnames(style) <- makeNames(rep("col", ncol(style)));
      }
      styleHsv <- col2hsv(rgb2col(style));
      tooDark <- which(styleHsv["v",] < min(lRange));
      if (length(tooDark) > 0) {
         style[,tooDark] <- col2rgb(makeColorDarker(
            darkFactor=-1.5/(1-min(lRange)),
            sFactor=-1.5/(1-min(lRange)),
            rgb2col(style[,tooDark,drop=FALSE])));
         styleHsv <- col2hsv(rgb2col(style));
      }
      tooLight <- which(styleHsv["v",] > max(lRange));
      if (length(tooLight) > 0) {
         style[,tooLight] <- col2rgb(makeColorDarker(
            darkFactor=1.2/max(lRange),
            sFactor=1./max(lRange),
            rgb2col(style[,tooLight,drop=FALSE])));
         styleHsv <- col2hsv(rgb2col(style));
      }
      styleHsv["v",] <- noiseFloor(styleHsv["v",],
         minimum=min(lRange),
         ceiling=max(lRange));
      style <- col2rgb(hsv2col(styleHsv));
   }
   if (!all(c(0,1) %in% sRange)) {
      styleHsv <- col2hsv(rgb2col(style));
      styleHsv["s",] <- noiseFloor(styleHsv["s",],
         minimum=min(sRange),
         ceiling=max(sRange));
      style <- col2rgb(hsv2col(styleHsv));
   }


   if (adjustRgb != 0) {
      if (!is.na(adjustPower) && !is.null(adjustPower)) {
         ## This method uses square root transform
         style1 <- round((style^adjustPower)/(255^adjustPower)*6 + adjustRgb);
      } else {
         ## This method shifts color brightness down slightly
         style1 <- round(style/255*6 + adjustRgb);
      }
      style <- style1 * 255/6;
      style[style < 1] <- 1;
      style[style > 255] <- 255;
   }

   #orig_style_name <- style_name <- names(args)[1]
   if (checkSat) {
      styleNA <- apply(style, 2, function(i){any(is.na(i))});
      iSats <- rep(1, ncol(style));
      if (verbose) {
         print("style:");
         print(style);
      }
      iSats[!styleNA] <- rgb2hsv(style[,!styleNA])["s",];
   }
   iVals <- sapply(seq_along(text), function(i){
      iText <- text[i];
      iStyle <- style[,i,drop=FALSE];
      if (any(is.na(iStyle)) || 
         (iStyle %in% "transparent" && subTransparent %in% c(NA,"")) ) {
         iText;
      } else {
         if (checkSat && iSats[i] <= satCutoff) {
            iGrey <- TRUE;
         } else {
            iGrey <- grey;
         }
         if (any(iStyle %in% c("transparent"))) {
            iStyle[iStyle %in% c("transparent")] <- subTransparent;
         }
         if (verbose) {
            print("iStyle:");
            print(iStyle);
         }
         make_style(rgb2col(iStyle), bg=bg, colors=colors, grey=iGrey)(iText);
      }
   });
   if (!is.null(names(text))) {
      names(iVals) <- names(text);
   }
   attr(iVals, "color") <- rgb2col(style);
   iVals;
}

#' Show R function arguments jam-style
#'
#' Show R function arguments jam-style
#'
#' This function displays R function arguments, organized with one argument
#' per line, and colorized using the \code{\link{crayon}} package if
#' installed.
#'
#' It has an optional subset ability, using \code{grepString} which filters
#' argument names by pattern matching. This feature is intended to help find
#' the parameter name using a substring, which is particularly helpful for
#' R functions which contain numerous parameters, and which have not obscured
#' them by use of generic function wrapper. Generic functions very often
#' contain no useful parameters, making it difficult to discover required
#' parameters without reading the function documentation from the proper
#' dispatched function and calling package.
#'
#' It also by default sorts parameters by name, to make it easier to find
#' from among many parameters.
#'
#' @param x function or character name of a function.
#' @param grepString NULL, logical, or character grep string used to filter
#'    names of function parameters. If logical, it is assumed to be
#'    sortVars, and indicates whether to sort the parameter names.
#' @param sortVars logical whether to sort the function parameter names.
#' @param asList logical whether to display one entry per line (TRUE), or
#'    display results as a data.frame.
#' @param useColor logical whether to display results in color, if the crayon
#'    package is available, and terminal console is capable.
#' @param verbose logical whether to print verbose output.
#'
jargs <- function
(x, grepString=NULL, sortVars=TRUE, asList=TRUE, useColor=TRUE,
 verbose=FALSE,
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
   if (useColor) {
      if (suppressPackageStartupMessages(require(crayon))) {
         useCrayon <- TRUE;
      } else {
         if (verbose) {
            printDebug("Turned color off since ", "crayon",
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
            printDebug("No arguments matched the grepString.", fgText="yellow");
         }
         invisible(NULL);
      }
   }

   if (asList) {
      handleArgsText <- function(argsTextA, i,
         col1="mediumpurple2", col2="mediumaquamarine",
         colT="dodgerblue3", colF="red1", lRange=c(0.25,0.9),
         ...) {
         deTextA <- deparse(argsTextA[[i]]);
         if (class(argsTextA[[i]]) %in% "call") {
            ## Multi-value entry
            argTextA <- argsTextA[[i]];
            whichMid <- tail(seq_along(argTextA), -1);
            whichEnds <- setdiff(seq_along(argTextA), whichMid);
            firstArg <- deparse(argTextA[[whichEnds]]);
            if (firstArg %in% c("function")) {
               ## Parse functions differently than other entries
               argTextA <- deparse(argsTextA[[i]]);
               whichMid <- head(tail(seq_along(argTextA), -1), -1);
               whichEnds <- setdiff(seq_along(argTextA), whichMid);
               firstArg <- argTextA[[head(whichEnds,1)]];
               if (length(whichMid) == 0) {
                  argTextA[whichEnds] <- make_styles(text=argTextA[whichEnds],
                     style=col1, adjustRgb=0, lRange=lRange);
                  deTextA <- make_styles(text=argTextA[whichEnds], style=col1,
                     adjustRgb=0, lRange=lRange);
               } else {
                  argTextA[whichMid] <- make_styles(text=argTextA[whichMid],
                     style=col2, adjustRgb=0, lRange=lRange);
                  argTextA[whichEnds] <- make_styles(text=argTextA[whichEnds],
                     style=col1, adjustRgb=0, lRange=lRange);
                  deTextA <- paste0(argTextA[head(whichEnds,1)],
                     paste(argTextA[whichMid], collapse=", "),
                     argTextA[tail(whichEnds,1)],
                     collapse=" ");
               }
            } else {
               if (firstArg %in% "c" &&
                   all(isColor(as.character(argTextA[whichMid])))) {
                  argTextB <- as.character(argTextA[whichMid]);
                  argTextA[whichMid] <- sapply(argTextA[whichMid], function(j){
                     handleArgsText(list(A=j),
                        "A",
                        col1=as.character(j),
                        col2=as.character(j));
                  });
               } else {
                  argTextA[whichMid] <- sapply(argTextA[whichMid], function(j){
                     handleArgsText(list(A=j), "A", col1=col2, col2=col1);
                  });
               }
               argTextA[whichEnds] <- make_styles(text=firstArg, style=col1,
                  adjustRgb=0, lRange=lRange);
               if (igrepHas("[a-zA-Z]", firstArg)) {
                  ## Format: function("value1", "value2")
                  deTextA <- paste0(argTextA[whichEnds], "(",
                     paste(argTextA[whichMid], collapse=", "), ")",
                     collapse=" ");
               } else if (length(argTextA[whichMid]) == 2) {
                  ## Format: "value1" || "value2"
                  deTextA <- paste(argTextA[head(whichMid,1)],
                     argTextA[whichEnds],
                     argTextA[tail(whichMid,1)], sep=" ");
               } else {
                  argTextA <- make_styles(text=deparse(argsTextA[[i]]),
                     style=col2, adjustRgb=0, lRange=lRange);
                  deTextA <- argTextA;
               }
            }
         } else if (length(deTextA) > 1 &&
            igrepHas("^[a-zA-Z0-9]+[(]", deTextA[1])) {
            ## Multi-value entry
            whichMid <- head(tail(seq_along(deTextA), -1), -1);
            whichEnds <- setdiff(seq_along(deTextA), whichMid);
            deTextA[whichMid] <- make_styles(
               text=as.character(deTextA[whichMid]),
               style=col2,
               adjustRgb=0,
               lRange=lRange);
            deTextA[whichEnds] <- make_styles(
               text=as.character(deTextA[whichEnds]),
               style=col1,
               adjustRgb=0,
               lRange=lRange);
            aText <- paste(i, paste(deTextA, collapse=" "), sep=" = ");
         } else if (class(argsTextA[[i]]) %in% "logical") {
            if (igrepHas("FALSE", deTextA)) {
               deTextA <- make_styles(text=as.character(deTextA),
                  style=colF, adjustRgb=0, lRange=lRange);
            } else {
               deTextA <- make_styles(text=as.character(deTextA),
                  style=colT, adjustRgb=0, lRange=lRange);
            }
         } else {
            if (length(argsTextA[[i]]) > 0 &&
               all(isColor(as.character(argsTextA[[i]])))) {
               deTextA <- make_styles(text=as.character(deTextA),
                  style=as.character(argsTextA[[i]]),
                  adjustRgb=0.1,
                  lRange=lRange);
            } else {
               deTextA <- make_styles(text=as.character(deTextA),
                  style=col2,
                  adjustRgb=0,
                  lRange=lRange);
            }
         }
         return(deTextA);
      }

      x1 <- as.vector(gsub("=$", "", sapply(names(argsText), function(i){
         if (verbose) {
            printDebug("i:", i);
         }
         argText <- argsText[[i]];
         deText <- deparse(argsText[[i]]);
         if (useCrayon) {
            col1 <- "mediumpurple2";
            col2 <- "mediumaquamarine";
            deText <- handleArgsText(argsText, i);
         }
         aText <- paste(i, paste(deText, collapse=" "), sep=" = ");
         aText;
      })));
      if (sortVars && length(x1) > 1) {
         x2 <- mixedSort(x1);
      } else {
         x2 <- x1;
      }
      varLen <- sapply(x2, function(i){
         nchar(strsplit(i, "=")[[1]])[1]
      });
      x3 <- sapply(1:length(x2), function(i){
         paste(c(rep(" ", max(varLen) - varLen[i]),
                 gsub("[ ]+", " ", x2[i])), collapse="");
      });
      cat(paste(x3, collapse=",\n"));
      cat("\n");
   } else {
      argsTable <- do.call(cbind, lapply(argsText, deparse));
      if (sortVars) {
         argsTable <- argsTable[,mixedSort(colnames(argsTable)), drop=FALSE];
      }
      print(argsTable);
      invisible(argsTable);
   }
}

#' Apply noise floor to a numeric values
#'
#' Apply noise floor, setting numeric values below the floor to a minimum
#' value.
#'
#' Noise floors are useful when detected numeric values are sometimes below
#' a clear noise threshold, and where some downstream ratio may be calculated
#' using these values. Applying a noise floor ensures the ratios and not
#' artificially higher, ironically in cases where the values involved are
#' least reliable. They can be said to help produce more conservative
#' ratios, when using noise floored values.
#'
#' @param x numeric vector or matrix
#' @param minimum numeric floor value
#' @param newValue numeric, by default the same as the floor value. Sometimes
#'    it can be useful to define a different value, one example is to define
#'    values as NA, or another distinct number away from the floor.
#' @param adjustNA logical whether to change NA values to the newValue.
#' @param ceiling numeric value, optionally a ceiling. If defined, then values
#'    above the ceiling value are set to newCeiling.
#' @param newCeiling numeric value when ceiling is defined, values above the
#'    ceiling are set to this numeric value.
#' @param ... additional parameters are ignored.
#'
#' @export
noiseFloor <- function
(x, minimum=0, newValue=minimum,
 adjustNA=FALSE, ceiling=NULL, newCeiling=ceiling,
 ...)
{
   ## Purpose is to apply a noise floor, that is, to set all values
   ## to be at least 'minimum' amount.
   ## This function performs no scaling or normalization.
   if (adjustNA) {
      x[is.na(x) | (!is.na(x) & x < minimum)] <- newValue;
   } else {
      x[!is.na(x) & x < minimum] <- newValue;
   }
   if (!is.null(ceiling)) {
      x[!is.na(x) & x > ceiling] <- newCeiling;
   }
   return(x);
}

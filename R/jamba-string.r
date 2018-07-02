##
## jamba-string.r
##
## igrepHas
## rbindList
## makeNames
## nameVector
## nameVectorN
##

#' vector contains any case-insensitive grep match
#'
#' vector contains any case-insensitive grep match
#'
#' This function checks the input vector for any elements matching the
#' grep pattern. The grep is performed case-insensitive (igrep). This function
#' is particularly useful when checking function arguments or object class,
#' where the class(a) might return multiple values, or where the name of
#' the class might be slightly different than expected, e.g. data.frame,
#' data_frame, DataFrame.
#'
#' @param pattern the grep pattern to use with \code{\link{grep}}
#' @param x vector to use in the grep
#' @param ignore.case logical default TRUE, meaning the grep will be performed
#'    in case-insensitive mode.
#' @param minCount integer minimum number of matches required to return TRUE.
#' @param naToBlank logical whether to convert NA to blank, instead of
#'    allowing grep to handle NA values as-is.
#'
#' @return logical indicating whether the grep match criteria were met,
#'    TRUE indicates the grep pattern was present in minCount or more
#'    number of entries.
#'
#' @seealso \code{\link{grep}}'
#' @examples
#' a <- c("data.frame","data_frame","tibble","tbl");
#' igrepHas("Data.*Frame", a);
#' igrepHas("matrix", a);
#'
#' @export
igrepHas <- function
(pattern, x=NULL, ignore.case=TRUE,
 minCount=1, naToBlank=FALSE,
 ...)
{
   ## Purpose is a quick check for greppable substring, for if() statements
   ##
   ## naToBlank=TRUE will convert NA values to "" prior to running grep
   ##
   ## The special case where minCount is negative (minCount == -1) or larger
   ## than length(x), it will be set to length(x) and therefore
   ## requires all elements of x to meet the grep criteria
   if (minCount < 0 || minCount > length(x)) {
      minCount <- length(x);
   }
   if (length(x) == 0) {
      return(FALSE);
   } else {
      if (naToBlank && any(is.na(x))) {
         x[is.na(x)] <- "";
      }
      length(grep(pattern=pattern, x=x, ignore.case=ignore.case, ...)) >= as.integer(minCount);
   }
}

#' case-insensitive grep, returning values
#'
#' case-insensitive grep, returning values
#'
#' This function is a simple wrapper around \code{\link{grep}} which
#' runs in case-insensitive mode, and returns matching values. It is
#' particularly helpful when grabbing values from a vector.
#'
#' @param ...,value,ignore.case parameters sent to \code{\link{grep}}
#'
#' @return vector of matching values
#'
#' @examples
#' V <- paste0(LETTERS[1:5], LETTERS[4:8]);
#' vigrep("d", V);
#'
#' @export
vigrep <- function
(..., value=TRUE, ignore.case=TRUE)
{
   ## Purpose is simple to provide quicker wrapper to igrep, returning values
   igrep(..., value=value, ignore.case=ignore.case);
}

#' grep, returning values
#'
#' grep, returning values
#'
#' This function is a simple wrapper around \code{\link{grep}} which
#' returns matching values. It is
#' particularly helpful when grabbing values from a vector, but where the
#' case (uppercase or lowercase) is known.
#'
#' @param ...,value,ignore.case parameters sent to \code{\link{grep}}
#'
#' @return vector of matching values
#'
#' @examples
#' V <- paste0(LETTERS[1:5], LETTERS[4:8]);
#' vgrep("D", V);
#' vgrep("d", V);
#' vigrep("d", V);
#'
#' @export
vgrep <- function
(..., value=TRUE, ignore.case=FALSE)
{
   ## Purpose is simple to provide quicker wrapper to grep, returning values
   grep(..., value=value, ignore.case=ignore.case);
}

#' case-insensitive grep
#'
#' case-insensitive grep
#'
#' This function is a simple wrapper around \code{\link{grep}} which
#' runs in case-insensitive mode. It is mainly used to save keystrokes,
#' but is consistently named alongside \code{\link{vgrep}} and
#' \code{\link{vigrep}}.
#'
#' @param ...,ignore.case parameters sent to \code{\link{grep}}
#'
#' @return vector of matching indices
#'
#' @examples
#' V <- paste0(LETTERS[1:5], LETTERS[4:8]);
#' igrep("D", V);
#' igrep("d", V);
#' vigrep("d", V);
#'
#' @export
igrep <- function
(..., ignore.case=TRUE)
{
   ## Purpose is simply to provide quick wrapper for case-insensitive grep()
   grep(ignore.case=ignore.case, ...);
}

#' case-insensitive grep, returning unmatched indices
#'
#' case-insensitive grep, returning unmatched indices
#'
#' This function is a simple wrapper around \code{\link{grep}} which
#' runs in case-insensitive mode, and returns unmatched entries.
#' It is mainly used to save keystrokes,
#' but is consistently named alongside \code{\link{vgrep}} and
#' \code{\link{vigrep}}, and quite helpful for writing concise code.
#'
#' @param ...,ignore.case,invert parameters sent to \code{\link{grep}}
#'
#' @return vector of non-matching indices
#'
#' @examples
#' V <- paste0(LETTERS[1:5], LETTERS[4:8]);
#' unigrep("D", V);
#' igrep("D", V);
#'
#' @export
unigrep <- function
(..., ignore.case=TRUE, invert=TRUE)
{
   ## purpose is to un-grep, return non-hits in case-insensitive fashion
   igrep(..., ignore.case=ignore.case, invert=invert);
}

#' case-insensitive grep, returning unmatched values
#'
#' case-insensitive grep, returning unmatched values
#'
#' This function is a simple wrapper around \code{\link{grep}} which
#' runs in case-insensitive mode, and returns unmatched values.
#' It is mainly used to save keystrokes,
#' but is consistently named alongside \code{\link{vgrep}} and
#' \code{\link{vigrep}}, and quite helpful for writing concise code.
#' It is particularly useful for removing unwanted entries from a long
#' vector, for example removing accession numbers from a long
#' vector of gene symbols in order to review gene annotations.
#'
#' @param ...,ignore.case,value,invert parameters sent to \code{\link{grep}}
#'
#' @return vector of non-matching indices
#'
#' @examples
#' V <- paste0(LETTERS[1:5], LETTERS[4:8]);
#' unigrep("D", V);
#' igrep("D", V);
#'
#' @export
unvigrep <- function
(..., ignore.case=TRUE, value=TRUE, invert=TRUE)
{
   ## purpose is to un-grep, return non-hits in case-insensitive fashion
   grep(..., ignore.case=ignore.case, value=value, invert=invert);
}

#' progressive case-insensitive value-grep
#'
#' progressive case-insensitive value-grep
#'
#' Purpose is to provide "progressive vigrep()",which is value-returning,
#' case-insensitive grep, starting with an ordered vector of grep patterns.
#' For example, it returns entries in the order they are matched, by the
#' progressive use of grep patterns.
#'
#' It is particularly good when using multiple grep patterns, since
#' \code{grep()} does not accept multiple patterns as input. This function
#' also only returns the unique matches in the order they were matched,
#' which alleviates the need to run a series of \code{grep()} functions
#' and collating their results.
#'
#' It is mainly to allow for prioritized ordering of matching entries, where
#' one would like certain matching  entries first, followed by another
#' set of matching entries, without duplication. For example,
#' one might grep for a few patterns, but want certain pattern hits to be
#' listed first.
#'
#' @param patterns vector of grep patterns
#' @param x vector to be tested by \code{\link{grep}}
#' @param maxValues integer or NULL, the maximum matching entries to
#'    return per grep pattern. This parameter is mainly useful when returning
#'    a list of matches, where one would like each list to contain a maximum
#'    number of entries.
#' @param sortFunc function or NULL, used to sort entries within each set of
#'    matching entries. Use NULL to avoid sorting entries.
#' @param rev logical whether to reverse the order of matching entries. Use
#'    TRUE if you would like entries matching the patterns to be placed last,
#'    and entries not matching the grep patterns to be placed first. This
#'    technique is effective at placing "noise named" at the end of a long
#'    vector, for example.
#' @param returnType character indicating whether to return a vector or list.
#'    A list will be in order of the grep patterns, using empty elements to
#'    indicate when no entries matched each pattern. This output is useful
#'    when you would like to know which patterns matched specific entries.
#' @param ignore.case logical parameter sent to \code{\link{grep}}, TRUE
#'    runs in case-insensitive mode, as by default.
#'
#' @examples
#' # a rather comical example
#' # set up a test set with labels containing several substrings
#' testTerms <- c("robot","tree","dog","mailbox","pizza","noob");
#' testWords <- pasteByRow(t(combn(testTerms,3)));
#'
#' # now pull out entries matching substrings in order
#' provigrep(c("pizza", "dog", "noob", "."), testWords);
#'
#' @export
provigrep <- function
(patterns, x, maxValues=NULL, sortFunc=c,
 rev=FALSE, returnType=c("vector", "list"), ignore.case=TRUE,
 ...)
{
   ## Purpose is to provide "progressive vigrep()" (which is value-returning,
   ## case-insensitive) mainly to allow for prioritized ordering of matching
   ## entries.
   ## For example, one might grep for a few patterns, but want certain pattern
   ## hits to come back before others.
   ##
   ## rev will return the entries in reverse order, which is effective if
   ## using set of patterns to down-prioritize.
   ##
   ## sortFunc is intended to allow for sorting each set of matched entries
   ## along the way, particularly useful when using a non-standard sort
   ## function.
   ##
   ## returnType="vector" returns the vector of matching entries
   ## returnType="list" returns a named list of matching entries, using
   ## the grep patterns as list names
   ##
   returnType <- match.arg(returnType);
   valueSetL <- lapply(patterns, function(i){
      x <- vigrep(pattern=i, x=x, ...);
      if (!is.null(sortFunc)) {
         x <- sortFunc(x);
      }
      x;
   });
   if (rev) {
      valueSetL <- rev(valueSetL);
   }
   if (returnType %in% "list") {
      if (is.null(names(patterns))) {
         names(valueSetL) <- makeNames(patterns);
      } else {
         names(valueSetL) <- names(patterns);
      }
      return(valueSetL);
   }
   valueSet <- unique(unlist(valueSetL));
   if (!is.null(maxValues) && maxValues > 0 && length(valueSet) > 0) {
      #valueSet <- valueSet[1:maxValues];
      valueSet <- head(valueSet, maxValues);
   }
   return(valueSet);
}

#' rbind a list of vectors into matrix or data.frame
#'
#' rbind a list of vectors into matrix or data.frame
#'
#' The purpose of this function is to emulate do.call(rbind, x) on a list
#' of vectors, while specifically handling when there are different
#' numbers of entries per vector. Instead of repeating the values to fill the
#' number of resulting columns, this function fills cells with blank fields.
#' In extensive timings tests at the time this function was created, this
#' method was notably faster than alternatives. That is, it runs
#' \code{do.call(rbind, x)} then subsequent steps to replace values with
#' blank entries was notably faster than other alternatives.
#'
#' @return
#' By default a matrix, or if returnDF=TRUE the object is coerced to a
#' data.frame. The rownames by default are derived from the list names,
#' but the colnames are not derived from the vector names.
#'
#' @param x input list of vectors.
#' @param emptyValue character value to use to represent missing values,
#'    whenever a blank cell is introduced into the resulting matrix
#' @param keepListNames logical whether to use list names as rownames
#'    in the resulting matrix or data.frame.
#' @param newColnames NULL or character vector of colnames to use for the
#'    resulting matrix or data.frame.
#' @param newRownames NULL or character vector of rownames to use for the
#'    resulting matrix or data.frame. If supplied, this value overrides the
#'    keepListNames=TRUE use of list names as rownames.
#' @param fixBlanks logical whether to use blank values instead of repeating
#'    each vector to the length of the maximum vector length when filling
#'    each row of the matrix or data.frame.
#' @param returnDF logical whether to return a data.frame, by default FALSE,
#'    a matrix is returned.
#' @param verbose logical whether to print verbose output during processing.
#'
#' @examples
#' L <- list(a=LETTERS[1:4], b=letters[1:3]);
#' do.call(rbind, L);
#' rbindList(L);
#' rbindList(L, returnDF=TRUE);
#'
#' @export
rbindList <- function
(x,
 emptyValue="",
 keepListNames=TRUE,
 newColnames=NULL,
 newRownames=NULL,
 fixBlanks=TRUE,
 returnDF=FALSE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to emulate do.call(rbind, x) on a list with variable
   ## numbers of entries but instead of repeating the values to fill the
   ## number of resulting columns, it goes back and empties out the fields
   ## which should have no value. In extensive timings tests at the time
   ## this function was created, this method was notably faster than
   ## alternatives.
   ##
   ## keepListNames=TRUE is default, the way R prepends list names to
   ## the element names when unlisting an object.
   ## keepListNames=FALSE will not use the list names, but keep the element
   ## names without changing them.
   ##
   ## newColnames and newRownames are optionally intended for assigning colnames
   ## and rownames to the resulting matrix, respectively.
   ##
   ## fixBlanks=FALSE will turn off the fixing of duplicated entries
   ##
   ## returnDF=TRUE will convert the output to data.frame, when usually it
   ## would be a matrix after calling rbind(). This step checks for non-unique
   ## rownames, and if present, calls makeNames(rownames(x), ...). The "..."
   ## can be used to customize how duplicate rownames are assigned.
   ##
   xLslen <- lengths(x);
   if (any(xLslen %in% 0)) {
      if (all(xLslen == 0)) {
         return(NULL);
      }
      x <- x[!xLslen == 0];
      xLslen <- xLslen[!xLslen == 0];
      #x[xLslen == 0] <- "";
   }
   ## Now run the normal do.call(rbind, ...) which duplicates entries
   ## but we will clean them up
   xDF <- suppressWarnings(do.call(rbind, x));
   if (!keepListNames) {
      rownames(xDF) <- unlist(lapply(x, function(ix){
         if (igrepHas("data.*frame|matrix", class(ix))) {
            rownames(ix);
         } else {
            names(ix);
         }}));
   }
   ## Mark the row entries to fix
   if (fixBlanks) {
      xLfix <- unique(xLslen[xLslen < ncol(xDF)]);
      if (length(xLfix) > 0) {
         ## Iterate chunks of rows which share the same lengths
         ## and blank out their subsequent column values
         for(i in xLfix) {
            whichRows <- which(xLslen == i);
            xDF[whichRows,((i+1):ncol(xDF))] <- "";
         }
      }
   }
   if (!is.null(newColnames)) {
      colnames(xDF) <- makeNames(rep(newColnames, length.out=ncol(xDF)));
   }
   if (!is.null(newRownames)) {
      rownames(xDF) <- makeNames(rep(newRownames, length.out=nrow(xDF)));
   }
   if (returnDF) {
      ## Check for duplicated rownames, allowed in matrices but not
      ## in data.frames
      if (length(tcount(rownames(xDF), minCount=2)) > 0) {
         rownames(xDF) <- makeNames(rownames(xDF), ...);
      }
      xDF <- unlistDataFrame(as.data.frame(xDF), verbose=verbose, ...);
   }
   return(xDF);
}

#' make unique vector names
#'
#' make unique vector names
#'
#' This function extends the basic goal from \code{\link[base]{make.names}}
#' which is intended to make syntactically valid names from a character vector.
#' This makeNames function makes names unique, and offers configurable methods
#' to handle duplicate names. By default, any duplicated entries receive a
#' suffix _v# where # is s running count of entries observed, starting at 1.
#' The \code{\link[base]{make.names}} function, by contrast, renames the
#' second observed entry starting at .1, leaving the original entry
#' unchanged. Optionally, makeNames can rename all entries with a numeric
#' suffix, for consistency.
#'
#' For example:
#' \code{A, A, A, B, B, C}
#' becomes:
#' \code{A_v1, A_v2, A_v3, B_v1, B_v2, C}
#'
#' Also, makeNames always allows "_".
#'
#' This makeNames function is similar to \code{\link[base]{make.unique}}
#' which also converts a vector into a unique vector by adding suffix values,
#' however the \code{\link[base]{make.unique}} function intends to allow
#' repeated operations which recognize duplicated entries and continually
#' increment the suffix number. This makeNames function currently does not
#' handle repeat operations. The recommended approach to workaround having
#' pre-existing versioned names would be to remove suffix values prior to
#' running this function. One small distinction from
#' \code{\link[base]{make.unique}} is that makeNames does version the first
#' entry in a set.
#'
#' @return character vector of unique names
#'
#' @param x character vector to be used when defining names. All other
#'    vector types will be coerced to character prior to use.
#' @param unique for compatibility with \code{\link[base]{make.names}} but
#'    this parameter is ignored. All results are unique.
#' @param suffix character separator between the original entry and the
#'    version, if necessary.
#' @param renameOnes logical whether to rename single, unduplicated, entries.
#' @param doPadInteger logical whether to pad integer values to a consistent
#'    number of digits, based upon all suffix values needed. This output
#'    allows for more consistent sorting of names. To define a fixed number
#'    of digits, use the useNchar parameter.
#' @param useNchar integer or NULL, number of digits to use when padding
#'    integer values with leading zero, only relevant when usePadInteger=TRUE.
#' @param startN integer number used when numberStyle is "number", this integer
#'    is used for the first entry to be renamed. You can use this value to
#'    make zero-based suffix values, for example.
#' @param numberStyle character style for version numbering
#'    \describe{
#'       \item{"number"}{Use integer numbers to represent each duplicated
#'          entry.}
#'       \item{"letters"}{Use lowercase letters to represent each duplicated
#'          entry. The 27th entry uses the pattern "aa" to represent two
#'          26-base digits. When doPadInteger=TRUE, a zero is still used
#'          to pad the resulting version numbers, again to allow easy sorting
#'          of text values, but also because there is no letter equivalent
#'          for the number zero.
#'          It is usually best to change the suffix to "_" or "" when using
#'          "letters".}
#'       \item{"LETTERS"}{Use uppercase letters to represent each duplicated
#'          entry, with the same rules as applied to "letters".}
#'    }
#' @param renameFirst logical whether to rename the first entry in a set of
#'    duplicated entries. If FALSE then the first entry in a set will not
#'    be versioned, even when renameOnes=TRUE.
#' @param keepNA logical whether to retain NA values using the string "NA".
#'    If keepNA is FALSE, then NA values will remain NA, thus causing some
#'    names to become <NA>, which can cause problems with some downstream
#'    functions which assume all names are either NULL or non-NA.
#'
#' @examples
#' V <- rep(LETTERS[1:3], c(2,3,1));
#' makeNames(V);
#' makeNames(V, renameOnes=TRUE);
#' makeNames(V, renameFirst=FALSE);
#' exons <- makeNames(rep("exon", 3), suffix="");
#' makeNames(rep(exons, c(2,3,1)), numberStyle="letters", suffix="");
#'
#' @export
makeNames <- function
(x,
 unique=TRUE,
 suffix="_v",
 renameOnes=FALSE,
 doPadInteger=FALSE,
 startN=1,
 numberStyle=c("number","letters","LETTERS"),
 useNchar=NULL,
 renameFirst=TRUE,
 keepNA=TRUE,
 ...)
{
   ## Purpose is to make unique names without the R mangling that comes
   ## with make.names().
   ## By default, unique entries are not renamed, and entries with two or
   ## more replicates are renamed to NAME_v1, NAME_v2, NAME_v3, etc.
   ##
   ## if renameOnes=TRUE, it will rename singlets to NAME_v1 even if there
   ## is only one entry.
   ##
   ## renameFirst=TRUE will rename each duplicated entry NAME_v1, NAME_v2,
   ## NAME_v3, etc.
   ## renameFirst=FALSE will not rename the first in a set of duplicated
   ## entries, e.g. NAME, NAME_v1, NAME_v2, etc.
   ##
   ## The distinction between renameOnes and renameFirst:
   ## renameOnes=TRUE will rename all singlets and duplicated entries,
   ## starting with the first entry.
   ## renameOnes=FALSE will not rename singlet entries.
   ## renameFirst=TRUE will only rename duplicated entries, starting with
   ## the first entry.
   ## renameFirst=FALSE will not rename the first entry in a set of
   ## duplicated entries.
   ##
   ## the suffix can be changed, e.g. "_r" will name names NAME_r1,
   ## NAME_r2, NAME_r3, etc.
   ##
   ## numberStyle="number" uses integers as the suffix
   ## numberStyle="letters" uses lowercase letters as digits, similar to Excel column names
   ## numberStyle="LETTERS" uses uppercase letters as digits, similar to Excel column names
   ## Be aware that letters can only go to roughly 18,000 entries, given the current implementation
   ## of colNum2excelName
   ##
   ## When useNchar is numeric, it sets doPadInteger=TRUE, and will use at least
   ## that many digits in padding the integer.
   ##
   ##
   ## TODO:
   ## Update logic to be analogous to using make.unique(), which intends
   ## to maintain previous versioning of names without appending deeper
   ## suffices as appropriate.
   ## E.g.     c("",    "",    "",    "_v1", "_v2", "_v3")
   ## becomes  c("_v4", "_v5", "_v6", "_v1", "_v2", "_v3")
   ## instead of
   ##          c("_v1", "_v2", "_v3", "_v1", "_v2", "_v3")
   ## or
   ##          c("_v1_v1", "_v2_v1", "_v3_v1", "_v1_v2", "_v2_v2", "_v3_v2")
   ##
   if (length(x) == 0) {
      return(x);
   }
   numberStyle <- match.arg(numberStyle);
   if (!is.null(useNchar)) {
      useNchar <- as.integer(useNchar);
      doPadInteger=TRUE;
   }
   doLetters <- FALSE;
   if (any(c("factor", "ordered") %in% class(x))) {
      x <- as.character(x);
   }
   if (keepNA && any(is.na(x))) {
      x <- rmNA(x,
         naValue="NA");
   }

   ## Convert entries to a named count of occurences of each entry
   xSub <- table(as.character(x));

   ## Vector of counts to be used
   versionsV <- as.integer(renameFirst):max(c(xSub,1)) + startN - 1;

   ## If using letters, define the set of letter upfront to save processing
   if (igrepHas("letters", numberStyle)) {
      doLetters <- TRUE;
      if (numberStyle %in% "letters") {
         useLetters <- letters[1:26];
         zeroVal <- "A";
      } else {
         useLetters <- LETTERS[1:26];
         zeroVal <- "a";
      }
      num2letters <- colNum2excelName(versionsV, useLetters=useLetters,
         zeroVal=zeroVal, ...);
      versionsV <- num2letters;
   }
   if (doPadInteger) {
      versionsV <- padInteger(versionsV, useNchar=useNchar, ...);
   }

   ## If no duplicated entries
   if (max(xSub) %in% c(-Inf,1)) {
      ## If not renaming the singlet entries, send the same list back
      if (!renameOnes) {
         return(x);
      } else {
         ## If renaming singlets, simply paste the suffix and first entry
         return(paste0(x, suffix, head(versionsV, 1)));
      }
   }

   if (renameOnes) {
      xUse <- 1:length(x);
   } else {
      xUse <- (x %in% names(xSub)[xSub > 1]);
   }
   xSub1 <- x[xUse];

   ## Preserve the original order
   names(xSub1) <- padInteger(seq_along(xSub1));
   ## Split the vector into a list of vectors, by name
   xSub2 <- split(xSub1, xSub1);
   names(xSub2) <- NULL;

   ## Optionally pad the integer to facilitate sorting
   ## Note: This padding only pads integers within each name,
   ## not across all names.
   xSub3 <- lapply(xSub2, function(i){
      versionsV[seq_along(i)];
   });

   ## Now simply paste the value, the suffix, and the new version
   xSub1v <- paste0(unlist(xSub2), suffix, unlist(xSub3));

   ## Re-order the vector using the original order
   names(xSub1v) <- names(unlist(xSub2));
   xSub1v2 <- xSub1v[names(xSub1)];

   ## Assign only the entries we versioned
   x[xUse] <- xSub1v2;

   ## Last check for renameFirst=FALSE, in which case we remove the first
   ## versioned entry
   if (!renameFirst) {
      escapeRegex <- function(string){
         gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string);
      }
      firstVer <- paste0(escapeRegex(paste0(suffix, head(versionsV, 1))), "$");
      x[xUse] <- gsub(firstVer, "", x[xUse]);
   }

   return(x);
}

#' assign unique names for a vector
#'
#' assign unique names for a vector
#'
#' This function assigns unique names to a vector, if necessary it runs
#' \code{\link{makeNames}} to create unique names. It differs from
#' \code{\link[stats]{setNames}} in that it ensures names are unique,
#' and when no names are supplied, it uses the vector itself to define
#' names. It is helpful to run this function inside an \code{\link{lapply}}
#' function call, which by default maintains names, but does not assign
#' names if the input data did not already have them.
#'
#' When used with a data.frame, it is particularly convenient to pull out
#' a named vector of values. For example, log2 fold changes by gene, where
#' the gene symbols are the name of the vector.
#'
#' \code{nameVector(genedata[,c("Gene","log2FC")])}
#'
#' @param x vector input, or data.frame, matrix, or tibble with two columns,
#'    the second column is used to name values in the first column.
#' @param y NULL or character vector of names. If NULL then x is used.
#'    Note that y is recycled to the length of x, prior to being sent
#'    to the makeNamesFunc.
#'    In fringe cases, y can be a matrix, data.frame, or tibble, in which
#'    case \code{\link{pasteByRow}} will be used to create a character string
#'    to be used for vector names. Note this case is activated only when x
#'    is not a two column matrix, data.frame, or tibble.
#' @param makeNamesFunc function to make names unique, by default
#'    \code{\link{makeNames}} which ensures names are unique.
#' @param ... passed to \code{\link{makeNamesFunc}}, or to
#'    \code{\link{pasteByRow}} if y is a two column data.frame, matrix, or
#'    tibble. Thus, \code{sep} can be defined here as a delimiter between
#'    column values.
#'
#' @examples
#' # it generally just creates names from the vector values
#' nameVector(LETTERS[1:5]);
#'
#' # if values are replicated, the makeNames() function makes them unique
#' V <- rep(LETTERS[1:5], each=3);
#' nameVector(V);
#'
#' # for a two-column data.frame, it creates a named vector using
#' # the values in the first column, and names in the second column.
#' df <- data.frame(seq_along(V), V);
#' df;
#' nameVector(df);
#'
#' # Lastly, admittedly a fringe case, it can take a multi-column data.frame
#' # to generate labels:
#' nameVector(V, df);
#'
#' @export
nameVector <- function
(x,
 y=NULL,
 makeNamesFunc=makeNames,
 ...)
{
   ## Purpose is to name a vector with its own values,
   ## useful for lapply which only names output if the input
   ## vector has names.
   ##
   ## A neat trick is to use the _v# naming scheme in makeNames to
   ## create unique names based upon a single label, e.g.
   ## set1colors <- nameVector(brewer.pal(15, "Set1"), "Set1");
   ##   Set1_v1   Set1_v2   Set1_v3   Set1_v4   Set1_v5   Set1_v6   Set1_v7   Set1_v8   Set1_v9
   ## "#E41A1C" "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00" "#FFFF33" "#A65628" "#F781BF" "#999999"
   ##
   ## Added bonus, if given a 2-column table, it'll use them as x and y
   if (igrepHas("dataframe", class(x))) {
      x <- as.data.frame(x);
   }
   if (igrepHas("data.frame", class(x)) && ncol(x) == 2) {
      y <- x[[2]];
      x <- x[[1]];
   } else if (igrepHas("matrix", class(x)) && ncol(x) == 2) {
      y <- x[,2];
      x <- x[,1];
   }
   if (length(y) > 0) {
      if (igrepHas("data.frame|matrix", class(y))) {
         ## If given a data.frame use pasteByRow() to create a string
         y <- pasteByRow(y, ...);
      }
      names(x) <- makeNamesFunc(rep(y, length.out=length(x)), ...);
   } else {
      names(x) <- makeNamesFunc(x, ...);
   }
   return(x);
}

#' define a named vector using vector names
#'
#' define a named vector using vector names
#'
#' This function creates a vector from the names of the input vector,
#' then assigns the same as names. The utility is mainly for
#' \code{\link{lapply}} functions which maintain the name of a vector
#' in its output. The reason to run \code{\link{lapply}} using names
#' is so the lapply function is operating only on the name and not the
#' data it references, which can be convenient when the name of the element
#' is useful to known inside the function body. The reason to name the names,
#' is so the list object returned by \code{\link{lapply}} is also named
#' with these same consistent names.
#'
#' Consider a list of data.frames, each of which represents stats results
#' from a contrast and fold change. The data.frame may not indicate the name
#' of the contrast, while the list itself may be named by the contrast.
#' One would \code{lapply(nameVectorN(listDF), function(iName)iName)} which
#' allows the internal function access to the name of each list element. This
#' could for example be added to the data.frame.
#'
#' @return vector of names, whose names are uniquely assigned using
#'    \code{\link{makeNames}} using the values of the vector.
#'
#' @param x vector or any object which has names available via \code{names(x)}
#' @param makeNamesFunc function used to create unique names, in the event that
#'    the names(x) are not unique.
#'
#' @examples
#' # a simple integer vector with character names
#' L <- nameVector(1:5, LETTERS[1:5]);
#' L;
#'
#' # we can make a vector of names, retaining the names
#' nameVectorN(L);
#'
#' # Now consider a named list, where the name is important
#' # to keep for downstream work.
#' K <- list(A=(1:3)^3, B=7:10, C=(1:4)^2);
#' K;
#' # Typical lapply-style work does not operate on the name,
#' # making it difficult to use the name inside the function.
#' # Here, we just add the name to the colnames, but anything
#' # could be useful.
#' lapply(K, function(i){
#'     data.frame(mean=mean(i), median=median(i));
#'  });
#'
#' # So the next step is to run lapply() on the names
#' lapply(names(K), function(i){
#'    iDF <- data.frame(mean=mean(K[[i]]), median=median(K[[i]]));
#'    colnames(iDF) <- paste(c("mean", "median"), i);
#'    iDF;
#' })
#' # The result is good, but the list is no longer named.
#' # The nameVectorN() function is helpful for maintaining the names.
#'
#' # So we run lapply() on the named-names, which keeps the names in
#' # the resulting list, and sends it into the function.
#' lapply(nameVectorN(K), function(i){
#'    iDF <- data.frame(mean=mean(K[[i]]), median=median(K[[i]]));
#'    colnames(iDF) <- paste(c("mean", "median"), i);
#'    iDF;
#' });
#'
#' @export
nameVectorN <- function
(x, makeNamesFunc=makeNames,
 ...)
{
   ## Purpose is to extend nameVector to create a named vector of
   ## names(x), useful when you want to run lapply() on the names
   ## of a vector, and return a list whose names are names(x)
   ## Use instead of lapply(nameVector(names(x)), function(i)c)
   if (is.null(names(x))) {
      names(x) <- seq_along(x);
   }
   nameVector(names(x), makeNamesFunc=makeNamesFunc, ...);
}

#' remove NULL entries from list
#'
#' remove NULL entries from list
#'
#' This function is a simple helper function to remove NULL from a list,
#' optionally replacing it with another value
#'
#' @return list with NULL entries either removed, or replaced with nullValue.
#' This function is typically called so it removed list elements which are
#' NULL, resulting in a list that contains non-NULL entries. This function
#' can also be useful when NULL values should be changed to something else,
#' perhaps a character value "NULL" to be used as a label.
#'
#' @param x list or other object which may contain NULL.
#'
#' @export
rmNULL <- function
(x, nullValue=NULL,
 ...)
{
   ## Purpose is similar to rmNA() which can also perform this function,
   ## to replace NULL with a non-NULL value.  It operates effectively
   ## on lists, which may contain some NULL elements
   #isNULL <- sapply(x, is.null);
   if (length(x) == 0) {
      x <- nullValue;
   } else {
      isNULL <- sapply(x, function(i){
         length(i) == 0;
      });
      if (length(nullValue) == 0) {
         x <- x[!isNULL];
      } else {
         x[isNULL] <- nullValue;
      }
   }
   x;
}

#' remove NA values
#'
#' remove NA values
#'
#' This function removes NA values, by default shortening a vector as a result,
#' but optionally replacing NA and Infinite values with fixed values.
#'
#' @return vector with NA entries either removed, or replaced with naValue,
#'    and NULL entries either removed or replaced by nullValue.
#'
#' @param x vector input
#' @param naValue NULL or single replacement value for NA entries. If NULL,
#'    then NA entries are removed from the result.
#' @param rmNULL logical whether to replace NULL entries with naValue
#' @param nullValue NULL or single replacement value for NULL entries. If NULL,
#'    then NULL entries are removed from the result.
#' @param rmInfinite logical whether to replace Infinite values with
#'    infiniteValue
#' @param infiniteValue value to use when rmInfinite==TRUE to replace
#'    entries which are Inf or -Inf.
#' @param rmNAnames logical whether to remove entries which have NA as the
#'    name, regardless whether the entry itself is NA.
#' @param verbose logical whether to print verbose output
#'
#' @export
rmNA <- function
(x,
 naValue=NULL,
 rmNULL=FALSE,
 nullValue=naValue,
 rmInfinite=TRUE,
 infiniteValue=NULL,
 rmNAnames=FALSE,
 verbose=FALSE,
 ...)
{
   ## Purpose is simply to remove NA entries from a vector.
   ##
   ## It will assign NA to some number (e.g. zero)
   ## if naValue is not NULL.
   ##
   ## rmNULL=TRUE will convert NULL into the naValue
   ##
   ## If rmInfinite is TRUE, it will remove infinite values.
   ## If infiniteValue is not NULL, infinite values will be assigned that value
   ## (e.g. to apply a max value and not remove those datapoints)
   ##
   ## rmNAnames=TRUE will remove NA names only when names exist
   if (rmNULL && is.null(x)) {
      x <- nullValue;
      return(x);
   }
   if (!class(x) %in% "list" && rmInfinite && any(is.infinite(x))) {
      x <- rmInfinite(x, infiniteValue=infiniteValue);
   }
   if (igrepHas("factor", class(x))) {
      if (any(is.na(as.character(x)))) {
         if (verbose) {
            printDebug("rmNA(): ",
               "NA is present in factor x.");
         }
         ## If x is a factor, only modify it if there is an NA,
         ## which we must check by converting to character, in case
         ## NA is not one of the factor levels but is present in x
         if (!is.null(naValue)) {
            ## If we are replacing NA, then add it as a factor then change it
            x <- addNA(x, ifany=TRUE);
            levels(x) <- rmNA(levels(x), naValue=naValue);
         } else {
            ## We not replacing NA, then remove NA from levels(x)
            notNA <- which(!is.na(as.character(x)));
            x <- factor(x[notNA], exclude=NA);
         }
      }
   } else if (any(is.na(x))) {
      whichNA <- which(is.na(x));
      if (length(naValue) > 0) {
         x[whichNA] <- naValue;
      } else {
         x <- x[-c(whichNA)];
      }
   }
   ## Note: NULL should only occur in lists, not vectors
   if (rmNULL) {
      isNULL <- sapply(x, is.null);
      if (any(isNULL)) {
         x[isNULL] <- naValue;
      }
   }
   ## Optionally remove entries with NA names
   if (rmNAnames && !is.null(names(x)) && any(is.na(names(x)))) {
      naNames <- which(!is.na(names(x)));
      x <- x[naNames];
   }
   x;
}

#' remove Infinite values
#'
#' remove Infinite values
#'
#' @param x vector input
#' @param infiniteValue NULL to remove Infinite values, or a replacement value
#' @param ... additional parameters are ignored
#'
#' @export
rmInfinite <- function
(x, infiniteValue=NULL,
 ...)
{
   ## Purpose is to remove infinite values from a vector.
   ## If infiniteValue is not NULL, infinite values will be assigned that value
   ## (e.g. to apply a max value and not remove those datapoints)
   if (!is.null(infiniteValue)) {
      if (igrepHas("character", class(infiniteValue))) {
         x[is.infinite(x)] <- paste0(gsub("1", "",
            sign(x[is.infinite(x)])), infiniteValue);
      } else {
         x[is.infinite(x)] <- infiniteValue * sign(x[is.infinite(x)]);
      }
   } else {
      x <- x[!is.infinite(x)];
   }
   return(x);
}

#' sort alphanumeric values keeping numeric values in proper order
#'
#' sort alphanumeric values keeping numeric values in proper order
#'
#' This function is a refactor of the \code{\link[gtools]{mixedsort}}
#' function from the \code{gtools} package. It was extended to make it faster,
#' and to handle special cases slightly differently. It was driven by some
#' need to sort gene symbols, and miRNA symbols in numeric order, for example:
#' \describe{
#'    \item{test set:}{miR-12,miR-1,miR-122,miR-1b,mir-1a}
#'    \item{\code{gtools::mixedsort}:}{miR-122,miR-12,miR-1,miR-1a,mir-1b}
#'    \item{\code{mixedSort}:}{miR-1,miR-1a,miR-1b,miR-12,miR-122}
#' }
#' The function does not by default recognize negative numbers as negative,
#' instead it treats '-' as a delimiter, unless keepNegative=TRUE.
#'
#' This function also attempts to maintain '.' as part of a decimal number,
#' which can be problematic when sorting IP addresses, for example.
#'
#' This function is really just a wrapper function for \code{\link{mixedOrder}}
#' which does the work of defining a proper order.
#'
#' @return vector of values from x, ordered by alphanumeric logic.
#'
#' @seealso \code{\link{mixedSort}}, \code{\link[gtools]{mixedorder}},
#'    \code{\link[gtools]{mixedsort}}
#'
#' @param x input vector
#' @param blanksFirst logical whether to order blank entries before entries
#'    containing a value.
#' @param NAlast logical whether to move NA entries to the end of the sort.
#' @param keepNegative logical whether to keep '-' associated with adjacent
#'    numeric values, in order to sort them as negative values.
#' @param keepInfinite logical whether to allow "Inf" to be considered
#'    a numeric infinite value.
#' @param keepDecimal logical whether to keep the decimal in numbers,
#'    sorting as a true number and not as a version number. By default
#'    keepDecimal=FALSE, which means "v1.200" should be ordered before
#'    "v1.30". When keepDecimal=TRUE, the numeric sort considers only
#'    "1.2" and "1.3" and sorts in that order.
#' @param ignore.case logical whether to ignore uppercase and lowercase
#'    characters when defining the sort order.
#' @param sortByName logical whether to sort the vector x by names(x) instead
#'    of sorting by x itself.
#' @param verbose logical whether to print verbose output.
#' @param ... additional parameters are sent to \code{\link{mixedOrder}}.
#'
#' @examples
#' x <- c("miR-12","miR-1","miR-122","miR-1b", "miR-1a", "miR-2");
#' sort(x);
#' mixedSort(x);
#'
#' @export
mixedSort <- function
(x,
 blanksFirst=TRUE,
 NAlast=TRUE,
 keepNegative=FALSE,
 keepInfinite=FALSE,
 keepDecimal=FALSE,
 ignore.case=TRUE,
 sortByName=FALSE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to wrapper and speed up gtools mixedsort()
   ##
   ## keepNegative=FALSE will try to remove '-' from the interior of a string,
   ## for example
   ## c("miR-10", "miR-9") should sort the same as
   ## c("miR_10", "miR_9")
   #if (!require(gtools)) {
   #   stop("This mixedSort() function requires the mixedorder() function from the gtools package.");
   #}
   ## sortByName=TRUE will sort the vector using the names, returning the
   ## original vector
   ##
   if (is.null(names(x))) {
      sortByName <- FALSE;
   }
   if (sortByName) {
      if (ignore.case) {
         x[mixedOrder(toupper(names(x)), blanksFirst=blanksFirst,
            NAlast=NAlast, keepNegative=keepNegative, verbose=verbose, ...)];
      } else {
         x[mixedOrder(names(x), blanksFirst=blanksFirst,
            NAlast=NAlast, keepNegative=keepNegative, verbose=verbose, ...)];
      }
   } else {
      if (ignore.case) {
         x[mixedOrder(toupper(x), blanksFirst=blanksFirst,
            NAlast=NAlast, keepNegative=keepNegative, verbose=verbose, ...)];
         #x[mixedOrder(x, blanksFirst=blanksFirst, ignore.case=ignore.case,
         #   NAlast=NAlast, keepNegative=keepNegative, verbose=verbose, ...)];
      } else {
         x[mixedOrder(x, blanksFirst=blanksFirst,
            NAlast=NAlast, keepNegative=keepNegative, verbose=verbose, ...)];
      }
   }
}

#' order alphanumeric values keeping numeric values in proper order
#'
#' order alphanumeric values keeping numeric values in proper order
#'
#' This function is a refactor of the \code{\link[gtools]{mixedorder}}
#' function from the \code{gtools} package. It was extended to make it faster,
#' and to handle special cases slightly differently. It was driven by some
#' need to sort gene symbols, and miRNA symbols in numeric order, for example:
#' \describe{
#'    \item{test set:}{miR-12,miR-1,miR-122,miR-1b,miR-1a,miR-2}
#'    \item{\code{sort}:}{miR-1,miR-12,miR-122,miR-1a,miR-1b,miR-2}
#'    \item{\code{gtools::mixedsort}:}{miR-122,miR-12,miR-2,miR-1,miR-1a,miR-1b}
#'    \item{\code{mixedSort}:}{miR-1,miR-1a,miR-1b,miR-2,miR-12,miR-122}
#' }
#' The function does not by default recognize negative numbers as negative,
#' instead it treats '-' as a delimiter, unless keepNegative=TRUE.
#'
#' This function also attempts to maintain '.' as part of a decimal number,
#' which can be problematic when sorting IP addresses, for example.
#'
#' This function is also available as a sort function \code{\link{mixedSort}}.
#'
#' @return integer vector of orders derived from x
#'
#' @seealso \code{\link{mixedSort}}, \code{\link[gtools]{mixedorder}},
#'    \code{\link[gtools]{mixedsort}}
#'
#' @param x input vector
#' @param blanksFirst logical whether to order blank entries before entries
#'    containing a value.
#' @param NAlast logical whether to move NA entries to the end of the sort.
#' @param keepNegative logical whether to keep '-' associated with adjacent
#'    numeric values, in order to sort them as negative values.
#' @param keepInfinite logical whether to allow "Inf" to be considered
#'    a numeric infinite value.
#' @param keepDecimal logical whether to keep the decimal in numbers,
#'    sorting as a true number and not as a version number. By default
#'    keepDecimal=FALSE, which means "v1.200" should be ordered before
#'    "v1.30". When keepDecimal=TRUE, the numeric sort considers only
#'    "1.2" and "1.3" and sorts in that order.
#' @param ignore.case logical whether to ignore uppercase and lowercase
#'    characters when defining the sort order.
#' @param sortByName logical whether to sort the vector x by names(x) instead
#'    of sorting by x itself.
#' @param verbose logical whether to print verbose output.
#' @param ... additional parameters are sent to \code{\link{mixedOrder}}.
#'
#' @examples
#' x <- c("miR-12","miR-1","miR-122","miR-1b", "miR-1a","miR-2");
#' mixedOrder(x);
#' x[mixedOrder(x)];
#' mixedSort(x);
#' order(x);
#' x[order(x)];
#' sort(x);
#'
#' @export
mixedOrder <- function
(x, ..., blanksFirst=TRUE, NAlast=TRUE,
 keepNegative=FALSE,
 keepInfinite=FALSE,
 keepDecimal=FALSE,
 verbose=FALSE,
 ignore.case=TRUE,
 useCaseTiebreak=TRUE,
 returnDebug=FALSE,
 returnType=c("order", "rank"))
{
   ## Purpose is to customize the mixedorder() function from
   ## the gtools package, mainly because it is painfully slow
   ## with large data.
   ##
   ## keepNegative=FALSE will try to remove '-' from the interior of a string,
   ## for example
   ## c("miR-10", "miR-9") should sort the same as
   ## c("miR_10", "miR_9")
   ##
   ## ignore.case=TRUE will sort using uppercase entries, then break ties
   ## using the original  case.  Otherwise, uppercase entries will all appear
   ## before lowercase entries.
   ##
   ## returnType="order" will return the result of order() which
   ## breaks all ties.
   ## returnType="rank" will return the rank order, which preserves
   ## ties, intended to be used by mmixedOrder.
   returnType <- match.arg(returnType);
   if (length(x) < 1) {
      return(NULL);
   } else if (length(x) == 1) {
      return(1);
   }
   if (is.numeric(x)) {
      if (returnType %in% "order") {
         return(order(x));
      } else {
         return(match(x, factor(x)));
      }
   }
   delim <- "\\$\\@\\$";

   if (!igrepHas("character", class(x))) {
      x <- as.character(x);
   }
   which.nas <- which(is.na(x));
   which.blanks <- grep("^[ \t]*$", x);
   if (blanksFirst) {
      x[which.blanks] <- Inf;
   } else {
      x[which.blanks] <- -Inf;
   }
   if (NAlast) {
      x[which.nas] <- Inf;
   } else {
      x[which.nas] <- -Inf;
   }

   if (keepNegative) {
      if (verbose) {
         printDebug("Using keepNegative=", "TRUE", c("orange","dodgerblue"));
      }
      ## delimString represents a decimal number with optional exponential
      delimString <- paste0("([+-]{0,1}[0-9]+[.]{0,1}[0-9]*",
         "([eE][\\+\\-]{0,1}[0-9]+\\.{0,1}[0-9]*|))");
      delimited <- gsub(paste0("^", delim, "|", delim, "$"), "",
         gsub(paste0(delim, "(", delim, "){1,}"),
            delim,
         gsub(delimString,
            paste0(delim, "\\1", delim),
         gsub("([0-9])-([0-9])",
            paste0("\\1", delim, "\\2"), x))));
   } else {
      if (verbose) {
         printDebug("Using keepNegative=", "FALSE", c("orange","orangered"));
      }
      if (keepDecimal) {
         if (verbose) {
            printDebug("Using keepDecimal=", "TRUE", c("orange","dodgerblue"));
         }
         delimited <- gsub(paste0("^", delim, "|", delim, "$"), "",
            gsub(paste0(delim, "(", delim, "){1,}"),
               delim,
            gsub("([-+]{0,1}[0-9]+[.]{0,1}[0-9]*)",
               paste0(delim, "\\1", delim),
            gsub("([0-9])-([0-9])",
               paste0("\\1", delim, "\\2"),
            gsub("-", delim, x)))));
      } else {
         if (verbose) {
            printDebug("Using keepDecimal=", "FALSE", c("orange","orangered"));
         }
         delimited <- gsub(paste0("^", delim, "|", delim, "$"), "",
            gsub(paste0(delim, "(", delim, "){1,}"),
               delim,
            gsub("([-+]{0,1}[0-9]+)",
               paste0(delim, "\\1", delim),
            gsub("([0-9])-([0-9])",
               paste0("\\1", delim, "\\2"),
            gsub("-", delim, x)))));
      }
   }

   ## Split delimited strings into columns, one row per entry
   step1m <- rbindList(strsplit(delimited, delim));

   ## Split the numeric values in their own matrix
   step1mNumeric <- matrix(ncol=ncol(step1m),
      data=suppressWarnings(as.numeric(step1m)));

   ## Optionally convert things like "Inf" from infinite, back to
   ## a character value
   if (!keepInfinite && any(is.infinite(step1mNumeric))) {
      if (verbose) {
         printDebug("Using keepInfinite=", "FALSE", c("orange","orangered"));
      }
      step1mNumeric[is.infinite(step1mNumeric)] <- NA;
   } else {
      if (verbose) {
         printDebug("Using keepInfinite=", "TRUE", c("orange","dodgerblue"));
      }
   }

   ## Put non-numeric values into their own matrix, defined by non-NA
   ## cells from step1mNumeric
   step1mCharacter <- step1m;
   step1mCharacter[!is.na(step1mNumeric)] <- NA;

   ## New method, hopefully faster
   if (ignore.case) {
      if (useCaseTiebreak) {
         rankCharacter <- apply(step1mCharacter, 2, function(i){
            if (verbose) {
               printDebug("useCaseTiebreak col sort.");
            }
            iOrder <- do.call(order, list(as.numeric(factor(toupper(i))),
               as.numeric(factor(i))));
            i2 <- i[iOrder];
            iRank1 <- match(i, unique(i2));
            return(iRank1);
         });
      } else {
         rankCharacter <- apply(step1mCharacter, 2, function(i){
            if (verbose) {
               printDebug("ignore.case col sort.");
            }
            iRank <- as.numeric(factor(toupper(i)));
            if (verbose) {
               printDebug(head(iRank, 40));
            }
            iRank;
         });
      }
   } else {
      rankCharacter <- apply(step1mCharacter, 2, function(i){
         if (verbose) {
            printDebug("as-is col sort.");
         }
         iRank <- as.numeric(factor(i));
         if (verbose) {
            printDebug(head(iRank, 40));
         }
         iRank;
      });
   }
   rankCharacter[is.na(step1mCharacter)] <- NA;
   rankNumeric <- apply(step1mNumeric, 2, rank, na.last="keep");
   rankNumeric[is.na(rankNumeric) | !is.na(rankCharacter)] <- 0;

   if (verbose) {
      printDebug("head(delimited):");
      print(head(data.frame(delimited=delimited)));
      printDebug("step1m:");
      print(head(step1m, 20));
      printDebug("step1mCharacter:");
      print(head(step1mCharacter, 20));
      printDebug("rankCharacter:");
      print(head(rankCharacter, 20));
      printDebug("step1mNumeric:");
      print(head(step1mNumeric, 20));
      printDebug("rankNumeric:");
      print(head(rankNumeric, 20));
      printDebug("ncol(rankNumeric):", ncol(rankNumeric));
   }

   ## Make character ranks higher than any existing numerical ranks
   ## Fill with the adjusted character string ranks
   ## some cells are NA here since they had a numeric value
   rankOverall <- rankCharacter + 1 + max(rankNumeric, na.rm=TRUE);
   ## Fill NA cells with the numeric rank
   rankOverall[is.na(rankOverall)] <- rankNumeric[is.na(rankOverall)];
   if (verbose) {
      printDebug("rankOverall:");
      print(head(rankOverall, 40));
   }
   ## TODO: add tiebreak using the original string
   #printDebug("dim(rankOverall):", dim(rankOverall));
   #rankOverall <- cbind(rankOverall, rank(x, ties.method="first"));
   #printDebug("head(x,10):", head(x,10));
   #printDebug("dim(rankOverall):", dim(rankOverall));

   if (verbose) {
      printDebug("rankOverall:");
      rownames(rankOverall) <- makeNames(x);
      print(head(rankOverall, 40));
   }

   ## Return the order(), which always gives unique values,
   ## i.e. wouldn't lend itself well to combining multiple
   ## outputs from mixedOrder() and using secondary values
   ## as a tiebreaker
   if (returnType %in% "order") {
      if (verbose) {
         printDebug("Returning order() tie-breaks resolved.");
      }
      retVal <- do.call(order, as.data.frame(rankOverall));
   } else {
      if (verbose) {
         printDebug("Returning order() tie-breaks preserved.");
      }
      iOrder <- do.call(order, as.data.frame(rankOverall));
      ## By using match, duplicated values are all ranked by the
      ## first occurrence, which lets this rank be useful when
      ## combining ranks across multiple columns
      retVal <- match(x, x[iOrder]);
   }
   if (returnDebug) {
      attr(retVal, "mixedSortNcol") <- ncol(rankNumeric);
      attr(retVal, "rankOverall") <- rankOverall;
   }
   return(retVal);
}

#' order alphanumeric values from a list
#'
#' order alphanumeric values from a list
#'
#' This function is a minor extension to \code{\link{mixedOrder}} in that
#' it accepts list input, similar to how \code{\link{order}} operates.
#' This function is mainly useful when sorting something like a data.frame,
#' where ties in column 1 should be maintained then broken by non-equal
#' values in column 2.
#'
#' In fact, \code{\link{mixedSortDF}} calls this \code{mmixedOrder} function,
#' in order to sort a data.frame properly by column.
#'
#' See \code{\link{mixedOrder}} and \code{\link{mixedSort}} for a better
#' description of how the sort order logic operates.
#'
#' @return integer vector of row orders
#'
#' @seealso \code{\link{mixedOrder}}, \code{\link{mixedSort}}
#'
#' @param ... parameters treated as a list of vectors to be ordered in
#'    proper order, based upon the mechanism by \code{\link{order}}.
#' @param na.last,decreasing,verbose,ignore.case parameters sent to
#'    \code{\link{mixedOrder}}.
#' @param matrixAsDF logical if \code{...} supplies only one matrix object,
#'    whether to convert it to data.frame, the coerce to a list, before
#'    processing. By default, in the event only one matrix object is supplied,
#'    this conversion is performed, in order to define a sort order based upon
#'    each column in order.
#'
#' @export
mmixedOrder <- function
(..., na.last=TRUE, decreasing=FALSE, verbose=FALSE, ignore.case=TRUE,
 matrixAsDF=TRUE)
{
   ## Purpose is to provide a wrapper for mixedOrder which allows multiple lists,
   ## similar to how order() works.
   ## This change enables mmixedOrder() to work properly on data.frames, while
   ## maintaining sort order of factors
   ##
   ## ignore.case=TRUE will convert to uppercase for initial sort, then use
   ## the original data to break ties.
   ## ignore.case=FALSE will use default R sort, which puts uppercase before
   ## all lowercase entries -- which always seems odd.
   ##
   ## matrixAsDF=TRUE will convert matrix into data.frame prior to running mmixedOrder(),
   ## which enables it to sort by column, consistent with how data.frames are ordered.
   z <- list(...);
   if (verbose) {
      printDebug("head(names(z),10):", head(names(z),10), c("orange", "lightblue"));
      printDebug("length(z):", length(z), c("orange", "lightblue"));
   }
   if (length(z) == 1) {
      if (igrepHas("matrix", class(z[[1]])) && matrixAsDF) {
         z[[1]] <- as.data.frame(z[[1]]);
      }
      if (is.list(z[[1]])) {
         z <- as.list(z[[1]]);
      }
   }
   ## decreasing can now take multiple arguments, so each entry may be separately ordered
   decreasing <- rep(decreasing, length.out=length(z));

   z1 <- lapply(seq_along(z), function(iNum){
      if (verbose) {
         printDebug("mmixedOrder iNum: ", iNum, c("orange", "lightblue"));
      }
      i <- z[[iNum]];
      iSign <- (-2*decreasing[iNum])+1;
      if (verbose) {
         printDebug("iSign:", iSign);
      }
      if (class(i) %in% c("numeric", "factor", "ordered")) {
         as.numeric(i) * iSign;
      } else {
         x2u <- unique(i);
         x2uo <- mixedOrder(x2u, ignore.case=ignore.case, ...);
         x2uof <- factor(i, levels=x2u[x2uo]);
         x2o <- as.numeric(x2uof) * iSign;
      }
   });
   do.call("order", c(z1, na.last=na.last, decreasing=FALSE));
}

#' sort data.frame keeping numeric values in proper order
#'
#' sort data.frame keeping numeric values in proper order
#'
#' This function is a wrapper around \code{\link{mmixedOrder}} so it operates
#' on data.frame columns in the proper order, using logic similar that used
#' by \code{\link{order}} when operating on data.frames. The sort order logic
#' is fully described in \code{\link{mixedSort}} and \code{\link{mixedOrder}}.
#'
#' @return data.frame whose rows are ordered using \code{\link{mmixedOrder}}
#'
#' @seealso \code{\link{mixedOrder}}, \code{\link{mixedSort}}
#'
#' @param df data.frame input
#' @param byCols integer vector referring to the order of columns to be
#'    used by \code{\link{mmixedOrder}} to order the data.frame. Note that
#'    negative values will reverse the sort order for the corresponding
#'    column number.
#'    byCols can also be a character vector of values in colnames(df),
#'    optionally including prefix "-" to reverse the sort. Note that the
#'    parameter \code{decreasing} can also be used to specify columns
#'    to have reverse sort.
#' @param na.last logical whether NA values should be ranked last
#' @param decreasing NULL or a logical vector indicating which columns
#'    in \code{byCols} should be sorted in decreasing order. By default, the
#'    sign(byCols) is used to define the sort order of each column, but it
#'    can be explicitly overridden with this \code{decreasing} parameter.
#' @param useRownames logical whether to use \code{rownames(df)} as a last
#'    tiebreaker in the overall rank ordering. This parameter has the primary
#'    effect of assuring a reproducible result, provided the rownames are
#'    consistently defined, or if rownames are actually row numbers.
#' @param verbose logical whether to print verbose output
#' @param ... additional arguments passed to \code{mmixedOrder(...)}
#'
#' @examples
#' # start with a vector of miRNA names
#' x <- c("miR-12","miR-1","miR-122","miR-1b", "miR-1a","miR-2");
#' # add some arbitrary group information
#' g <- rep(c("Air", "Treatment", "Control"), 2);
#' # create a data.frame
#' df <- data.frame(group=g, miRNA=x, stringsAsFactors=FALSE);
#'
#' # sort the data.frame by each column
#' mixedSortDF(df);
#'
#' # mixedSort respects factor order, so reorder the factor levels
#' # to demonstrate. "Control" should come first, for example.'
#' gf <- factor(g, levels=c("Control","Air", "Treatment"));
#' df2 <- data.frame(groupfactor=gf, miRNA=x, stringsAsFactors=FALSE);
#'
#' # now the sort properly keeps the group factor levels in order,
#' # which also sorting the miRNA names in their proper order.
#' mixedSortDF(df2);
#'
#' x <- data.frame(l1=letters[1:10], l2=rep(letters[1:2+10], 5), L1=LETTERS[1:10], L2=rep(LETTERS[1:2+20], each=5))
#' @export
mixedSortDF <- function
(df,
 byCols=1:ncol(df),
 na.last=TRUE,
 decreasing=NULL,
 useRownames=TRUE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to order a data.frame using mmixedOrder()
   ## byCols is a vector of column numbers, with negative values
   ## referring to columns for decreasing order
   ##
   ## If given a matrix, convert to data.frame so it is treated as a list
   ## in mmixedOrder()
   ##
   ## useRownames=TRUE will use rownames as a tiebreaker, the last in the
   ## sort precedence

   ## Handle character input for byCols
   if (igrepHas("character", class(byCols))) {
      if (length(colnames(df)) == 0) {
         ## If given colnames, but colnames(df) is empty, we cannot proceed
         stop("mixedSortDF(): byCols is supplied as character by colnames(df) is empty.");
      }
      byMatch <- match(byCols, colnames(df));

      ## If any colnames are not matched, check for prefix "-"
      byMatchNA <- is.na(byMatch);
      if (any(byMatchNA)) {
         negByCols <- grepl("^-", byCols);
         byMatchNAneg <- (byMatchNA & negByCols)
         ## Make sure the prefix "-" occurs for NA match
         if (any(byMatchNAneg)) {
            ## Remove prefix "-" from unmatched "-" prefix colnames
            byCols[byMatchNAneg] <- gsub("^-",
               "",
               byCols[byMatchNAneg]);
            byMatch <- match(byCols, colnames(df));
            byMatch[byMatchNAneg] <- byMatch[byMatchNAneg] * -1;
         }
      }
      if (verbose) {
         printDebug("mixedSortDF(): ",
            "Converted byCols to integer.");
      }
      #byCols <- rmNA(byMatch);
   }

   ## Determine byCols to keep:
   ## each column only once
   ## is not NA
   ## is no greater than ncol(df)
   ## is not a list column
   dfColnums <- seq_len(ncol(df));
   byColsKeep <- (!is.na(byCols) &
      seq_along(byCols) %in% match(unique(abs(byCols)), dfColnums));
   byColsSortable <- sapply(byCols[byColsKeep], function(i){
      !igrepHas("list", class(df[[abs(i)]]))
   });
   byColsKeep[byColsKeep] <- byColsSortable;

   ## Apply byCols to keep
   byCols <- byCols[byColsKeep];
   if (length(decreasing) > 0) {
      decreasingV <- rep(-1*decreasing, length.out=length(byCols));
      decreasingV <- (decreasingV[byColsKeep] * sign(byCols)) < 0;
   } else {
      decreasingV <- (byCols < 0);
   }

   if (verbose) {
      printDebug("mixedSortDF(): ",
         "byCols:",
         byCols,
         fgText=list("yellow", "yellow",
            ifelse(byCols < 0,
               "red",
               "lightgreen")));
      printDebug("mixedSortDF(): ",
         "decreasing:",
         decreasingV,
         fgText=list("yellow", "yellow",
            ifelse(decreasingV,
               "red",
               "lightgreen")));
   }
   if (igrepHas("matrix", class(df))) {
      if (useRownames && !is.null(rownames(df))) {
         dfOrder <- mmixedOrder(
            data.frame(check.names=FALSE,
               stringsAsFactors=FALSE,
               df[,abs(byCols),drop=FALSE],
               rowNamesX=rownames(df)),
            decreasing=decreasingV,
            na.last=na.last,
            ...);
         df <- df[dfOrder,,drop=FALSE];
      } else {
         dfOrder <- mmixedOrder(
            as.data.frame(df[,abs(byCols),drop=FALSE]),
            decreasing=decreasingV,
            na.last=na.last,
            ...);
      }
      df <- df[dfOrder,,drop=FALSE];
   } else {
      if (useRownames && !is.null(rownames(df))) {
         dfOrder <- mmixedOrder(
            data.frame(check.names=FALSE,
               stringsAsFactors=FALSE,
               df[,abs(byCols),drop=FALSE],
               rowNamesX=rownames(df)),
            decreasing=decreasingV,
            na.last=na.last,
            ...);
         df <- df[dfOrder,,drop=FALSE];
      } else {
         dfOrder <- mmixedOrder(df[,abs(byCols),drop=FALSE],
            decreasing=decreasingV,
            na.last=na.last,
            ...);
         df <- df[dfOrder,,drop=FALSE];
      }
   }
}

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
#' @param pattern the grep pattern to use with \code{\link[base]{grep}}
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
#' @seealso \code{\link[base]{grep}}
#'
#' @examples
#' a <- c("data.frame","data_frame","tibble","tbl");
#' igrepHas("Data.*Frame", a);
#' igrepHas("matrix", a);
#'
#' @family jam grep functions
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
      length(grep(pattern=pattern,
         x=x,
         ignore.case=ignore.case,
         ...)) >= as.integer(minCount);
   }
}

#' case-insensitive grep, returning values
#'
#' case-insensitive grep, returning values
#'
#' This function is a simple wrapper around \code{\link[base]{grep}} which
#' runs in case-insensitive mode, and returns matching values. It is
#' particularly helpful when grabbing values from a vector.
#'
#' @param ...,value,ignore.case parameters sent to \code{\link[base]{grep}}
#'
#' @return vector of matching values
#'
#' @examples
#' V <- paste0(LETTERS[1:5], LETTERS[4:8]);
#' vigrep("d", V);
#'
#' @family jam grep functions
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
#' This function is a simple wrapper around \code{\link[base]{grep}} which
#' returns matching values. It is
#' particularly helpful when grabbing values from a vector, but where the
#' case (uppercase or lowercase) is known.
#'
#' @param ...,value,ignore.case parameters sent to \code{\link[base]{grep}}
#'
#' @return vector of matching values
#'
#' @examples
#' V <- paste0(LETTERS[1:5], LETTERS[4:8]);
#' vgrep("D", V);
#' vgrep("d", V);
#' vigrep("d", V);
#'
#' @family jam grep functions
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
#' This function is a simple wrapper around \code{\link[base]{grep}} which
#' runs in case-insensitive mode. It is mainly used to save keystrokes,
#' but is consistently named alongside \code{\link{vgrep}} and
#' \code{\link{vigrep}}.
#'
#' @param ...,ignore.case parameters sent to \code{\link[base]{grep}}
#'
#' @return vector of matching indices
#'
#' @examples
#' V <- paste0(LETTERS[1:5], LETTERS[4:8]);
#' igrep("D", V);
#' igrep("d", V);
#' vigrep("d", V);
#'
#' @family jam grep functions
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
#' This function is a simple wrapper around \code{\link[base]{grep}} which
#' runs in case-insensitive mode, and returns unmatched entries.
#' It is mainly used to save keystrokes,
#' but is consistently named alongside \code{\link{vgrep}} and
#' \code{\link{vigrep}}, and quite helpful for writing concise code.
#'
#' @param ...,ignore.case,invert parameters sent to \code{\link[base]{grep}}
#'
#' @return vector of non-matching indices
#'
#' @examples
#' V <- paste0(LETTERS[1:5], LETTERS[4:8]);
#' unigrep("D", V);
#' igrep("D", V);
#'
#' @family jam grep functions
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
#' This function is a simple wrapper around \code{\link[base]{grep}} which
#' runs in case-insensitive mode, and returns unmatched values.
#' It is mainly used to save keystrokes,
#' but is consistently named alongside \code{\link{vgrep}} and
#' \code{\link{vigrep}}, and quite helpful for writing concise code.
#' It is particularly useful for removing unwanted entries from a long
#' vector, for example removing accession numbers from a long
#' vector of gene symbols in order to review gene annotations.
#'
#' @param ...,ignore.case,value,invert parameters sent to \code{\link[base]{grep}}
#'
#' @return vector of non-matching indices
#'
#' @examples
#' V <- paste0(LETTERS[1:5], LETTERS[4:8]);
#' unigrep("D", V);
#' igrep("D", V);
#'
#' @family jam grep functions
#'
#' @export
unvigrep <- function
(..., ignore.case=TRUE, value=TRUE, invert=TRUE)
{
   ## purpose is to un-grep, return non-hits in case-insensitive fashion
   grep(..., ignore.case=ignore.case, value=value, invert=invert);
}

#' provigrep: progressive case-insensitive value-grep
#'
#' case-insensitive value-grep for a vector of patterns
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
#' @param patterns `character` vector of regular expression patterns,
#'    ultimately passed to `base::grep()`.
#' @param x `character` vector that is the subject of `base::grep()`.
#' @param maxValues `integer` or NULL, the maximum matching entries to
#'    return per grep pattern. Note that each grep pattern may match multiple
#'    values, and values are only returned at most once each, so restricting
#'    items returned by one grep pattern may allow an item to be matched
#'    by subsequent patterns, see examples. This argument is most commonly
#'    used with `maxValues=1` which returns only the first matching entry
#'    per pattern.
#' @param sortFunc `function` or NULL, used to sort entries within each set of
#'    matching entries. Use NULL to avoid sorting entries.
#' @param rev `logical` whether to reverse the order of matching entries. Use
#'    TRUE if you would like entries matching the patterns to be placed last,
#'    and entries not matching the grep patterns to be placed first. This
#'    technique is effective at placing "noise names" at the end of a long
#'    vector, for example.
#' @param returnType `character` indicating whether to return a vector or list.
#'    A list will be in order of the grep patterns, using empty elements to
#'    indicate when no entries matched each pattern. This output is useful
#'    when you would like to know which patterns matched specific entries.
#' @param ignore.case `logical` parameter sent to \code{\link[base]{grep}}, TRUE
#'    runs in case-insensitive mode, as by default.
#' @param value `logical` indicating whether to return the matched value,
#'    or when `value=FALSE` the index position is returned.
#' @param ... additional arguments are passed to `vigrep()`.
#'
#' @examples
#' # a rather comical example
#' # set up a test set with labels containing several substrings
#' set.seed(1);
#' testTerms <- c("robot","tree","dog","mailbox","pizza","noob");
#' testWords <- pasteByRow(t(combn(testTerms,3)));
#'
#' # now pull out entries matching substrings in order
#' provigrep(c("pizza", "dog", "noob", "."), testWords);
#' # more detail about the sort order is shown with returnType="list"
#' provigrep(c("pizza", "dog", "noob", "."), testWords, returnType="list");
#' # rev=TRUE will reverse the order of the list
#' provigrep(c("pizza", "dog", "noob", "."), testWords, returnType="list", rev=TRUE);
#' provigrep(c("pizza", "dog", "noob", "."), testWords, rev=TRUE);
#'
#' # another example showing ordering of duplicated entries
#' set.seed(1);
#' x <- paste0(
#'    sample(letters[c(1,2,2,3,3,3,4,4,4,4)]),
#'    sample(1:5));
#' x;
#' # sort by letter
#' provigrep(letters[1:4], x)
#'
#' # show more detail about how the sort is performed
#' provigrep(letters[1:4], x, returnType="list")
#'
#' # rev=TRUE will reverse the order of pattern matching
#' # which is most useful when "." is the last pattern:
#' provigrep(c(letters[1:3], "."), x, returnType="list")
#' provigrep(c(letters[1:3], "."), x, returnType="list", rev=TRUE)
#'
#' # example demonstrating maxValues
#' # return in list format
#' provigrep(c("[ABCD]", "[CDEF]", "[FGHI]"), LETTERS, returnType="list")
#'
#' # maxValues=1
#' provigrep(c("[ABCD]", "[CDEF]", "[FGHI]"), LETTERS, returnType="list", maxValues=1)
#' provigrep(c("[ABCD]", "[CDEF]", "[FGHI]"), LETTERS, returnType="list", maxValues=1, value=FALSE)
#' proigrep(c("[ABCD]", "[CDEF]", "[FGHI]"), LETTERS, maxValues=1)
#'
#' @family jam grep functions
#'
#' @export
provigrep <- function
(patterns,
 x,
 maxValues=NULL,
 sortFunc=c,
 rev=FALSE,
 returnType=c("vector", "list"),
 ignore.case=TRUE,
 value=TRUE,
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
   x_unique <- make.unique(as.character(x),
      sep="_v");

   ## Iterate each grep pattern
   valueSetL <- lapply(patterns, function(i){
      z <- vigrep(pattern=i,
         x=x,
         value=TRUE,
         ignore.case=ignore.case,
         ...);
      if (length(sortFunc) > 0 && !identical(c, sortFunc)) {
         ## If sortFunc is not c(), then run it
         z <- sortFunc(z);
      }
      ## Here the values are converted to index positions
      match(make.unique(as.character(z), sep="_v"),
         x_unique);
   });

   ## Apply maxValues
   if (length(maxValues) > 0) {
      valueSetL <- jamba::heads(valueSetL,
         n=maxValues);
   }

   ## Make each item only represented once across the list
   if (length(names(patterns)) == 0) {
      names(valueSetL) <- makeNames(patterns);
   } else {
      names(valueSetL) <- names(patterns);
   }
   f1 <- factor(names(valueSetL),
      levels=names(valueSetL));
   m1 <- match(
      make.unique(sep="_v",
         as.character(unlist(valueSetL))),
      as.character(seq_along(x)));
   r1 <- rep(f1, lengths(valueSetL));
   if (value && "list" %in% returnType) {
      valueSetL <- split(x[m1[!is.na(m1)]], r1[!is.na(m1)]);
   } else {
      valueSetL <- split(m1[!is.na(m1)], r1[!is.na(m1)]);
   }

   ## Optionally reverse the list
   if (rev) {
      valueSetL <- rev(valueSetL);
   }

   ## Optionally return the list format
   if ("list" %in% returnType) {
      return(valueSetL);
   }

   if (value) {
      valueSet <- x[unique(unlist(valueSetL))];
   } else {
      valueSet <- unique(unlist(valueSetL));
   }

   return(valueSet);
}

#' proigrep: progressive case-insensitive grep
#'
#' case-insensitive grep for a vector of patterns
#'
#' @rdname provigrep
#'
#' @export
proigrep <- function
(...,
 value=FALSE)
{
   provigrep(...,
      value=value);
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
#' @param nullValue optional value used to replace NULL entries in
#'    the input list, useful especially when the data was produced
#'    by `strsplit()` with `""`. Use `nullValue=""` to replace `NULL`
#'    with `""` and preserve the original list length. Otherwise when
#'    `nullValue=NULL` any empty entries will be silently dropped.
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
#' @family jam list functions
#'
#' @export
rbindList <- function
(x,
 emptyValue="",
 nullValue=NULL,
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
      if (length(nullValue) > 0) {
         x[xLslen == 0] <- head(nullValue, 1);
         xLslen[xLslen == 0] <- 1;
      }
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
#' @family jam string functions
#'
#' @param x character vector to be used when defining names. All other
#'    vector types will be coerced to character prior to use.
#' @param unique argument which is ignored, included only for
#'    compatibility with `base::make.names`. All results from
#'    `makeNames()` are unique.
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
#'    names to become `<NA>`, which can cause problems with some downstream
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
   if (any(c("factor", "ordered") %in% class(x))) {
      x <- as.character(x);
   }
   if (keepNA && any(is.na(x))) {
      x <- rmNA(x,
         naValue="NA");
   }

   ## First check for duplicates using anyDuplicated()
   ## version 0.0.35.900, this change speeds assignment
   ## in large vectors when most entries are not duplicated.
   dupes <- duplicated(x);

   ## Convert entries to a named count of occurences of each entry
   if (any(dupes)) {
      xSubDupes <- table(x[dupes]) + 1;
      maxCt <- max(c(1,xSubDupes));
      xSubOnes <- setNames(rep(1, sum(!dupes)), x[!dupes]);
      xSub <- c(xSubOnes, xSubDupes);
   } else {
      xSub <- setNames(rep(1, sum(!dupes)), x[!dupes]);
      maxCt <- 1;
   }
   ## version 0.0.34.900 and previous used the method below
   #xSub <- table(as.character(x));

   ## Vector of counts to be used
   versionsV <- as.integer(renameFirst):maxCt + startN - 1;

   ## If using letters, define the set of letter upfront to save processing
   if (igrepHas("letters", numberStyle)) {
      if (numberStyle %in% "letters") {
         useLetters <- letters[1:26];
         zeroVal <- "A";
      } else {
         useLetters <- LETTERS[1:26];
         zeroVal <- "a";
      }
      num2letters <- colNum2excelName(versionsV,
         useLetters=useLetters,
         zeroVal=zeroVal,
         ...);
      versionsV <- num2letters;
   }
   if (doPadInteger) {
      versionsV <- padInteger(versionsV,
         useNchar=useNchar,
         ...);
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
#' names. It is helpful to run this function inside an \code{\link[base]{lapply}}
#' function call, which by default maintains names, but does not assign
#' names if the input data did not already have them.
#'
#' When used with a data.frame, it is particularly convenient to pull out
#' a named vector of values. For example, log2 fold changes by gene, where
#' the gene symbols are the name of the vector.
#'
#' \code{nameVector(genedata[,c("Gene","log2FC")])}
#'
#' @return vector with names defined
#'
#' @family jam string functions
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
#' \code{\link[base]{lapply}} functions which maintain the name of a vector
#' in its output. The reason to run \code{\link[base]{lapply}} using names
#' is so the lapply function is operating only on the name and not the
#' data it references, which can be convenient when the name of the element
#' is useful to known inside the function body. The reason to name the names,
#' is so the list object returned by \code{\link[base]{lapply}} is also named
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
#' @family jam string functions
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
(x,
 makeNamesFunc=makeNames,
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
#' @family jam practical functions
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
#' @family jam practical functions
#'
#' @param x vector input
#' @param naValue NULL or single replacement value for NA entries. If NULL,
#'    then NA entries are removed from the result.
#' @param rmNULL logical whether to replace NULL entries with `nullValue`
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
   if (length(x) == 0) {
      if (rmNULL) {
         x <- nullValue;
      }
      return(x);
   }

   if (!class(x) %in% "list" && rmInfinite && any(is.infinite(x))) {
      x <- rmInfinite(x,
         infiniteValue=infiniteValue);
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
#' This function removes any positive or negative infinite numerical
#' values, optionally replacing them with a given value or NA.
#'
#' @return numeric vector with infinite values either removed, or
#'    replaced with the supplied value.
#'
#' @family jam practical functions
#'
#' @param x vector input
#' @param infiniteValue NULL to remove Infinite values, or a replacement value
#' @param ... additional parameters are ignored
#'
#' @export
rmInfinite <- function
(x,
 infiniteValue=NULL,
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
#' This function is a refactor of the \code{gtools::mixedsort}
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
#' @family jam sort functions
#' @family jam string functions
#'
#' @param x input vector
#' @param blanksFirst logical whether to order blank entries before entries
#'    containing a value.
#' @param na.last `logical` indicating whether to move NA entries at
#'    the end of the sort.
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
#' @param NAlast `logical` deprecated in favor of argument `na.last`
#'    for consistency with `base::sort()`.
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
 na.last=NAlast,
 keepNegative=FALSE,
 keepInfinite=FALSE,
 keepDecimal=FALSE,
 ignore.case=TRUE,
 sortByName=FALSE,
 verbose=FALSE,
 NAlast=TRUE,
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
         x[mixedOrder(toupper(names(x)),
            blanksFirst=blanksFirst,
            na.last=na.last,
            keepNegative=keepNegative,
            keepInfinite=keepInfinite,
            keepDecimal=keepDecimal,
            verbose=verbose,
            ...)];
      } else {
         x[mixedOrder(names(x),
            blanksFirst=blanksFirst,
            na.last=na.last,
            keepNegative=keepNegative,
            keepInfinite=keepInfinite,
            keepDecimal=keepDecimal,
            verbose=verbose,
            ...)];
      }
   } else {
      if (ignore.case) {
         x[mixedOrder(toupper(x),
            blanksFirst=blanksFirst,
            na.last=na.last,
            keepNegative=keepNegative,
            keepInfinite=keepInfinite,
            keepDecimal=keepDecimal,
            verbose=verbose,
            ...)];
      } else {
         x[mixedOrder(x,
            blanksFirst=blanksFirst,
            na.last=na.last,
            keepNegative=keepNegative,
            keepInfinite=keepInfinite,
            keepDecimal=keepDecimal,
            verbose=verbose,
            ...)];
      }
   }
}

#' order alphanumeric values keeping numeric values in proper order
#'
#' order alphanumeric values keeping numeric values in proper order
#'
#' This function is a refactor of `gtools::mixedorder()` which was
#' the source of inspiration for this function, thanks to Gregory R. Warnes!
#' This function was designed to improve the efficiency for large vectors,
#' and to handle special cases slightly differently. It was driven by some
#' need to sort gene symbols, and miRNA symbols in numeric order, for example:
#'
#' \describe{
#'    \item{test set:}{miR-12,miR-1,miR-122,miR-1b,miR-1a,miR-2}
#'    \item{\code{sort}:}{miR-1,miR-12,miR-122,miR-1a,miR-1b,miR-2}
#'    \item{\code{gtools::mixedsort}:}{miR-122,miR-12,miR-2,miR-1,miR-1a,miR-1b}
#'    \item{\code{mixedSort}:}{miR-1,miR-1a,miR-1b,miR-2,miR-12,miR-122}
#' }
#'
#' This function does not by default consider negative numbers as negative,
#' instead it treats '-' as a delimiter, unless keepNegative=TRUE.
#'
#' When `keepNegative=TRUE` this function also recognizes scientific
#' notation, for example `"1.23e-2"` will be treated as numeric `0.0123`.
#' Note that `keepNegative=TRUE` also forces `keepDecimal=TRUE`.
#'
#' When `keepDecimal=TRUE` this function maintains numeric values that
#' include one `"."`.
#'
#' This function is the core of a family of mixedSort functions:
#'
#' \describe{
#'    \item{`mixedSort()`}{Applies `mixedOrder()` to an input vector.}
#'    \item{`mixedSorts()`}{Applies `mixedOrder()` to a list of vectors,
#'       returning the list where each vector is independently sorted.}
#'    \item{`mixedSortDF()`}{Applies `mixedOrder()` to each column of a
#'    `data.frame` or comparable object, optionally specifying the order
#'    of columns used during the sort.}
#' }
#'
#' Extra thanks to Gregory R. Warnes for the `gtools::mixedorder()`
#' that proved to be so useful it ultimately inspired this function.
#'
#' @return integer vector of orders derived from x, or when `returnType="rank"`
#' an integer vector of ranks, allowing ties. The rank is therefore
#' valid for use in chains, such as multiple columns of a data.frame.
#'
#' @family jam sort functions
#' @family jam string functions
#'
#' @seealso `gtools::mixedorder()`, `gtools::mixedsort()`
#'
#' @param x input vector
#' @param blanksFirst logical whether to order blank entries before entries
#'    containing a value.
#' @param na.last logical whether to move NA entries to the end of the sort.
#'    When `na.last=TRUE` then `NA` values will always be last, even following
#'    blanks and infinite values. When `na.last=FALSE` then `NA` values
#'    will always be first, even before blanks and negative infinite values.
#' @param keepNegative logical whether to keep '-' associated with adjacent
#'    numeric values, in order to sort them as negative values. Note that
#'    `keepNegative=TRUE` also forces `keepDecimal=TRUE`, and enables
#'    matching of scientific notation such as `-1.23e-10` as a numeric
#'    value. When `keepNegative=FALSE` the dash `"-"` is treated as
#'    a common delimiter.
#' @param keepInfinite logical whether to allow "Inf" in the input `x`
#'    to be considered a numeric infinite value. Note that `"-Inf"` is
#'    only treated as a negative infinite value when `keepNegative=TRUE`.
#'    Also note that `"Inf"` is only recognized as infinite when it
#'    appears between non-character delimiters, and not part of a
#'    larger character string like `"Information"`. Be careful
#'    with `keepInfinite=TRUE` when sorting gene symbols, there are
#'    gene symbols like `"Inf3"` which should not be sorted as infinite.
#'    Lastly, infinite values are sorted at the end, notably after
#'    all character values which differs from some mixed sorting
#'    algorithms.
#' @param keepDecimal logical whether to keep the decimal in numbers,
#'    sorting as a true number and not as a version number. By default
#'    `keepDecimal=FALSE``, which means "v1.200" will be ordered after
#'    "v1.30", since it considers `"1.200"` effectively as `1` and `200`,
#'    and `"1.30"` effectively as `1` and `30`.
#'    When `keepDecimal=TRUE`, the numeric sort orders `"v1.200"` before
#'    `"v1.30"`.
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
#' ## Complex example including NA, blanks, and infinite "Inf"
#' x <- c("Inf",
#'    "+Inf12",
#'    NA,
#'    "-Inf14",
#'    "-",
#'    "---",
#'    "Jnf12",
#'    "Hnf12",
#'    "--",
#'    "Information");
#' ## By default, strings are sorted as-is, "Hnf" before "Inf" before "Jnf"
#' ## blanks are first, NA values are last
#' x[mixedOrder(x)];
#'
#' ## blanks are last, but before NA values which are also last
#' x[mixedOrder(x, blanksFirst=FALSE)];
#'
#' ## Recognize infinite, but not the negative sign
#' ## Now infinite values are at the end, ordered by the number that follows.
#' x[mixedOrder(x, blanksFirst=FALSE, keepInfinite=TRUE)]
#'
#' ## Now also recognize negative infinite values,
#' ## which puts "-Inf14" at the very beginning.
#' x[mixedOrder(x, blanksFirst=FALSE, keepInfinite=TRUE, keepNegative=TRUE)]
#'
#'
#' @export
mixedOrder <- function
(x,
 ...,
 na.last=NAlast,
 blanksFirst=TRUE,
 keepNegative=FALSE,
 keepInfinite=FALSE,
 keepDecimal=FALSE,
 verbose=FALSE,
 ignore.case=TRUE,
 useCaseTiebreak=TRUE,
 returnDebug=FALSE,
 returnType=c("order", "rank"),
 NAlast=TRUE)
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
   ## Artificial delimiter inserted between defined break positions
   delim <- "\\$\\@\\$";

   if (!igrepHas("character", class(x))) {
      x <- as.character(x);
   }
   which_nas <- which(is.na(x));
   which_blanks <- grep("^[-+ \t]*$", x);
   if (1 == 2) {
      if (blanksFirst) {
         x[which_blanks] <- -Inf;
      } else {
         x[which_blanks] <- Inf;
      }
      if (na.last) {
         x[which_nas] <- Inf;
      } else {
         x[which_nas] <- -Inf;
      }
   }

   if (keepNegative) {
      if (verbose) {
         printDebug("mixedOrder(): ",
            "Using keepNegative:", "TRUE",
            fgText=c("darkorange1","dodgerblue","cyan"));
         printDebug("mixedOrder(): ",
            "Therefore using keepDecimal:", "TRUE",
            fgText=c("darkorange1","dodgerblue","cyan"));
      }
      ## delimString represents a decimal number with optional exponential
      delimString <- paste0(
         "([+-]{0,1}([0-9]+[.]{0,1}[0-9]*|[0-9]*[.]{0,1}[0-9]+)",
         "([eE][-+]{0,1}[0-9]+|))");
      delimited <- gsub(
            paste0("^", delim, "|", delim, "$"),
            "",
         gsub(
            paste0(delim, "(", delim, "){1,}"),
            delim,
         gsub(
            delimString,
            paste0(delim, "\\1", delim),
         gsub(
            "([0-9])[-+]*([-+][0-9])",
            paste0("\\1", delim, "\\2"),
            x))));
   } else {
      if (verbose) {
         printDebug("mixedOrder(): ",
            "Using keepNegative:", "FALSE",
            fgText=c("darkorange1","dodgerblue","orangered"));
      }
      if (keepDecimal) {
         if (verbose) {
            printDebug("mixedOrder(): ",
               "Using keepDecimal:", "TRUE",
               fgText=c("darkorange1","dodgerblue","cyan"));
         }
         delimited <- gsub(
               paste0("^", delim, "|", delim, "$"),
               "",
            gsub(
               paste0(delim, "(", delim, "){1,}"),
               delim,
            gsub(
               "([0-9]+[.]{0,1}[0-9]*|[0-9]*[.]{0,1}[0-9]+)",
               paste0(delim, "\\1", delim),
            #gsub(
            #   "([0-9])-([0-9])",
            #   paste0("\\1", delim, "\\2"),
            gsub(
               "[-+]",
               delim,
               x))));
      } else {
         if (verbose) {
            printDebug("mixedOrder(): ",
               "Using keepDecimal:", "FALSE",
               fgText=c("darkorange1","dodgerblue","orangered"));
         }
         delimited <- gsub(
               paste0("^", delim, "|", delim, "$"),
            "",
            gsub(
               paste0(delim, "(", delim, "){1,}"),
               delim,
            gsub(
               "([0-9]+)",
               paste0(delim, "\\1", delim),
            #gsub(
            #   "([0-9])-([0-9])",
            #   paste0("\\1", delim, "\\2"),
            gsub(
               "[-+]",
               delim,
               x))));
      }
   }
   if (verbose) {
      printDebug("mixedOrder(): ",
         "delim:");
      print(delim);
      printDebug("delimited:")
      print(delimited);
      printDebug("x:")
      print(x);
      printDebug("which_blanks:")
      print(which_blanks);
   }
   if (any(which_blanks)) {
      delimited[which_blanks] <- x[which_blanks];
   }
   if (verbose) {
      printDebug("delimited:")
      print(delimited);
   }

   ## Split delimited strings into columns, one row per entry
   step1m <- rbindList(
      rmNULL(strsplit(delimited, delim),
         nullValue=""));

   ## Split the numeric values in their own matrix
   step1mNumeric <- matrix(ncol=ncol(step1m),
      data=suppressWarnings(as.numeric(step1m)));

   ## Optionally convert "Inf" from infinite back to character value
   if (!keepInfinite && any(is.infinite(step1mNumeric))) {
      if (verbose) {
         printDebug("mixedOrder(): ",
            "Using keepInfinite:", "FALSE",
            fgText=c("darkorange1","dodgerblue","orangered"));
      }
      step1mNumeric[is.infinite(step1mNumeric)] <- NA;
      which_inf_pos <- FALSE;
      which_inf_neg <- FALSE;
   } else {
      which_inf_pos <- is.infinite(step1mNumeric) & (step1mNumeric > 0);
      which_inf_neg <- is.infinite(step1mNumeric) & (step1mNumeric < 0);
      if (verbose) {
         printDebug("mixedOrder(): ",
            "Using keepInfinite:", "TRUE",
            fgText=c("darkorange1","dodgerblue","cyan"));
         printDebug("head(step1m, 40):");
         print(head(step1m, 40));
         printDebug("dim(step1m):", dim(step1m));
         printDebug("head(step1mNumeric, 40):");
         print(head(step1mNumeric, 40));
         printDebug("dim(step1mNumeric):", dim(step1mNumeric));
      }
   }
   ## Exception to converting Inf is with keepBlanks, na.last
   if (any(which_blanks)) {
      if (blanksFirst) {
         step1mNumeric[which_blanks,1] <- -Inf;
      } else {
         step1mNumeric[which_blanks, ncol(step1mNumeric)] <- Inf;
      }
   }
   if (any(which_nas)) {
      if (na.last) {
         step1mNumeric[which_nas, ncol(step1mNumeric)] <- Inf;
      } else {
         step1mNumeric[which_nas,1] <- -Inf;
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
   ## If keeping infinite values, make their rank the highest
   if (any(which_inf_pos)) {
      ## Add the highest current rank, keeping the original order
      max_rank <- max(rankOverall, na.rm=TRUE);
      rankOverall[which_inf_pos] <- rmNA(rankOverall[which_inf_pos], naValue=0) + max_rank + 1;
   }
   if (any(which_inf_neg)) {
      ## Simply flip the sign of the rank, keeping the original order
      ## but allowing these ranks to follow the NA and blanks if needed
      rankOverall[which_inf_neg] <- -1 * rankOverall[which_inf_neg];
   }

   ## Backfill blanks or NA
   if (any(which_blanks) && !blanksFirst) {
      rankOverall[which_blanks, 1] <- Inf;
   }
   if (any(which_blanks) && blanksFirst) {
      rankOverall[which_blanks, 1] <- -Inf;
   }
   if (any(which_nas) && na.last) {
      rankOverall[which_nas, 1] <- Inf;
   }
   if (any(which_nas) && !na.last) {
      rankOverall[which_nas, 1] <- -Inf;
   }

   ## Rank initial string as a tiebreaker
   rankX <- rank(x,
      na.last=na.last);
   rankOverall <- cbind(rankOverall, rankX);

   if (verbose) {
      printDebug("mixedOrder(): ",
         "rankOverall:");
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
#' This function is a minor extension to `mixedOrder()` ("multiple mixedOder")
#' which accepts list input, similar to how `base::order()` operates.
#' This function is mainly useful when sorting something like a
#' data.frame, where ties in column 1 should be maintained then
#' broken by non-equal values in column 2, and so on.
#'
#' This function essentially converts any non-numeric column
#' to a factor, whose levels are sorted using `mixedOrder()`.
#' That factor is converted to numeric value, multiplied by `-1`
#' when `decreasing=TRUE`. Finally the list of numeric vectors
#' is passed to `base::order()`.
#'
#' In fact, `mixedSortDF()` calls this `mmixedOrder()` function,
#' in order to sort a `data.frame` properly by column.
#'
#' See `mixedOrder()` and `mixedSort()` for a better
#' description of how the sort order logic operates.
#'
#' @return integer vector of row orders
#'
#' @family jam sort functions
#' @family jam string functions
#'
#' @param ... parameters treated as a list of vectors to be ordered in
#'    proper order, based upon the mechanism by \code{\link[base]{order}}.
#' @param na.last,decreasing,verbose,ignore.case parameters sent to
#'    `mixedOrder()` and `base::order()`.
#' @param matrixAsDF logical if \code{...} supplies only one matrix object,
#'    whether to convert it to data.frame, the coerce to a list, before
#'    processing. By default, in the event only one matrix object is supplied,
#'    this conversion is performed, in order to define a sort order based upon
#'    each column in order.
#'
#' @family jam string functions
#'
#' @export
mmixedOrder <- function
(...,
 na.last=TRUE,
 decreasing=FALSE,
 verbose=FALSE,
 ignore.case=TRUE,
 matrixAsDF=TRUE)
{
   ## Purpose is to provide a wrapper for mixedOrder which allowsmultiple lists,
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
      printDebug("mmixedOrder(): ",
         "head(names(z),10):",
         head(names(z),10));
      printDebug("mmixedOrder(): ",
         "length(z):",
         length(z));
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
   decreasing <- rep(decreasing,
      length.out=length(z));

   z1 <- lapply(seq_along(z), function(iNum){
      if (verbose) {
         printDebug("mmixedOrder(): ",
            "iNum: ",
            iNum);
      }
      i <- z[[iNum]];
      iSign <- (-2*decreasing[iNum])+1;
      if (verbose) {
         printDebug("mmixedOrder(): ",
            "iSign:",
            iSign);
      }
      if (any(class(i) %in% c("numeric", "factor", "ordered"))) {
         as.numeric(i) * iSign;
      } else {
         x2u <- unique(i);
         x2uo <- mixedOrder(x2u,
            ignore.case=ignore.case,
            na.last=na.last,
            ...);
         x2uof <- factor(i,
            levels=x2u[x2uo]);
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
#' by \code{\link[base]{order}} when operating on data.frames. The sort order logic
#' is fully described in \code{\link{mixedSort}} and \code{\link{mixedOrder}}.
#'
#' @return data.frame whose rows are ordered using \code{\link{mmixedOrder}}
#'
#' @family jam sort functions
#' @family jam string functions
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
#' @param na.last logical whether to move NA entries to the end of the sort.
#'    When `na.last=TRUE` then `NA` values will always be last, even following
#'    blanks and infinite values. When `na.last=FALSE` then `NA` values
#'    will always be first, even before blanks and negative infinite values.
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
#' x <- data.frame(l1=letters[1:10],
#'    l2=rep(letters[1:2+10], 5),
#'    L1=LETTERS[1:10],
#'    L2=rep(LETTERS[1:2+20], each=5));
#'
#' @export
mixedSortDF <- function
(df,
 byCols=seq_len(ncol(df)),
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
   ## sort precedence, but only if there are rownames(df).
   if (useRownames && length(rownames(df)) == 0) {
      useRownames <- FALSE;
   }

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
      byCols <- rmNA(byMatch);
      if (verbose) {
         printDebug("mixedSortDF(): ",
            "Converted byCols to integer:",
            byCols);
      }
   }

   ## Determine byCols to keep:
   ## each column only once
   ## is not NA
   ## is no greater than ncol(df)
   ## is not a list column
   dfColnums <- seq_len(ncol(df));
   byColsKeep <- (
      !is.na(byCols) &
      abs(byCols) %in% dfColnums &
      seq_along(byCols) %in% match(unique(abs(byCols)), abs(byCols))
   );
   if (any(byColsKeep)) {
      byColsSortable <- sapply(abs(byCols)[byColsKeep], function(i){
         !igrepHas("list", class(df[[i]]))
      })
      byColsKeep[byColsKeep] <- byColsSortable;
   }

   ## Apply byCols to keep
   byCols <- byCols[byColsKeep];
   if (length(decreasing) > 0) {
      decreasing1 <- rep(-1*decreasing, length.out=length(byCols) + useRownames);
      decreasingV <- (decreasing1[byColsKeep] * sign(byCols)) < 0;
      if (useRownames) {
         decreasingV <- c(decreasingV, tail(decreasing1, 1));
      }
   } else {
      decreasingV <- (byCols < 0);
      if (useRownames) {
         decreasingV <- c(decreasingV, FALSE);
      }
   }
   if (verbose) {
      printDebug("mixedSortDF(): ",
         "byCols: ",
         format(byCols),
         fgText=list("darkorange1", "dodgerblue",
            rep(NA, length(byCols))),
         bgText=list(NA, NA,
            ifelse(byCols < 0,
               "slateblue",
               "tomato3")));
      printDebug("mixedSortDF(): ",
         "decreasing:",
         decreasingV,
         fgText=list("darkorange1", "dodgerblue",
            rep(NA, length(decreasingV))),
         bgText=list(NA, NA,
            ifelse(decreasingV,
               "slateblue",
               "tomato3")));
   }
   byCols <- abs(byCols);

   ## Check for empty byCols, if not using rownames, return df as-is
   if (length(byCols) == 0 && !useRownames) {
      return(df);
   }

   if (igrepHas("matrix", class(df))) {
      if (useRownames) {
         dfOrder <- mmixedOrder(
            data.frame(check.names=FALSE,
               stringsAsFactors=FALSE,
               df[,(byCols),drop=FALSE],
               rowNamesX=rownames(df)),
            decreasing=decreasingV,
            na.last=na.last,
            ...);
      } else {
         dfOrder <- mmixedOrder(
            as.data.frame(df[,(byCols),drop=FALSE]),
            decreasing=decreasingV,
            na.last=na.last,
            ...);
      }
   } else {
      if (useRownames) {
         dfOrder <- mmixedOrder(
            data.frame(check.names=FALSE,
               stringsAsFactors=FALSE,
               df[,(byCols),drop=FALSE],
               rowNamesX=rownames(df)),
            decreasing=decreasingV,
            na.last=na.last,
            ...);
      } else {
         dfOrder <- mmixedOrder(
            df[,(byCols),drop=FALSE],
            decreasing=decreasingV,
            na.last=na.last,
            ...);
      }
   }
   df[dfOrder,,drop=FALSE];
}

#' apply unique to each element of a list
#'
#' Apply unique to each element of a list, usually a list of vectors
#'
#' This function will attempt to use [S4Vectors::unique()] which is
#' substantially faster than any `apply` family function, especially
#' for very long lists. However, when `S4Vectors` is not installed,
#' it applies uniqueness to the `unlist`ed vector of values, which is
#' also substantially faster than the `apply` family functions for
#' long lists, but which may still be less efficient than the
#' C implementation provided by `S4Vectors`.
#'
#' @return `list` with unique values in each list element.
#'
#' @param x input list of vectors
#' @param keepNames boolean indicating whether to keep the list element
#'    names in the returned results.
#' @param incomparables see [unique()] for details, this value is only
#'    sent to [S4Vectors::unique()] when the Bioconductor package
#'    `S4Vectors` is installed, and is ignored otherwise for efficiency.
#' @param useBioc boolean indicating whether this function should try
#'    to use [S4Vectors::unique()] when the Bioconductor package
#'    `S4Vectors` is installed, otherwise it will use a somewhat less
#'    efficient bulk operation.
#'
#' @family jam string functions
#' @family jam list functions
#'
#' @examples
#' L1 <- list(CA=nameVector(LETTERS[c(1:4,2,7,4,6)]),
#'    B=letters[c(7:11,9,3)],
#'    D=nameVector(LETTERS[4]));
#' L1;
#' uniques(L1);
#'
#' if (1 == 1) {
#' if (suppressWarnings(suppressPackageStartupMessages(require(IRanges)))) {
#'    printDebug("Bioc CompressedList:");
#'    print(system.time(uniques(rep(L1, 10000), useBioc=TRUE)));
#' }
#' if (suppressWarnings(suppressPackageStartupMessages(require(S4Vectors)))) {
#'    printDebug("Bioc SimpleList:");
#'    print(system.time(uniques(rep(L1, 10000), useSimpleBioc=TRUE)));
#' }
#' printDebug("Simple list, keepNames=FALSE:");
#' print(system.time(uniques(rep(L1, 10000), useBioc=FALSE, keepNames=FALSE)));
#' printDebug("Simple list, keepNames=TRUE:");
#' print(system.time(uniques(rep(L1, 10000), useBioc=FALSE, keepNames=TRUE)));
#' }
#'
#' @export
uniques <- function
(x,
 keepNames=TRUE,
 incomparables=FALSE,
 useBioc=TRUE,
 useSimpleBioc=FALSE,
 ...)
{
   ## Purpose is to take a list of vectors and return unique members
   ## for each vector in the list.
   ##
   ## keepNames=TRUE will keep the first name for the each duplicated entry
   if (useBioc || useSimpleBioc) {
      if (!suppressWarnings(suppressPackageStartupMessages(require(S4Vectors)))) {
         useSimpleBioc <- FALSE;
         useBioc <- FALSE;
      }
   }
   if (useBioc) {
      if (!suppressWarnings(suppressPackageStartupMessages(require(IRanges)))) {
         useBioc <- FALSE;
      }
   }
   xNames <- names(x);
   if (useSimpleBioc) {
      ## Former method used List(x)
      ## which reverted to SimpleList for simple list input
      ## and SimpleList does not have the amazing optimization
      as.list(
         unique(List(x),
            incomparables=incomparables,
            ...));
   } else if (useBioc) {
      ## Pro tip: use specific class to invoke optimized functions
      ## otherwise they revert to base lapply(x, unique)
      xclasses <- sclass(x);
      xlist <- list();
      xclassesu <- unique(xclasses);
      for (xclass in xclassesu) {
         xclassidx <- which(xclasses %in% xclass);
         if ("character" %in% xclass) {
            xlist[xclassidx] <- as.list(unique(CharacterList(x[xclassidx]),
               incomparables=incomparables))
         } else if ("factor" %in% xclass) {
            xlist[xclassidx] <- as.list(unique(FactorList(x[xclassidx]),
               incomparables=incomparables))
         } else if ("integer" %in% xclass) {
            xlist[xclassidx] <- as.list(unique(IntegerList(x[xclassidx]),
               incomparables=incomparables))
         } else if ("logical" %in% xclass) {
            xlist[xclassidx] <- as.list(unique(LogicalList(x[xclassidx]),
               incomparables=incomparables))
         } else if ("raw" %in% xclass) {
            xlist[xclassidx] <- as.list(unique(RawList(x[xclassidx]),
               incomparables=incomparables))
         } else if ("Rle" %in% xclass) {
            xlist[xclassidx] <- as.list(unique(RleList(x[xclassidx]),
               incomparables=incomparables))
         } else if ("complex" %in% xclass) {
            xlist[xclassidx] <- as.list(unique(ComplexList(x[xclassidx]),
               incomparables=incomparables))
         } else if ("GRanges" %in% xclass) {
            xlist[xclassidx] <- lapply(unique(GenomicRanges::GRangesList(x[xclassidx])), function(gr){gr});
         } else {
            xlist[xclassidx] <- lapply(x[xclassidx], unique);
         }
      }
      names(xlist) <- xNames;
      return(xlist);
   } else if (!keepNames) {
      lapply(x, unique);
   } else {
      xu <- unlist(unname(x),
         use.names=TRUE);
      if (length(xNames) == 0) {
         names(x) <- seq_along(x);
      } else {
         names(x) <- makeNames(names(x));
      }
      xn <- factor(rep(names(x), rlengths(x)),
         levels=names(x));
      ## Concatenate name with value so uniqueness requires both
      xun <- paste0(xn, "!!", xu);
      xmatch <- match(unique(xun), xun);
      xuse <- xu[xmatch];
      xnuse <- xn[xmatch];
      if (!keepNames) {
         xuse <- unname(xuse);
      }
      xlist <- split(xuse, xnuse);
      names(xlist) <- xNames;
      xlist;
   }
}

#' paste a list into a delimited vector
#'
#' Paste a list of vectors into a character vector, with values
#' delimited by default with a comma.
#'
#' This function is essentially a wrapper for [S4Vectors::unstrsplit()]
#' except that it also optionally applies uniqueness to each vector
#' in the list, and sorts values in each vector using [mixedOrder()].
#'
#' The sorting and uniqueness is applied to the `unlist`ed vector of
#' values, which is substantially faster than any `apply` family function
#' equivalent. The uniqueness is performed by [uniques()], which itself
#' will use [S4Vectors::unique()] if available.
#'
#' @return character vector with the same names and in the same order
#'    as the input list `x`.
#'
#' @param x input list of vectors
#' @param sep character delimiter used to paste multiple values together
#' @param doSort boolean indicating whether to sort each vector
#'    using [mixedOrder()].
#' @param makeUnique boolean indicating whether to make each vector in
#'    the input list unique before pasting its values together.
#' @param na.rm boolean indicating whether to remove NA values from
#'    each vector in the input list. When `na.rm` is `TRUE` and a
#'    list element contains only `NA` values, the resulting string
#'    will be `""`.
#' @param keepFactors logical indicating whether to preserve factors,
#'    keeping factor level order when `doSort=TRUE`. When
#'    `keepFactors=TRUE`, if any list element is a factor, all elements
#'    are converted to factors. Note that this step combines overall
#'    factor levels, and non-factors will be ordered using
#'    `base::order()` instead of `jamba::mixedOrder()` (for now.)
#' @param useBioc boolean indicating whether this function should try
#'    to use [S4Vectors::unstrsplit()] when the Bioconductor package
#'    `S4Vectors` is installed, otherwise it will use a much less
#'    efficient [mapply()] operation.
#' @param ... additional arguments are passed to `mixedOrder()` when
#'    `doSort=TRUE`.
#'
#' @examples
#' L1 <- list(CA=LETTERS[c(1:4,2,7,4,6)], B=letters[c(7:11,9,3)]);
#'
#' cPaste(L1);
#' #               CA                 B
#' # "A,B,C,D,B,G,D,F"   "g,h,i,j,k,i,c"
#'
#' cPaste(L1, doSort=TRUE);
#' #               CA                 B
#' # "A,B,B,C,D,D,F,G"   "c,g,h,i,i,j,k"
#'
#' ## The sort can be done with convenience function cPasteS()
#' cPasteS(L1);
#' #               CA                 B
#' # "A,B,B,C,D,D,F,G"   "c,g,h,i,i,j,k"
#'
#' ## Similarly, makeUnique=TRUE and cPasteU() are the same
#' cPaste(L1, makeUnique=TRUE);
#' cPasteU(L1);
#' #           CA             B
#' # "A,B,C,D,G,F" "g,h,i,j,k,c"
#'
#' ## Change the delimiter
#' cPasteSU(L1, sep="; ")
#' #                CA                  B
#' # "A; B; C; D; F; G" "c; g; h; i; j; k"
#'
#' # test mix of factor and non-factor
#' L2 <- c(
#'    list(D=factor(letters[1:12],
#'       levels=letters[12:1])),
#'    L1);
#' L2;
#' cPasteSU(L2, keepFactors=TRUE);
#'
#' # tricky example with mix of character and factor
#' # and factor levels are inconsistent
#' # end result: factor levels are defined in order they appear
#' L <- list(entryA=c("miR-112", "miR-12", "miR-112"),
#'    entryB=factor(c("A","B","A","B"),
#'       levels=c("B","A")),
#'    entryC=factor(c("C","A","B","B","C"),
#'       levels=c("A","B","C")),
#'    entryNULL=NULL)
#' L;
#' cPaste(L);
#' cPasteU(L);
#'
#' # by default keepFactors=FALSE, which means factors are sorted as characters
#' cPasteS(L);
#' cPasteSU(L);
#' # keepFactors=TRUE will keep unique factor levels in the order they appear
#' # this is the same behavior as unlist(L[c(2,3)]) on a list of factors
#' cPasteSU(L, keepFactors=TRUE);
#' levels(unlist(L[c(2,3)]))
#'
#' @family jam string functions
#' @family jam list functions
#'
#' @export
cPaste <- function
(x,
 sep=",",
 doSort=FALSE,
 makeUnique=FALSE,
 na.rm=FALSE,
 keepFactors=FALSE,
 checkClass=TRUE,
 useBioc=TRUE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to utilize the vectorized function unstrsplit() from the S4Vectors package
   if (length(x) == 0) {
      return("");
   }
   if (!suppressWarnings(suppressPackageStartupMessages(require(S4Vectors)))) {
      warn("cPaste() is substantially faster when Bioconductor package S4Vectors is installed.");
      #stop("The IRanges package is required by cPaste() for the CharacterList class.");
      useBioc <- FALSE;
   }
   xNames <- names(x);
   if (!igrepHas("list", class(x))) {
      x <- list(x);
      xNames <- NULL;
   }
   ## Assign temporary names if none are present
   if (length(names(x)) == 0) {
      names(x) <- seq_along(x);
   } else {
      names(x) <- makeNames(names(x));
   }

   ## For speed, we sort and/or convert to character class as a vector,
   ## rather than a bunch of tiny vectors inside a list. Vector sorting
   ## is fast; splitting vector into a list is fast.
   if (checkClass) {
      xclass <- sclass(x);
      if (is.list(xclass)) {
         xclass <- cPaste(xclass,
            checkClass=FALSE);
      }
   } else {
      xclass <- rep("character",
         length.out=length(x));
   }
   if (verbose) {
      if (length(x) > 100) {
         printDebug("cPaste(): ",
            "head(xclass, 100):",
            head(xclass, 100));
      } else {
         printDebug("cPaste(): ",
            "xclass:",
            xclass);
      }
   }
   ## Optionally make vectors unique
   if (makeUnique) {
      x <- uniques(x,
         useBioc=useBioc,
         ...);
   }

   ## Handle potential mix of factor and non-factor
   if (any(grepl("factor", xclass))) {
      if (any(!grepl("factor", xclass))) {
         # x contains mix of factor and non-factor
         if (verbose) {
            printDebug("cPaste(): ",
               "mix of factor and non-factor");
         }
         if (doSort && keepFactors) {
            if (verbose) {
               printDebug("cPaste(): ",
                  "converting non-factor to factor with mixedSort()");
            }
            # if sorting we must convert non-factor to factor using mixedSort
            xnonfactor <- which(!grepl("factor", xclass));
            xnonfactorlevels <- mixedSort(unique(unlist(x[xnonfactor])));
            ## carefully split using factor split so empty entries are not lost
            x[xnonfactor] <- split(
               factor(unlist(x[xnonfactor]),
                  levels=xnonfactorlevels),
               factor(
                  rep(xnonfactor, lengths(x[xnonfactor])),
                  levels=xnonfactor)
               );
         } else {
            if (verbose) {
               printDebug("cPaste(): ",
                  "converting factor to character");
            }
            # if not sorting, or not keeping factor levels
            # convert factor to character
            xisfactor <- which(grepl("factor", xclass));
            x[xisfactor] <- lapply(x[xisfactor], as.character);
         }
      } else {
         # all values are factors
         if (verbose) {
            printDebug("cPaste(): ",
               "all values are factor");
         }
      }
   } else {
      # no values are factors, leave as-is
      if (verbose) {
         printDebug("cPaste(): ",
            "no values are factor");
      }
   }
   xu <- unlist(x);
   if (igrepHas("factor", class(xu)) && doSort && !keepFactors) {
      # if sorting AND if xu is factor AND we do not want to keep factor levels
      # then convert to character
      if (verbose) {
         printDebug("cPaste(): ",
            "converting factor to character to drop factor levels during sort");
      }
      xu <- as.character(xu);
   }

   ## We define a vector of names as a factor, so the
   ## order of the factor levels will maintain the
   ## original order of input data during the
   ## split() which occurs later.
   ## Using a factor also preserves empty levels,
   ## in the case that NA values are removed.
   xn <- factor(rep(names(x), rlengths(x)),
      levels=names(x));
   if (doSort && length(xu) > 1) {
      if (igrepHas("factor", class(xu))) {
         xuOrder <- order(xu, ...);
      } else {
         xuOrder <- mixedOrder(xu, ...);
      }
      xu <- xu[xuOrder];
      xn <- xn[xuOrder];
   }

   ## Optionally remove NA values
   if (na.rm && length(xu) > 0 && any(is.na(xu))) {
      whichNotNA <- which(!is.na(xu));
      xu <- xu[whichNotNA];
      xn <- xn[whichNotNA];
   }

   ## split() using a factor keeps the data in original order
   x <- split(
      as.character(unname(xu)),
      xn);

   if (useBioc) {
      ## Note: The explicit conversion to class CharacterList is required
      ## in order to avoid errors with single list elements of NA when
      ## na.rm=FALSE. Specifically, unstrsplit() requires all elements in
      ## the list to be "character" class, and a single NA is class "logical"
      ## and causes an error.
      xNew <- S4Vectors::unstrsplit(
         IRanges::CharacterList(x),
         sep=sep);
   } else {
      xNew <- mapply(paste,
         x,
         collapse=sep);
   }

   ## Revert names(x) to their original state
   names(xNew) <- xNames;

   return(xNew);
}

#' paste a list into a delimited vector using sorted values
#'
#' Paste a list of vectors into a character vector, with values sorted
#' then delimited by default with a comma.
#'
#' This function is convenient a wrapper for `cPaste(.., doSort=TRUE)`.
#'
#' @inheritParams cPaste
#'
#' @family jam string functions
#' @family jam list functions
#'
#' @export
cPasteS <- function
(x,
 sep=",",
 doSort=TRUE,
 makeUnique=FALSE,
 na.rm=FALSE,
 keepFactors=FALSE,
 checkClass=TRUE,
 useBioc=TRUE,
 ...)
{
   ## Purpose is to call cPaste with doSort=TRUE
   cPaste(x=x,
      sep=sep,
      doSort=doSort,
      makeUnique=makeUnique,
      na.rm=na.rm,
      keepFactors=keepFactors,
      checkClass=checkClass,
      useBioc=useBioc,
      ...);
}


#' paste a list into a delimited vector using sorted, unique values
#'
#' Paste a list of vectors into a character vector, with unique values
#' sorted then delimited by default with a comma.
#'
#' This function is convenient a wrapper for `cPaste(.., doSort=TRUE, makeUnique=TRUE)`.
#'
#' @inheritParams cPaste
#'
#' @family jam string functions
#' @family jam list functions
#'
#' @export
cPasteSU <- function
(x,
 sep=",",
 doSort=TRUE,
 makeUnique=TRUE,
 na.rm=FALSE,
 keepFactors=FALSE,
 checkClass=TRUE,
 useBioc=TRUE,
 ...)
{
   ## Purpose is to call cPaste with doSort=TRUE and makeUnique=TRUE
   cPaste(x=x,
      sep=sep,
      doSort=doSort,
      makeUnique=makeUnique,
      na.rm=na.rm,
      keepFactors=keepFactors,
      checkClass=checkClass,
      useBioc=useBioc,
      ...);
}

#' paste a list into a delimited vector using unique values
#'
#' Paste a list of vectors into a character vector of unique values,
#' usually delimited by a comma.
#'
#' This function is convenient a wrapper for `cPaste(.., makeUnique=TRUE)`.
#'
#' @inheritParams cPaste
#'
#' @family jam string functions
#' @family jam list functions
#'
#' @export
cPasteUnique <- function
(x,
 sep=",",
 doSort=FALSE,
 makeUnique=TRUE,
 na.rm=FALSE,
 keepFactors=FALSE,
 checkClass=TRUE,
 useBioc=TRUE,
 ...)
{
   ## Purpose is to call cPaste with makeUnique=TRUE
   cPaste(x=x,
      sep=sep,
      doSort=doSort,
      makeUnique=makeUnique,
      na.rm=na.rm,
      keepFactors=keepFactors,
      checkClass=checkClass,
      useBioc=useBioc,
      ...);
}

#' paste a list into a delimited vector using unique values
#'
#' Paste a list of vectors into a character vector of unique values,
#' usually delimited by a comma.
#'
#' This function is convenient a wrapper for `cPaste(.., makeUnique=TRUE)`.
#'
#' @inheritParams cPaste
#'
#' @family jam string functions
#' @family jam list functions
#'
#' @export
cPasteU <- function
(x,
 sep=",",
 doSort=FALSE,
 makeUnique=TRUE,
 na.rm=FALSE,
 keepFactors=FALSE,
 checkClass=TRUE,
 useBioc=TRUE,
 ...)
{
   ## Purpose is to call cPaste with makeUnique=TRUE
   cPaste(x=x,
      sep=sep,
      doSort=doSort,
      makeUnique=makeUnique,
      na.rm=na.rm,
      keepFactors=keepFactors,
      checkClass=checkClass,
      useBioc=useBioc,
      ...);
}

#' Rename columns in a data.frame or matrix
#'
#' Rename columns in a data.frame or matrix
#'
#' This function is intended to rename one or more columns in a
#' data.frame or matrix. It will gracefully ignore columns
#' which do not match, in order to make it possible to call the
#' function multiple times without problem. It makes it possible
#' to verify that columns are properly named by re-running the
#' function as needed.
#'
#' This function will also recognize input objects `GRanges`,
#' `ucscData`, and `IRanges`, which store annotation in `DataFrame`
#' accessible via `values(x)`.
#'
#' @return data.frame (or tibble or DataFrame) with columns `from`
#'    renamed to `to` as matched. When `colnames(x)` do not match
#'    values in `from` the corresponding values in `from` are ignored.
#'
#' @family jam practical functions
#'
#' @param x data.frame or matrix
#' @param from character vector of colnames expected to be in `x`.
#'    Any values that do not match `colnames(x)` are ignored.
#' @param to character vector with `length(to) == length(from)`
#'    corresponding to the target name for any colnames that
#'    match `from`.
#'
#' @examples
#' df <- data.frame(A=1:5, B=6:10, C=11:15);
#' df;
#' df2 <- renameColumn(df,
#'    from=c("A","C"),
#'    to=c("a_new", "c_new"));
#' df2;
#' df3 <- renameColumn(df2,
#'    from=c("A","C","B"),
#'    to=c("a_new", "c_new","b_new"));
#' df3;
#'
#' @export
renameColumn <- function
(x,
 from,
 to,
 verbose=FALSE,
 ...)
{
   ## Purpose is simply to rename one or more colnames in a data.frame or matrix.
   ## This method makes sure to rename only colnames which exist in 'from'
   ## and renames them in the appropriate order to 'to'.
   ## Therefore you can re-run this method and it will not make changes
   ## that are not warranted.  Note that it will also only make changes, thus
   ## if you perform some operation on 'from' to generate 'to' and some entries
   ## do not change, this method will not rename those columns.
   if (igrepHas("ucscdata|granges|iranges", class(x))) {
      if (verbose) {
         printDebug("renameColumn(): ",
            "Recognized GRanges input.");
      }
      renameSet <- which(from %in% colnames(values(x)) & from != to);
      renameWhich <- match(from[renameSet], colnames(values(x)));
      if (verbose) {
         printDebug("renameColumn(): ",
            "Renaming ",
            format(length(renameWhich), big.mark=","),
            " columns.");
      }
      if (length(renameWhich) > 0) {
         colnames(values(x))[renameWhich] <- to[renameSet];
      }
   } else {
      renameSet <- which(from %in% colnames(x) & from != to);
      renameWhich <- match(from[renameSet], colnames(x));
      if (verbose) {
         printDebug("renameColumn(): ",
            "Renaming ",
            format(length(renameWhich), big.mark=","),
            " columns.");
      }
      if (length(renameWhich) > 0) {
         colnames(x)[renameWhich] <- to[renameSet];
      }
   }
   return(x);
}

#' Fill blank entries in a vector
#'
#' Fill blank entries in a vector
#'
#' This function takes a character vector and fills any blank (missing)
#' entries with the last non-blank entry in the vector. It is intended
#' for situations like imported Excel data, where there may be one
#' header value representing a series of cells.
#'
#' The method used does not loop through the data, and should scale
#' fairly well with good efficiency even for extremely large vectors.
#'
#' @return
#' Character vector where blank entries are filled with the
#' most recent non-blank value.
#'
#' @param x character vector
#' @param blankGrep vector of grep patterns, or `NA`, indicating
#'    the type of entry to be considered blank.
#'    Each `blankGrep` pattern is searched using `jamba::proigrep()`, which
#'    by default uses case-insensitive regular expression pattern
#'    matching.
#' @param first options character string intended when the first
#'    entry of `x` is blank. By default `""` is used.
#' @param ... additional parameters are ignored.
#'
#' @examples
#' x <- c("A", "", "", "", "B", "C", "", "", NA,
#'    "D", "", "", "E", "F", "G", "", "");
#' data.frame(x, fillBlanks(x));
#'
#' @family jam string functions
#'
#' @export
fillBlanks <- function
(x,
 blankGrep=c("[ \t]*"),
 first="",
 ...)
{
   ## Purpose is to take a character vector, and fill blank values
   ## with the most recent non-blank value.
   ## Intended for data imported from something like Excel, where
   ## people sometimes use one heading for a section of multiple
   ## columns or rows

   ## Ensure all non-NA patterns have a leading "^"
   blankGrep <- rmNA(blankGrep);
   if (length(blankGrep) == 0) {
      blankGrep <- "^$";
   }
   addToGrep1 <- which(!is.na(blankGrep) & !grepl("^\\^|^[(]*\\^", blankGrep));
   blankGrep[addToGrep1] <- paste0("^", blankGrep[addToGrep1]);
   ## Ensure all non-NA patterns have a leading "^"
   addToGrep2 <- which(!is.na(blankGrep) & !grepl("\\$$|\\$[)]*$", blankGrep));
   blankGrep[addToGrep2] <- paste0(blankGrep[addToGrep2], "$");

   xBlank <- sort(proigrep(blankGrep, x));
   if (length(xBlank) == 0) {
      return(x);
   }
   if (1 %in% xBlank) {
      xBlank <- setdiff(xBlank, 1);
      x[1] <- first;
   }
   xIsBlank <- rep(FALSE, length.out=length(x));
   xIsBlank[xBlank] <- TRUE;

   whichNotBlank <- which(!xIsBlank);
   xWhichNotBlankU <- unique(c(1, whichNotBlank));
   xNonBlankVals <- x[!xIsBlank];
   xNonBlankWhich <- as.numeric(cut(seq_along(x), c(xWhichNotBlankU-1, Inf)));
   xFilled <- xNonBlankVals[xNonBlankWhich];
   return(xFilled);
}

#' Format an integer as a string
#'
#' Format an integer as a string
#'
#' This function is a quick wrapper function around `base::format()`
#' to display integer values as text strings. It will also return a
#' matrix if the input is a matrix.
#'
#' @return character vector if `x` is a vector, or if `x` is a matrix
#' a matrix will be returned.
#'
#' @family jam string functions
#'
#' @param x numeric vector or matrix
#' @param big.mark,trim,scientific options sent to `base::format()` but
#'    configured with defaults intended for integer values.
#' @param forceInteger logical indicating whether numeric values should
#'    be rounded to the nearest integer value prior to `base::format()`.
#'    This option is intended to hide decimal values where they are not
#'    informative.
#'
#' @examples
#' x <- c(1234, 1234.56, 1234567.89);
#' ## By default, commas are used for big.mark, and decimal values are hidden
#' formatInt(x);
#'
#' ## By default, commas are used for big.mark
#' formatInt(x, forceInteger=FALSE);
#'
#' @export
formatInt <- function
(x,
 big.mark=",",
 trim=TRUE,
 forceInteger=TRUE,
 scientific=FALSE,
 ...)
{
   ## Purpose is to format a pretty integer, with commas separating thousandths digits
   if (forceInteger) {
      y <- format(x=round(x),
         big.mark=big.mark,
         trim=trim,
         scientific=scientific,
         ...);
   } else {
      y <- format(x=x,
         big.mark=big.mark,
         trim=trim,
         scientific=scientific,
         ...);
   }
   if (class(x) %in% c("matrix")) {
      y <- matrix(data=y,
         nrow=nrow(x),
         ncol=ncol(x),
         dimnames=list(rownames(x), colnames(x)));
   } else if (igrepHas("character|num|integer|float", class(x))) {
      if (!is.null(names(x))) {
         names(y) <- names(x);
      }
   }
   y;
}

#' Convert list of vectors to data.frame with item, value, name
#'
#' Convert list of vectors to data.frame with item, value, name
#'
#' This function converts a list of vectors to a tall data.frame
#' with colnames `item` to indicate the list name, `value` to indicate
#' the vector value, and `name` to indicate the vector name if
#' `useVectorNames=TRUE` and if names exist.
#'
#' @param x list of vectors
#' @param makeUnique logical indicating whether the data.frame should
#'    contain unique rows.
#' @param useVectorNames logical indicating whether vector names should
#'    be included in the data.frame, if they exist.
#' @param ... additional arguments are ignored.
#'
#' @family jam list functions
#'
#' @export
list2df <- function
(x,
 makeUnique=TRUE,
 useVectorNames=TRUE,
 ...)
{
   ## Purpose is to take a list of vectors, and turn it
   ## into a data.frame, where the list names are in column 1
   ## and the values are in column 2.
   ##
   ## This function is good for converting something like
   ## conflicts(,TRUE) into a usable data.frame, e.g.:
   ## cf1 <- conflicts(,TRUE);
   ## xdf <- list2df(cf1);
   ## irMethods <- xdf[igrep("IRanges", xdf[,1]),2];
   ## irConflicts <- xdf[xdf[,2] %in% irMethods & !xdf[,1] %in% "package:IRanges",];

   xdf <- data.frame(item=rep(names(x), lengths(x)),
      value=unname(unlist(x)));
   if (length(useVectorNames) > 0 && useVectorNames) {
      xdf$name <- unname(unlist(lapply(x, names)));
   }
   if (makeUnique) {
      xdf <- unique(xdf);
   }
   return(xdf);
}

#' Jam-specific recursive apply
#'
#' Jam-specific recursive apply
#'
#' This function is a very lightweight customization to `base::rapply()`,
#' specifically that it does not remove `NULL` entries.
#'
#' @family jam list functions
#'
#' @param x `list`
#' @param FUN `function` to be called on non-list elements in `x`.
#' @param how `character` string indicating whether to return the
#'    `list` or whether to call `unlist()` on the result.
#' @param ... additional arguments are passed to `FUN`.
#'
#' @examples
#' L <- list(entryA=c("miR-112", "miR-12", "miR-112"),
#'    entryB=factor(c("A","B","A","B"),
#'       levels=c("B","A")),
#'    entryC=factor(c("C","A","B","B","C"),
#'       levels=c("A","B","C")),
#'    entryNULL=NULL)
#' rapply(L, length)
#' jam_rapply(L, length)
#'
#' L0 <- list(A=1:3, B=list(C=1:3, D=4:5, E=NULL));
#' rapply(L0, length)
#' jam_rapply(L0, length)
#'
#' @export
jam_rapply <- function
(x,
 FUN,
 how=c("unlist", "list"),
 ...)
{
   how <- match.arg(how);
   newlist <- lapply(x, function(i){
      if (is.list(i)){
         jam_rapply(i,
            FUN=FUN,
            how=how,
            ...);
      } else {
         FUN(i,
            ...)
      }
   });
   if ("unlist" %in% how) {
      newlist <- unlist(newlist);
   }
   return(newlist);
}


#' sort alphanumeric values within a list format
#'
#' sort alphanumeric values within a list format
#'
#' This function is an extension to `mixedSort()` to sort each vector
#' in a list. It applies the sort to the whole unlisted vector then
#' splits back into list form.
#'
#' In the event the input is a nested list of lists, only the first
#' level of list structure is maintained in the output data. For
#' more information, see `rlengths()` which calculates the recursive
#' nested list sizes.
#'
#' @family jam sort functions
#' @family jam string functions
#' @family jam list functions
#'
#' @inheritParams mixedSort
#'
#' @examples
#' # set up an example list of mixed alpha-numeric strings
#' set.seed(12);
#' x <- paste0(sample(letters, replace=TRUE, 52), rep(1:30, length.out=52));
#' x;
#' # split into a list as an example
#' xL <- split(x, rep(letters[1:5], c(6,7,5,4,4)));
#' xL;
#'
#' # now run mixedSorts(xL)
#' # Notice "e5" is sorted before "e28"
#' mixedSorts(xL)
#'
#' # for fun, compare to lapply(xL, sort)
#' # Notice "e5" is sorted after "e28"
#' lapply(xL, sort)
#'
#' # Make a nested list
#' set.seed(1);
#' l1 <- list(
#'    A=sample(nameVector(11:13, rev(letters[11:13]))),
#'    B=list(
#'       C=sample(nameVector(4:8, rev(LETTERS[4:8]))),
#'       D=sample(nameVector(LETTERS[2:5], rev(LETTERS[2:5])))
#'    )
#' )
#' l1;
#' # The output is a nested list with the same structure
#' mixedSorts(l1);
#' mixedSorts(l1, sortByName=TRUE);
#'
#' # when one entry is missing
#' L0 <- list(A=3:1,
#'    B=list(C=c(1:3,NA,0),
#'    D=LETTERS[c(4,5,2)],
#'    E=NULL));
#' L0
#' mixedSorts(L0)
#' mixedSorts(L0, na.rm=TRUE)
#'
#' @export
mixedSorts <- function
(x,
 blanksFirst=TRUE,
 na.last=NAlast,
 keepNegative=FALSE,
 keepInfinite=TRUE,
 keepDecimal=FALSE,
 ignore.case=TRUE,
 sortByName=FALSE,
 na.rm=FALSE,
 verbose=FALSE,
 NAlast=TRUE,
 debug=FALSE,
 ...)
{
   ## Purpose is to take a list of vectors and run mixedSort() efficiently
   ##
   xNames <- names(x);
   xclass <- unique(rapply(x, class, how="unlist"));
   xu <- unlist(x);
   if (length(names(x)) == 0) {
      names(x) <- seq_along(x);
   } else {
      names(x) <- makeNames(names(x));
   }
   ## vector names
   xun <- unname(jam_rapply(x, names));
   if (length(xun) < length(xu)) {
      if (sortByName) {
         stop("Cannot sort by name because not all vectors have names.");
      }
      xun <- NULL;
   }
   if (length(xclass) > 1) {
      xu <- as.character(xu);
   }

   ## We define a vector of names as a factor, so the
   ## order of the factor levels will maintain the
   ## original order of input data during the
   ## split() which occurs later.
   ## Using a factor also preserves empty levels,
   ## in the case that NA values are removed.
   xrn <- jam_rapply(x, length);
   xn <- factor(
      rep(names(xrn),
         xrn),
      levels=names(xrn));
   #xn <- factor(rep(names(x), rlengths(x)),
   #   levels=names(x));

   if (sortByName) {
      xu_use <- xun;
   } else {
      xu_use <- xu;
   }
   if ("factor" %in% class(xu_use)) {
      xuOrder <- order(xu_use,
         na.last=na.last);
   } else {
      xuOrder <- mixedOrder(xu_use,
         blanksFirst=blanksFirst,
         na.last=na.last,
         keepNegative=keepNegative,
         keepInfinite=keepInfinite,
         keepDecimal=keepDecimal,
         ignore.case=ignore.case,
         ...);
   }
   xu <- xu[xuOrder];
   xn <- xn[xuOrder];
   xu_use <- xu_use[xuOrder];
   if (length(xun) > 0) {
      xun <- xun[xuOrder];
   }

   ## Optionally remove NA values
   if (na.rm && any(is.na(xu_use))) {
      printDebug("Removing NA values");
      whichNotNA <- which(!is.na(xu_use));
      xu <- xu[whichNotNA];
      xn <- xn[whichNotNA];
      if (length(xun) > 0) {
         xun <- xun[whichNotNA];
      }
      xu_use <- xu_use[whichNotNA];
      if (any("list" %in% sapply(x, class))) {
         x <- jam_rapply(x, function(i){i[!is.na(i)]}, "list")
      }
   }
   if (length(xun) > 0) {
      names(xu) <- xun;
   } else {
      xu <- unname(xu);
   }

   ## split() using a factor keeps the data in original order
   if (debug) {
      return(list(
         xu=xu,
         xn=xn,
         xun=xun,
         x=x)
      );
   }
   if (any("list" %in% sapply(x, class))) {
      xu_ordered <- unlist(unname(split(xu, xn)))
      xnew <- relist_named(xu_ordered, x);
      if (length(xnew) == length(xNames)) {
         names(xnew) <- xNames;
      }
   } else {
      xnew <- split(xu, xn);
      names(xnew) <- xNames;
   }
   return(xnew);
}

#' relist a vector which allows re-ordered names
#'
#' relist a vector which allows re-ordered names
#'
#' This function is a simple update to `utils::relist()`
#' that allows updating the names of each list element.
#' More specifically, this function does not replace the
#' updated names with the corresponding names from
#' the list `skeleton`, as is the case in
#' `utils:::relist.default()`.
#'
#' It is somewhat surprising that `utils::relist()` is
#' simply a nested for loop, instead of `rapply()` or some
#' fancy vectorized alternative. That said, the function works,
#' and there is much to be commended for functions that work.
#'
#' @return `list` object with the same structure as the `skeleton`.
#'
#' @family jam list functions
#'
#' @param x vector to be applied to the `skeleton` list
#'    structure in order.
#' @param skeleton `list` object representing the desired
#'    final list structure, or `vector` when the input
#'    data `x` should be returned as-is, without change.
#'    Specifically, when `skeleton` is a `vector`, the
#'    `names(x)` are maintained without change.
#' @param ... additional arguments are ignored.
#'
#' @export
relist_named <- function
(x,
 skeleton,
 ...)
{
   ##
   ind <- 1L;
   result <- skeleton;
   if ("list" %in% class(skeleton)) {
      for (i in seq_along(skeleton)) {
         size <- length(unlist(result[[i]]));
         result[[i]] <- relist_named(
            x[seq.int(ind, length.out=size)],
            result[[i]]);
         ind <- ind + size;
      }
   } else {
      result <- x;
   }
   result;
}

#' Uppercase the first letter in each word
#'
#' Uppercase the first letter in each word
#'
#' This function is a simple mimic of the Perl function `ucfirst` which
#' converts the first letter in each word to uppercase. When
#' `lowercaseAll=TRUE` it also forces all other letters to lowercase,
#' otherwise mixedCase words will retain capital letters in the middle
#' of words.
#'
#' @param x character vector.
#' @param lowercaseAll logical indicating whether to force all letters
#'    to lowercase before applying uppercase to the first letter.
#' @param firstWordOnly logical indicating whether to apply the
#'    uppercase only to the first word in each string. Note that it
#'    still applies the logic to every entry in the input vector `x`.
#' @param ... additional arguments are ignored.
#'
#' @family jam string functions
#'
#' @examples
#' ucfirst("TESTING_ALL_UPPERCASE_INPUT")
#' ucfirst("TESTING_ALL_UPPERCASE_INPUT", TRUE)
#' ucfirst("TESTING_ALL_UPPERCASE_INPUT", TRUE, TRUE)
#'
#' ucfirst("testing mixedCase upperAndLower case input")
#' ucfirst("testing mixedCase upperAndLower case input", TRUE)
#' ucfirst("testing mixedCase upperAndLower case input", TRUE, TRUE)
#'
#' @export
ucfirst <- function
(x,
 lowercaseAll=FALSE,
 firstWordOnly=FALSE,
 ...)
{
   ## Purpose is to mimic the Perl function,
   ## and upper-case the first letter of a word
   ##
   ## lowercaseAll=TRUE will make everything after
   ## the first character into lowercase.
   ##
   if (lowercaseAll) {
      x <- tolower(x);
   }
   if (firstWordOnly) {
      newX <- sub("(^|\\b|[^[a-zA-Z0-9])([a-zA-Z])",
         "\\1\\U\\2",
         x,
         perl=TRUE);
   } else {
      newX <- gsub("(^|\\b|[^[a-zA-Z0-9])([a-zA-Z])",
         "\\1\\U\\2",
         x,
         perl=TRUE);
   }

   return(newX);
}

#' Global substitution into ordered factor
#'
#' Global substitution into ordered factor
#'
#' This function is an extension of `base::gsub()` that
#' returns an ordered factor output. When input is also a
#' factor, the output factor levels are retained in the
#' same order, after applying the string substitution.
#'
#' This function is very useful when making changes via `base::gsub()`
#' to a factor with ordered levels, because it retains the
#' the order of levels after modification.
#'
#' Tips:
#'
#' * To convert a character vector to a factor, whose levels are
#' sorted, use `sortFunc=sort`.
#' * To convert a character vector to a factor, whose levels are
#' the order they appear in the input `x`, use `sortFunc=c`.
#' * To convert a character vector to a factor, whose levels are
#' sorted alphanumerically, use `sortFunc=mixedSort`.
#'
#' @return factor whose levels are based upon the order of
#'    input levels when the input `x` is a factor; or if the
#'    input `x` is not a factor, it is converted to a factor
#'    using the provided sort function `sortFunc`.
#'
#' @param pattern,replacement,x,ignore.case,perl,fixed,useBytes
#'    arguments sent to `base::gsub()`
#' @param sortFunc function used to sort factor levels, which
#'    is not performed if the input `x` is a `factor`.
#' @param ... additional arguments are passed to `sortFunc`
#'
#' @family jam string functions
#'
#' @examples
#' x <- c(paste0(
#'    rep(c("first", "second", "third"), 2),
#'    rep(c("Section", "Choice"), each=3)),
#'    "Choice");
#' f <- factor(x, levels=x);
#' f;
#'
#' # default gsub() will return a character vector
#' gsub("(first|second|third)", "", f)
#' # converting to factor resets the factor level order
#' factor(gsub("(first|second|third)", "", f))
#'
#' ## gsubOrdered() maintains the factor level order
#' gsubOrdered("(first|third)", "", f)
#' gsubOrdered("(first)", "", f)
#'
#' # to convert character vector to factor, levels in order they appear
#' gsubOrdered("", "", x, sortFunc=c)
#'
#' # to convert character vector to factor, levels alphanumeric sorted
#' gsubOrdered("", "", x, sortFunc=mixedSort)
#'
#' @export
gsubOrdered <- function
(pattern,
 replacement,
 x,
 ignore.case=FALSE,
 perl=FALSE,
 fixed=FALSE,
 useBytes=FALSE,
 sortFunc=mixedSort,
 ...)
{
   ## Purpose is to perform gsub() but maintain order of factor levels consistent with the
   ## input data.
   ##
   ## If input data is not a factor, it is converted to a factor,
   ## using sortFunc() to order the levels.
   ##
   ## To have levels ordered based upon their original order,
   ## use sortFunc=c
   ##
   ## To have levels ordered based upon sample sorting,
   ## use sortFunc=sortSamples
   ##
   ## To have levels ordered based upon alphenumeric sorting,
   ## use sortFunc=mixedSort
   ##
   ## The special case where is.na(pattern) it will change NA values
   ## to the replacement, and relevel the factor accordingly
   xNames <- names(x);
   if (!igrepHas("factor", class(x))) {
      if (is.na(pattern) && any(is.na(x))) {
         x <- addNA(factor(x, levels=unique(sortFunc(x))));
      } else {
         x <- factor(x, levels=unique(sortFunc(x)));
      }
   }
   if (is.na(pattern)) {
      if (!any(is.na(x))) {
         return(x);
      }
      xNA <- which(is.na(x));
      y <- as.character(x);
      y[xNA] <- replacement;
      yLevels <- levels(x);
      yLevels[is.na(yLevels)] <- replacement;
      yLevels <- unique(yLevels);
   } else {
      y <- gsub(pattern=pattern, replacement=replacement, x=x, ignore.case=ignore.case,
         perl=perl, fixed=fixed, useBytes=useBytes, ...);
      yLevels <- unique(gsub(pattern=pattern, replacement=replacement, x=levels(x), ignore.case=ignore.case,
         perl=perl, fixed=fixed, useBytes=useBytes, ...));
   }
   if (!is.null(xNames)) {
      names(y) <- xNames;
   }
   return(factor(y, levels=yLevels));
}

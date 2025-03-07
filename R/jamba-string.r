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
#' @param pattern the grep pattern to use with `base::grep()`
#' @param x vector to use in the grep
#' @param ignore.case logical default TRUE, meaning the grep will be performed
#'    in case-insensitive mode.
#' @param minCount integer minimum number of matches required to return TRUE.
#' @param naToBlank logical whether to convert NA to blank, instead of
#'    allowing grep to handle NA values as-is.
#' @param ... additional arguments are ignored.
#'
#' @returns logical indicating whether the grep match criteria were met,
#'    TRUE indicates the grep pattern was present in minCount or more
#'    number of entries.
#'
#' @seealso `base::grep()`
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
(pattern,
 x=NULL,
 ignore.case=TRUE,
 minCount=1,
 naToBlank=FALSE,
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
#' This function is a simple wrapper around `base::grep()` which
#' runs in case-insensitive mode, and returns matching values. It is
#' particularly helpful when grabbing values from a vector.
#'
#' @param ...,value,ignore.case parameters sent to `base::grep()`
#'
#' @returns vector of matching values
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
#' This function is a simple wrapper around `base::grep()` which
#' returns matching values. It is
#' particularly helpful when grabbing values from a vector, but where the
#' case (uppercase or lowercase) is known.
#'
#' @param ...,value,ignore.case parameters sent to `base::grep()`
#'
#' @returns vector of matching values
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
#' This function is a simple wrapper around `base::grep()` which
#' runs in case-insensitive mode. It is mainly used to save keystrokes,
#' but is consistently named alongside \code{\link{vgrep}} and
#' \code{\link{vigrep}}.
#'
#' @param ...,ignore.case parameters sent to `base::grep()`
#'
#' @returns vector of matching indices
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

#' case-insensitive logical grepl
#'
#' case-insensitive logical grepl
#'
#' This function is a simple wrapper around `base::grepl()` which
#' runs in case-insensitive mode simply by adding default `ignore.case=TRUE`.
#' It is mainly used for convenience.
#'
#' @param ...,ignore.case parameters sent to `base::grep()`
#'
#' @returns `logical` vector indicating pattern match
#'
#' @examples
#' V <- paste0(LETTERS[1:5], LETTERS[4:8]);
#' ig1 <- grepl("D", V);
#' ig2 <- igrepl("D", V);
#' ig3 <- grepl("d", V);
#' ig4 <- igrepl("d", V);
#' data.frame(V,
#'    grepl_D=ig1,
#'    grepl_d=ig3,
#'    igrepl_D=ig2,
#'    igrepl_d=ig4);
#'
#' @family jam grep functions
#'
#' @export
igrepl <- function
(...,
 ignore.case=TRUE)
{
   ## Purpose is simply to provide quick wrapper for case-insensitive grep()
   grepl(ignore.case=ignore.case,
      ...);
}

#' case-insensitive grep, returning unmatched indices
#'
#' case-insensitive grep, returning unmatched indices
#'
#' This function is a simple wrapper around `base::grep()` which
#' runs in case-insensitive mode, and returns unmatched entries.
#' It is mainly used to save keystrokes,
#' but is consistently named alongside \code{\link{vgrep}} and
#' \code{\link{vigrep}}, and quite helpful for writing concise code.
#'
#' @param ...,ignore.case,invert parameters sent to `base::grep()`
#'
#' @returns vector of non-matching indices
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
#' This function is a simple wrapper around `base::grep()` which
#' runs in case-insensitive mode, and returns unmatched values.
#' It is mainly used to save keystrokes,
#' but is consistently named alongside \code{\link{vgrep}} and
#' \code{\link{vigrep}}, and quite helpful for writing concise code.
#' It is particularly useful for removing unwanted entries from a long
#' vector, for example removing accession numbers from a long
#' vector of gene symbols in order to review gene annotations.
#'
#' @param ...,ignore.case,value,invert parameters sent to `base::grep()`
#'
#' @returns vector of non-matching indices
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
#' `grep()` does not accept multiple patterns as input. This function
#' also only returns the unique matches in the order they were matched,
#' which alleviates the need to run a series of `grep()` functions
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
#' @param ignore.case `logical` parameter sent to `base::grep()`, TRUE
#'    runs in case-insensitive mode, as by default.
#' @param value `logical` indicating whether to return the matched value,
#'    or when `value=FALSE` the index position is returned.
#' @param ... additional arguments are passed to `vigrep()`.
#'
#' @returns `character` vector with entries in `x` reordered to match
#'    the order of `patterns` provided,  or `list` when `returnType="list"`
#'    named by `patterns` in the order provided. When `value=FALSE` then
#'    it returns `integer` index values of `x`.
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
#' The purpose of this function is to emulate `do.call(rbind, x)` on a list
#' of vectors, while specifically handling when there are different
#' numbers of entries per vector. The output `matrix` number of columns
#' will be the longest vector (or largest number of columns) in the
#' input list `x`.
#'
#' Instead of recycling values in each row to fill the target number
#' of columns, this function fills cells with blank fields,
#' with default argument `fixBlanks=TRUE`.
#'
#' In extensive timings tests at the time this function was created,
#' this technique was notably faster than alternatives.
#' It runs  `do.call(rbind, x)` then subsequently replaces recycled values
#' with blank entries, in a manner that is notably faster than
#' alternative approaches such as pre-processing the input data.
#'
#' @returns `matrix` unless `returnDF=TRUE` in which the output is coerced
#'    to a `data.frame`.
#'    The rownames by default are derived from the list names,
#'    but the colnames are not derived from the vector names.
#'    If input `x` contains `data.frame` or `matrix` objects, the output
#'    will retain those values.
#'
#' @param x `list` of atomic `vector`, `matrix`, or `data.frame`
#'    objects.
#' @param emptyValue `character` value to use to represent missing values,
#'    whenever a blank cell is introduced into the resulting matrix
#' @param nullValue optional value used to replace NULL entries in
#'    the input list, useful especially when the data was produced
#'    by `strsplit()` with `""`. Use `nullValue=""` to replace `NULL`
#'    with `""` and preserve the original list length. Otherwise when
#'    `nullValue=NULL` any empty entries will be silently dropped.
#' @param keepListNames `logical` whether to use list names as rownames
#'    in the resulting matrix or data.frame.
#' @param newColnames NULL or `character` vector of colnames to use for the
#'    resulting matrix or data.frame.
#' @param newRownames NULL or `character` vector of rownames to use for the
#'    resulting matrix or data.frame. If supplied, this value overrides the
#'    keepListNames=TRUE use of list names as rownames.
#' @param fixBlanks `logical` whether to use blank values instead of repeating
#'    each vector to the length of the maximum vector length when filling
#'    each row of the matrix or data.frame.
#' @param returnDF `logical` whether to return a data.frame, by default FALSE,
#'    a matrix is returned.
#' @param verbose `logical` whether to print verbose output during processing.
#' @param ... Additional arguments are ignored.
#'
#' @examples
#' L <- list(a=LETTERS[1:4], b=letters[1:3]);
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
      #xDF <- unlistDataFrame(as.data.frame(xDF), verbose=verbose, ...);
      xDF <- data.frame(check.names=FALSE,
         stringsAsFactors=FALSE,
         xDF);
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
#' @returns character vector of unique names
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
#' @param ... Additional arguments are ignored.
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
   ## numberStyle="letters" uses lowercase letters as digits
   ## numberStyle="LETTERS" uses uppercase letters as digits
   ## Be aware that letters can only go to roughly 18,000 entries,
   ## given the current implementation of colNum2excelName
   ##
   ## When useNchar is numeric, it sets doPadInteger=TRUE,
   ## and will use at least that many digits in padding the integer.
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
      xSubOnes <- stats::setNames(rep(1, sum(!dupes)), x[!dupes]);
      xSub <- c(xSubOnes, xSubDupes);
   } else {
      xSub <- stats::setNames(rep(1, sum(!dupes)), x[!dupes]);
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
#' @returns vector with names defined
#'
#' @family jam string functions
#'
#' @param x `character` vector, or `data.frame` or equivalent
#'    (matrix, or tibble) with two columns, the second column
#'    is used to name values in the first column.
#' @param y `character` or NULL, with names. If NULL then x is used.
#'    Note that y is recycled to the length of x, prior to being sent
#'    to the makeNamesFunc.
#'    In fringe cases, y can be a matrix, data.frame, or tibble, in which
#'    case `pasteByRow()` will be used to create a character string
#'    to be used for vector names. Note this case is activated only when x
#'    is not a two column matrix, data.frame, or tibble.
#' @param makeNamesFunc `function` to make names unique, by default
#'    `makeNames()` which ensures names are unique.
#' @param ... passed to `makeNamesFunc`, or to
#'    `pasteByRow()` if y is a two column data.frame, matrix, or
#'    tibble. Thus, `sep` can be defined here as a delimiter between
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
#' @returns vector of names, whose names are uniquely assigned using
#'    \code{\link{makeNames}} using the values of the vector.
#'
#' @family jam string functions
#'
#' @param x `character` vector or any object which has names available
#'    `names(x)`.
#' @param makeNamesFunc `function` used to create unique names, in the event that
#'    the names(x) are not unique.
#' @param ... Additional arguments are ignored.
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
#'     data.frame(mean=mean(i), median=stats::median(i));
#'  });
#'
#' # So the next step is to run lapply() on the names
#' lapply(names(K), function(i){
#'    iDF <- data.frame(mean=mean(K[[i]]), median=stats::median(K[[i]]));
#'    colnames(iDF) <- paste(c("mean", "median"), i);
#'    iDF;
#' })
#' # The result is good, but the list is no longer named.
#' # The nameVectorN() function is helpful for maintaining the names.
#'
#' # So we run lapply() on the named-names, which keeps the names in
#' # the resulting list, and sends it into the function.
#' lapply(nameVectorN(K), function(i){
#'    iDF <- data.frame(mean=mean(K[[i]]), median=stats::median(K[[i]]));
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
#' @returns list with NULL entries either removed, or replaced with nullValue.
#' This function is typically called so it removed list elements which are
#' NULL, resulting in a list that contains non-NULL entries. This function
#' can also be useful when NULL values should be changed to something else,
#' perhaps a character value "NULL" to be used as a label.
#'
#' @family jam practical functions
#'
#' @param x `list` or other object which may contain NULL.
#' @param nullValue `character` optional replacement value, default NULL,
#'    which causes the entry to be removed.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' x <- list(A=1:6, B=NULL, C=letters[11:16]);
#' rmNULL(x)
#' rmNULL(x, nullValue=NA)
#'
#' @export
rmNULL <- function
(x,
 nullValue=NULL,
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
#' @returns vector with NA entries either removed, or replaced with naValue,
#'    and NULL entries either removed or replaced by nullValue.
#'
#' @family jam practical functions
#'
#' @param x vector input
#' @param naValue NULL or single replacement value for NA entries. If NULL,
#'    then NA entries are removed from the result.
#' @param rmNULL `logical` whether to replace NULL entries with `nullValue`
#' @param nullValue NULL or single replacement value for NULL entries. If NULL,
#'    then NULL entries are removed from the result.
#' @param rmInfinite `logical` whether to replace Infinite values with
#'    infiniteValue
#' @param infiniteValue value to use when rmInfinite==TRUE to replace
#'    entries which are Inf or -Inf.
#' @param rmNAnames `logical` whether to remove entries which have NA as the
#'    name, regardless whether the entry itself is NA.
#' @param verbose `logical` whether to print verbose output
#' @param ... additional arguments are ignored.
#'
#' @examples
#' # by default it removes NA, shortening the vector
#' rmNA(c(1, 5, 4, NA, 10, NA))
#'
#' # convenient to replace NA with a fixed value
#' rmNA(c(1, 5, 4, NA, 10, NA), naValue=0)
#'
#' m <- matrix(ncol=3, 1:9)
#' m[1, 2] <- NA;
#' rmNA(m, naValue=-1)
#'
#' # by default NA and Inf is removed
#' rmNA(c(1, 5, 4, NA, 10, NA, Inf, -Inf))
#'
#' # NA and Inf can be replaced, note Inf retains the sign
#' rmNA(c(1, 5, 4, NA, 10, NA, Inf, -Inf), naValue=0, infiniteValue=100)
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

   # No change for now, version 0.0.87.900
   # Call rmNAs() when input x is list?
   # if (is.list(x)) {
   #    return(rmNAs(x,
   #       naValue=naValue,
   #       rmNULL=rmNULL,
   #       nullValue=nullValue,
   #       rmInfinite=rmInfinite,
   #       infiniteValue=infiniteValue,
   #       rmNAnames=rmNAnames,
   #       verbose=verbose));
   # }

   if (!"list" %in% class(x) && rmInfinite && any(is.infinite(x))) {
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
#' @returns numeric vector with infinite values either removed, or
#'    replaced with the supplied value.
#'
#' @family jam practical functions
#'
#' @param x vector input
#' @param infiniteValue NULL to remove Infinite values, or a replacement value
#' @param ... additional parameters are ignored
#'
#' @examples
#' rmInfinite(c(1, 5, 4, 10, Inf, 1, -Inf))
#'
#' rmInfinite(c(1, 5, 4, 10, Inf, 1, -Inf), infiniteValue=1000)
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


#' apply unique to each element of a list
#'
#' Apply unique to each element of a list, usually a list of vectors
#'
#' This function will attempt to use `S4Vectors::unique()` which is
#' substantially faster than any `apply` family function, especially
#' for very long lists. However, when `S4Vectors` is not installed,
#' it applies uniqueness to the `unlist`ed vector of values, which is
#' also substantially faster than the `apply` family functions for
#' long lists, but which may still be less efficient than the
#' C implementation provided by `S4Vectors`.
#'
#' @returns `list` with unique values in each list element.
#'
#' @param x input list of vectors
#' @param keepNames boolean indicating whether to keep the list element
#'    names in the returned results.
#' @param incomparables see [unique()] for details, this value is only
#'    sent to `S4Vectors::unique()` when the Bioconductor package
#'    `S4Vectors` is installed, and is ignored otherwise for efficiency.
#' @param useBioc `logical`, default TRUE,  indicating whether this
#'    function should try to use `S4Vectors::unique()` when the
#'    Bioconductor package `S4Vectors` is installed, otherwise it will
#'    use a somewhat less efficient bulk operation.
#' @param useSimpleBioc `logical`, default FALSE, whether to use a legacy
#'    mechanism with `S4Vectors` and is maintained for edge cases where
#'    it might be faster.
#' @param xclass `character` optional vector of classes, used to invoke
#'    optimized logic when the class is known upfront.
#' @param ... additional arguments are ignored.
#'
#' @family jam list functions
#'
#' @examples
#' L1 <- list(CA=nameVector(LETTERS[c(1:4,2,7,4,6)]),
#'    B=letters[c(7:11,9,3)],
#'    C2=NULL,
#'    D=nameVector(LETTERS[4]));
#' L1;
#' uniques(L1);
#'
#' uniques(L1, useBioc=FALSE);
#'
#' @export
uniques <- function
(x,
 keepNames=TRUE,
 incomparables=FALSE,
 useBioc=TRUE,
 useSimpleBioc=FALSE,
 xclass=NULL,
 ...)
{
   ## Purpose is to take a list of vectors and return unique members
   ## for each vector in the list.
   ##
   ## keepNames=TRUE will keep the first name for the each duplicated entry
   if (useBioc || useSimpleBioc) {
      if (!requireNamespace("S4Vectors", quietly=TRUE)) {
         useSimpleBioc <- FALSE;
         useBioc <- FALSE;
      }
   }
   if (useBioc) {
      if (!requireNamespace("IRanges", quietly=TRUE)) {
         useBioc <- FALSE;
      }
   }
   xNames <- names(x);
   if (useSimpleBioc) {
      ## Former method used List(x)
      ## which reverted to SimpleList for simple list input
      ## and SimpleList does not have the amazing optimization
      as.list(
         unique(S4Vectors::List(x),
            incomparables=incomparables,
            ...));
   } else if (useBioc) {
      ## Pro tip: use specific class to invoke optimized functions
      ## otherwise they revert to base lapply(x, unique)
      if (length(xclass) == 0) {
         xclass <- sclass(x);
      }
      if (is.list(xclass)) {
         xclass <- cPaste(xclass,
            checkClass=FALSE,
            useBioc=useBioc,
            ...)
      }
      if (any(grepl(",", xclass))) {
         xclass <- gsub(",.*$", "", xclass);
      }
      xlist <- list();
      xclassesu <- unique(xclass);
      for (xclassu in xclassesu) {
         xclassidx <- which(xclass %in% xclassu);
         if ("character" %in% xclassu) {
            xlist[xclassidx] <- as.list(unique(
               IRanges::CharacterList(x[xclassidx]),
               incomparables=incomparables))
         } else if ("factor" %in% xclassu) {
            xlist[xclassidx] <- as.list(unique(
               IRanges::FactorList(x[xclassidx]),
               incomparables=incomparables))
         } else if ("integer" %in% xclassu) {
            xlist[xclassidx] <- as.list(unique(
               IRanges::IntegerList(x[xclassidx]),
               incomparables=incomparables))
         } else if ("logical" %in% xclassu) {
            xlist[xclassidx] <- as.list(unique(
               IRanges::LogicalList(x[xclassidx]),
               incomparables=incomparables))
         } else if ("raw" %in% xclassu) {
            xlist[xclassidx] <- as.list(unique(
               IRanges::RawList(x[xclassidx]),
               incomparables=incomparables))
         } else if ("Rle" %in% xclassu) {
            xlist[xclassidx] <- as.list(unique(
               IRanges::RleList(x[xclassidx]),
               incomparables=incomparables))
         } else if ("complex" %in% xclassu) {
            xlist[xclassidx] <- as.list(unique(
               IRanges::ComplexList(x[xclassidx]),
               incomparables=incomparables))
         } else if ("GRanges" %in% xclassu &&
               requireNamespace("GenomicRanges", quietly=TRUE)) {
            xlist[xclassidx] <- lapply(
               unique(GenomicRanges::GRangesList(x[xclassidx])), function(gr){
                  gr
               });
         } else {
            xlist[xclassidx] <- lapply(x[xclassidx], unique);
         }
      }
      names(xlist) <- xNames;
      return(xlist);
   } else if (!keepNames) {
      lapply(x, unique);
   } else if (length(xclass) == 0 || length(unique(xclass)) > 1) {
      # new class-sensitive approach
      if (length(xclass) == 0) {
         xclass <- sclass(x);
      }
      if (is.list(xclass)) {
         xclass <- cPaste(xclass,
            checkClass=FALSE,
            useBioc=useBioc,
            ...)
      }
      if (any(grepl(",", xclass))) {
         xclass <- gsub(",.*$", "", xclass);
      }
      xlist <- list();
      xclassesu <- unique(xclass);
      for (xclassu in xclassesu) {
         xclassidx <- which(xclass %in% xclassu);
         if (xclassu %in% c("character", "factor",
            "numeric", "integer", "logical")) {
            xlist[xclassidx] <- uniques(x[xclassidx],
               useBioc=useBioc,
               useSimpleBioc=useSimpleBioc,
               xclass=rep(xclassu, length(xclassidx)))
         } else {
            tryCatch({
               xlist[xclassidx] <- uniques(x[xclassidx],
                  useBioc=useBioc,
                  useSimpleBioc=useSimpleBioc,
                  xclass=rep(xclassu, length(xclassidx)))
            }, error=function(e){
               # print(e);# debug
               errmsg <- paste0("Class not supported by uniques(): ",
                  xclassu);
               stop(errmsg);
            })
         }
      }
      names(xlist) <- xNames;
      return(xlist);

   } else {
      # non-Bioc method when only one class is involved
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
      if (is.null(xuse)) {
         xuse <- character(0);
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
#' * `cPaste()` concatenates vector values using a delimiter.
#' * `cPasteS()` sorts each vector using `mixedSort()`.
#' * `cPasteU()` applies `uniques()` to retain unique values per vector.
#' * `cPasteSU()` applies `mixedSort()` and `uniques()`.
#'
#' This function is essentially a wrapper for `S4Vectors::unstrsplit()`
#' except that it also optionally applies uniqueness to each vector
#' in the list, and sorts values in each vector using `mixedOrder()`.
#'
#' The sorting and uniqueness is applied to the `unlist`ed vector of
#' values, which is substantially faster than any `apply` family function
#' equivalent. The uniqueness is performed by `uniques()`, which itself
#' will use `S4Vectors::unique()` if available.
#'
#' @returns `character` vector with the same names and in the same order
#'    as the input list `x`.
#'
#' @param x `list` of vectors
#' @param sep `character` delimiter used to paste multiple values together
#' @param doSort `logical` indicating whether to sort each vector
#'    using [mixedOrder()].
#' @param makeUnique `logical` indicating whether to make each vector in
#'    the input list unique before pasting its values together.
#' @param na.rm `logical` indicating whether to remove NA values from
#'    each vector in the input list. When `na.rm` is `TRUE` and a
#'    list element contains only `NA` values, the resulting string
#'    will be `""`.
#' @param keepFactors `logical` only used when `useLegacy=TRUE` and
#'    `doSort=TRUE`; indicating whether to preserve factors,
#'    keeping factor level order. When
#'    `keepFactors=TRUE`, if any list element is a `factor`, all elements
#'    are converted to factors. Note that this step combines overall
#'    factor levels, and non-factors will be ordered using
#'    `base::order()` instead of `jamba::mixedOrder()` (for now.)
#' @param checkClass `logical`, default TRUE, whether to check the class
#'    of each vector in the input list.
#'    * When TRUE, it confirms the class of each element in the `list`
#'    before processing, to prevent conversion which may otherwise
#'    lose information.
#'    * For all cases when a known vector is split into a `list`,
#'    `checkClass=FALSE` is preferred since there is only one class
#'    in the resulting `list` elements. This approach is faster
#'    especially for for large input lists, 10000 or more.
#'    * When `checkClass=FALSE` it assumes all entries can be
#'    coerced to `character`, which is fastest, but does not preserve
#'    factor levels due to R coersion methods used by `unlist()`.
#' @param useBioc `logical` indicating whether this function should try
#'    to use `S4Vectors::unstrsplit()` when the Bioconductor package
#'    `S4Vectors` is installed, otherwise it will use a less
#'    efficient `mapply()` operation.
#' @param useLegacy `logical` indicating whether to enable to previous
#'    legacy process used by `cPaste()`.
#' @param honorFactor `logical` passed to `mixedSorts()`, whether any
#'    `factor` vector should be sorted in factor level order.
#'    When `honorFactor=FALSE` then even `factor` vectors are sorted
#'    as if they were `character` vectors, ignoring the factor levels.
#' @param verbose `logical` indicating whether to print verbose output.
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
 useLegacy=FALSE,
 honorFactor=TRUE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to utilize the vectorized function unstrsplit() from the S4Vectors package
   if (length(x) == 0) {
      return("");
   }
   if (TRUE %in% useBioc &&
         !requireNamespace("S4Vectors", quietly=TRUE)) {
      if (verbose) {
         warning(paste0("cPaste() is substantially faster when",
            " Bioconductor package S4Vectors is installed."));
      }
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
         xclass=xclass,
         ...);
   }

   ## Optionally sort vectors
   if (doSort) {
      x <- mixedSorts(x,
         xclass=xclass,
         honorFactor=honorFactor,
         ...);
   }

   ## Legacy processing below
   if (useLegacy) {
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
   }

   # specifically enforce na.rm=TRUE
   if (length(na.rm) > 0 && TRUE %in% na.rm) {
      x <- rmNAs(x,
         naValue=NULL);
   }

   if (useBioc) {
      ## Note: The explicit conversion to class CharacterList is required
      ## in order to avoid errors with single list elements of NA when
      ## na.rm=FALSE. Specifically, unstrsplit() requires all elements in
      ## the list to be "character" class, and a single NA is class "logical"
      ## and causes an error.

      if (verbose) {
         printDebug("cPaste(): ",
            "Using Bioc unstrsplit().")
      }
      # if "factor" and non-factor classes are present, convert them to one class
      if (any(grepl("factor", ignore.case=TRUE, xclass)) &&
            length(unique(xclass)) > 1) {
         xfactor <- grepl("factor", ignore.case=TRUE, xclass);
         x[xfactor] <- lapply(x[xfactor], as.character);
      }
      xNew <- S4Vectors::unstrsplit(
         IRanges::CharacterList(x),
         sep=sep);
   } else {
      if (verbose) {
         printDebug("cPaste(): ",
            "Using mapply().")
      }
      xNew <- mapply(paste,
         x,
         collapse=sep);
   }

   ## Revert names(x) to their original state
   names(xNew) <- xNames;

   return(xNew);
}


#' @rdname cPaste
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


#' @rdname cPaste
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

#' @rdname cPaste
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

#' @rdname cPaste
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

#' Rename columns in a data.frame, matrix, tibble, or GRanges object
#'
#' Rename columns in a data.frame, matrix, tibble, or GRanges object
#'
#' This function is intended to rename one or more columns in a
#' `data.frame`, `matrix`, tibble, or `GRanges` related object.
#' It will gracefully ignore columns which do not match,
#' in order to make it possible to call the
#' function again without problem.
#'
#' This function will also recognize input objects `GRanges`,
#' `ucscData`, and `IRanges`, which store annotation in `DataFrame`
#' accessible via `S4Vectors::values()`. Note the `IRanges` package
#' is required, for its generic function `values()`.
#'
#' The values supplied in `to` and `from` are converted from `factor`
#' to `character` to avoid coersion by R to `integer`, which was
#' noted in output prior to jamba version `0.0.72.900`.
#'
#'
#' @returns `data.frame` or object equivalent to the input `x`,
#'    with columns `from` renamed to values in `to`. For genomic
#'    ranges objects such as `GRanges` and `IRanges`, the colnames
#'    are updated in `S4Vectors::values(x)`.
#'
#' @family jam practical functions
#'
#' @param x `data.frame`, `matrix`, `tbl`, or `GRanges` equivalent
#'    object. It will work on any object for which `colnames()`
#'    is defined.
#' @param from `character` vector of colnames expected to be in `x`.
#'    Any values that do not match `colnames(x)` are ignored.
#' @param to `character` vector with `length(to) == length(from)`
#'    corresponding to the target name for any colnames that
#'    match `from`.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... Additional arguments are ignored.
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

   # coerce factor to character to prevent being coerced to integer
   if ("factor" %in% class(from)) {
      from <- as.character(from);
   }
   if ("factor" %in% class(to)) {
      to <- as.character(to);
   }
   if (length(to) != length(from)) {
      stop("length(from) must be equal to length(to)");
   }

   if (igrepHas("ucscdata|granges|iranges", class(x))) {
      if (verbose) {
         printDebug("renameColumn(): ",
            "Recognized GRanges input.");
      }
      if (!check_pkg_installed("IRanges")) {
         stop(paste(
            "Input data requires the IRanges Bioconductor package,",
            "install with BiocManager::install('IRanges')"));
      }
      renameSet <- which(from %in% colnames(S4Vectors::values(x)) & from != to);
      renameWhich <- match(from[renameSet], colnames(S4Vectors::values(x)));
      if (verbose) {
         printDebug("renameColumn(): ",
            "Renaming ",
            formatInt(length(renameWhich)),
            " columns.");
      }
      if (length(renameWhich) > 0) {
         colnames(S4Vectors::values(x))[renameWhich] <- to[renameSet];
      }
   } else {
      renameSet <- which(from %in% colnames(x) & from != to);
      renameWhich <- match(from[renameSet], colnames(x));
      if (verbose) {
         printDebug("renameColumn(): ",
            "Renaming ",
            formatInt(length(renameWhich)),
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
#' for situations like imported 'Excel' data, where there may be one
#' header value representing a series of cells.
#'
#' The method used does not loop through the data, and should scale
#' fairly well with good efficiency even for extremely large vectors.
#'
#' @returns `character` vector where blank entries are filled with the
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
#' @returns `character` vector if `x` is a vector, or if `x` is a matrix
#' a matrix will be returned.
#'
#' @family jam string functions
#'
#' @param x `numeric` vector or matrix
#' @param big.mark,trim,scientific passed to `base::format()` but
#'    configured with defaults intended for integer values:
#'    * `big.mark=","` adds comma between thousands.
#'    * `trim=TRUE` to trim excess whitespace.
#'    * `scientific=FALSE` to prevent exponential notation.
#' @param forceInteger `logical`, default TRUE, whether to round `numeric`
#'    to `integer` prior to calling `base::format()`.
#' @param ... Additional arguments are ignored.
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
   if ("matrix" %in% class(x)) {
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
#' @returns `data.frame` with two columns, or three columns when
#'    `useVectorNames=TRUE` and the input `x` contains names.
#'
#' @examples
#' list2df(list(lower=head(letters, 5), UPPER=head(LETTERS, 10)))
#'
#' list2df(list(lower=nameVector(head(letters, 5)),
#'    UPPER=nameVector(head(LETTERS, 10))))
#'
#' list2df(list(lower=nameVector(head(letters, 5)),
#'    UPPER=nameVector(head(LETTERS, 10))),
#'    useVectorNames=FALSE)
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
#' @returns `vector` or `list` based upon argument `how`.
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
#' nested list sizes. An exception is when the data contained in `x`
#' represents multiple classes, see below.
#'
#' When data in `x` represents multiple classes, for example `character`
#' and `factor`, the mechanism is slightly different and not as well-
#' optimized for large length `x`. The method uses
#' `rapply(x, how="replace", mixedSort)` which recursively, and iteratively,
#' calls `mixedSort()` on each vector, and therefore returns data in the
#' same nested `list` structure as provided in `x`.
#'
#' When data in `x` represents only one class, data is `unlist()` to one
#' large vector, which is sorted with `mixedSort()`, then split back into
#' `list` structure representing `x` input.
#'
#' @family jam sort functions
#' @family jam list functions
#'
#' @returns `list` after applying `mixedSort()` to its elements.
#'
#' @inheritParams mixedSort
#' @param xclass `character` vector of classes in `x`, used for slight
#'    optimization to re-use this vector if it has already been
#'    defined for `x`. When `NULL` it is created within this function.
#' @param indent `numeric` used only when `verbose=TRUE` to determine
#'    the number of spaces indented for verbose output, passed to
#'    `printDebug()`.
#' @param honorFactor `logical`, default TRUE, used to enforce factor level
#'    sort order, when FALSE it sorts as `character`.
#' @param na.rm `logical`, default FALSE, indicating whether to remove
#'    NA values.
#' @param debug `logical`, default FALSE, whether to print detailed
#'    debug output.
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
#' # Notice "e6" is sorted before "e30"
#' mixedSorts(xL)
#'
#' # for fun, compare to lapply(xL, sort)
#' # Notice "e6" is sorted after "e30"
#' lapply(xL, sort)
#'
#' # test super-long list
#' xL10k <- rep(xL, length.out=10000);
#' names(xL10k) <- as.character(seq_along(xL10k));
#' print(head(mixedSorts(xL10k), 10))
#'
#' # Now make some list vectors into factors
#' xF <- xL;
#' xF$c <- factor(xL$c)
#' # for fun, reverse the levels
#' xF$c <- factor(xF$c,
#'    levels=rev(levels(xF$c)))
#' xF
#' mixedSorts(xF)
#'
#' # test super-long list
#' xF10k <- rep(xF, length.out=10000);
#' names(xF10k) <- as.character(seq_along(xF10k));
#' print(head(mixedSorts(xF10k), 10))
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
#' # Make a nested list with two sub-lists
#' set.seed(1);
#' l2 <- list(
#'    A=list(
#'       E=sample(nameVector(11:13, rev(letters[11:13])))
#'    ),
#'    B=list(
#'       C=sample(nameVector(4:8, rev(LETTERS[4:8]))),
#'       D=sample(nameVector(LETTERS[2:5], rev(LETTERS[2:5])))
#'    )
#' )
#' l2;
#' # The output is a nested list with the same structure
#' mixedSorts(l2);
#' mixedSorts(l2, sortByName=TRUE);
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
 useCaseTiebreak=TRUE,
 sortByName=FALSE,
 na.rm=FALSE,
 verbose=FALSE,
 NAlast=TRUE,
 honorFactor=TRUE,
 xclass=NULL,
 indent=0,
 debug=FALSE,
 ...)
{
   ## Purpose is to take a list of vectors and run mixedSort() efficiently
   ##
   # if empty then return without change
   if (length(x) == 0) {
      return(x)
   }
   # if entirely empty then return without change
   if (all(lengths(x) == 0)) {
      return(x)
   }
   xNames <- names(x);

   # recursive class() because rapply drops NULL entries
   rclass <- function(x){
      if (is.list(x)) {
         return(lapply(x, rclass))
      }
      return(class(x))
   }
   if (length(xclass) == 0) {
      # xclass <- rapply(x, class, how="unlist");
      xclass <- unlist(rclass(x))
   }
   xclassu <- unique(xclass);
   if (!TRUE %in% honorFactor &&
         any(c("factor") %in% xclass)) {
      xclass[xclass %in% "factor"] <- "character"
   }
   xclass <- unique(xclass);

   if (length(names(x)) == 0) {
      names(x) <- seq_along(x);
   } else {
      if (any(duplicated(names(x)))) {
         names(x) <- makeNames(names(x));
      }
   }

   # check for nested list
   x_has_list <- any(sapply(x, function(i){"list" %in% class(i)}));

   # if (!TRUE %in% sortByName && length(xclass) > 1) {
   if (length(xclass) > 1) {
      # slight optimization
      # split simple (non-nested) list into subsets by class
      # then run each class in bulk/optimized mode
      # then reassign
      if (!x_has_list) {
         #
         if (verbose) {
            printDebug("mixedSorts(): ",
               indent=indent,
               "Performing sort for each class subtype.");
         }
         xclass_sets <- cPaste(lapply(x, class));
         for (xclass_set in unique(xclass_sets)) {
            if (verbose) {
               printDebug("mixedSorts(): ",
                  indent=indent + 3,
                  "Class subtype: ", xclass_set);
            }
            k <- which(xclass_sets %in% xclass_set);
            if ("NULL" %in% xclass_set) {
               if (verbose) {
                  printDebug("mixedSorts(): ",
                     indent=indent + 6,
                     "Skipping NULL class subtype: ", xclass_set);
               }
            } else {
               x[k] <- mixedSorts(x[k],
                  blanksFirst=blanksFirst,
                  na.last=na.last,
                  keepNegative=keepNegative,
                  keepInfinite=keepInfinite,
                  keepDecimal=keepDecimal,
                  ignore.case=ignore.case,
                  useCaseTiebreak=useCaseTiebreak,
                  sortByName=sortByName,
                  na.rm=na.rm,
                  xclass=xclass_sets[k],
                  honorFactor=honorFactor,
                  indent=indent + 12,
                  verbose=verbose,
                  ...)
            }
         }
         names(x) <- xNames;
         return(x);
      } else {
         # iterate nested list individually
         if (verbose) {
            printDebug("mixedSorts(): ",
               indent=indent,
               "Performing rapply() mixedSort() for each nested sublist.");
         }
         xnew <- rapply(x, how="replace", function(i){
            mixedSort(i,
               blanksFirst=blanksFirst,
               na.last=na.last,
               keepNegative=keepNegative,
               keepInfinite=keepInfinite,
               keepDecimal=keepDecimal,
               ignore.case=ignore.case,
               useCaseTiebreak=useCaseTiebreak,
               sortByName=sortByName,
               honorFactor=honorFactor,
               ...)
         });
         names(xnew) <- xNames;
         return(xnew);
      }
   }
   # at this point xclass only has one value
   # unless sortByName=TRUE
   if (verbose) {
      printDebug("mixedSorts(): ",
         indent=indent,
         "Sorting list containing a single class.");
   }

   ## unlist values
   if (length(xclassu) > 1 &&
      "factor" %in% xclassu) {
      # when factor is included, it is converted to character at this step
      if (verbose) {
         printDebug("mixedSorts(): ",
            indent=indent,
            "Condensing factor sublist to character.");
      }
      xu <- unlist(rapply(x, how="unlist", function(i){
         if ("factor" %in% class(i)) {
            as.character(i)
         } else {
            i
         }
      }));
   } else {
      xu <- unlist(x);
   }

   ## vector names
   xun <- unname(jam_rapply(x, names));
   if (TRUE %in% sortByName &&
      length(xun) < length(xu)) {
      stop("Cannot sort by name because not all vectors have names.");
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

   if (TRUE %in% sortByName) {
      if (verbose) {
         printDebug("mixedSorts(): ",
            indent=indent,
            "Performing sortByName.");
      }
      xu_use <- xun;
   } else {
      xu_use <- xu;
   }
   # print("xu_use:");print(xu_use);
   if (honorFactor %in% TRUE && "factor" %in% class(xu_use)) {
      if (verbose) {
         printDebug("mixedSorts(): ",
            indent=indent,
            "Ordering by factor levels.");
      }
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
         useCaseTiebreak=useCaseTiebreak,
         honorFactor=honorFactor,
         ...);
   }
   xu <- xu[xuOrder];
   xn <- xn[xuOrder];
   xu_use <- xu_use[xuOrder];
   if (length(xun) > 0) {
      xun <- xun[xuOrder];
   }

   ## Optionally remove NA values
   if (TRUE %in% na.rm && any(is.na(xu_use))) {
      if (verbose) {
         printDebug("mixedSorts(): ",
            indent=indent,
            "Removing NA values.");
      }
      whichNotNA <- which(!is.na(xu_use));
      xu <- xu[whichNotNA];
      xn <- xn[whichNotNA];
      if (length(xun) > 0) {
         xun <- xun[whichNotNA];
      }
      xu_use <- xu_use[whichNotNA];
      # shrink input x so it still works with relist() below
      if (any(x_has_list)) {
         if (verbose) {
            printDebug("mixedSorts(): ",
               indent=indent,
               "Removing NA values from nested input list.");
         }
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
   if (x_has_list) {
      if (verbose) {
         printDebug("mixedSorts(): ",
            indent=indent,
            "Re-creating nested list structure.");
         printDebug("mixedSorts(): ",
            indent=indent,
            "xu:");
         print(xu)
         printDebug("mixedSorts(): ",
            indent=indent,
            "xn:");
         print(xn)
      }
      # xu_ordered <- unlist(unname(split(xu, xn)))
      xu_ordered <- unlist(unname(split(xu, xn)))
      xnew <- relist_named(xu_ordered, x);
      if (length(xnew) == length(xNames)) {
         names(xnew) <- xNames;
      }
   } else {
      if (verbose) {
         printDebug("mixedSorts(): ",
            indent=indent,
            "Re-creating list structure.");
      }
      xnew <- split(xu, xn);
      names(xnew) <- xNames;
   }
   return(xnew);
}

#' relist a vector which allows re-ordered names
#'
#' relist a vector which imposes the model object list structure while
#' allowing vector elements and names to be re-ordered
#'
#' This function is a simple update to `utils::relist()`
#' that allows the order of vectors to change, alongside the
#' correct names for each element.
#'
#' More specifically, this function does not replace the
#' updated names with the corresponding names from
#' the list `skeleton`, as is the case in default implementation of
#' `utils::relist()`.
#'
#' This function is called by `mixedSorts()` which iteratively calls
#' `mixedOrder()` on each vector component of the input `list`,
#' and permits nested lists. The result is a single sorted vector
#' which is split into the `list` components, then relist-ed to
#' the original structure. During the process, it is important
#' to retain vector names in the order defined by `mixedOrder()`.
#'
#' @returns `list` object with the same structure as the `skeleton`.
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
#' @examples
#' # generate nested list
#' x <- list(A=nameVector(LETTERS[3:1]),
#'    B=list(
#'       E=nameVector(LETTERS[10:7]),
#'       D=nameVector(LETTERS[5:4])),
#'    C=list(
#'       G=nameVector(LETTERS[19:16]),
#'       F=nameVector(LETTERS[15:11]),
#'       H=list(
#'          I=nameVector(LETTERS[22:20]))
#'       ))
#' x
#'
#' # unlisted vector of items
#' xu <- unlist(unname(x))
#' # unlisted vector of names
#' xun <- unname(jam_rapply(x, names));
#' names(xu) <- xun;
#'
#' # recursive list element lengths
#' xrn <- jam_rapply(x, length);
#' # define factor in order of list structure
#' xn <- factor(
#'    rep(names(xrn),
#'       xrn),
#'    levels=names(xrn));
#'
#' # re-create the original list
#' xu_new <- unlist(unname(split(xu, xn)))
#' xnew <- relist_named(xu_new, x);
#' xnew
#'
#' # re-order elements
#' k <- mixedOrder(xu_new);
#' xuk <- unlist(unname(split(xu[k], xn[k])))
#' xk <- relist_named(xuk, x);
#' xk
#'
#' # the default relist() function does not support this use case
#' xdefault <- relist(xuk, x);
#' xdefault
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
#' @returns `character` vector where letters are converted to uppercase.
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
#' @returns factor whose levels are based upon the order of
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


#' Pattern replacement with multiple patterns
#'
#' Pattern replacement with multiple patterns
#'
#' This function is a simple wrapper around `base::gsub()`
#' when considering a series of pattern-replacement
#' combinations. It applies each pattern match and replacement
#' in order and is therefore not vectorized.
#'
#' When `x` input is a `list` each vector in the `list` is processed,
#' somewhat differently than processing one vector.
#' 1. When the `list` contains another `list`, or when `length(x) < 100`,
#' each value in `x` is iterated calling `gsubs()`.
#' This process is the slowest option, however not noticeble until
#' `x` has length over 10,000.
#' 2. When the `list` does not contain another `list` and all values are
#' non-factor, or all values are `factor`, they are unlisted,
#' processed as a vector, then relisted. This process is nearly the
#' same speed as processing one single vector, except the time it
#' takes to confirm the list element classes.
#' 3. When values contain a mix of non-factor and `factor` values, they
#' are separately unlisted, processed by `gsubs()`, then relisted
#' and combined afterward. Again, this process is only slightly slower
#' than option 2 above, given that it calls `gsubs()` twice, with two
#' vectors.
#' 4. Note that `factor` values at input are
#' replaced with `character` values at output, consistent with `gsub()`.
#'
#' @returns `character` vector when input `x` is an atomic vector,
#'    or `list` when input `x` is a `list`.
#'
#' @family jam string functions
#'
#' @param pattern `character` vector of patterns
#' @param replacement `character` vector of replacements
#' @param x `character` vector with input data to be curated
#' @param ignore.case `logical` indicating whether to perform
#'    pattern matching in case-insensitive manner, where
#'    `ignore.case=TRUE` will ignore the uppercase/lowercase
#'    distinction.
#' @param replaceMultiple `logical` vector indicating whether to perform
#'    global substitution, where `replaceMultiple=FALSE` will
#'    only replace the first occurrence of the pattern, using
#'    `base::sub()`. Note that this vector can refer to individual
#'    entries in `pattern`.
#' @param ... additional arguments are passed to `base::gsub()`
#'    or `base::sub()`.
#'
#' @examples
#' gsubs(c("one", "two"), c("three", "four"), "one two five six")
#' gsubs(c("one", "two"), c("three"), "one two five six")
#'
#' @export
gsubs <- function
(pattern,
 replacement,
 x,
 ignore.case=TRUE,
 replaceMultiple=rep(TRUE, length(pattern)),
 ...)
{
   ## Purpose is to curate a text field using a series of gsub()
   ## commands, operating on a vector of from,to vectors.
   ## 'pattern' is expected to be a vector of regular expression patterns
   ## used by gsub()
   ##
   ## 'replacement' is expected to be a vector of replacement patterns, as
   ## used by gsub(), including relevant regular expression operators.
   ## If 'replacement' is empty, the "" is used, thereby replacing patterns with
   ## empty characters.
   ##
   ## replaceMultiple is a logical vector indicating whether each pattern
   ## replacement should use gsub() if replaceMultiple==TRUE, or sub()
   ## if replaceMultiple==FALSE. The default is TRUE, which uses gsub().
   ## One would use replaceMultiple=FALSE in order to replace only the
   ## first occurrence of a pattern, like replacing the first tab character
   ## only.
   ##
   ## This function allows the patterns and replacements to be defined
   ## upfront, then applied to any relevant character vectors consistently,
   ## for example across columns of a data.frame.
   if (length(x) == 0 || length(pattern) == 0) {
      return(x);
   }
   if (length(replaceMultiple) == 0) {
      replaceMultiple <- TRUE;
   }
   replaceMultiple <- rep(replaceMultiple, length.out=length(pattern));
   if (length(replacement) == 0) {
      replacement <- "";
   }
   replacement <- rep(replacement, length.out=length(pattern));

   # if input x is a list, iterate each element in the list.
   # Possible optimization for long lists that are not nested:
   # unlist into one character vector, split back into original form afterward.
   if (is.list(x) || "list" %in% class(x)) {
      # iterate each entry in x
      if (length(x) > 100) {
         # with more than 100 entries, iterative replacement gets slow
         x_class <- (sclass(x))
         if ("list" %in% x_class) {
            x_class <- cPaste(x_class)
         }
         if (!igrepHas("list", x_class)) {
            x_is_factor <- grepl("factor", x_class)
            x_lengths <- lengths(x)
            x_split <- factor(rep(seq_along(x), x_lengths),
               levels=seq_along(x))
            if (all(x_is_factor) || !any(x_is_factor)) {
               # if no factor, or all factor, we can unlist()
               x_unlist <- unlist(x, use.names=FALSE)
               x_new_vector <- gsubs(pattern=pattern,
                  replacement=replacement,
                  x=x_unlist,
                  ignore.case=ignore.case,
                  replaceMultiple=replaceMultiple,
                  ...)
               x_new <- split(x_new_vector, x_split)
               return(x_new)
            } else if (any(x_is_factor) && !all(x_is_factor)) {
               xnf <- x[!x_is_factor]
               xif <- x[x_is_factor]
               # non-factor
               xnf_split <- factor(
                  rep(seq_along(x)[!x_is_factor],
                     x_lengths[!x_is_factor]),
                  levels=seq_along(x))
               xnf_unlist <- unlist(x[!x_is_factor], use.names=FALSE)
               xnf_new_vector <- gsubs(pattern=pattern,
                  replacement=replacement,
                  x=xnf_unlist,
                  ignore.case=ignore.case,
                  replaceMultiple=replaceMultiple,
                  ...)
               # factor
               xif_split <- factor(
                  rep(seq_along(x)[x_is_factor],
                     x_lengths[x_is_factor]),
                  levels=seq_along(x))
               xif_unlist <- unlist(x[x_is_factor], use.names=FALSE)
               xif_new_vector <- gsubs(pattern=pattern,
                  replacement=replacement,
                  x=xif_unlist,
                  ignore.case=ignore.case,
                  replaceMultiple=replaceMultiple,
                  ...)
               # re-assemble
               x_new <- split(xnf_new_vector, x_split[!x_is_factor])
               xif_new <- split(xif_new_vector, x_split[x_is_factor])
               x_new[x_is_factor] <- xif_new[x_is_factor];
               return(x_new)
            }
         }
      }
      x_new <- lapply(x, function(ix){
         gsubs(pattern=pattern,
            replacement=replacement,
            x=ix,
            ignore.case=ignore.case,
            replaceMultiple=replaceMultiple,
            ...)
      })
      return(x_new);
   }

   for (i in seq_along(pattern)) {
      if (replaceMultiple[[i]]) {
         x <- gsub(pattern=pattern[i],
            replacement=replacement[i],
            x=x,
            ignore.case=ignore.case,
            ...);
      } else {
         x <- sub(pattern=pattern[i],
            replacement=replacement[i],
            x=x,
            ignore.case=ignore.case,
            ...);
      }
   }
   return(x);
}

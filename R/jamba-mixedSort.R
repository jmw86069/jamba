
#' sort alphanumeric values keeping numeric values in proper order
#'
#' sort alphanumeric values keeping numeric values in proper order
#'
#' This function is a refactor of `gtools` mixedsort(), a clever bit of
#' R coding from the `gtools` package. It was extended to make it slightly
#' faster, and to handle special cases slightly differently.
#' It was driven by the need to sort gene symbols, miRNA symbols, chromosome
#' names, all with proper numeric order, for example:
#'
#' \describe{
#'    \item{test set:}{miR-12,miR-1,miR-122,miR-1b,mir-1a}
#'    \item{gtools::mixedsort:}{miR-122,miR-12,miR-1,miR-1a,mir-1b}
#'    \item{mixedSort:}{miR-1,miR-1a,miR-1b,miR-12,miR-122}
#' }
#'
#' The function does not by default recognize negative numbers as negative,
#' instead it treats '-' as a delimiter, unless `keepNegative=TRUE`.
#'
#' This function also attempts to maintain '.' as part of a decimal number,
#' which can be problematic when sorting IP addresses, for example.
#'
#' This function is really just a wrapper function for `mixedOrder()`,
#' which does the work of defining the appropriate order.
#'
#' The sort logic is roughly as follows:
#'
#' * Split each term into alternating chunks containing `character`
#' or `numeric` substrings, split across columns in a matrix.
#' * Apply appropriate `ignore.case` logic to the character substrings,
#' effectively applying `toupper()` on substrings
#' * Define rank order of character substrings in each matrix column,
#' maintaining ties to be resolved in subsequent columns.
#' * Convert `character` to `numeric` ranks via `factor` intermediate,
#' defined higher than the highest `numeric` substring value.
#' * When `ignore.case=TRUE` and `useCaseTiebreak=TRUE`, an additional
#' tiebreaker column is defined using the `character` substring values
#' without applying `toupper()`.
#' * A final tiebreaker column is the input string itself, with `toupper()`
#' applied when `ignore.case=TRUE`.
#' * Apply order across all substring columns.
#'
#' Therefore, some expected behaviors:
#'
#' * When `ignore.case=TRUE` and `useCaseTiebreak=TRUE` (default for both)
#' the input data is ordered without regard to case, then the tiebreaker
#' applies case-specific sort criteria to the final product. This logic
#' is very close to default `sort()` except for the handling of internal
#' `numeric` values inside each string.
#'
#' @returns `vector` of values from argument `x`, ordered by
#'    `mixedOrder()`. The output class should match `class(x)`.
#'
#' @family jam sort functions
#'
#' @param x `vector`
#' @param blanksFirst `logical` whether to order blank entries before entries
#'    containing a value.
#' @param na.last `logical` indicating whether to move NA entries at
#'    the end of the sort.
#' @param keepNegative `logical` whether to keep '-' associated with adjacent
#'    numeric values, in order to sort them as negative values.
#' @param keepInfinite `logical` whether to allow "Inf" to be considered
#'    a numeric infinite value.
#' @param keepDecimal `logical` whether to keep the decimal in numbers,
#'    sorting as a true number and not as a version number. By default
#'    keepDecimal=FALSE, which means "v1.200" should be ordered before
#'    "v1.30". When keepDecimal=TRUE, the numeric sort considers only
#'    "1.2" and "1.3" and sorts in that order.
#' @param ignore.case `logical` whether to ignore uppercase and lowercase
#'    characters when defining the sort order. Note that when `x` is
#'    `factor` the factor levels are converted using
#'    `unique(toupper(levels(x)))`, therefore the values in `x` will be
#'    sorted by factor level.
#' @param useCaseTiebreak `logical` indicating whether to break ties
#'    when `ignore.case=TRUE`, using mixed case as a tiebreaker.
#' @param honorFactor `logical`, default TRUE, indicating whether to honor
#'    factor level order in the output, otherwise when FALSE it sorts
#'    as `character`.
#' @param sortByName `logical` whether to sort the vector x by names(x) instead
#'    of sorting by x itself.
#' @param verbose `logical` whether to print verbose output.
#' @param NAlast `logical` deprecated in favor of argument `na.last`
#'    for consistency with `base::sort()`.
#' @param ... additional parameters are sent to \code{\link{mixedOrder}}.
#'
#' @examples
#' x <- c("miR-12","miR-1","miR-122","miR-1b", "miR-1a", "miR-2");
#' sort(x);
#' mixedSort(x);
#'
#' # test honorFactor
#' mixedSort(factor(c("Cnot9", "Cnot8", "Cnot10")))
#' mixedSort(factor(c("Cnot9", "Cnot8", "Cnot10")), honorFactor=TRUE)
#'
#' # test ignore.case
#' mixedSort(factor(c("Cnot9", "Cnot8", "CNOT9", "Cnot10")))
#' mixedSort(factor(c("CNOT9", "Cnot8", "Cnot9", "Cnot10")))
#' mixedSort(factor(c("Cnot9", "Cnot8", "CNOT9", "Cnot10")), ignore.case=FALSE)
#' mixedSort(factor(c("Cnot9", "Cnot8", "CNOT9", "Cnot10")), ignore.case=TRUE)
#'
#' mixedSort(factor(c("Cnot9", "Cnot8", "CNOT9", "Cnot10")), useCaseTiebreak=TRUE)
#' mixedSort(factor(c("CNOT9", "Cnot8", "Cnot9", "Cnot10")), useCaseTiebreak=FALSE)
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
 useCaseTiebreak=TRUE,
 honorFactor=FALSE,
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
   ## sortByName=TRUE will sort the vector using the names, returning the
   ## original vector
   ##
   if (length(names(x)) == 0) {
      sortByName <- FALSE;
   }
   ignore.case <- c(ignore.case, TRUE);
   ignore.case <- head(ignore.case, 1);
   useCaseTiebreak <- head(c(useCaseTiebreak, TRUE), 1);
   sortByName <- head(c(sortByName, TRUE), 1);
   if (TRUE %in% sortByName) {
      if (length(names(x)) == 0) {
         warning("sortByName=TRUE but names(x) is empty. Returning x unchanged.");
         return(x);
      }
      fx <- function(x){names(x)}
   } else {
      fx <- function(x){x}
   }
   x[mixedOrder(fx(x),
      blanksFirst=blanksFirst,
      na.last=na.last,
      keepNegative=keepNegative,
      keepInfinite=keepInfinite,
      keepDecimal=keepDecimal,
      ignore.case=ignore.case,
      useCaseTiebreak=useCaseTiebreak,
      honorFactor=honorFactor,
      verbose=verbose,
      returnType="order",
      ...)];
}

#' order alphanumeric values keeping numeric values in proper order
#'
#' order alphanumeric values keeping numeric values in proper order
#'
#' This function is a refactor of `gtools` mixedorder() which was
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
#' Extra thanks to Gregory R. Warnes for the `gtools` mixedorder()
#' that proved to be so useful it ultimately inspired this function.
#'
#' @returns `integer` vector of orders derived from x,
#'    or when `returnType="rank"` an integer vector of ranks allowing ties.
#'    The rank is therefore valid for use in chains, such as multiple
#'    columns of a `data.frame`.
#'
#' @family jam sort functions
#'
#' @seealso `gtools::mixedorder()`, `gtools::mixedsort()`
#'
#' @param x input vector
#' @param blanksFirst `logical` whether to order blank entries before entries
#'    containing a value.
#' @param na.last `logical` whether to move NA entries to the end of the sort.
#'    When `na.last=TRUE` then `NA` values will always be last, even following
#'    blanks and infinite values. When `na.last=FALSE` then `NA` values
#'    will always be first, even before blanks and negative infinite values.
#' @param keepNegative `logical` whether to keep '-' associated with adjacent
#'    numeric values, in order to sort them as negative values. Note that
#'    `keepNegative=TRUE` also forces `keepDecimal=TRUE`, and enables
#'    matching of scientific notation such as `-1.23e-10` as a numeric
#'    value. When `keepNegative=FALSE` the dash `"-"` is treated as
#'    a common delimiter.
#' @param keepInfinite `logical` whether to allow "Inf" in the input `x`
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
#' @param keepDecimal `logical` whether to keep the decimal in numbers,
#'    sorting as a true number and not as a version number. By default
#'    `keepDecimal=FALSE``, which means "v1.200" will be ordered after
#'    "v1.30", since it considers `"1.200"` effectively as `1` and `200`,
#'    and `"1.30"` effectively as `1` and `30`.
#'    When `keepDecimal=TRUE`, the numeric sort orders `"v1.200"` before
#'    `"v1.30"`.
#' @param ignore.case `logical` whether to ignore uppercase and lowercase
#'    characters when defining the sort order.
#' @param useCaseTiebreak `logical` indicating whether to break ties
#'    when `ignore.case=TRUE`, using mixed case as a tiebreaker.
#' @param returnType `character` string to define the return type:
#'    * "order": returns `integer` order, equivalent to `order()`
#'    * "rank": returns `integer` rank, equivalent to `rank()`
#' @param returnDebug `logical` indicating whether to include
#'    additional debug info as attributes.
#' @param honorFactor `logical` indicating whether to honor the
#'    order of `levels` if the input `x` is a `factor`. The default
#'    `honorFactor=FALSE` is to maintain consistent legacy behavior.
#'    The purpose of this function is to enable alphanumeric sorting,
#'    which is not the purpose of sorting by factor levels.
#' @param NAlast `logical` DEPRECATED in favor of `na.last` for
#'    consistency with other base R functions.
#' @param verbose `logical` whether to print verbose output.
#' @param ... additional parameters are sent to `mixedOrder()`.
#' @param debug `logical` indicating whether to return intermediate data
#'    useful only for debugging purposes.
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
#' # test factor level order
#' factor1 <- factor(c("Cnot9", "Cnot8", "Cnot10"))
#' sort(factor1)
#' mixedSort(factor1)
#' factor1[mixedOrder(factor1)]
#' factor1[mixedOrder(factor1, honorFactor=TRUE)]
#'
#' @export
mixedOrder <- function
(x,
 ...,
 blanksFirst=TRUE,
 na.last=NAlast,
 keepNegative=FALSE,
 keepInfinite=FALSE,
 keepDecimal=FALSE,
 ignore.case=TRUE,
 useCaseTiebreak=TRUE,
 honorFactor=FALSE,
 returnDebug=FALSE,
 returnType=c("order", "rank"),
 NAlast=TRUE,
 verbose=FALSE,
 debug=FALSE)
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
   if (is.numeric(x) ||
         (is.factor(x) && TRUE %in% honorFactor)) {
      if (TRUE %in% ignore.case && is.factor(x)) {
         x <- factor(toupper(x),
            levels=unique(toupper(levels(x))))
      }
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

   if (TRUE %in% keepNegative) {
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
      if (TRUE %in% keepDecimal) {
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
   if (verbose > 1) {
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

   # Optionally return intermediate debug data
   if (TRUE %in% debug) {
      return(list(delimited=delimited, delim=delim))
   }

   ## Split delimited strings into columns, one row per entry
   step1m <- rbindList(
      rmNULL(strsplit(delimited, delim),
         nullValue=""));

   ## Split the numeric values in their own matrix
   step1mNumeric <- matrix(ncol=ncol(step1m),
      data=suppressWarnings(as.numeric(step1m)));

   ## Optionally convert "Inf" from infinite back to character value
   if (!TRUE %in% keepInfinite && any(is.infinite(step1mNumeric))) {
      if (verbose > 1) {
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
      if (verbose > 1) {
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
      if (TRUE %in% blanksFirst) {
         step1mNumeric[which_blanks,1] <- -Inf;
      } else {
         step1mNumeric[which_blanks, ncol(step1mNumeric)] <- Inf;
      }
   }
   if (any(which_nas)) {
      if (TRUE %in% na.last) {
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
   # Current issue: each column performs its own tiebreak with useCaseTiebreak
   # instead of performing tiebreak after all columns.
   rankCharacterTiebreak <- NULL;
   if (TRUE %in% ignore.case) {
      # first perform rank with logic for ignore.case=TRUE
      rankCharacter <- apply(step1mCharacter, 2, function(i){
         if (verbose) {
            printDebug("mixedOrder(): ",
               "ignore.case col sort.");
         }
         iRank <- as.numeric(factor(toupper(i)));
         if (verbose > 1) {
            printDebug(head(iRank, 20));
         }
         iRank;
      });
      # then optionally perform rank with logic for ignore.case=FALSE
      if (TRUE %in% useCaseTiebreak) {
         rankCharacterTiebreak <- apply(step1mCharacter, 2, function(i){
            if (verbose) {
               printDebug("mixedOrder(): ",
                  "useCaseTiebreak col sort.");
            }
            # same rank without toupper()
            iRank <- as.numeric(factor(i));
            if (verbose > 1) {
               printDebug(head(iRank, 20));
            }
            return(iRank);
         });
      } else {
         rankCharacter <- apply(step1mCharacter, 2, function(i){
            if (verbose) {
               printDebug("mixedOrder(): ",
                  "ignore.case col sort.");
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
            printDebug("mixedOrder(): ",
               "as-is col sort.");
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
      if (length(rankCharacterTiebreak) > 0) {
         printDebug("rankCharacterTiebreak:");
         print(head(rankCharacterTiebreak, 20));
      }
      printDebug("step1mNumeric:");
      print(head(step1mNumeric, 20));
      printDebug("rankNumeric:");
      print(head(rankNumeric, 20));
      # printDebug("ncol(rankNumeric):", ncol(rankNumeric));
   }

   ## Make character ranks higher than any existing numerical ranks
   ## Fill with the adjusted character string ranks
   ## some cells are NA here since they had a numeric value
   rankOverall <- rankCharacter + 1 + max(rankNumeric, na.rm=TRUE);
   ## Fill NA cells with the numeric rank
   if (any(is.na(rankOverall))) {
      rankOverall[is.na(rankOverall)] <- rankNumeric[is.na(rankOverall)];
   }
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
   if (any(which_blanks)) {
      if (!TRUE %in% blanksFirst) {
         rankOverall[which_blanks, 1] <- Inf;
      } else if (TRUE %in% blanksFirst) {
         rankOverall[which_blanks, 1] <- -Inf;
      }
   }
   if (any(which_nas)) {
      if (TRUE %in% na.last) {
         rankOverall[which_nas, 1] <- Inf;
      } else if (!TRUE %in% na.last) {
         rankOverall[which_nas, 1] <- -Inf;
      }
   }

   # useCaseTiebreak
   if (length(rankCharacterTiebreak) > 0) {
      rankOverall <- cbind(rankOverall,
         rankCharacterTiebreak);
   }

   ## Rank initial string as a tiebreaker
   if (TRUE %in% ignore.case) {
      rankX <- rank(toupper(x),
         na.last=na.last);
   } else {
      rankX <- rank(x,
         na.last=na.last);
   }
   rankOverall <- cbind(rankOverall, rankX);

   if (TRUE %in% verbose) {
      printDebug("rankOverall:");
      rownames(rankOverall) <- makeNames(x, suffix=".");
      print(head(rankOverall, 20));
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
   if (TRUE %in% returnDebug) {
      attr(retVal, "mixedSortNcol") <- ncol(rankNumeric);
      attr(retVal, "rankOverall") <- rankOverall;
   }
   return(retVal);
}

#' order alphanumeric values from a list
#'
#' order alphanumeric values from a list
#'
#' This function is a minor extension to `mixedOrder()`,
#' "multiple `mixedOrder()`",
#' which accepts `list` input, similar to how `base::order()` operates.
#' This function is mainly useful when sorting something like a
#' `data.frame`, where ties in column 1 should be maintained then
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
#' @returns `integer` vector of row orders
#'
#' @param ... arguments treated as a `list` of vectors to be ordered in
#'    proper order, based upon the mechanism by `base::order()`, and
#'    as such `data.frame` is equivalent to a `list`.
#' @param blanksFirst,na.last,keepNegative,keepInfinite,keepDecimal,ignore.case,useCaseTiebreak,sortByName
#'    arguments passed to `mixedOrder()`, except `sortByName` which is not
#'    passed along.
#' @param verbose `logical` indicating whether to print verbose output,
#'    passed as `verbose - 1` to `mixedOrder()`.
#' @param matrixAsDF `logical` if `...` supplies only one matrix object,
#'    then `matrixAsDF=TRUE` will cause it to be converted to a `data.frame`,
#'    then coerce to a `list` before processing.
#'    By default, in the event only one matrix object is supplied,
#'    this conversion is performed, in order to define a sort order based upon
#'    each column in order, consistent with behavior of `data.frame` input.
#' @param decreasing `logical`, default FALSE, used to reverse the sort order.
#' @param NAlast `logical` deprecated in favor of argument `na.last`
#'    for consistency with `base::sort()`.
#' @param honorFactor `logical`, default TRUE, used to enforce factor level
#'    sort order, when FALSE it sorts as `character`.
#'
#' @family jam sort functions
#'
#' @examples
#' # test factor level order
#' factor1 <- factor(c("Cnot9", "Cnot8", "Cnot10"))
#' sort(factor1)
#' mixedSort(factor1)
#' factor1[mixedOrder(factor1)]
#' factor1[mixedOrder(factor1, honorFactor=FALSE)]
#' factor1[mixedOrder(factor1, honorFactor=TRUE)]
#'
#' factor1[mmixedOrder(list(factor1))]
#' factor1[mmixedOrder(list(factor1), honorFactor=FALSE)]
#' factor1[mmixedOrder(list(factor1), honorFactor=TRUE)]
#'
#' @export
mmixedOrder <- function
(...,
 decreasing=FALSE,
 blanksFirst=TRUE,
 na.last=NAlast,
 keepNegative=FALSE,
 keepInfinite=FALSE,
 keepDecimal=FALSE,
 ignore.case=TRUE,
 useCaseTiebreak=TRUE,
 sortByName=FALSE,
 NAlast=TRUE,
 honorFactor=TRUE,
 verbose=FALSE,
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
      if (is.numeric(i) ||
         any(c("numeric", "POSIXct", "POSIXt", "Date") %in% class(i)) ||
         (any(c("factor", "ordered") %in% class(i)) &&
            TRUE %in% honorFactor)) {
         as.numeric(i) * iSign;
      } else {
         x2u <- unique(i);
         x2uo <- mixedOrder(x2u,
            na.last=na.last,
            blanksFirst=blanksFirst,
            keepNegative=keepNegative,
            keepInfinite=keepInfinite,
            keepDecimal=keepDecimal,
            verbose=verbose,
            ignore.case=ignore.case,
            honorFactor=honorFactor,
            useCaseTiebreak=useCaseTiebreak,
            ...);
         x2uof <- factor(i,
            levels=x2u[x2uo]);
         x2o <- as.numeric(x2uof) * iSign;
      }
   });
   do.call("order", c(z1,
      na.last=na.last,
      decreasing=FALSE));
}

#' sort data.frame keeping numeric values in proper order
#'
#' sort data.frame keeping numeric values in proper order
#'
#' This function is a wrapper around `mmixedOrder()` so it operates
#' on `data.frame` columns in the proper order, using logic similar that used
#' by `base::order()` when operating on a `data.frame`. The sort order logic
#' is fully described in `mixedSort()` and `mixedOrder()`.
#'
#' Note that `byCols` can either be given as `integer` column index values,
#' or `character` vector of `colnames(x)`. In either case, using negative
#' prefix `-` will reverse the sort order of the corresponding column.
#'
#' For example `byCols=c(2, -1)` will sort column 2 increasing, then
#' column 1 decreasing.
#'
#' Similarly, one can supply `colnames(df)`, such as
#' `byCols=c("colname2", "-colname1")`. Values are matched as-is to
#' `colnames(df)` first, then any values not matched are compared again
#' after removing prefix `-` from the start of each `character` string.
#' Therefore, if `colnames(df)` contains `"-colname1"` it will be matched
#' as-is, but `"--colname1"` will only be matched after removing the first `-`,
#' after which the sort order will be reversed for that column.
#'
#' For direct control over the sort order of each column defined in `byCols`,
#' you can supply `logical` vector to argument `decreasing`, and this vector
#' is recycled to `length(byCols)`.
#'
#' Finally, for slight efficiency, only unique columns defined in `byCols`
#' are used to determine the row order, so even if a column is defined twice
#' in `byCols`, only the first instance is passed to `mmixedOrder()` to
#' determine row order.
#'
#' @returns `data.frame` whose rows are ordered using `mmixedOrder()`.
#'
#' @family jam sort functions
#'
#' @param df `data.frame` input
#' @param byCols one of two types of input:
#'    1. `integer` vector referring to the order of columns to be
#'    used by `mmixedOrder()` to order the `data.frame`. Note that
#'    negative values will reverse the sort order for the corresponding
#'    column number. To sort `rownames(df)` use zero `0`, and to reverse
#'    sorting `rownames(x)` use `-0.1` where the negative sign will
#'    reverse the sort, and `-0.1` will be rounded to `0`.
#'    2. `character` vector of values in `colnames(df)`,
#'    optionally including prefix `"-"` to reverse the sort.
#'    Note that the argument `decreasing` can also be used to specify
#'    columns to have reverse sort, either as a single value or vector
#'    to be applied to each column in `byCols`. To sort `rownames(df)`
#'    use `"rownames"` or `"row.names"`. To reverse sorting `rownames(df)`
#'    use `"-rownames"` or `"-row.names"`.
#' @param na.last `logical` whether to move NA entries to the end of the sort.
#'    When `na.last=TRUE` then `NA` values will always be last, even following
#'    blanks and infinite values. When `na.last=FALSE` then `NA` values
#'    will always be first, even before blanks and negative infinite values.
#' @param decreasing NULL or `logical` vector indicating which columns
#'    in `byCols` should be sorted in decreasing order. By default, the
#'    `sign(byCols)` is used to define the sort order of each column, but it
#'    can be explicitly overridden with this `decreasing` parameter.
#' @param useRownames `logical` whether to use `rownames(df)` as a last
#'    tiebreaker in the overall rank ordering. This parameter has the primary
#'    effect of assuring a reproducible result, provided the rownames are
#'    consistently defined, or if rownames are actually row numbers.
#'    When `useRownames=FALSE` then rows that would otherwise be ties
#'    will be returned in the same order they were provided in `df`.
#' @param verbose `logical` whether to print verbose output. When
#'    `verbose=2` there is slightly more verbose output.
#' @param blanksFirst,keepNegative,keepInfinite,keepDecimal,ignore.case,useCaseTiebreak,sortByName
#'    arguments passed to `mmixedOrder()`, except `sortByName` which is not
#'    passed along.
#' @param honorFactor `logical`, default TRUE, indicating whether to honor
#'    factor level order in the output, otherwise when FALSE it sorts
#'    as `character`.
#' @param ... additional arguments passed to `mmixedOrder()` for custom
#'    sort options as described in `mixedSort()`.
#'
#' @examples
#' # start with a vector of miRNA names
#' x <- c("miR-12","miR-1","miR-122","miR-1b", "miR-1a","miR-2");
#' # add some arbitrary group information
#' g <- rep(c("Air", "Treatment", "Control"), 2);
#' # create a data.frame
#' df <- data.frame(group=g,
#'    miRNA=x,
#'    stringsAsFactors=FALSE);
#'
#' # input data
#' df;
#'
#' # output when using order()
#' df[do.call(order, df), , drop=FALSE];
#'
#' # output with mixedSortDF()
#' mixedSortDF(df);
#'
#' # mixedSort respects factor order
#' # reorder factor levels to demonstrate.
#' # "Control" should come first
#' gf <- factor(g, levels=c("Control", "Air", "Treatment"));
#' df2 <- data.frame(groupfactor=gf,
#'    miRNA=x,
#'    stringsAsFactors=FALSE);
#'
#' # now the sort properly keeps the group factor levels in order,
#' # which also sorting the miRNA names in their proper order.
#' mixedSortDF(df2);
#'
#'
#' x <- data.frame(l1=letters[1:10],
#'    l2=rep(letters[1:2+10], 5),
#'    L1=LETTERS[1:10],
#'    L2=rep(LETTERS[1:2+20], each=5));
#' set.seed(123);
#' rownames(x) <- sample(seq_len(10));
#' x;
#'
#' # sort by including rownames
#' mixedSortDF(x, byCols=c("rownames"));
#' mixedSortDF(x, byCols=c("L2", "-rownames"));
#'
#' # demonstrate sorting a matrix with no rownames
#' m <- matrix(c(2, 1, 3, 4), ncol=2);
#' mixedSortDF(m, byCols=-2)
#'
#' # add rownames
#' rownames(m) <- c("c", "a");
#' mixedSortDF(m, byCols=0)
#' mixedSortDF(m, byCols="-rownames")
#' mixedSortDF(m, byCols="rownames")
#'
#' mixedSortDF(data.frame(factor1=factor(c("Cnot9", "Cnot8", "Cnot10"))), honorFactor=FALSE)
#'
#' # test date columns
#' testfiles <- system.file(package="jamba", c("TODO.md", "README.md", "NEWS.md"))
#' testinfo <- file.info(testfiles)
#' testinfo
#' mixedSortDF(testinfo, byCols="mtime")
#'
#' @export
mixedSortDF <- function
(df,
 byCols=seq_len(ncol(df)),
 na.last=TRUE,
 decreasing=NULL,
 useRownames=FALSE,
 verbose=FALSE,
 blanksFirst=TRUE,
 keepNegative=FALSE,
 keepInfinite=FALSE,
 keepDecimal=FALSE,
 ignore.case=TRUE,
 useCaseTiebreak=TRUE,
 sortByName=FALSE,
 honorFactor=TRUE,
 ...)
{
   ## Purpose is to order a data.frame using mmixedOrder()
   ## byCols is a vector of column numbers, with negative values
   ## referring to columns for decreasing order

   # if input has no rows, return without processing
   if (length(df) == 0 || (length(dim(df) > 1) && dim(df)[1] == 0)) {
      return(df)
   }

   ## If given a matrix, convert to data.frame so it is treated as a list
   ## in mmixedOrder()
   ##
   ## useRownames=TRUE will use rownames as a tiebreaker, the last in the
   ## sort precedence, but only if there are rownames(df).
   if (useRownames && length(rownames(df)) == 0) {
      useRownames <- FALSE;
   }

   if (length(decreasing) == 0) {
      decreasing <- FALSE;
   }
   if (length(decreasing) < length(byCols)) {
      decreasing <- rep(decreasing,
         length.out=length(byCols));
   }

   ## Handle character input for byCols
   if (igrepHas("character", class(byCols))) {
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
            decreasing[byMatchNAneg] <- !decreasing[byMatchNAneg];
            #byMatch[byMatchNAneg] <- byMatch[byMatchNAneg] * -1;
         }
      }

      ## Finally check for row names:
      # "rownames", "row.names",
      # "-rownames", "-row.names",
      #  "0", or "-0"
      byMatchNA <- is.na(byMatch);
      if (length(rownames(df)) > 0 && any(byMatchNA)) {
         byMatchNA_rowname <- (
            grepl("^[-]{0,1}(rownames|row.names|0)$", byCols) &
               byMatchNA);
         if (verbose > 1) {
            printDebug("mixedSortDF(): ",
               "byMatchNA_rowname: ", byMatchNA_rowname);
         }
         if (any(byMatchNA_rowname)) {
            useRownames <- FALSE;
            byMatch[byMatchNA_rowname] <- 0;
            byMatchNA_rowname_decreasing <- (
               grepl("^[-]((rowname|row.name)[s]{0,1}|0)$", byCols) &
                  byMatchNA);
            if (any(byMatchNA_rowname_decreasing)) {
               decreasing[byMatchNA_rowname_decreasing] <- !decreasing[byMatchNA_rowname_decreasing];
            }
            if (verbose > 1) {
               printDebug("mixedSortDF(): ",
                  "byMatch: ", byMatch);
               printDebug("mixedSortDF(): ",
                  "decreasing: ", decreasing);
            }
         }
      }

      ## Keep only the columns with a match
      byMatchNA <- is.na(byMatch);
      byCols <- byMatch[!byMatchNA];
      decreasing <- decreasing[!byMatchNA];
      if (verbose > 1) {
         printDebug("mixedSortDF(): ",
            "Converted byCols to integer:",
            byCols);
         printDebug("mixedSortDF(): ",
            "decreasing:",
            decreasing);
      }
   } else {
      if (any(byCols < 0)) {
         decreasing[byCols < 0] <- !decreasing[byCols < 0];
         byCols <- abs(round(byCols));
      }
      # ensure byCols are no greater than ncol(df)
      decreasing <- decreasing[byCols <= ncol(df)];
      byCols <- byCols[byCols <= ncol(df)];
   }

   ## Append rowname sort when useRownames==TRUE
   if (any(useRownames)) {
      byCols <- c(byCols, 0);
      decreasing <- c(decreasing, FALSE);
   }

   ## if no rownames exist, remove entries where byCols == 0
   if (any(byCols == 0) && length(rownames(df)) == 0) {
      decreasing <- decreasing[!byCols == 0];
      byCols <- byCols[!byCols == 0];
   }

   ## Remove duplicate and NA byCols
   byCols_keep <- !duplicated(byCols) & !is.na(byCols);
   if (any(!byCols_keep)) {
      decreasing <- decreasing[byCols_keep];
      byCols <- byCols[byCols_keep];
   }

   ## Determine byCols to keep:
   dfColnums <- seq_len(ncol(df));
   if (any(byCols == 0) && length(rownames(df)) > 0) {
      dfColnums <- seq_len(ncol(df) + 1);
      byCols[byCols == 0] <- ncol(df) + 1;
   }

   # Return df if there are no byCols
   if (length(byCols) == 0) {
      if (verbose) {
         printDebug("mixedSortDF(): ",
            "No columns available to sort.");
      }
      return(df);
   }

   ## each column only once
   ## is not NA
   ## is no greater than ncol(df)
   ## is not a list column
   byCols_sortable <- sapply(byCols, function(i){
      (i == (ncol(df) + 1)) ||
      !igrepHas("list", class(df[[i]]))
   })

   # legacy code
   if (FALSE) {
      byColsKeep <- (
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
   }
   if (verbose) {
      printDebug("mixedSortDF(): ",
         "    byCols: ",
         head(format(
            justify="right",
            c(byCols,
               as.character(decreasing))),
               length(byCols)),
         fgText=list("darkorange1", "dodgerblue",
            rep("white", length(byCols))),
         bgText=list(NA, NA,
            ifelse(!decreasing,
               "slateblue",
               "firebrick3")));
      printDebug("mixedSortDF(): ",
         "decreasing: ",
         tail(format(
            justify="right",
            c(byCols,
               as.character(decreasing))),
            length(byCols)),
         fgText=list("darkorange1", "dodgerblue",
            rep("white", length(byCols))),
         bgText=list(NA, NA,
            ifelse(!decreasing,
               "slateblue",
               "firebrick3")));
   }

   ## mmixedOrder() to determine row order
   dfOrder <- mmixedOrder(
      rmNULL(c(
         data.frame(
            check.names=FALSE,
            stringsAsFactors=FALSE,
            df),
         data.frame(
            check.names=FALSE,
            stringsAsFactors=FALSE,
            rowNamesX=rownames(df)))
      )[byCols],
      decreasing=decreasing,
      na.last=na.last,
      blanksFirst=blanksFirst,
      keepNegative=keepNegative,
      keepInfinite=keepInfinite,
      keepDecimal=keepDecimal,
      ignore.case=ignore.case,
      useCaseTiebreak=useCaseTiebreak,
      honorFactor=honorFactor,
      ...);

   if (igrepHas("matrix|data.frame", class(df))) {
      df[dfOrder, , drop=FALSE];
   } else {
      df[dfOrder, ];
   }
}

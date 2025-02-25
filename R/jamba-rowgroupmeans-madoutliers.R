
#' Calculate row group means, or other statistics
#'
#' Calculate row group means, or other statistics, where: `rowGroupMeans()`
#' calculates row summary stats; and `rowGroupRmOutliers()` is a convenience
#' function to call `rowGroupMeans(..., rmOutliers=TRUE, returnType="input")`.
#'
#' This function by default calculates group mean values
#' per row in a numeric matrix. However, the stat function
#' can be changed to calculate row medians, row MADs, etc.
#'
#' An added purpose of this function is optional outlier
#' filtering, via calculation of MAD values and applying
#' a MAD threshold cutoff. The intention is to identify
#' technical outliers that otherwise adversely affect the
#' calculated group mean or median values. To inspect the
#' data after outlier removal, use the parameter `returnType="input"`
#' which will return the input data matrix with `NA`
#' substituted for outlier points. Outlier detection and
#' removal is performed by `jamba::rowRmMadOutliers()`.
#'
#' @family jam numeric functions
#'
#' @returns `numeric` matrix based upon `returnType`:
#'    * When `returnType="output"` the output is a numeric matrix
#'    with the same number of columns as the number of unique
#'    `groups` labels. When `groups` is a factor and
#'    `keepNULLlevels=TRUE`, the number of columns will be the
#'    number of factor levels, otherwise it will be the number of
#'    factor levels used in `groups`.
#'    * When `returnType="input"` the output is a numeric matrix
#'    with the same dimensions as the input data. This output is
#'    intended for use with `rmOutliers=TRUE` which will replace
#'    outlier points with `NA` values. Therefore, this matrix can
#'    be used to see the location of outliers.
#'
#' The function also returns attributes when `includeAttributes=TRUE`,
#' although the default is FALSE. The attributes describe the
#' number of samples per group overall:
#' \describe{
#'    \item{attr(out, "n")}{The attribute `"n"` is used to describe
#'       the number of replicates per group.}
#'    \item{attr(out, "nLabel")}{The attribute `"nLabel"` is
#'       a simple text label in the form `"n=3"`.}
#' }
#'
#' Note that when `rmOutliers=TRUE` the number of replicates per
#' group will vary depending upon the outliers removed. In that
#' case, remember that the reported `"n"` is always the total
#' possible columns available prior to outlier removal.
#'
#' @param x `numeric` data matrix
#' @param groups `character` or `factor` vector of group labels,
#'    either as a character vector, or a factor. See the parameter
#'    `groupOrder` for ordering of group labels in the output
#'    data matrix.
#' @param na.rm `logical`, default TRUE, passed to the stats func
#'    to ignore NA values.
#' @param useMedian `logical`, default TRUE,  indicating whether the default
#'    stat should be "mean" or "median".
#' @param rmOutliers `logical`, default FALSE, indicating whether to
#'    apply outlier detection and removal.
#' @param crossGroupMad `logical` indicating whether to calculate
#'    row MAD values using the median across groups for each row.
#'    The median is calculated using non-NA and non-zero row group
#'    MAD values. When `crossGroupMad=TRUE` it also calculates
#'    the non-NA, non-zero median row MAD across all rows,
#'    which defines the minimum difference from median applied across
#'    all values to be considered an outlier.
#' @param madFactor `numeric` value indicating the multiple of the
#'    MAD value to define outliers. For example `madFactor=5`
#'    will take the MAD value for a group multiplied by 5,
#'    5*MAD, as a threshold for outliers. So any points more than
#'    5*MAD distance from the median per group are outliers.
#' @param returnType `character`, default "output", the return data type:
#'    * `"output"` returns one summary stat value per group, per row;
#'    * `"input"` is useful when `rmOutliers=TRUE` in that it returns
#'    a matrix with the same dimensions as the input, except with
#'    outlier points replaced with NA.
#' @param rowStatsFunc `function`, default NULL, which takes a numeric matrix
#'    as input, and returns a numeric vector equal to the number of
#'    rows of the input data matrix. When supplied, `useMedian` is ignored.
#'    Examples: `base::rowMeans()`,
#'    `matrixStats::rowMedians()`, `matrixStats::rowMads`.
#' @param groupOrder `character` string indicating how character group
#'    labels are ordered in the final data matrix, when `returnType="output"`.
#'    Note that when `groups` is a factor, the factor levels are kept
#'    in that order. Otherwise, `"same"` keeps groups in the same
#'    order they appear in the input matrix; `"sort"` applies
#'    `jamba::mixedSort()` to the labels.
#' @param keepNULLlevels `logical`, default FALSE,  whether to keep factor
#'    levels even when there are no corresponding columns in `x`.
#'    When `TRUE` and `returnType="output"` the output matrix will
#'    contain one colname for each factor level, with NA values used
#'    to fill empty factor levels. This mechanism can be helpful to
#'    ensure that output matrices have consistent colnames.
#' @param includeAttributes `logical`, default FALSE, whether to include
#'    attributes with `"n"` number of replicates per group, and `"nLabel"`
#'    with replicate label in `n=#` form.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional parameters are passed to `rowStatsFunc`,
#'    and if `rmOutliers=TRUE` to `jamba::rowRmMadOutliers()`.
#'
#' @examples
#' x <- matrix(ncol=9, stats::rnorm(90));
#' colnames(x) <- LETTERS[1:9];
#' use_groups <- rep(letters[1:3], each=3)
#' rowGroupMeans(x, groups=use_groups)
#'
#' # rowGroupRmOutliers returns the input data after outlier removal
#' rowGroupRmOutliers(x, groups=use_groups, returnType="input")
#'
#' # rowGroupMeans(..., returnType="input") also returns the input data
#' rowGroupMeans(x, groups=use_groups, rmOutliers=TRUE, returnType="input")
#'
#' # rowGroupMeans with outlier removal
#' rowGroupMeans(x, groups=use_groups, rmOutliers=TRUE)
#'
#' @export
rowGroupMeans <- function
(x,
 groups,
 na.rm=TRUE,
 useMedian=TRUE,
 rmOutliers=FALSE,
 crossGroupMad=TRUE,
 madFactor=5,
 returnType=c("output",
    "input"),
 rowStatsFunc=NULL,
 groupOrder=c("same",
    "sort"),
 keepNULLlevels=FALSE,
 includeAttributes=FALSE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to provide rowMeans() but using groups of columns
   ##
   ## If rmOutliers==TRUE and madFactor is not NULL, then replicates within each group will
   ## be tested for outliers using rowRmMadOutliers().  madFactor represents the fold threshold
   ## from the stats::mad() of each group, for each row, and a default of 5 is fairly lenient in that it
   ## only filters outliers when one point is 5 times the MAD away from median.
   ## rmOutliers=FALSE, madFactor=5,
   ##
   ## if rowStatsFunc is not NULL, it is expected to be a function which takes a matrix of data
   ## and produces row-based numeric summary statistics. If na.rm=TRUE is required, it must be
   ## encoded into the function definition, for example
   ##    rowStatsFunc=function(x){rowMeans(x, na.rm=TRUE)}
   ##
   ## groupOrder="same" will order groups in the order they're defined in groups,
   ## groupOrder="sort" will use mixedSort
   ##
   ## keepNULLlevels=TRUE, if groups is a factor, then all factor levels
   ## will be maintained even if no values exist with that factor level.
   ## keepNULLlevels=FALSE, if groups is a factor, then only factor levels
   ## containing values will be returned.
   groupOrder <- match.arg(groupOrder);

   returnType <- match.arg(returnType);

   if (verbose && rmOutliers && length(madFactor) > 0) {
      printDebug("rowGroupMeans(): ",
         "running with rmOutliers=",
         "TRUE",
         " and madFactor=",
         madFactor);
   }
   if (verbose && length(rowStatsFunc) > 0) {
      printDebug("rowGroupMeans(): ",
         "running custom rowStatsFunc");
   }
   if (!igrepHas("factor|order", class(groups))) {
      if (groupOrder %in% "same") {
         if (verbose) {
            printDebug("rowGroupMeans(): ",
               "Using groups in observed order:",
               unique(groups));
         }
         groups <- factor(groups,
            levels=unique(groups));
      } else {
         if (verbose) {
            printDebug("rowGroupMeans(): ",
               "Using groups in mixedSort order:",
               mixedSort(unique(groups)));
         }
         groups <- factor(groups,
            levels=mixedSort(unique(groups)));
      }
   }

   ## Define row functions
   # 0.0.104.900 - support sparse Matrix objects
   # inherits(x, "sparseMatrix")
   is_sparse <- FALSE;
   use_fn_type <- "base";
   # fn_rowMedians <- matrixStats::rowMedians
   if (length(rowStatsFunc) > 0) {
      use_fn_type <- "custom";
   } else if (inherits(x, "sparseMatrix")) {
      is_sparse <- TRUE;
      if (requireNamespace("sparseMatrixStats", quietly=TRUE)) {
         use_fn_type <- "sparseMatrixStats";
         fn_rowMedians <- sparseMatrixStats::rowMedians
         fn_rowMeans <- sparseMatrixStats::rowMeans2
         rowStatsFunc <- fn_rowMeans;
         if (TRUE %in% useMedian) {
            rowStatsFunc <- fn_rowMedians;
         }
      }
   } else if (requireNamespace("matrixStats", quietly=TRUE)) {
      use_fn_type <- "matrixStats";
      fn_rowMedians <- matrixStats::rowMedians
      # fn_rowMeans <- function(x, na.rm=TRUE){base::rowMeans(x, na.rm=na.rm)}
      fn_rowMeans <- base::rowMeans
      rowStatsFunc <- fn_rowMeans;
      if (TRUE %in% useMedian) {
         rowStatsFunc <- fn_rowMedians;
      }
   }
   if ("base" %in% use_fn_type) {
      fn_rowMedians <- function(x, ...){
         apply(x, 1, function(i){
            stats::median(i,
               ...)
         })
      }
      fn_rowMeans <- function(x, ...){
         apply(x, 1, function(i){
            mean(i,
               ...)
         })
      }
      rowStatsFunc <- fn_rowMeans;
      if (TRUE %in% useMedian) {
         rowStatsFunc <- fn_rowMedians;
      }
   }
   if (verbose) {
      printDebug("rowGroupMeans(): ",
         "Using stat functions from:",
         use_fn_type);
   }
   # optionally calculate cross-group MAD values
   rowMadValues <- NULL;
   minDiff <- NULL;
   if (rmOutliers && length(madFactor) > 0 && crossGroupMad) {
      if (verbose) {
         printDebug("rowGroupMeans(): ",
            "Applying rmOutliers with crossGroupMad")
      }
      if ("sparseMatrixStats" %in% use_fn_type) {
         mad_rowStatsFunc <- function(x, ...){
            sparseMatrixStats::rowMads(x,
               na.rm=TRUE)}
      } else if ("matrixStats" %in% use_fn_type) {
         mad_rowStatsFunc <- function(x, ...){
            matrixStats::rowMads(x,
               na.rm=TRUE)}
      } else {
         mad_rowStatsFunc <- function(x, ...){
            apply(x, 1, function(i){
               stats::mad(i,
                  na.rm=TRUE)})
         }
      }
      x_mads <- rowGroupMeans(x=x,
         groups=groups,
         returnType="output",
         rowStatsFunc=mad_rowStatsFunc,
         verbose=verbose);
      x_mads[x_mads == 0] <- NA;
      # take median of each row group MAD value
      rowMadValues <- fn_rowMedians(x_mads,
         na.rm=TRUE);
      minDiff <- stats::median(rowMadValues[rowMadValues > 0],
         na.rm=TRUE);
   }

   # iterate each group and apply rowStatsFunc
   x2L <- tapply(X=colnames(x),
      INDEX=groups,
      simplify=FALSE,
      FUN=function(i){
         iM <- x[, i, drop=FALSE];
         if (rmOutliers && length(madFactor) > 0) {
            ## Optionally filter for outliers before aggregation
            iM <- rowRmMadOutliers(iM,
               madFactor=madFactor,
               rowMadValues=rowMadValues,
               minDiff=minDiff,
               verbose=verbose,
               ...);
         }
         if ("input" %in% returnType) {
            return(iM);
         }
         ## All logic now applies rowStatsFunc
         if (length(rowStatsFunc) > 0) {
            ## If given a custom function, use it and not anything else
            iV <- rowStatsFunc(iM,
               na.rm=na.rm);
         } else {
            # unnecessary
            stop("rowStatsFunc was not properly defined.")
         }
         if (length(rownames(iM)) > 0) {
            if (is.vector(iV)) {
               names(iV) <- rownames(iM);
            } else if (is.matrix(iV)) {
               rownames(iV) <- rownames(iM);
            }
         }
         if (verbose > 1) {
            printDebug("rowGroupMeans(): ",
               "columns:",
               i,
               ", head(iV, 10):");
            print(head(iV, 10));
         }
         iV;
      });
   if (keepNULLlevels && any(lengths(x2L) == 0)) {
      if (verbose) {
         printDebug("rowGroupMeans(): ",
            "keeping NULL factor levels, converting to NA.");
      }
      # jamba::rmNULL() is used to force jamba function
      x2 <- do.call(cbind,
         jamba::rmNULL(x2L,
            nullValue=NA));
   } else {
      x2 <- do.call(cbind, x2L);
   }

   ## Return the replicate count as an attribute
   if ("output" %in% returnType && TRUE %in% includeAttributes) {
      try({
         nReps <- nameVector(rmNA(naValue=0,
            tcount(groups)[colnames(x2)]),
            colnames(x2));
         nRepsLabel <- nameVector(paste0("n=", nReps), colnames(x2));
         attr(x2, "n") <- nReps;
         attr(x2, "nLabel") <- nRepsLabel;
         if (verbose > 1) {
            printDebug("rowGroupMeans(): ",
               "Added n and nLabel as attributes.");
         }
      });
   }

   return(x2);
}

#' @rdname rowGroupMeans
#'
#' @export
rowGroupRmOutliers <- function
(x,
 groups,
 na.rm=TRUE,
 rmOutliers=TRUE,
 crossGroupMad=TRUE,
 madFactor=5,
 returnType=c("input"),
 groupOrder=c("same",
    "sort"),
 keepNULLlevels=FALSE,
 includeAttributes=FALSE,
 verbose=FALSE,
 ...)
{
   # wrapper to rowGroupMeans()
   returnType <- match.arg(returnType);
   groupOrder <- match.arg(groupOrder);
   rowGroupMeans(x=x,
      groups=groups,
      na.rm=na.rm,
      rmOutliers=TRUE,
      crossGroupMad=crossGroupMad,
      madFactor=madFactor,
      returnType=returnType,
      groupOrder=groupOrder,
      keepNULLlevels=keepNULLlevels,
      includeAttributes=includeAttributes,
      verbose=verbose,
      ...);
}

#' Remove outlier points per row by MAD factor threshold
#'
#' Remove outlier points per row by MAD factor threshold
#'
#' This function applies outlier detection and removal per
#' row of the input numeric matrix.
#'
#' * It first calculates MAD per row.
#' * The MAD threshold cutoff is a multiple of the MAD value,
#' defined by `madFactor`, multiplying the per-row MAD by the
#' `madFactor`.
#' * The absolute difference from median is calculated for each
#' point.
#' * Outlier points are defined:
#'
#'    1. Points with MAD above the MAD threshold, and
#'    2. Points with difference from median at or above `minDiff`
#'
#' The `minDiff` parameter affects cases such as 3 replicates,
#' where all replicates are well within a known threshold
#' indicating low variance, but where two replicates might
#' be nearly identical. Consider:
#'
#' * Three numeric values: `c(10.0001, 10.0002, 10.001)`.
#' * The third value differs from median by only 0.0008.
#' * The third value `10.001` is 5x MAD factor away from median.
#' * `minDiff = 0.01` would require the minimum difference from
#' median to be at least 0.01 to be eligible to be an outlier point.
#'
#' One option to define `minDiff` from the data is to use:
#' `minDiff <- stats::median(rowMads(x))`
#'
#' In this case, the threshold is defined by the median difference
#' from median across all rows.
#' This type of threshold will only be reasonable if the variance
#' across all rows is expected to be fairly similar.
#'
#' This function is substantially faster when the
#' `matrixStats` package is installed, but will use the
#' `apply(x, 1, mad)` format as a last option.
#'
#' ## Assumptions
#'
#' 1. This function assumes the input data is appropriate for
#' the use of MAD as a summary statistic.
#' 2. Specifically, numeric values per row are expected to be roughly
#' normally distributed.
#' 3. Outlier points are assumed to be present in less than half overall
#' non-NA data.
#' 4. Outlier points are assumed to be technical outliers, and therefore
#' not the direct result of the experimental measurements being studied.
#' Technical outliers are often caused by some instrument measurement,
#' methodological failure, or other upstream protocol failure.
#'
#' The default threshold of 5x MAD factor is a fairly lenient
#' criteria, above which the data may even be assumed not to conform
#' to most downstream statistical techniques.
#'
#' For measurements considered to be more robust, or required to be more
#' robust, the threshold 2x MAD is applied. This criteria is usually a
#' reasonable expectation of housekeeper gene expression across replicates
#' within each sample group.
#'
#' @returns `numeric` matrix with the same dimensions
#' as the input `x` matrix. Outliers are replaced with `NA`.
#'
#' If `includeAttributes=TRUE` then attributes will be
#' included:
#'
#' * `outlierDF` which is a `data.frame` with colnames
#'    * rowMedians: `numeric` median on each row
#'    * rowMadValues: `numeric` MAD for each row
#'    * rowThresholds: `numeric` threshold after applying `madFactor` and
#'    `minDiff`
#'    * rowReps: `integer` number of non-NA values in the input data
#'    * rowTypes: `factor` indicating the type of threshold: `"madFactor"`
#'    means the row applied the normal `MAD * madFactor` threshold;
#'    `"minDiff"` means the row applied the `minDiff` threshold which
#'    was the larger threshold.
#'
#' * `minDiff` with the `numeric` value supplied
#' * `madFactor` with the `numeric` MAD factor threshold supplied
#' * `outliersRemoved` with the `integer` total number of new NA values
#' produced by the outlier removal process.
#'
#' @family jam numeric functions
#'
#' @param x numeric matrix
#' @param madFactor `numeric` value to multiply by each row MAD
#'    to define the threshold for outlier detection.
#' @param na.rm `logical` indicating whether to ignore NA values
#'    when calculating the MAD value. It should probably always be
#'    `TRUE`, however setting to `FALSE` will prevent any
#'    calculations in rows that contain `NA` values, which could
#'    be useful.
#' @param minDiff `numeric` value indicating the minimum difference
#'    from median to qualify as an outlier. This value protects
#'    against removing outliers which are already extremely
#'    similar. Consider this example:
#'    * Three numeric values: `c(10.0001, 10.0002, 10.001)`.
#'    * The third value differs from median by only 0.0008.
#'    * The third value `10.001` is 5x MAD factor away from median.
#'    * `minDiff = 0.01` would require the minimum difference from
#'    median to be at least 0.01 to be eligible to be an outlier point.
#' @param minReps `numeric` minimum number of non-NA values per row
#'    for outliers to be filtered on the row. This argument is typically
#'    only relevant for rows with `n=2` non-NA values, and when
#'    `rowMadValues` is supplied and may define a threshold less than
#'    half the difference in the two points on the given row.
#'    Otherwise, n=2 defines each point at exactly 1x MAD from median,
#'    and would therefore never be considered an outlier.
#' @param includeAttributes `logical` indicating whether to return
#'    attributes that describe the threshold and type of threshold
#'    used per row, in addition to the madFactor and minDiff values
#'    defined.
#' @param rowMadValues `numeric` optional set of row MAD values to use,
#'    which is mostly helpful when combining MAD values across multiple
#'    samples groups on each row of data, where the combined MAD values
#'    may be more reliable than individual group MAD values.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional parameters are ignored.
#'
#' @examples
#' set.seed(123);
#' x <- matrix(ncol=5, stats::rnorm(25))*5 + 10;
#' ## Define some outlier points
#' x[1:2,3] <- x[1:2,3]*5 + 50;
#' x[2:3,2] <- x[2:3,2]*5 - 100;
#' rownames(x) <- head(letters, nrow(x));
#'
#' rowRmMadOutliers(x, madFactor=5);
#'
#' x2 <- rowRmMadOutliers(x, madFactor=2,
#'    includeAttributes=TRUE);
#' x2
#'
#' x3 <- rowRmMadOutliers(x2,
#'    madFactor=2,
#'    rowMadValues=attr(x2, "outlierDF")$rowMadValues,
#'    includeAttributes=TRUE);
#' x3
#'
#' @export
rowRmMadOutliers <- function
(x,
 madFactor=5,
 na.rm=TRUE,
 minDiff=0,
 minReps=3,
 includeAttributes=FALSE,
 rowMadValues=NULL,
 verbose=FALSE,
 ...)
{
   ## Purpose is to perform rmMadOutlier() function of removing outliers
   ## outside madFactor times the MAD for each row.
   ## It take a matrix input, and returns a matrix output with outliers
   ## converted to NA values.
   ##
   ## minDiff defines a minimum difference required in order to be an outlier,
   ## for example if three values are c(10.110, 10.112, 10.141) the third value
   ## is a MAD outlier, but differs from median by only 0.009 even though it
   ## is 10x MAD units away from median.
   ## Typically, minDiff in log2 space would be something like log2(1.2)
   ##
   if (length(minDiff) == 0) {
      minDiff <- 0;
   }

   # define custom functions based upon object type and packages available
   use_fn_type <- "base";
   if (inherits(x, "sparseMatrix")) {
      is_sparse <- TRUE;
      if (check_pkg_installed("sparseMatrixStats")) {
         use_fn_type <- "sparseMatrixStats";
         fn_rowMads <- sparseMatrixStats::rowMads
         fn_rowMedians <- sparseMatrixStats::rowMedians
         fn_rowSums <- sparseMatrixStats::rowSums2
      }
   } else if (check_pkg_installed("matrixStats")) {
      use_fn_type <- "matrixStats";
      fn_rowMads <- matrixStats::rowMads
      fn_rowMedians <- matrixStats::rowMedians
      fn_rowSums <- base::rowSums
   }
   if ("base" %in% use_fn_type) {
      fn_rowMads <- function(x, ...) {
         apply(x, 1, function(i){stats::mad(i, ...)})}
      fn_rowMedians <- function(x, ...) {
         apply(x, 1, function(i){stats::median(i, ...)})}
      fn_rowSums <- base::rowSums
   }

   # re-use MAD values, or calculate MAD values per row
   if (length(rowMadValues) > 0) {
      if (length(names(rowMadValues)) > 0 &&
            length(rownames(x)) > 0) {
         if (!all(rownames(x) %in% names(rowMadValues))) {
            stop(paste0("all rownames(x) must be present in names(rowMadValues), ",
               "or rowMadValues must have length equal to nrow(x)."));
            stop("rownames(x) must all be present in names(rowMadValues)");
         }
         xMads <- rowMadValues[rownames(x)];
      } else if (length(rowMadValues) == nrow(x)) {
         xMads <- rowMadValues;
      } else {
         stop(paste0("rowMadValues must have length equal to nrow(x), ",
         "or all rownames(x) must be present in names(rowMadValues)."));
      }
   } else {
      xMads <- fn_rowMads(x,
         na.rm=na.rm);
   }

   # calculate median and non-NA replicates per row
   xMedians <- fn_rowMedians(x,
      na.rm=TRUE);
   xReps <- fn_rowSums(!is.na(x));

   # rowThresholds is the threshold difference from median for each row
   # and must be at least minDiff
   rowThresholds <- noiseFloor(xMads * madFactor,
      minimum=minDiff);
   x_NA_before <- sum(is.na(x));
   # set any points that exceed this threshold to NA
   x[abs(x - xMedians) > rowThresholds & xReps >= minReps] <- NA;
   x_NA_after <- sum(is.na(x));
   if (TRUE %in% includeAttributes) {
      rowTypes <- ifelse((xMads * madFactor) < minDiff,
         "minDiff",
         "madFactor");
      attr(x, "outlierDF") <- data.frame(
         rowMedians=xMedians,
         rowMadValues=xMads,
         rowThresholds=rowThresholds,
         rowReps=xReps,
         rowTypes=factor(rowTypes),
         row.names=rownames(x));
      attr(x, "minDiff") <- minDiff;
      attr(x, "madFactor") <- madFactor;
      attr(x, "outliersRemoved") <- (x_NA_after - x_NA_before);
   }
   x;
}

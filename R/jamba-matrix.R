#' Fix matrix dimension ratio
#'
#' Fix matrix dimension ratio
#'
#' This function is experimental, replicating the logic used inside
#' `imageDefault()` to ensure a numeric matrix is roughly 1:1 ratio
#' of nrow:ncol. It currently duplicates columns or rows `n` times
#' in an effort to make the resulting matrix less than a 2:1 ratio.
#' The purpose is to allow `rasterImage()` or `grid.raster()` with
#' argument `interpolate=TRUE` to produce an output raster image
#' that has interpolated the image with reasonably square pixels.
#' Without this adjustment, a matrix with 2,000 rows and 10 columns
#' would be interpolated much more on the x-axis than the y-axis,
#' blurring the data along the x-axis.
#'
#' The main goal is to enable arguments `useRaster=TRUE` and
#' `interpolate=TRUE` which allows an output image to contain
#' more rows than pixels, and still have the pixels represent
#' properly smoothed content.
#'
#' See the examples for visual examples of the effect, showing
#' `image.default()`, `jamba::imageDefault()`,
#' `graphics::rasterImage()`, and `grid::grid.raster()`.
#'
#' @param x `matrix` input
#' @param maxRatioFix integer value indicating the maximum multiple
#'    used to duplicate columns or rows. This value is used to prevent
#'    replicating a matrix with 1 million rows and 10 columns into
#'    a 10 million by 10 million matrix. For example `maxRatioFix=100`
#'    will not replicate columns or rows more than 100 times.
#' @param ratioThreshold numeric value indicating the ratio of nrow:ncol
#'    above which this function will adjust the dimensions of the
#'    output matrix. For example when `ratioThreshold=3` there must be
#'    3 times more rows than columns, or 3 times more columns than rows.
#' @param rasterTarget integer number reflecting the target minimum
#'    number of rows and columns. This value is used to protect from
#'    interpolating a 5x5 matrix, which yields a blurry result. When
#'    `rasterTarget=200`, a 5x5 matrix will be expanded to 200x200,
#'    and the 200x200 matrix will be interpolated to yield a sharp image.
#' @param minRasterMultiple integer vector of 1 or 2 values, referring
#'    to the minimum number of times each row or column is replicated,
#'    respectively. For example `minRasterMultiple=c(2,5)` will at minimum
#'    replicate each row 2 times, and each column 5 times.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' m <- matrix(rainbow(9), ncol=3);
#' m2 <- fix_matrix_ratio(m);
#' par("mfrow"=c(1,3));
#' imageByColors(m, useRaster=FALSE,
#'    main="m\nuseRaster=FALSE");
#' imageByColors(m, useRaster=TRUE, fixRasterRatio=FALSE,
#'    main="m\nuseRaster=FALSE\nfixRasterRatio=FALSE");
#' imageByColors(m2, useRaster=TRUE, fixRasterRatio=FALSE,
#'    main="m2\nuseRaster=FALSE\nfixRasterRatio=FALSE");
#'
#' m <- matrix(colors()[1:90], ncol=3)
#' dim(m)
#' m2 <- fix_matrix_ratio(m);
#' dim(m2);
#' par("mfrow"=c(1,4));
#' imageByColors(m, useRaster=FALSE,
#'    main="m\nuseRaster=FALSE");
#' imageByColors(m, useRaster=TRUE, interpolate=FALSE,
#'    main="m\nuseRaster=TRUE\ninterpolate=FALSE");
#' imageByColors(m, useRaster=TRUE, interpolate=TRUE, fixRasterRatio=FALSE,
#'    main="m\nuseRaster=TRUE\ninterpolate=TRUE");
#' imageByColors(m2, useRaster=TRUE, fixRasterRatio=FALSE,
#'    main="fix_matrix_ratio(m)\nuseRaster=TRUE\ninterpolate=TRUE");
#' par("mfrow"=c(1,1));
#'
#' ## Complicated example showing the effect of interpolate=TRUE
#' testHeatdata <- matrix(rnorm(90000), ncol=9)[,1:9];
#' testHeatdata <- testHeatdata[order(testHeatdata[,5]),];
#' g1 <- seq(from=10, to=10000, by=1000);
#' testHeatdata[g1+rep(1:3, each=length(g1)),] <- 9;
#' for (i in seq(from=125, to=235, by=3)) {
#'    ix <- round(sin(deg2rad(i))*5+5);
#'    iy <- round(-cos(deg2rad(i))*5500 + 3500);
#'    testHeatdata[iy:(iy+4), ix] <- 10;
#' }
#' g2 <- 3011+c(1:12*90);
#' testHeatdata[g2+rep(1:3, each=length(g2)), c(3,7)] <- 10;
#' testHeatdata <- testHeatdata[10000:1,];
#' col <- getColorRamp("RdBu_r", n=15, lens=1, trimRamp=c(4,1));
#' par("mfrow"=c(1,2));
#' image.default(z=t(testHeatdata), col=col, useRaster=TRUE, interpolate=FALSE,
#'    main="image.default(..., useRaster=TRUE,\ninterpolate=FALSE)");
#' imageDefault(z=t(testHeatdata), col=col, useRaster=TRUE,
#'    main="imageDefault(..., useRaster=TRUE,\ninterpolate=TRUE)");
#' par("mfrow"=c(1,1));
#'
#' @family jam numeric functions
#'
#' @export
fix_matrix_ratio <- function
(x,
 maxRatioFix=10,
 minRasterMultiple=NULL,
 rasterTarget=200,
 ratioThreshold=3,
 verbose=FALSE,
 ...)
{
   if (!is.matrix(x)) {
      stop("x must be a matrix");
   }
   if (length(x) == 0) {
      return(x);
   }
   if (length(ratioThreshold) == 0) {
      ratioThreshold <- 3;
   }
   ratioThreshold <- head(ratioThreshold, 1);
   if (ratioThreshold < 1) {
      stop("ratioThreshold must be higher than 1.");
   }
   rasterTarget <- rep(rasterTarget, length.out=2);
   if (length(minRasterMultiple) == 0) {
      if (length(rasterTarget) > 0) {
         minRasterMultiple <- c(ceiling(rasterTarget[1]/nrow(x)),
            ceiling(rasterTarget[2]/ncol(x)));
      } else {
         minRasterMultiple <- c(1,1);
      }
   } else {
      minRasterMultiple <- rep(minRasterMultiple, length.out=2);
   }
   if (verbose) {
      printDebug("minRasterMultiple:", minRasterMultiple);
   }
   if (!is.null(maxRatioFix)) {
      maxRatioFix <- rep(maxRatioFix, length.out=2);
   }

   if (any(minRasterMultiple > 1) ||
         (nrow(x)-1) > ratioThreshold * (ncol(x)-1) ||
         (ncol(x) > ratioThreshold * nrow(x)) ) {
      if (verbose) {
         printDebug("fix_matrix_ratio(): ",
            c("Fixing the matrix ratio."));
      }
      dimRange <- range(c(ncol(x), nrow(x)));
      if (ncol(x) > ratioThreshold * nrow(x)) {
         dupRowX <- floor((ncol(x)-1)/(nrow(x)-1));
         dupRowX <- min(c(dupRowX, maxRatioFix[1]));
         dupColX <- 1;
      } else {
         dupColX <- floor((nrow(x)-1)/(ncol(x)-1));
         dupColX <- min(c(dupColX, maxRatioFix[2]));
         dupRowX <- 1;
      }
      if (verbose) {
         printDebug("fix_matrix_ratio(): ",
            "dupColX: ",
            dupColX);
         printDebug("fix_matrix_ratio(): ",
            "dupRowX: ",
            dupRowX);
      }

      ## Ensure that minRasterMultiple is applied
      dupColX <- min(c(maxRatioFix[2],
         max(c(dupColX, minRasterMultiple[2]))));
      dupRowX <- min(c(maxRatioFix[1],
         max(c(dupRowX, minRasterMultiple[1]))));

      if (verbose) {
         printDebug("fix_matrix_ratio(): ",
            "minRasterMultiple:",
            minRasterMultiple);
         printDebug("fix_matrix_ratio(): ",
            "maxRatioFix:",
            maxRatioFix);
         printDebug("fix_matrix_ratio(): ",
            "dupColX: ",
            dupColX);
         printDebug("fix_matrix_ratio(): ",
            "dupRowX: ",
            dupRowX);
      }
      newCols <- rep(1:(ncol(x)-0), each=dupColX);
      newRows <- rep(1:(nrow(x)-0), each=dupRowX);

      x <- x[newRows,newCols,drop=FALSE];
   } else if (ncol(x) > ratioThreshold * nrow(x)) {
      if (verbose) {
         printDebug("fix_matrix_ratio(): ",
            "ncol(x) is more than ", ratioThreshold, "x nrow(x).");
      }
      dupRowX <- floor((ncol(x)-0)/(nrow(x)-0));
      newRows <- rep(1:(nrow(x)-0), each=dupRowX);
      x <- x[newRows,,drop=FALSE];
   }
   return(x);
}

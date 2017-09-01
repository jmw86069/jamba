##
## jamba-plots.r
##
## plotSmoothScatter
## smoothScatterJam
## imageDefault

#' Smooth scatter plot with enhancements
#'
#' Produce scatter plot using point density instead of displaying
#' individual data points.
#'
#' This function intends to make several potentially customizable
#' features of \code{\link[graphics]{smoothScatter}} plots much easier
#' to customize. For example bandwidthN allows defining the number of
#' bandwidth steps used by the kernel density function, and importantly
#' bases the number of steps on the visible plot window, and not the range
#' of data, which can differ substantially. The nbin parameter is related,
#' but is used to define the level of detail used in the image function,
#' which when plotting numerous smaller panels, can be useful to reduce
#' unnecessary visual details.
#'
#' A related new function could be part of ggplot2, and is certainly
#' on the todo list. However, frankly it is quite difficult to grok where one
#' would obtain several of the values required by this function, notably
#' the visible plot space per panel. The documentation for ggplot2 developers
#' is not well described. That said, panelSmoothScatter could be created to
#' handle the main features of this function. However, it would also
#' need to call a custom imageDefault() function in order to handle the
#' custom (and hopefully beneficial) mechanism of creating a properly-scaled
#' raster image.
#'
#' @param x numeric vector, or data matrix with two or  more columns.
#' @param y numeric vector, or if data is supplied via x as a matrix, y
#'    is NULL.
#' @param bandwidthN integer number of bandwidth steps to use across the
#'    visible plot window. Note that this bandwidth differs from default
#'    \code{\link[graphics]{smoothScatter}} in that it uses the visible
#'    plot window instead of the data range, so if the plot window is not
#'    sufficiently similar to the data range, the resulting smoothed
#'    density will not be visibly distorted. This parameter also permits
#'    display of higher (or lower) level of detail.
#' @param transformation function which converts point density to a number,
#'    typically related to square root or cube root transformation.
#' @param xlim optional numeric x-axis range
#' @param ylim optional numeric y-axis range
#' @param nbin integer number of bins to use when converting the kernel
#'    density result (which uses bandwidthN above) into a usable image.
#'    For example, nbin=256 produces notably high detail, while nbin=32
#'    produces lower resolution which may be more suitable when plotting
#'    multiple smaller plot panels.
#' @param nrpoints integer number of outlier datapoints to display,
#'    as defined by the
#'    hidden but very useful
#'    \code{grDevices:::.smoothScatterCalcDensity}
#'    function. The base \code{link[graphics]{smoothScatter}} function
#'    plots 100 such points, perhaps to overcome the default over-smoothing
#'    of data which results in large areas not displaying density. The
#'    default here is zero, since the new default bandwidthN parameter
#'    typically already indicates these points.
#' @param colramp one of several inputs recognized by
#'    \code{\link{getColorRamp}}. It typically recognizes either the name of
#'    a color ramp from RColorBrewer, the name of functions from the
#'    \code{\link[viridis]{viridis}} package, or single R colors, or
#'    a vector of R colors.
#' @param doTest logical, defines whether to create a visual set of test
#'    plots to demonstrate the utility of this function.
#' @param fillBackground logical, whether to fill the background with the
#'    first colramp color, which is useful especially when that color is
#'    not white, and most visible when the x- and y-axes are not identical
#'    to the data range. Run doTest=TRUE with colramp="viridis" as a test.
#' @param naAction character value, indicating how to handle missing NA values,
#'    typically when x is NA and y is not NA, or vice versa. valid values:
#'    \describe{
#'       \item{"remove"}{ignore any points where either x or y are NA}
#'       \item{"floor0"}{change any NA values to zero 0 for either x or y}
#'       \item{"floor1"}{change any NA values to one 1 for either x or y}
#'    }
#'    The latter two options are useful when the desired plot should indicate
#'    the presence of an NA value in either x or y, while also indicating the
#'    the corresponding non-NA value in the opposing axis. The driving use
#'    was plotting gene fold changes from two experiments, where the two
#'    experiments may not have measured the same genes.
#' @param xaxt character value compatible with par(xaxt), used to control
#'    the x-axis range, similar to its use in plot(...) generic functions.
#' @param yaxt character value compatible with par(yaxt), used to control
#'    the y-axis range, similar to its use in plot(...) generic functions.
#' @param applyRangeCeiling logical, indicates how to handle points outside
#'    the visible plot range. Valid values:
#'    \describe{
#'       \item{TRUE}{apply floor and ceiling, which fixes
#'       these values to the edges of the visible plot area, so they have a
#'    visible indication without forcing the plot space to be expanded outside
#'    what may be a more useful (zoomed) range.}
#'       \item{FALSE}{do not apply floor
#'    and ceiling, meaning points outside the visible range are simply not
#'    displayed. Not applying a ceiling is desirable for example, if there
#'    is a huge number of points at zero, and the presence of these points
#'    adversely affects the kernel density.}
#'    }
#' @param useRaster logical, indicating whether to produce plots using the
#'    \code{\link[graphics]{rasterImage}} function, which produces a plot
#'    raster image offline then scales this image to usable plot space,
#'    often resulting in substantially faster plot output, with subtantially
#'    smaller object size especially in vector output files like PDF and SVG.
#' @seealso \code{\link{smoothScatterJam}},
#'    \code{\link[graphics]{smoothScatter}}
#' @examples
#' plotSmoothScatter(doTest=TRUE, colramp="viridis");
#' plotSmoothScatter(doTest=TRUE, colramp="navy", fillBackground=FALSE);
#' plotSmoothScatter(doTest=TRUE, colramp=c("white","navy","orange"));
#'
#' @export
plotSmoothScatter <- function
(x, y=NULL, bandwidthN=300,
 transformation=function(x)x^0.25,
 xlim=NULL, ylim=NULL, nbin=256, nrpoints=0,
 colramp=c("white", "lightblue", "blue", "orange", "orangered2"),
 doTest=FALSE, fillBackground=TRUE,
 naAction=c("remove", "floor0", "floor1"),
 xaxt="s", yaxt="s",
 applyRangeCeiling=TRUE, useRaster=TRUE,
 ...)
{
   ## Purpose is to wrapper the smoothScatter() function in order to increase
   ## the apparent detail by adjusting the bandwidth parameters in a somewhat
   ## more automated/intuitive way than the default parameters.
   ## Now, the smoothScatter() function uses some default number, which results
   ## in a low amount of detail, and in my opinion loses important features.
   ## To see an example of the difference, and hopefully the visual improvement,
   ## run this function plotSmoothScatter(doTest=TRUE)
   ##
   ## fillBackground=TRUE will fill the plot with the background color
   ## (first color in colramp), which corrects the issue when the
   ## smoothScatter only returns data for part of the plot.
   ##
   ## dotest=TRUE will display some visual benefits of plotSmoothScatter()
   ##
   ## colramp is either a color function, or a vector or single color. If the
   ## class is "character" it is sent to getColorRamp() which returns a color
   ## ramp. It currently recognizes Brewer colors, viridis, or other
   ## combinations of colors.
   ##
   ## plotSmoothScatter(doTest=TRUE, colramp="viridis")
   ## plotSmoothScatter(doTest=TRUE, colramp="navy")
   ## plotSmoothScatter(doTest=TRUE, colramp=c("white","navy","orange")
   ##
   naAction <- match.arg(naAction);
   if (is.null(colramp)) {
      colramp <- c("white", "lightblue", "blue", "orange", "orangered2");
   }
   if ("character" %in% class(colramp)) {
      colramp <- colorRampPalette(getColorRamp(colramp));
   }
   if (doTest) {
      testN <- 20000;
      testN1 <- floor(testN*0.9);
      testN2 <- testN - testN1;
      x <- matrix(ncol=2, data=rnorm(testN*2));
      x[,2] <- x[,1] + rnorm(testN)*0.1;

      ## Add secondary line offset from the main correlation
      xSample <- sample(1:nrow(x), floor(nrow(x)/20));

      xSub <- t(t(x[xSample,,drop=FALSE])+c(0.6,-0.4));
      x <- rbind(x, xSub);
      oPar <- par(no.readonly=TRUE);
      par("mfrow"=c(3,3));
      for (noise in c(0.3,0.6,1.2)) {
         n <- seq(from=testN1+1, to=testN);
         x[n,2] <- x[n,1] + rnorm(testN2) * noise;
         smoothScatter(x, colramp=colramp,
            main=paste0("smoothScatter\n",
               "10% noise at rnorm(x)*", noise, "\n",
               "default bandwidth"),
            xlim=c(-4,4), ylim=c(-4,4), ...);
         plotSmoothScatter(x, doTest=FALSE, colramp=colramp,
            fillBackground=fillBackground,
            main=paste0("plotSmoothScatter\n",
               "10% noise at rnorm(x)*", noise, "\n",
               "custom bandwidthN=", bandwidthN/2),
            xlim=c(-4,4), ylim=c(-4,4), bandwidthN=bandwidthN/2, ...);
         plotSmoothScatter(x, doTest=FALSE, colramp=colramp,
            fillBackground=fillBackground,
            main=paste0("plotSmoothScatter\n",
               "10% noise at rnorm(x)*", noise, "\n",
               "custom bandwidthN=", bandwidthN),
            xlim=c(-4,4), ylim=c(-4,4), bandwidthN=bandwidthN, ...);
      }
      par(oPar);
      return(NULL);
   }

   if (!is.null(y) && is.numeric(y) && is.numeric(x)) {
      x <- matrix(ncol=2, c(x, y));
   }
   if ((is.matrix(x) || is.data.frame(x)) && ncol(x) == 2) {
      y <- x[,2];
      x <- x[,1];
   } else if (is.null(Y) && (is.matrix(x) || is.data.frame(x))) {
      stop("Cannot handle matrix input x, when Y is NULL.");
   }
   ## Deal with NA values
   if (naAction == "remove") {
      naValues <- is.na(x) | is.na(y);
      x <- x[!naValues];
      y <- y[!naValues];
   } else if (naAction == "floor0") {
      naValuesX <- is.na(x);
      x[naValuesX] <- 0;
      naValuesY <- is.na(y);
      y[naValuesY] <- 0;
   } else if (naAction == "floor1") {
      naValuesX <- is.na(x);
      x[naValuesX] <- 1;
      naValuesY <- is.na(y);
      y[naValuesY] <- 1;
   }

   bandwidthN <- rep(bandwidthN, length.out=2);
   if (is.null(xlim)) {
      xlim <- range(x);
   }
   if (is.null(ylim)) {
      ylim <- range(y);
   }
   ## Apply a ceiling to values outside the range
   if (applyRangeCeiling) {
      tooHighX <- x > max(xlim);
      tooLowX <- x < min(xlim);
      x[tooHighX] <- max(xlim);
      x[tooLowX] <- min(xlim);
      tooHighY <- y > max(ylim);
      tooLowY <- y < min(ylim);
      y[tooHighY] <- max(ylim);
      y[tooLowY] <- min(ylim);
   }
   xlim4 <- sort((c(-1,1) * diff(xlim)*0.02) + xlim);
   ylim4 <- sort((c(-1,1) * diff(ylim)*0.02) + ylim);
   ## Adjust for uneven plot aspect ratio, by using the plot par("pin") which is the actual dimensions
   ## Note that it doesn't require the actual coordinates of the plot, just the relative size of the display
   pin1 <- par("pin")[1] / par("pin")[2];
   bandwidthXY <- c(diff(xlim4)/bandwidthN[1], diff(ylim4)/bandwidthN[2]*pin1);
   if (fillBackground) {
      nullPlot(doBoxes=FALSE, doUsrBox=TRUE, fill=head(colramp(11),1), xlim=xlim4, ylim=ylim4, ...);
      axis(1, las=2);
      axis(2, las=2);
      smoothScatterJam(x=x, y=y, add=TRUE,
         transformation=transformation,
         bandwidth=bandwidthXY, nbin=nbin, nrpoints=nrpoints,
         xlim=xlim4, ylim=ylim4, xaxs="i", yaxs="i", xaxt=xaxt, yaxt=yaxt,
         colramp=colramp, useRaster=useRaster, ...);
   } else {
      smoothScatterJam(x=x, y=y,
         transformation=transformation,
         bandwidth=bandwidthXY, nbin=nbin, nrpoints=nrpoints,
         xlim=xlim4, ylim=ylim4, xaxs="i", yaxs="i", xaxt=xaxt, yaxt=yaxt,
         colramp=colramp, useRaster=useRaster, ...);
   }
   invisible(list(x=x, y=y,
                 transformation=transformation,
                 bandwidth=bandwidthXY, nbin=nbin,
                 xlim=xlim4, ylim=ylim4, xaxs="i", yaxs="i", xaxt=xaxt, yaxt=yaxt,
                 colramp=colramp));
}

#' Smooth scatter plot, Jam style
#'
#' Produce smooth scatter plot, a helper function called by
#' \code{\link{plotSmoothScatter}}.
#'
#' This function is only necessary in order to override the
#' \code{\link[graphics]{smoothScatter}} function which calls image.default().
#' Instead, this function calls \code{\link{imageDefault}} which is required
#' in order to utilize custom raster image scaling, particularly important
#' when the x- and y-axis ranges are not similar, e.g. where the x-axis spans
#' 10 units, but the y-axis spans 10,000 units. The bulk of this function
#' and its parameters are simply copied from
#' \code{\link[graphics]{smoothScatter}} for consistency with the parent
#' function, with due credit and respect to its authors.
#'
#' @param x numeric vector, or data matrix with two or  more columns.
#' @param y numeric vector, or if data is supplied via x as a matrix, y
#'    is NULL.
#' @param nbin integer number of bins to use when converting the kernel
#'    density result (which uses bandwidthN above) into a usable image.
#'    For example, nbin=123 is the default used by
#'    \code{\link[graphics]{smoothScatter}}, however the
#'    \code{\link{plotSmoothScatter}} function default is higher (256).
#' @param bandwidth numeric vector used to define the y- and x-axis
#'    bandwidths, respectively, for the hidden but very useful
#'    \code{grDevices:::.smoothScatterCalcDensity}
#'    function, which calculates the underlying 2-dimensional kernel
#'    density of data points. This parameter is also why the
#'    wrapper function \code{\link{plotSmoothScatter}} was created, in
#'    order to avoid ever having to define this parameter directly.
#' @param nrpoints integer number of outlier datapoints to display,
#'    as defined by the
#'    hidden but very useful
#'    \code{grDevices:::.smoothScatterCalcDensity}
#'    function. The base \code{link[graphics]{smoothScatter}} function
#'    plots 100 such points, perhaps to overcome the default over-smoothing
#'    of data which results in large areas not displaying density. The
#'    default here is zero, since the new default bandwidthN parameter
#'    typically already indicates these points.
#' @param transformation function which converts point density to a number,
#'    typically related to square root or cube root transformation.
#' @param postPlotHook is NULL for no post-plot hook, or a function which
#'    is called after producing the image plot. By default it is simply used
#'    to draw a box around the image, but could be used to layer additional
#'    information atop the image plot, for example contours, labels, etc.
#' @param xlab character x-axis label
#' @param ylab character y-axis label
#' @param xlim numeric x-axis range for the plot
#' @param ylim numeric y-axis range for the plot
#' @param xaxs character value compatible with par(xaxs), mainly useful
#'    for suppressing the x-axis, in order to produce a custom x-axis
#'    range, most useful to restrict the axis range expansion done by R
#'    by default.
#' @param yaxs character value compatible with par(yaxs), mainly useful
#'    for suppressing the y-axis, in order to produce a custom y-axis
#'    range, most useful to restrict the axis range expansion done by R
#'    by default.
#' @param xaxt character value compatible with par(xaxt), mainly useful
#'    for suppressing the x-axis, in order to produce a custom x-axis
#'    by other mechanisms, e.g. log-scaled x-axis tick marks.
#' @param yaxt character value compatible with par(yaxt), mainly useful
#'    for suppressing the y-axis, in order to produce a custom y-axis
#'    by other mechanisms, e.g. log-scaled y-axis tick marks.
#' @param useRaster NULL or logical, indicating whether to invoke
#'    \code{\link[graphics]{rasterImage}} to produce a raster image.
#'    If NULL, it determines whether to produce a raster image within the
#'    \code{\link{imageDefault}} function, which checks the options
#'    using \code{getOption("preferRaster", FALSE)} to determine among
#'    other things, whether the user prefers raster images, and if the
#'    dev.capabilities supports raster.
#'
#' @seealso \code{\link{plotSmoothScatter}},
#'    \code{\link{imageDefault}},
#'    \code{\link[graphics]{smoothScatter}}
#'
#' @export
smoothScatterJam <- function
(x, y=NULL, nbin=128, bandwidth,
 colramp=colorRampPalette(c("white", blues9)),
 nrpoints=100, pch=".", cex=1, col="black",
 transformation=function(x) x^0.25, postPlotHook=box,
 xlab=NULL, ylab=NULL, xlim, ylim,
 xaxs=par("xaxs"), yaxs=par("yaxs"), xaxt=par("xaxt"), yaxt=par("yaxt"),
 useRaster=NULL,
 ...)
{
   ## Purpose is to wrapper the graphics::smoothScatter() function
   ## solely so it uses imageDefault, which enables improved
   ## useRaster=TRUE functionality
   ##
   ## postPlotHook is a function called after the plot is created,
   ## useful for drawing a box around the plot, or performing
   ## other customizations.
   ##
   if (!suppressPackageStartupMessages(require(grDevices))) {
      stop("smoothScatterJam() requires the grDevices package.");
   }
   #if (!suppressPackageStartupMessages(require(KernSmooth))) {
   #   stop("smoothScatterJam() requires the KernSmooth package.");
   #}
   if (!is.numeric(nrpoints) | (nrpoints < 0) | (length(nrpoints) !=1)) {
      stop("'nrpoints' should be numeric scalar with value >= 0.");
   }
   xlabel <- if (!missing(x)) {
      deparse(substitute(x));
   }
   ylabel <- if (!missing(y)) {
      deparse(substitute(y));
   }
   xy <- xy.coords(x, y, xlabel, ylabel);
   xlab <- if (is.null(xlab)) {
      xy$xlab;
   } else {
      xlab;
   }
   ylab <- if (is.null(ylab)) {
      xy$ylab;
   } else {
      ylab;
   }
   x <- cbind(xy$x, xy$y)[is.finite(xy$x) & is.finite(xy$y),,drop=FALSE];
   if (!missing(xlim)) {
      stopifnot(is.numeric(xlim), length(xlim) == 2, is.finite(xlim));
      x <- x[min(xlim) <= x[, 1] & x[, 1] <= max(xlim),];
   } else {
      xlim <- range(x[, 1]);
   }
   if (!missing(ylim)) {
      stopifnot(is.numeric(ylim), length(ylim) == 2, is.finite(ylim));
      x <- x[min(ylim) <= x[, 2] & x[, 2] <= max(ylim),];
   } else {
      ylim <- range(x[, 2]);
   }
   map <- grDevices:::.smoothScatterCalcDensity(x, nbin, bandwidth);
   xm <- map$x1;
   ym <- map$x2;
   dens <- map$fhat;
   dens[] <- transformation(dens);
   imageDefault(xm, ym, z=dens, col=colramp(256), xlab=xlab,
      ylab=ylab, xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs, xaxt=xaxt, yaxt=yaxt,
      useRaster=useRaster, ...);
   imageL <- list(xm=xm, ym=ym, z=dens, col=colramp(256), xlab=xlab,
      ylab=ylab, xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs, xaxt=xaxt, yaxt=yaxt);
   if (!is.null(postPlotHook)) {
      postPlotHook();
   }
   if (nrpoints > 0) {
      nrpoints <- min(nrow(x), ceiling(nrpoints));
      stopifnot((nx <- length(xm)) == nrow(dens),
         (ny <- length(ym)) == ncol(dens));
      ixm <- 1L + as.integer((nx - 1) * (x[, 1] - xm[1])/(xm[nx] - xm[1]));
      iym <- 1L + as.integer((ny - 1) * (x[, 2] - ym[1])/(ym[ny] - ym[1]));
      sel <- order(dens[cbind(ixm, iym)])[seq_len(nrpoints)];
      points(x[sel,], pch=pch, cex=cex, col=col);
   }
   invisible(imageL);
}

#' Create a blank plot with optional labels
#'
#' Create a blank plot with optional labels for margins
#'
#' This function creates an empty plot space, using the current
#' \code{\link{par}} settings for margins, text size, etc. By default
#' it displays a box around the plot window, and labels the margins and
#' plot area for review. It can be useful as a visual display of various
#' base graphics settings, or to create an empty plot window with pre-defined
#' axis ranges. Lastly, one can use this function to create a "blank" plot
#' which uses a defined background color, which can be a useful precursor to
#' drawing an image density which may not cover the whole plot space.
#'
#' The doBoxes=TRUE functionality was adapted from Earl F. Glynn's margin
#' tutorial:
#' \link{http://research.stowers.org/mcm/efg/R/Graphics/Basics/mar-oma/index.htm}
#' with much respect for his effective visual.
#'
#' @param xaxt character value compatible with \code{options(xaxt)}
#' @param yaxt character value compatible with \code{options(xaxt)}
#' @param xlab character x-axis label
#' @param ylab character y-axis label
#' @param col character colors passed to \code{plot()}
#' @param xlim numeric x-axis range
#' @param ylim numeric y-axis range
#' @param las integer value indicating whether axis labels should be
#'    parallel (1) or perpendicular (2) to the axis line.
#' @param doBoxes logical whether to draw annotated boxes around the plot
#'    and inner and outer margins.
#' @param doUsrBox logical whether to draw a colored bow indicating the
#'    exact plot space, using the function \code{\link{usrBox}}.
#' @param fill character R color used to fill the background of the plot
#'    as used by \code{\link{usrBox}}.
#' @param doAxes logical whether to draw default x- and y-axes.
#' @param doMargins logical whether to label margins, only active when
#'    doBoxes=TRUE.
#' @param plotAreaTitle character label printed in the center of the plot
#'    area.
#' @param plotSrt numeric angle for the plotAreaTitle, which is good for
#'    labeling this plot with vertical text when displaying a plot panel
#'    inside a grid layout, where the plot is taller than it is wide.
#' @param plotNumPrefix character or integer label appended as suffix to
#'    margin labels, which is useful when annotating multiple plots in a
#'    grid layout, where labels are sometimes quite close together. This
#'    label is but a simple attempt to sidestep the real problem of fitting
#'    labels inside each visual component.
#' @param bty character passed to the basic \code{plot()} function, usually
#'    bty="n" suppresses the default box, which can then be optionally drawn
#'    based upon the doBoxes parameter.
#' @param showMarginsOnly logical whether to create a new plot or to annotate
#'    an existing active plot.
#' @examples
#' nullPlot()
#'
#' @export
nullPlot <- function
(xaxt="n", yaxt="n", xlab="", ylab="", col="transparent",
 xlim=c(1,2), ylim=c(1,2), las=par("las"),
 doBoxes=TRUE, doUsrBox=doBoxes, fill="#FFFF9966", doAxes=FALSE, doMargins=TRUE,
 plotAreaTitle="Plot Area", plotSrt=0, plotNumPrefix="", bty="n",
 showMarginsOnly=FALSE,
 ...)
{
   ## Purpose is just to create a dead-simple plot of two points,
   ## so that when the plot window is resized, it does not hang the computer
   ## (as happens something when really intricate plots are resized)
   ##
   ## doBoxes=TRUE will also draw boxes around relevant features including
   ## plot region, margins, outer margins (if defined), and add labels.
   ##
   ## doUsrBox is a colored box with light yellow background covering the
   ## plot region, accomplished with usrBox(...).
   ##
   ## doMargins=TRUE will add text labels indicating various margin settings.
   ##
   ## plotAreaTitle text is positioned in the center of the plot region,
   ## which is good for labeling a plot region in a layout.
   ##
   ## plotSrt defines the angle of the plotAreaTitle, which is good for
   ## labeling this plot with vertical text when displaying a plot panel
   ## inside a grid layout, where the plot is taller than it is wide.
   ##
   ## doBoxes was adapted from Earl F. Glynn's margin tutorial:
   ## http://research.stowers.org/mcm/efg/R/Graphics/Basics/mar-oma/index.htm
   ## Update Aug2017: The link is now defunct, and no archive is available.
   ##
   ## showMarginsOnly=TRUE will not create a new plot, but
   ## instead just annotates margins on the existing plot

   if (showMarginsOnly) {
      parUsr <- par("usr");
      ylim <- parUsr[3:4];
      xlim <- parUsr[1:2];
      points(range(xlim), range(ylim), xaxt=xaxt, yaxt=yaxt, col=col,
         xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, bty=bty, add=TRUE, ...);
   } else {
      plot(range(xlim), range(ylim), xaxt=xaxt, yaxt=yaxt, col=col,
         xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, bty=bty, ...);
   }

   if (doUsrBox) {
      usrBox(fill=fill, ...);
   }
   if (doBoxes) {
      box("plot", col="darkred");

      box("figure",lty="dashed", col="navy");

      ## Print margins
      if (doMargins) {
         Margins <- capture.output(par()$mar);
         Margins <- substr(Margins, 5, nchar(Margins));
         Margins <- paste0("  mar=c(", gsub(" ",",",Margins), ")",
            plotNumPrefix);
         if (plotSrt == 90) {
            plotLas <- 2;
         } else {
            plotLas <- 1;
         }
         if (par("mar")[3] == 0) {
            mtext(Margins, NORTH<-3, line=-1, cex=0.7, col="navy",
               las=plotLas, adj=plotLas-1);
         } else {
            mtext(Margins, NORTH<-3, line=1, cex=0.7, col="navy",
               las=plotLas, adj=plotLas-1);
         }

         box("inner", lty="dotted", col="darkgreen");
         if (any(par("oma") > 0)) {
            mtext("Outer Margin Area", SOUTH<-1, line=0.4, adj=1.0,
               cex=1.5, col="darkgreen",
               outer=TRUE, las=plotLas);
         }
         box("outer", lty="solid", col="darkgreen");

         ## Text: vector of strings in mtext call
         lapply(1:4, function(i){
            if (par("mar")[i] > 0) {
               newLas <- as.integer(3 - i%%2*2);
               par("las"=newLas);
               mtext(paste0("mar[", i, "]", plotNumPrefix, "=", par("mar")[i]),
                  side=i, line=0.4, cex=0.6, col="navy",
                  outer=FALSE, las=newLas);
            }
         });
         par("las"=1);
         lapply(1:4, function(i){
            if (par("oma")[i] > 0) {
               newLas <- as.integer(3 - i%%2*2);
               par("las"=newLas);
               mtext(paste0("oma[", i, "]", plotNumPrefix, "=", 
                  format(digits=2, par("oma")[i])),
                  i, line=0.4, cex=0.6, col="navy", outer=TRUE, las=newLas);
            }
         });
         par("las"=1);
      }

      ## Print a title in the center of the plot region
      iXpd <- par("xpd");
      on.exit(par("xpd"=iXpd));
      par("xpd"=NA);
      text(x=mean(range(xlim)),
         y=mean(range(ylim)),
         labels=plotAreaTitle,
         col="darkred",
         cex=2,
         srt=plotSrt);
      par("xpd"=iXpd);

      ## Print axis labels
      if (doAxes) {
         axis(1, las=las, col="darkred", col.axis="darkred", ...);
         axis(2, las=las, col="darkred", col.axis="darkred", ...);
      }
   }
}

#' Draw colored box indicating R plot space
#'
#' Draw colored box indicating the active R plot space
#'
#' This function simply draws a box indicating the active plot space,
#' and by default it shades the box light yellow with transparency. It
#' can be useful to indicate the active plot area while allowing pre-drawn
#' plot elements to be shown, or can be useful precursor to provide a colored
#' background for the plot.
#'
#' The plot space is defined using \code{par("usr")} and therefore requires
#' an active R device is already opened.
#'
#' @param fill character R color used to fill the background of the plot
#' @param label character text optionally used to label the center of the
#'    plot space.
#' @param parUsr numeric vector length 4, indicating the R plot space,
#'    consistent with \code{par("usr")}. It can thus be used to define a
#'    different area, though using the \code{\link{rect}} function directly
#'    seems more appropriate.
#' @param debug logical whether to print the parUsr value being used.
#' @examples
#' usrBox()
#'
#' @export
usrBox <- function
(fill="#FFFF9966", label=NULL, parUsr=par("usr"), debug=FALSE,
 ...)
{
   ## Purpose is to draw a rectangle, filled transparent yellow,
   ## showing the par("usr") area as defined by R.
   ## This function can also be used to change the plot background color.
   if (debug) {
      printDebug("parUsr: ", c(format(digits=2, parUsr)), c("orange", "lightblue"));
   }
   rect(col=fill, parUsr[1], parUsr[3], parUsr[2], parUsr[4], ...);
   if (!is.null(label)) {
      text(mean(parUsr[c(1,2)]), mean(parUsr[c(3,4)]), label, ...);
   }
}

#' Display a color raster image
#'
#' Display a color raster image
#'
#' This function augments the \code{\link[graphics]{image}} function, in
#' that it handles the useRaster parameter for non-symmetric data matrices,
#' in order to minimize the distortion from image-smoothing when pixels are
#' not square.
#'
#' The function also by default creates the image map using coordinates where
#' each integer represents the center point of one column or row of data,
#' known in the default \code{\link{image}} function as \code{oldstyle=TRUE}.
#' For consistency, \code{imageDefault} will only accept \code{oldstyle=TRUE}.
#'
#' @param x location of grid lines at which the intervals in z are measured.
#' @param y location of grid lines at which the intervals in z are measured.
#' @param z numeric or logical matrix containing the values to be plotted,
#'    where NA values are allowed.
#' @param zlim numeric range allowed for values in z.
#' @param xlim numeric range to plot on the x-axis, by default the x range.
#' @param ylim numeric range to plot on the y-axis, by default the y range.
#' @param col character vector of colors to be mapped to values in z.
#' @param add logical whether to add to an existing active R plot, or create
#'    a new plot window.
#' @param xaxs character value compatible with par(xaxs), mainly useful
#'    for suppressing the x-axis, in order to produce a custom x-axis
#'    range, most useful to restrict the axis range expansion done by R
#'    by default.
#' @param yaxs character value compatible with par(yaxs), mainly useful
#'    for suppressing the y-axis, in order to produce a custom y-axis
#'    range, most useful to restrict the axis range expansion done by R
#'    by default.
#' @param xaxt character value compatible with par(xaxt), mainly useful
#'    for suppressing the x-axis, in order to produce a custom x-axis
#'    by other mechanisms, e.g. log-scaled x-axis tick marks.
#' @param yaxt character value compatible with par(yaxt), mainly useful
#'    for suppressing the y-axis, in order to produce a custom y-axis
#'    by other mechanisms, e.g. log-scaled y-axis tick marks.
#' @param ylab character label for the y-axis
#' @param xlab character label for the x-axis
#' @param breaks numeric vector of breakpoints for colors.
#' @param oldstyle logical whether to delineate axis coordinates with an
#'    integer spacing for each column and row. Note: the only allowed parameter
#'    is TRUE, since useRaster=TRUE requires it. Therefore, this function
#'    for consistency will only output this format.
#' @param useRaster logical whether to force raster image scaling, which
#'    is especially useful for large data matrices. In this case a bitmap
#'    raster image is created instead of polygons, then the bitmap is scaled
#'    to fit the plot space. Otherwise, individual polygons can be obscured
#'    on monitor screens, or may result in an extremely large file size when
#'    writing to vector image format such as PDF or SVG.
#' @param fixRasterRatio logical whether to implement a simple workaround
#'    to the requirement for square pixels, in the event the x- and y-axis
#'    dimensions are not roughly equal.
#' @param maxRatioFix integer maximum number of times any axis may be
#'    replicated to create a matrix of roughly equal x- and y-axis dimensions.
#' @param minRasterMultiple integer minimum number of times the x- and y-axis
#'    will be duplicated, which is mostly useful when creating useRaster=TRUE
#'    for small matrix sizes, otherwise the result will be quite blurry. For
#'    example, minRasterMultiple=10 will duplicate each axis 10 times. Values
#'    are aplied to rows then columns. These values are automatically defined
#'    if minRasterMultiple is NULL and rasterTarget is not NULL.
#' @param rasterTarget integer number of cells below which cells are duplicated
#'    in order to maintain detail. The default 200 defines
#'    minRasterMultiple=c(1,1) if there are 200 rows and 200 columns, or
#'    minRasterMultiple=c(1,100) if there are 200 rows but 2 columns.
#' @param interpolate logical whether to implement image interpolation,
#'    by default TRUE when useRaster=TRUE.
#' @param verbose logical whether to enable verbose output, useful for
#'    debugging.
#'
#' @seealso \code{\link[graphics]{image}}, \code{\link{plotSmoothScatter}}
#' @examples
#' plotSmoothScatter(doTest=TRUE)
#'
#' @export
imageDefault <- function
(x=seq_len(nrow(z)+1)-0.5,
 y=seq_len(ncol(z)+1)-0.5,
 z, zlim=range(z[is.finite(z)]), xlim=range(x), ylim=range(y),
 col=heat.colors(12), add=FALSE, xaxs="i", yaxs="i", xaxt="n", yaxt="n",
 xlab, ylab, breaks, oldstyle=TRUE, useRaster=NULL,
 fixRasterRatio=TRUE, maxRatioFix=100,
 minRasterMultiple=NULL, rasterTarget=200,
 interpolate=getOption("interpolate", TRUE),
 verbose=FALSE,
 ...)
{
   ## Purpose is to override image.default() by using interpolate=TRUE
   ## for raster images, unless otherwise specified. It also implements
   ## a simple technique to allow raster functions to work with substantially
   ## non-symmetric input data, without causing notable distortion in the
   ## rendered image, when fixRasterRatio=TRUE.
   ##
   ## fixRasterRatio is a simple workaround when useRaster=TRUE, which
   ## duplicates rows or columns to provide an approximately square matrix,
   ## since the useRaster methodology works best with that ratio. Otherwise
   ## the image manipulation assumes square pixels, and blending is stretched
   ## on whichever is the smaller axis dimension.
   ##
   ## minRasterMultiple=10 will duplicate the matrix 10 times for each row
   ## and column at a minimum, in addition to any ratio adjustment is
   ## performed by fixRasterRatio.
   ## The goal is to help with small matrices, where useRaster results in a
   ## blurry rasterized image.  Duplicating each row and column helps to
   ## sharpen the edges between cells. minRasterMultiple can be a two-value
   ## vector, for rows and columns respectively, and is extended to length 2
   ## as necessary.
   ##
   if (interpolate && is.null(useRaster)) {
      useRaster <- TRUE;
   }
   if (!is.null(rasterTarget)) {
      rasterTarget <- rep(rasterTarget, length.out=2);
   }
   if (length(minRasterMultiple) == 0) {
      if (!is.null(rasterTarget)) {
         minRasterMultiple <- c(ceiling(head(rasterTarget,1)/ncol(z)),
            ceiling(tail(rasterTarget,1)/nrow(z)));
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

   if (missing(z)) {
      if (!missing(x)) {
         if (is.list(x)) {
            z <- x$z
            y <- x$y
            x <- x$x
         } else {
            if (is.null(dim(x))) {
               stop("argument must be matrix-like");
            }
            z <- x;
            x <- seq.int(0, 1, length.out=nrow(z));
         }
         if (missing(xlab)) {
            xlab <- "";
         }
         if (missing(ylab)) {
            ylab <- "";
         }
      } else {
         stop("no 'z' matrix specified");
      }
   } else if (is.list(x)) {
      xn <- deparse(substitute(x))
      if (missing(xlab)) {
         xlab <- paste(xn, "x", sep="$");
      }
      if (missing(ylab)) {
         ylab <- paste(xn, "y", sep="$");
      }
      y <- x$y;
      x <- x$x;
   } else {
      if (missing(xlab))
         if (missing(x)) {
            xlab <- "";
         } else {
            xlab <- deparse(substitute(x));
         }
      if (missing(ylab))
         if (missing(y)) {
            ylab <- "";
         } else {
            ylab <- deparse(substitute(y));
         }
   }
   if (any(!is.finite(x)) || any(!is.finite(y))) {
      stop("'x' and 'y' values must be finite and non-missing");
   }
   if (any(diff(x) <= 0) || any(diff(y) <= 0)) {
      stop("increasing 'x' and 'y' values expected");
   }
   if (!is.matrix(z)) {
      stop("'z' must be a matrix");
   }
   if (length(x) > 1 && length(x) == nrow(z)) {
      dx <- 0.5 * diff(x);
      x <- c(x[1L] - dx[1L], x[-length(x)] + dx, x[length(x)] +
             dx[length(x) - 1]);
   }
   if (length(y) > 1 && length(y) == ncol(z)) {
      dy <- 0.5 * diff(y);
      y <- c(y[1L] - dy[1L], y[-length(y)] + dy, y[length(y)] +
             dy[length(y) - 1]);
   }
   if (missing(breaks)) {
      nc <- length(col);
      if (!missing(zlim) && (any(!is.finite(zlim)) || diff(zlim) < 0)) {
         stop("invalid z limits");
      }
      if (diff(zlim) == 0) {
         if (zlim[1L] == 0) {
            zlim <- c(-1, 1);
         } else {
            zlim <- zlim[1L] + c(-0.4, 0.4) * abs(zlim[1L]);
         }
      }
      z <- (z - zlim[1L])/diff(zlim);
      if (oldstyle) {
         zi <- floor((nc - 1) * z + 0.5);
      } else {
         zi <- floor((nc - 1e-05) * z + 1e-07);
      }
      zi[zi < 0 | zi >= nc] <- NA;
   } else {
      if (length(breaks) != length(col) + 1) {
         stop("must have one more break than colour");
      }
      if (any(!is.finite(breaks))) {
         stop("breaks must all be finite");
      }
      if (R.version["major"] < 3) {
         zi <- .C("bincode", as.double(z), length(z), as.double(breaks),
            length(breaks), code=integer(length(z)), (TRUE),
            (TRUE), nok=TRUE, NAOK=TRUE, DUP=FALSE, PACKAGE="base")$code - 1;
      } else {
         zi <- .bincode(z, breaks, TRUE, TRUE) - 1L;
      }
   }
   if (!add) {
      plot(NA, NA, xlim=xlim, ylim=ylim, type="n", xaxs=xaxs,
         yaxs=yaxs, xaxt=xaxt, yaxt=yaxt, xlab=xlab, ylab=ylab, ...);
   }
   if (length(x) <= 1) {
      x <- par("usr")[1L:2];
   }
   if (length(y) <= 1) {
      y <- par("usr")[3:4];
   }
   if (length(x) != nrow(z) + 1 || length(y) != ncol(z) + 1) {
      stop("dimensions of z are not length(x)(-1) times length(y)(-1)");
   }
   check_irregular <- function(x, y) {
      dx <- diff(x);
      dy <- diff(y);
      (length(dx) && !isTRUE(all.equal(dx, rep(dx[1], length(dx))))) ||
         (length(dy) && !isTRUE(all.equal(dy, rep(dy[1], length(dy)))));
   }
   if (is.null(useRaster)) {
      useRaster <- getOption("preferRaster", FALSE);
      if (useRaster) {
         useRaster <- FALSE;
         ras <- dev.capabilities("raster");
         if (identical(ras, "yes")) {
            useRaster <- TRUE;
         }
         if (identical(ras, "non-missing")) {
            useRaster <- all(!is.na(zi));
         }
      }
   }
   if (verbose) {
      printDebug("imageDefault() useRaster: ", useRaster,
         fgText=c("orange", "lightgreen"));
   }
   if (useRaster && fixRasterRatio) {
      ## To try to deal with the raster function not handling the case of
      ## non-square data matrices, we'll try to make the data roughly square by
      ## duplicating some data
      ##
      ## First we'll only handle when there is more than 2:1 ratio. Everything
      ## else is close enough not to bother
      #if (class(x) %in% c("matrix")) {
      if (verbose) {
         printDebug("dim(z): ", dim(z), fgText=c("orange", "lightgreen"));
         printDebug("dim(zi): ", dim(zi), fgText=c("orange", "lightgreen"));
         printDebug("length(x): ", length(x), fgText=c("orange", "lightgreen"));
         printDebug("length(y): ", length(y), fgText=c("orange", "lightgreen"));
         printDebug("head(x): ", head(round(digits=2,x),20),
            fgText=c("orange", "lightgreen"));
         printDebug("tail(x): ", tail(round(digits=2,x),20),
            fgText=c("orange", "lightgreen"));
         printDebug("head(y): ", head(round(digits=2,y),20),
            fgText=c("orange", "lightgreen"));
         printDebug("tail(y): ", tail(round(digits=2,y),20),
            fgText=c("orange", "lightgreen"));
      }
      if (any(minRasterMultiple > 1) ||
          (length(y)-1) > 2*(length(x)-1) ||
          (length(x) > 2*length(y)) ) {
         dimRange <- range(c(length(x), length(y)));
         if (length(x) > 2*length(y)) {
            dupRowX <- floor((length(x)-1)/(length(y)-1));
            dupRowX <- min(c(dupRowX, maxRatioFix[1]));
            dupColX <- 1;
         } else {
            dupColX <- floor((length(y)-1)/(length(x)-1));
            dupColX <- min(c(dupColX, maxRatioFix[2]));
            dupRowX <- 1;
         }
         if (verbose) {
            printDebug("dupColX: ", dupColX, c("orange", "lightgreen"));
            printDebug("dupRowX: ", dupRowX, c("orange", "lightgreen"));
         }

         ## Ensure that minRasterMultiple is applied
         dupColX <- min(c(maxRatioFix[2],
            max(c(dupColX, minRasterMultiple[2]))));
         dupRowX <- min(c(maxRatioFix[1],
            max(c(dupRowX, minRasterMultiple[1]))));

         if (verbose) {
            printDebug("maxRatioFix:", maxRatioFix, c("orange", "lightgreen"));
            printDebug("dupColX: ", dupColX, c("orange", "lightgreen"));
            printDebug("dupRowX: ", dupRowX, c("orange", "lightgreen"));
         }
         newCols <- rep(1:(length(x)-1), each=dupColX);
         newRows <- rep(1:(length(y)-1), each=dupRowX);
         ## Column processing
         xNcolSeq <- seq(from=0.5, to=(length(x)-1)+0.5,
            length.out=length(newCols)+1);
         newColBreaks <- breaksByVector(newCols);
         newColLabels <- newColBreaks$newLabels;
         ## Row processing
         yNrowSeq <- seq(from=0.5, to=(length(y)-1)+0.5,
            length.out=length(newRows)+1);
         newRowBreaks <- breaksByVector(newRows);
         newRowLabels <- newRowBreaks$newLabels;
         dim(zi) <- dim(z);
         z <- z[newCols,,drop=FALSE];
         zi <- zi[newCols,,drop=FALSE];
         z <- z[,newRows,drop=FALSE];
         zi <- zi[,newRows,drop=FALSE];
         x <- xNcolSeq;
         y <- yNrowSeq;
      } else if (length(x) > 2*length(y)) {
         dupRowX <- floor((length(x)-1)/(length(y)-1));
         newRows <- rep(1:(length(y)-1), each=dupRowX);
         yNrowSeq <- seq(from=0.5, to=(length(y)-1)+0.5,
            length.out=length(newRows)+1);
         newRowBreaks <- breaksByVector(newRows);
         newRowLabels <- newRowBreaks$newLabels;
         dim(zi) <- dim(z);
         z <- z[,newRows,drop=FALSE];
         zi <- zi[,newRows,drop=FALSE];
         y <- yNrowSeq;
      }
   }
   if (useRaster) {
      if (check_irregular(x, y)) {
         stop("useRaster=TRUE can only be used with a regular grid");
      }
      if (!is.character(col)) {
         p <- palette();
         pl <- length(p);
         col <- as.integer(col);
         col[col < 1L] <- NA_integer_;
         col <- p[((col - 1L)%%pl) + 1L];
      }
      zc <- col[zi + 1L];
      dim(zc) <- dim(z);
      zc <- t(zc)[ncol(zc):1L, , drop=FALSE];
      rasterImage(as.raster(zc), min(x), min(y), max(x), max(y),
         interpolate=interpolate);
      invisible(zc);
   } else {
      .External.graphics(graphics:::C_image, x, y, zi, col);
      invisible(list(x=x, y=y, zi=zi, col=col));
   }
}

#' Display color raster image using a matrix of colors
#'
#' Display color raster image using a matrix of colors
#'
#' This function is similar to \code{\link{image}} except that
#' it takes a matrix which already has colors defined for each cell.
#' This function calls \code{\link{imageDefault}} which enables updated
#' use of the \code{useRaster} functionality.
#'
#' Additionally, if \code{cellnote} is supplied, which contains a matrix
#' of labels for the image cells, those labels will also be displayed.
#' By default, labels are grouped, so that only one label is displayed
#' whenever two or more labels appear in consecutive cells. This behavior
#' can be disabled with groupCellnotes=FALSE.
#'
#' The \code{groupCellnotes} behavior uses \code{\link{breaksByVector}} to
#' determine where to place consecutive labels, and it applies this logic
#' starting with rows, then columns. Note that labels are only grouped when
#' both the cell color and the cell label are identical for consecutive
#' cells.
#'
#' In general, if a large rectangular set of cells contains the same label,
#' and cell colors, the resulting label will be positioned in the
#' center. However, when the square is not symmetric, the label will be
#' grouped only where consecutive columns contain the same groups of
#' consecutive rows for a given label. In theory one could use polygon
#' functions from the \code{sp} or \code{rgeos} package to detect contiguous
#' polygons, and position the label in the center of those polygons.
#'
#' It is helpful to rotate labels partially to prevent overlaps, e.g.
#' srtCellnote=10 or srtCellnote=80.
#'
#' To do:
#' \itemize{
#'    \item{Detect the size of the area being labeled and determine whether
#'       to rotate the label sideways.}
#'    \item{Detect the size of the label, compared to its bounding box,
#'       and resize the label to fit the available space.}
#'    \item{Optionally draw border around contiguous colored and labeled
#'       polygons. Whether to draw border based only upon color, or color
#'       and label, or just label... it may get confusing.}
#'    \item{Label proper contiguous polygons based upon color and label,
#'       especially when color and label are present on multiple rows and
#'       columns, but not always the same columns per row.}
#' }
#'
#' @param x matrix or data.frame containing colors
#' @param useRaster logical sent to \code{\link{imageDefault}} to enable
#'    raster rendering, as opposed to polygon rendering. This parameter is
#'    highly recommended when the matrix is large (>50 columns or rows).
#' @param fixRasterRatio logical sent to \code{\link{imageDefault}}.
#' @param maxRatioFix numeric sent to \code{\link{imageDefault}}.
#' @param xaxt,yaxt character values compatible with \code{\link{par}()} to
#'    determine whether x- and y-axes are plotted. Set both to "n" to
#'    suppress display of axes.
#' @param doPlot logical whether to create a plot, or simply return data which
#'    would have been used to create the plot.
#' @param cellnote matrix or data.frame of labels to be displayed on the
#'    image. If groupCellnotes==TRUE labels will be placed in the center
#'    of consecutive cells with the same label and identical color.
#'    Currently, cell text is colored using \code{\link{setTextContrastColor}}
#'    which uses either white or black depending upon the brightness of
#'    the background color.
#' @param cexCellnote,srtCellnote,fontCellnote numeric vectors, with values
#'    applied to cellnote text to be compatible with par("cex"), par("srt"),
#'    and par("font"), respectively. If supplied a matrix or data.frame with
#'    it is used as-is or expanded to equivalent dimensions of \code{x}.
#'    If the vector is named by colnames(x) then it is applied
#'    by column in order, otherwise it is applied by row, with values recycled
#'    to the number of columns or rows, respectively. Note \code{cexCellnote}
#'    can also be a list, with the list elements being applied to individual
#'    cells in order. If the list is named by colnames(x), each list element
#'    is applied to values in each column, in order. In future this parameter
#'    may also accept a matrix of cex values as input. Final note: values are
#'    applied to each cell, but when cell labels are combined with
#'    groupCellnotes==TRUE, the value for the first matching cell is used.
#'    Remember that values are placed by coordinate, bottom-to-top on the
#'    y-axis, and left-to-right on the x-axis.
#' @param groupCellnotes logical whether to group labels where consecutive
#'    cells contain the same label and identical cell colors, thus only
#'    displaying one label in the center of these groups.
#' @param adjBy character value indicating how to apply adjustments for
#'    cexCellnote, srtCellnote, and fontCellnote, as described above.
#' @param verbose logical whether to print verbose output.
#' @param xpd NULLL or logical used for \code{par("xpd")} to define whether
#'    to crop displayed output to the plot area. If xpd=NULL then par("xpd")
#'    will not be modified, otherwise par("xpd"=xpd) will be defined while
#'    adding any cell notes, then reverted to its previous value afterward.
#'    This parameter is mainly useful when cellnote labels may overhang the
#'    plot space, and would be cropped and not visible if
#'    \code{par("xpd"=TRUE)}.
#' @param doTest logical whether to run a test showing basic features of
#'    \code{imageByColors}.
#'
#' @examples
#' a1 <- c("red","blue")[c(1,1,2)];
#' b1 <- c("yellow","orange")[c(1,2,2)];
#' c1 <- c("purple","orange")[c(1,1,2)];
#' d1 <- c("purple","green")[c(1,2,2)];
#' df1 <- data.frame(a=a1, b=b1, c=c1, d=d1);
#'
#' # default using polygons
#' imageByColors(df1, cellnote=df1);
#'
#' # useRaster=TRUE, edges are slightly blurred with small tables
#' imageByColors(df1, cellnote=df1, useRaster=TRUE);
#'
#' # some text features, rotation, font size, etc
#' imageByColors(df1, cellnote=df1, useRaster=TRUE, adjBy="column",
#'    cexCellnote=list(c(1.5,1.5,1), c(1,1.5), c(1.6,1.2), c(1.6,1.5)),
#'    srtCellnote=list(c(90,0,0), c(0,45), c(0,0,0), c(0,90,0)));
#' @export
imageByColors <- function
(x, useRaster=FALSE, fixRasterRatio=TRUE, maxRatioFix=100,
 xaxt="s", yaxt="s", doPlot=TRUE, cellnote=NULL, cexCellnote=1,
 srtCellnote=0, fontCellnote=1, groupCellnotes=TRUE, adjBy=c("column","row"),
 verbose=FALSE, xpd=NULL, doTest=FALSE,
 ...)
{
   ## Purpose is to take as input a matrix with color names
   ## and convert it for use by the image() function, which otherwise
   ## expects numerical values and an associated color ramp.
   ##
   ## if cellnote is provided, the text will be centered in each cell
   ##
   ## groupCellnotes=TRUE will label only one entry per group, if a
   ## series of cells have the same label.  Currently only supported for
   ## columns, not rows
   ##
   ## if xpd=NULL then par("xpd") will not be modified,
   ## otherwise par("xpd"=xpd) will be defined while
   ## adding any cell notes, then reverted to its previous value afterward.
   ## if xpd=FALSE then par("xpd") will be modified to par("xpd"=FALSE) while
   ## adding any cell notes, then reverted to its previous value afterward.
   ## The intent 
   ##
   ## adjBy allows adjusting the cellnote using srtCellnote, cexCellnote, fontCellnote
   ## either by row or by column, helpful when using the colors beside a heatmap.
   adjBy <- match.arg(adjBy);

   ## Optionally run a test demonstrating imageByColors()
   if (doTest) {
      a1 <- c("red4","blue")[c(1,1,2)];
      b1 <- c("yellow","orange")[c(1,2,2)];
      c1 <- c("purple","orange")[c(1,2,2)];
      d1 <- c("purple","green4")[c(1,2,2)];
      df1 <- data.frame(a=a1, b=b1, c=c1, d=d1);
      rownames(df1) <- 1:3;
      imageByColors(df1, cellnote=df1, adjBy="column", doRaster=TRUE,
         srtCellnote=list(c(90,0,0), c(0,0), c(0,0), c(0,90)),
         cexCellnote=list(c(1.5,1.5,1), c(1,1.5), c(1.6,1.2), c(1.6,1.5)),
         fontCellnote=list(c(1,1,2), c(1,2), c(1,2), c(2,2)),
         maxRatioFix=maxRatioFix,
         verbose=verbose, ...);
   }

   ##
   cellnoteX <- NULL;
   srtCellnoteDF <- NULL;
   if (class(x) %in% c("data.frame")) {
      x <- as.matrix(x);
   }
   xNcolSeq <- 1:ncol(x);
   xNrowSeq <- 1:nrow(x);

   ## Fix blank colors, sent in various forms, all of which we convert to
   ## "transparent"
   blankMatch <- c(NA, "NA", "...", "", "blank", "empty", "-");
   if (any(x %in% blankMatch)) {
      if (verbose) {
         printDebug("Blank values changed to transparent:",
            formatInt(sum(x %in% blankMatch)));
      }
      x[x %in% c(NA, "NA", "...", "", "blank", "empty", "-")] <- "transparent";
   }

   if (1 == 2 && useRaster && fixRasterRatio) {
      ## To try to deal with the raster function not handling the case of
      ## non-square data matrices, we'll try to make the data roughly square by
      ## duplicating some data
      ##
      ## First we'll only handle when there is more than 2:1 ratio. Everything
      ## else is close enough not to bother
      if (nrow(x) > 2*ncol(x)) {
         dupColX <- floor(nrow(x)/ncol(x));
         dupColX <- min(c(dupColX, maxRatioFix));
         newCols <- rep(1:ncol(x), each=dupColX);
         xNcolSeq <- seq(from=0.5, to=ncol(x)+0.5, length.out=length(newCols));
         newColBreaks <- breaksByVector(newCols);
         newColLabels <- newColBreaks$newLabels;
         x <- x[,newCols,drop=FALSE];
      }
   }
   xFac <- as.factor(x);
   xFacM <- matrix(data=as.numeric(xFac), ncol=ncol(x), dimnames=dimnames(x));
   if (doPlot) {
      imageDefault(x=xNcolSeq, y=xNrowSeq, z=t(xFacM),
            col=levels(xFac), xaxt="n", yaxt="n", oldstyle=TRUE,
            useRaster=useRaster,
            xlab="", ylab="", axes=FALSE,
            fixRasterRatio=fixRasterRatio,
            maxRatioFix=maxRatioFix,
            verbose=verbose,
            ...);
      #if (!xaxt %in% "n") {
      #   axis(1, las=2, cex.axis=0.6);
      #}
      #if (!yaxt %in% "n") {
      #   axis(2, las=2, cex.axis=0.6);
      #}
   }
   ## Optionally add labels to the cells
   if (!is.null(cellnote)) {
      if (class(cellnote) %in% c("data.frame")) {
         cellnote <- as.matrix(cellnote);
      }
      cellnoteY <- matrix(ncol=ncol(cellnote), nrow=nrow(cellnote),
         dimnames=dimnames(cellnote), rep(1:nrow(cellnote), ncol(cellnote)));
      cellnoteX <- matrix(ncol=ncol(cellnote), nrow=nrow(cellnote),
         dimnames=dimnames(cellnote), rep(1:ncol(cellnote), each=nrow(cellnote)));
      cellnote1 <- cellnote;
      xrow <- 1;
      xcol <- 1;
      xrowfac <- 1;
      xcolfac <- 1;
      if (verbose) {
         printDebug("dim(cellnote):", dim(cellnote));
      }
      if (groupCellnotes) {
         if (nrow(x) > 1) {
            xrow <- rep(1:nrow(cellnote), each=2);
            xrowfac <- 2;
         }
         if (ncol(x) > 1) {
            xcol <- rep(1:ncol(cellnote), each=2);
            xcolfac <- 2;
         }
         if (verbose) {
            printDebug("   xrow:", xrow);
            printDebug("   xcol:", xcol);
         }
         cellnote <- cellnote[xrow,xcol,drop=FALSE];
         x1 <- x[xrow,xcol,drop=FALSE];
         if (nrow(cellnote) > 1) {
            cellnoteL <- apply(cellnote, 2, function(i){
               i1 <- rmNA(i, naValue="")
               cellnoteXi <- 1:nrow(cellnote);
               if (length(unique(i1)) == 1) {
                  cellnoteXi[trunc(nrow(cellnote)/2+0.5)] <- nrow(cellnote)/2+0.5;
                  cellnoteVi <- rep("",nrow(cellnote));
                  cellnoteVi[trunc(nrow(cellnote)/2+0.5)] <- unique(i1);
               } else {
                  bbv1 <- breaksByVector(i1, returnFractions=TRUE);
                  cellnoteXi[trunc(bbv1$labelPoints)] <- bbv1$labelPoints;
                  cellnoteVi <- bbv1$newLabels;
               }
               list(cellnoteXi=cellnoteXi, cellnoteVi=cellnoteVi);
            });
            cellnoteY <- do.call(cbind, lapply(cellnoteL, function(i){
               i$cellnoteXi;
            }));
            cellnote <- do.call(cbind, lapply(cellnoteL, function(i){
               i$cellnoteVi;
            }));
         } else {
            cellnoteY <- matrix(ncol=ncol(cellnote), nrow=nrow(cellnote),
               dimnames=dimnames(cellnote), rep(1:nrow(cellnote), ncol(cellnote)));
         }
         if (ncol(cellnote) > 1) {
            cellnoteL <- apply(cellnote, 1, function(i){
               i1 <- rmNA(i, naValue="")
               cellnoteXi <- 1:ncol(cellnote);
               if (length(unique(i1)) == 1) {
                  cellnoteXi[trunc(ncol(cellnote)/2+0.5)] <- ncol(cellnote)/2+0.5;
                  cellnoteVi <- rep("",ncol(cellnote));
                  cellnoteVi[trunc(ncol(cellnote)/2+0.5)] <- unique(i1);
               } else {
                  bbv1 <- breaksByVector(i1, returnFractions=TRUE);
                  cellnoteXi[trunc(bbv1$labelPoints)] <- bbv1$labelPoints;
                  cellnoteVi <- bbv1$newLabels;
               }
               list(cellnoteXi=cellnoteXi, cellnoteVi=cellnoteVi);
            });
            cellnoteX <- rbindList(lapply(cellnoteL, function(i){
               i$cellnoteXi;
            }));
            cellnote <- rbindList(lapply(cellnoteL, function(i){
               i$cellnoteVi;
            }));
         } else {
            cellnoteX <- matrix(ncol=ncol(cellnote), nrow=nrow(cellnote),
               dimnames=dimnames(cellnote),
               rep(1:ncol(cellnote), each=nrow(cellnote)));
            if (verbose) {
               printDebug("cellnoteX:");
               print(head(cellnoteX));
            }
         }
      } else {
         x1 <- x;
      }
      cellWhich <- which(!cellnote %in% c(NA, ""));
      if (xcolfac > 1) {
         cellX <- (as.vector(cellnoteX)[cellWhich]+0.5)/xcolfac;
      } else {
         cellX <- as.vector(cellnoteX)[cellWhich];
      }
      if (verbose) {
         printDebug("xcolfac:", xcolfac,
            ",\ncellX:", cellX,
            ",\nas.vector(cellnoteX):", as.vector(cellnoteX),
            ",\ncellWhich:", cellWhich,
            ",\nclass(cellnote):", class(cellnote));
         print(cellnote);
      }
      if (xrowfac > 1) {
         cellY <- (as.vector(cellnoteY)[cellWhich]+0.5)/xrowfac;
      } else {
         cellY <- as.vector(cellnoteY)[cellWhich];
      }
      celltext <- as.vector(cellnote)[cellWhich];
      cellColor <- as.vector(x1)[cellWhich];

      prepMatrixParam <- function(x, param, ...) {
         ## Purpose is to take a vector or list as input, and
         ## return a vector sufficient to cover the matrix x.
         ##
         ## If given a vector, with length=ncol(x)*nrow(x),
         ## its values are returned as-is, to be applied to the matrix
         ## by column.
         ## Otherwise if given a vector, its values are extended to ncol(x)
         ## then repeated within each column.
         ## If given a list, the list is repeated to ncol(x), then
         ## each list element is repeated to nrow(x) as needed.
         ##
         if (igrepHas("matrix|data.frame", class(param))) {
            paramX <- rep(seq_len(ncol(param)), length.out=ncol(x));
            paramY <- rep(seq_len(nrow(param)), length.out=nrow(x));
            param <- param[paramY,paramX,drop=FALSE];
            dimnames(param) <- dimnames(x);
            return(param);
         }
         if (length(param) != prod(ncol(x), nrow(x))) {
            param <- rep(param, length.out=ncol(x));
            if (!igrepHas("list", class(param))) {
               param <- as.list(param);
            }
            if (!all(lengths(param) == nrow(x))) {
               param <- lapply(param, function(i){
                  rep(i, length.out=nrow(x));
               });
            }
         }
         param <- matrix(ncol=ncol(x), nrow=nrow(x), unlist(param),
            dimnames=dimnames(x));
         return(param);
      }
      if (!is.null(names(cexCellnote)) &&
          all(names(cexCellnote) %in% colnames(x))) {
         adjBy <- "column";
      } else if (!is.null(names(cexCellnote)) &&
          all(names(cexCellnote) %in% rownames(x))) {
         adjBy <- "row";
      }

      if (adjBy %in% "column") {
         cexCellnote <- prepMatrixParam(x, cexCellnote);
         fontCellnote <- prepMatrixParam(x, fontCellnote);
         srtCellnote <- prepMatrixParam(x, srtCellnote);
      } else {
         cexCellnote <- t(prepMatrixParam(t(x), cexCellnote));
         fontCellnote <- t(prepMatrixParam(t(x), fontCellnote));
         srtCellnote <- t(prepMatrixParam(t(x), srtCellnote));
      }
      if (verbose) {
         printDebug("head(cexCellnote):");
         print(head(cexCellnote));
      }

      ## paramCell is the cell for which the parameter is applied, by column,
      ## then by row.
      ## Note that for labels spanning two rows or columns, it simply takes
      ## the first parameter.
      paramCell <- floor(cellX)*nrow(x)-(nrow(x)) + floor(cellY);
      if (verbose) {
         printDebug("head(cellX):");print(head(cellX));
         printDebug("head(paramCell):");print(head(paramCell));
      }
      srtCellnoteDF <- data.frame(cellX=cellX, cellY=cellY, celltext=celltext,
         cexCellnote=cexCellnote[paramCell],
         srtCellnote=srtCellnote[paramCell],
         fontCellnote=fontCellnote[paramCell],
         paramCell=paramCell,
         cellColor=cellColor);
      if (verbose) {
         printDebug("srtCellnoteDF:");
         print(head(srtCellnoteDF));
      }
      if (!is.null(xpd)) {
         parXpd <- par("xpd");
         par("xpd"=xpd);
      }
      ## srt can be set only once per text() call, so we must loop through
      ## each srtCellnote value
      ## Text can only be customized once per text() call, so for every combination of
      ## srt, font, and cex, we must run this function again
      srtCellnoteDF[,"textKey"] <- pasteByRow(srtCellnoteDF[,c("cexCellnote",
         "srtCellnote", "fontCellnote")], sep="_");
      if (doPlot) {
         text1 <- tapply(seq_len(nrow(srtCellnoteDF)),
            srtCellnoteDF[,"textKey"], function(iRow){
            if (verbose) {
               printDebug("textKey:", head(srtCellnoteDF[iRow,"textKey"], 1),
                  ", iRow:", head(iRow));
            }
            text(x=srtCellnoteDF[iRow,"cellX"],
               y=srtCellnoteDF[iRow,"cellY"],
               labels=srtCellnoteDF[iRow,"celltext"],
               pos=NULL,
               cex=head(srtCellnoteDF[iRow,"cexCellnote"],1),
               srt=head(srtCellnoteDF[iRow,"srtCellnote"],1),
               font=head(srtCellnoteDF[iRow,"fontCellnote"],1),
               col=setTextContrastColor(srtCellnoteDF[iRow,"cellColor"],
                  keepAlpha=TRUE, useGrey=18, ...),
               adj=c(0.5,0.5));
         });
      }
      if (!is.null(xpd)) {
         par("xpd"=parXpd);
      }
   }
   ## Print column and row labels if defined
   if (doPlot) {
      if (!xaxt %in% "n" && !is.null(colnames(x))) {
         axis(1, las=2, at=1:ncol(x), labels=colnames(x), ...);
      }
      if (!yaxt %in% "n" && !is.null(rownames(x))) {
         axis(2, las=2, at=1:nrow(x), labels=rownames(x), ...);
      }
      box();
   }
   invisible(list(x=xNcolSeq, y=xNrowSeq, z=t(xFacM), col=levels(xFac),
      cellnoteX=cellnoteX, srtCellnoteDF=srtCellnoteDF,
      cexCellnote=cexCellnote));
}

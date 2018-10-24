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
#'    \code{viridis} package, or single R colors, or
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
#' @param add logical whether to add to an existing active R plot, or create
#'    a new plot window.
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
#'
#' @examples
#' # doTest=TRUE invisibly returns the test data
#' x <- plotSmoothScatter(doTest=TRUE);
#'
#' # so it can be plotted again with different settings
#' plotSmoothScatter(x, colramp="viridis");
#'
#' @export
plotSmoothScatter <- function
(x,
 y=NULL,
 bandwidthN=300,
 transformation=function(x)x^0.25,
 xlim=NULL,
 ylim=NULL,
 nbin=256,
 nrpoints=0,
 colramp=c("white", "lightblue", "blue", "orange", "orangered2"),
 doTest=FALSE,
 fillBackground=TRUE,
 naAction=c("remove", "floor0", "floor1"),
 xaxt="s",
 yaxt="s",
 add=FALSE,
 applyRangeCeiling=TRUE,
 useRaster=TRUE,
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
      ## create somewhat noisy correlation data
      n <- 20000;
      x <- matrix(ncol=2, data=rnorm(n*2));
      x[,2] <- x[,1] + rnorm(n)*0.1;

      ## Add secondary line offset from the main correlation
      ## using 5% the original data
      xSample <- sample(1:nrow(x), floor(nrow(x)*0.05));
      xSub <- t(t(x[xSample,,drop=FALSE])+c(0.6,-0.7));
      ## Add more noise to a subset of data
      n1 <- 3000;
      x2 <- rbind(x, xSub);
      n2 <- sample(seq_len(nrow(x2)), n1);
      x2[n2,2] <- x2[n2,1] + rnorm(n1) * 0.6;
      oPar <- par(no.readonly=TRUE);
      par("mfrow"=c(2,2), "mar"=c(2,3,4,1));
      smoothScatter(x2, #col=getColorRamp("Blues"),
         main="smoothScatter default", ylab="", xlab="");
      plotSmoothScatter(x2, colramp=getColorRamp("Blues"),
         main="plotSmoothScatter, Blues");
      plotSmoothScatter(x2, colramp=colramp,
         main="plotSmoothScatter");
      plotSmoothScatter(x2, colramp=colramp, bandwidthN=600,
         main="plotSmoothScatter higher bandwidth");
      par(oPar);
      invisible(x2);
   } else {

      if (!is.null(y) && is.numeric(y) && is.numeric(x)) {
         x <- matrix(ncol=2, c(x, y));
      }
      ## Handle data.frame, matrix
      if (is.matrix(x) && ncol(x) >= 2) {
         y <- x[,2];
         x <- x[,1];
      } else if (any(class(x) %in% c("data.frame","DataFrame","tbl")) &&
         ncol(x) >= 2) {
         y <- x[[2]];
         x <- x[[1]];
      } else if (is.null(y) && (is.matrix(x) || is.data.frame(x))) {
         stop("Cannot handle matrix input x, when y is NULL.");
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
      ## Adjust for uneven plot aspect ratio, by using the plot par("pin")
      ## which contains the actual dimensions.
      ## Note that it does not require the actual coordinates of the plot,
      ## just the relative size of the display
      pin1 <- par("pin")[1] / par("pin")[2];
      bandwidthXY <- c(diff(xlim4)/bandwidthN[1], diff(ylim4)/bandwidthN[2]*pin1);
      if (fillBackground) {
         nullPlot(doBoxes=FALSE, doUsrBox=TRUE, fill=head(colramp(11),1),
            xaxs="i", yaxs="i", xaxt="n", yaxt="n",
            xlim=xlim4, ylim=ylim4, add=add, ...);
         axis(1, las=2, xaxt=xaxt);
         axis(2, las=2, yaxt=yaxt);
         smoothScatterJam(x=x,
            y=y,
            add=TRUE,
            transformation=transformation,
            bandwidth=bandwidthXY,
            nbin=nbin,
            nrpoints=nrpoints,
            xlim=xlim4,
            ylim=ylim4,
            xaxs="i",
            yaxs="i",
            xaxt="n",
            yaxt="n",
            colramp=colramp,
            useRaster=useRaster,
            ...);
      } else {
         smoothScatterJam(x=x,
            y=y,
            transformation=transformation,
            bandwidth=bandwidthXY,
            nbin=nbin,
            nrpoints=nrpoints,
            xlim=xlim4,
            ylim=ylim4,
            xaxs="i",
            yaxs="i",
            xaxt=xaxt,
            yaxt=yaxt,
            colramp=colramp,
            useRaster=useRaster,
            ...);
      }
      invisible(list(x=x,
         y=y,
         transformation=transformation,
         bandwidth=bandwidthXY,
         nbin=nbin,
         xlim=xlim4,
         ylim=ylim4,
         xaxs="i",
         yaxs="i",
         xaxt=xaxt,
         yaxt=yaxt,
         colramp=colramp));
   }
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
(x,
 y=NULL,
 nbin=128,
 bandwidth,
 colramp=colorRampPalette(c("white", blues9)),
 nrpoints=100,
 pch=".",
 cex=1,
 col="black",
 transformation=function(x) x^0.25,
 postPlotHook=box,
 xlab=NULL,
 ylab=NULL,
 xlim,
 ylim,
 add=FALSE,
 xaxs=par("xaxs"),
 yaxs=par("yaxs"),
 xaxt=par("xaxt"),
 yaxt=par("yaxt"),
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
   imageDefault(xm, ym, z=dens, col=colramp(256), xlab=xlab, add=add,
      ylab=ylab, xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs, xaxt=xaxt, yaxt=yaxt,
      useRaster=useRaster, ...);
   imageL <- list(xm=xm, ym=ym, z=dens, col=colramp(256), xlab=xlab, add=add,
      ylab=ylab, xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs, xaxt=xaxt, yaxt=yaxt);
   if (length(postPlotHook) > 0) {
      if (igrepHas("function", class(postPlotHook))) {
         postPlotHook(...);
      } else {
         postPlotHook;
      }
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
#' \code{\link[graphics]{par}} settings for margins, text size, etc. By default
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
#' @param add logical whether to add to an existing active R plot, or create
#'    a new plot window.
#' @examples
#' nullPlot()
#'
#' @export
nullPlot <- function
(xaxt="n",
 yaxt="n",
 xlab="",
 ylab="",
 col="transparent",
 xlim=c(1,2),
 ylim=c(1,2),
 las=par("las"),
 doBoxes=TRUE,
 doUsrBox=doBoxes,
 fill="#FFFF9966",
 doAxes=FALSE,
 doMargins=TRUE,
 plotAreaTitle="Plot Area",
 plotSrt=0,
 plotNumPrefix="",
 bty="n",
 showMarginsOnly=FALSE,
 add=FALSE,
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
      points(range(xlim),
         range(ylim),
         xaxt=xaxt,
         yaxt=yaxt,
         col=col,
         xlab=xlab,
         ylab=ylab,
         xlim=xlim,
         ylim=ylim,
         bty=bty,
         add=TRUE,
         ...);
   } else {
      if (!add) {
         plot(range(xlim),
            range(ylim),
            xaxt=xaxt,
            yaxt=yaxt,
            col=col,
            xlab=xlab,
            ylab=ylab,
            xlim=xlim,
            ylim=ylim,
            bty=bty,
            #add=add,
            ...);
      }
   }

   if (doUsrBox) {
      usrBox(fill=fill,
         ...);
   }
   if (doBoxes) {
      box("plot",
         col="darkred");

      box("figure",
         lty="dashed",
         col="navy");

      ## Print margins
      if (doMargins) {
         Margins <- capture.output(par()$mar);
         Margins <- substr(Margins, 5, nchar(Margins));
         MarginsN <- as.numeric(unlist(strsplit(Margins, "[ ]+")));
         MarginsV <- format(MarginsN,
            nsmall=0,
            scientific=FALSE,
            digits=2);
         Margins <- paste0("  mar=c(", paste(MarginsV, collapse=","), ")",
            plotNumPrefix);
         if (plotSrt == 90) {
            plotLas <- 2;
         } else {
            plotLas <- 1;
         }
         if (par("mar")[3] == 0) {
            mtext(Margins,
               NORTH<-3,
               line=-1,
               cex=0.7,
               col="navy",
               las=plotLas,
               adj=plotLas-1);
         } else {
            mtext(Margins,
               NORTH<-3,
               line=1,
               cex=0.7,
               col="navy",
               las=plotLas,
               adj=plotLas-1);
         }

         box("inner", lty="dotted", col="darkgreen");
         if (any(par("oma") > 0)) {
            mtext("Outer Margin Area",
               SOUTH<-1,
               line=0.4,
               adj=1.0,
               cex=1.5,
               col="darkgreen",
               outer=TRUE,
               las=plotLas);
         }
         box("outer", lty="solid", col="darkgreen");

         ## Text: vector of strings in mtext call
         lapply(1:4, function(i){
            if (par("mar")[i] > 0) {
               newLas <- as.integer(3 - i%%2*2);
               par("las"=newLas);
               mtext(paste0("mar[", i, "]",
                     plotNumPrefix, "=",
                     format(digits=2, nsmall=0, scientific=FALSE,
                        par("mar")[i])),
                  side=i,
                  line=0.4,
                  cex=0.6,
                  col="navy",
                  outer=FALSE,
                  las=newLas);
            }
         });
         par("las"=1);
         lapply(1:4, function(i){
            if (par("oma")[i] > 0) {
               newLas <- as.integer(3 - i%%2*2);
               par("las"=newLas);
               mtext(paste0("oma[", i, "]",
                     plotNumPrefix, "=",
                     format(digits=2, nsmall=0, scientific=FALSE,
                        par("oma")[i])),
                  i,
                  line=0.4,
                  cex=0.6,
                  col="navy",
                  outer=TRUE,
                  las=newLas);
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
#'    different area, though using the \code{\link[graphics]{rect}} function
#'    directly seems more appropriate.
#' @param debug logical whether to print the parUsr value being used.
#' @examples
#' # usrBox() requires that a plot device is already open
#' nullPlot(doBoxes=FALSE);
#' usrBox();
#'
#' @export
usrBox <- function
(fill="#FFFF9966",
 label=NULL,
 parUsr=par("usr"),
 debug=FALSE,
 add=FALSE,
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
#' known in the default \code{\link[graphics]{image}} function as \code{oldstyle=TRUE}.
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
 z,
 zlim=range(z[is.finite(z)]),
 xlim=range(x),
 ylim=range(y),
 col=heat.colors(12),
 add=FALSE,
 xaxs="i",
 yaxs="i",
 xaxt="n",
 yaxt="n",
 xlab,
 ylab,
 breaks,
 flip=c("none","x","y","xy"),
 oldstyle=TRUE,
 useRaster=NULL,
 fixRasterRatio=TRUE,
 maxRatioFix=100,
 minRasterMultiple=NULL,
 rasterTarget=200,
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
   flip <- match.arg(flip);
   if (interpolate && is.null(useRaster)) {
      useRaster <- TRUE;
   }
   if (!is.null(rasterTarget)) {
      rasterTarget <- rep(rasterTarget, length.out=2);
   }
   if (length(minRasterMultiple) == 0) {
      if (!is.null(rasterTarget)) {
         minRasterMultiple <- c(ceiling(rasterTarget[1]/ncol(z)),
            ceiling(rasterTarget[2]/nrow(z)));
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
      if (verbose) {
         printDebug("imageDefault(): ",
            "missing(z)");
      }
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
      if (verbose) {
         printDebug("imageDefault(): ",
            c("!missing(z),","is.list(x)"));
      }
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
      if (verbose) {
         printDebug("imageDefault(): ",
            c("!missing(z),","!is.list(x)"));
      }
      if (missing(xlab))
         if (missing(x)) {
            xlab <- "";
         } else {
            xlab <- deparse(substitute(x));
         }
      if (missing(ylab)) {
         if (missing(y)) {
            ylab <- "";
         } else {
            ylab <- deparse(substitute(y));
         }
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
      if (verbose) {
         printDebug("imageDefault(): ",
            "preparing to redefine x using diff(x)/2",
            ", length(x):",
            length(x));
      }
      x <- c(
         x[1] - dx[1],
         x[-length(x)] + dx,
         x[length(x)] + dx[length(x) - 1]
      );
      if (verbose) {
         printDebug("imageDefault(): ",
            "redefined x using diff(x)/2",
            ", length(x):",
            length(x));
      }
   }
   if (length(y) > 1 && length(y) == ncol(z)) {
      dy <- 0.5 * diff(y);
      y <- c(
         y[1] - dy[1],
         y[-length(y)] + dy,
         y[length(y)] + dy[length(y) - 1]
      );
   }
   if (missing(breaks)) {
      nc <- length(col);
      if (!missing(zlim) && (any(!is.finite(zlim)) || diff(zlim) < 0)) {
         stop("invalid z limits");
      }
      if (diff(zlim) == 0) {
         if (zlim[1] == 0) {
            zlim <- c(-1, 1);
         } else {
            zlim <- zlim[1] + c(-0.4, 0.4) * abs(zlim[1]);
         }
      }
      z <- (z - zlim[1])/diff(zlim);
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
   if (igrepHas("y", flip)) {
      ylim <- rev(ylim);
   }
   if (igrepHas("x", flip)) {
      xlim <- rev(xlim);
   }
   if (verbose) {
      printDebug("imageDefault(): ",
         "xlim:",
         xlim);
      printDebug("imageDefault(): ",
         "ylim:",
         ylim);
   }
   if (!add) {
      plot(NA,
         NA,
         xlim=xlim,
         ylim=ylim,
         type="n",
         xaxs=xaxs,
         yaxs=yaxs,
         xaxt=xaxt,
         yaxt=yaxt,
         xlab=xlab,
         ylab=ylab,
         ...);
   }
   if (length(x) <= 1) {
      x <- par("usr")[1:2];
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
      (
         length(dx) &&
         !isTRUE(all.equal(dx, rep(dx[1], length(dx))))
      ) || (
         length(dy) &&
         !isTRUE(all.equal(dy, rep(dy[1], length(dy))))
      );
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
      printDebug("imageDefault(): ",
         "useRaster: ",
         useRaster);
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
         printDebug("imageDefault(): ",
            "dim(z): ", dim(z));
         printDebug("imageDefault(): ",
            "dim(zi): ", dim(zi));
         printDebug("imageDefault(): ",
            "length(x): ", length(x));
         printDebug("imageDefault(): ",
            "length(y): ", length(y));
         printDebug("imageDefault(): ",
            "head(x): ", head(round(digits=2,x),20));
         printDebug("imageDefault(): ",
            "tail(x): ", tail(round(digits=2,x),20));
         printDebug("imageDefault(): ",
            "head(y): ", head(round(digits=2,y),20));
         printDebug("imageDefault(): ",
            "tail(y): ", tail(round(digits=2,y),20));
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
#' This function is similar to \code{\link[graphics]{image}} except that
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
#' @param xaxt,yaxt character values compatible with \code{\link[graphics]{par}} to
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
#' @param keepTextAlpha logical passed to \code{\link{setTextContrastColor}}
#'    indicating whether the text label color should inherit the alpha
#'    transparency from the background color. If TRUE then fully transparent
#'    background colors will not have a visible label.
#'
#' @examples
#' a1 <- c("red4","blue")[c(1,1,2)];
#' b1 <- c("yellow","orange")[c(1,2,2)];
#' c1 <- c("purple","orange")[c(1,2,2)];
#' d1 <- c("purple","green4")[c(1,2,2)];
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
(x,
 useRaster=FALSE,
 fixRasterRatio=TRUE,
 maxRatioFix=100,
 xaxt="s",
 yaxt="s",
 doPlot=TRUE,
 cellnote=NULL,
 cexCellnote=1,
 srtCellnote=0,
 fontCellnote=1,
 groupCellnotes=TRUE,
 adjBy=c("column","row"),
 verbose=FALSE,
 xpd=NULL,
 flip=c("none","y","x","xy"),
 keepTextAlpha=FALSE,
 doTest=FALSE,
 add=FALSE,
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
   flip <- match.arg(flip);

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

   xFac <- as.factor(x);
   xFacM <- matrix(data=as.numeric(xFac), ncol=ncol(x), dimnames=dimnames(x));
   if (doPlot) {
      imageDefault(x=xNcolSeq, y=xNrowSeq, z=t(xFacM),
         col=levels(xFac),
         xaxt="n", yaxt="n", oldstyle=TRUE,
         useRaster=useRaster,
         xlab="", ylab="", axes=FALSE,
         flip=flip,
         fixRasterRatio=fixRasterRatio,
         maxRatioFix=maxRatioFix,
         verbose=verbose,
         ...);
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
      srtCellnoteDF <- data.frame(cellX=cellX,
         cellY=cellY,
         celltext=celltext,
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
                  keepAlpha=keepTextAlpha,
                  useGrey=18,
                  ...),
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
   invisible(list(x=xNcolSeq,
      y=xNrowSeq,
      z=t(xFacM),
      col=levels(xFac),
      cellnoteX=cellnoteX,
      srtCellnoteDF=srtCellnoteDF,
      cexCellnote=cexCellnote));
}

#' Draw text with shadow border
#'
#' Draw text with shadow border
#'
#' Draws text with the same syntax as \code{graphics::text()} except that
#' this function adds a contrasting color border around the text, which
#' helps visibility when the background color is either not known, or is
#' not expected to be a fixed contrasting color.
#'
#' The function draws the label n times with the chosed background
#' color, then the label itself atop the background text. It does not
#' typically have a noticeable effect on rendering time, but it may
#' impact downstream uses in vector file formats like SVG and PDF, where
#' text is stored as proper text and font objects. Take care when editing
#' text that the underlying shadow text is also edited in sync.
#'
#' The parameter \code{doTest=TRUE} will display a visual example. The
#' background color can be modified with \code{fill="navy"} for example.
#'
#'
#' @param x,y numeric coordinates, either as vectors x and y, or x as a
#' two-color matrix recognized by \code{\link[grDevices]{xy.coords}}.
#' @param labels vector of labels to display at the corresponding xy
#'    coordinates.
#' @param col,bg,shadowColor the label color, and background (outline) color,
#'    and shadow color (if \code{shadow=TRUE}), for each
#'    element in \code{labels}. Colors are applied in order, and recycled to
#'    \code{length(labels)} as needed. By default \code{bg} will choose
#'    a contrasting color, based upon \code{\link{setTextContrastColor}}.
#'    Also by default, the shadow is "black" true to its name, since it is
#'    expected to darken the area around it.
#' @param r the outline radius, expressed as a fraction of the width of the
#'    character "A" as returned by \code{\link[graphics]{strwidth}}.
#' @param offset the outline offset position in xy coordinates, expressed
#'    as a fraction of the width of the character "A" as returned by
#'    \code{\link[graphics]{strwidth}}, and \code{\link[graphics]{strheight}},
#'    respectively.
#'    The offset is only applied when \code{shadow=TRUE} to enable the shadow
#'    effect.
#' @param n the number of steps around the label used to create the outline.
#'    A higher number may be useful for very large font sizes, otherwise 8
#'    is a reasonably good balance between detail and the number of labels
#'    added.
#' @param outline logical whether to enable outline drawing.
#' @param shadow logical whether to enable shadow drawing.
#' @param alphaOutline,alphaShadow alpha transparency to use for the outline
#'    and shadow colors, respectively.
#' @param doTest logical whether to create a visual example of output. Note
#'    that it calls \code{\link{usrBox}} to color the plot area, and the
#'    background can be overridden with something like \code{fill="navy"}.
#' @param ... other parameters are passed to \code{\link[graphics]{text}}.
#'    Note that certain parameters are not vectorized in that function,
#'    such as \code{srt} which requires only a fixed value. To rotate each
#'    label independently, multiple calls to \code{\link[graphics]{text}} or
#'    \code{\link{shadowText}} must be made. Other parameters like \code{adj}
#'    only accept up to two values, and those two values affect all label
#'    positioning.
#'
#' @examples
#' shadowText(doTest=TRUE);
#' shadowText(doTest=TRUE, fill="navy");
#' shadowText(doTest=TRUE, fill="red4");
#'
#' @export
shadowText <- function
(x, y=NULL, labels=NULL,
 col="white", bg=setTextContrastColor(col),
 r=0.1, offset=c(0.15, -0.15), n=8,
 outline=TRUE, alphaOutline=0.9,
 shadow=FALSE, shadowColor="black", alphaShadow=0.2,
 cex=par("cex"), font=par("font"),
 doTest=FALSE,
 ...)
{
   ## Purpose is to draw text with a border around it to help
   ## make text more visible even with light and dark features
   ## beneath the text.
   ## It can be used by overriding the text function prior to
   ## running plot functions, e.g.
   ## 'text<-shadowText'.
   ## Then reset to the default text function afterwards, e.g.
   ## 'text<-graphics::text'
   ##
   ## doTest=TRUE will display an example

   #cex <- rep(cex, length.out=length(labels));
   #font <- rep(font, length.out=length(labels));
   #srt <- rep(srt, length.out=length(labels));

   if (doTest) {
      ## Example shadow text
      nullPlot(xlim=c(1,9), ylim=c(0,10), doBoxes=FALSE,
         doUsrBox=TRUE, ...);
      if (length(col) == 1 && col == "white") {
         col <- c("white", "white", "yellow", "green4", "red4", "blue");
         bg <- setTextContrastColor(col);
      }
      if (length(labels) == 0) {
         labels <- LETTERS[1:12];
      } else {
         labels <- rep(labels, length.out=12);
      }
      st1 <- shadowText(x=rep(2,9), y=9:1,
         labels=c("outline=FALSE","shadow=FALSE",labels[1:7]),
         outline=FALSE, shadow=FALSE,
         col=col,
         bg=bg,
         cex=c(1,1,1,1,1,1,1,1,1),
         offset=offset, n=n, r=r,
         doTest=FALSE);
      st2 <- shadowText(x=rep(4,9), y=9:1-0.3,
         labels=c("outline=TRUE","shadow=FALSE",labels[1:7]),
         outline=TRUE, shadow=FALSE,
         col=col,
         bg=bg,
         cex=c(1.1,1,1,1,1,1,1,1,1),
         offset=offset, n=n, r=r,
         doTest=FALSE);
      st3 <- shadowText(x=rep(6,9), y=9:1,
         labels=c("outline=FALSE","shadow=TRUE",labels[1:7]),
         outline=FALSE, shadow=TRUE,
         col=col,
         bg=bg,
         cex=c(1,1.1,1,1,1,1,1,1,1),
         offset=offset, n=n, r=r,
         doTest=FALSE);
      st4 <- shadowText(x=rep(8,9), y=9:1-0.3,
         labels=c("outline=TRUE","shadow=TRUE",labels[1:7]),
         outline=TRUE, shadow=TRUE,
         col=col,
         bg=bg,
         cex=c(1.1,1.1,1,1,1,1,1,1,1),
         offset=offset, n=n, r=r,
         doTest=FALSE);
      invisible(list(st1=st1, st2=st2, st3=st3));
   } else {
      cex <- rep(cex, length.out=length(labels));
      font <- rep(font, length.out=length(labels));
      xy <- xy.coords(x, y);
      xo <- r * strwidth("A");
      yo <- r * strheight("A");
      if (length(offset) == 0) {
         offset <- c(0.15, 0.15);
      }
      offset <- rep(offset, length.out=2);
      offsetX <- offset[1] * strwidth("A");
      offsetY <- offset[2] * strheight("A");

      ## Angular sequence with n steps
      theta <- tail(seq(0, 2*pi, length.out=n+1), -1);

      ## Outline has no offset
      if (outline) {
         ## Make a matrix of coordinates per label
         outlineX <- matrix(ncol=n, byrow=TRUE,
            rep(xy$x, each=n) + cos(theta)*xo);
         outlineY <- matrix(ncol=n, byrow=TRUE,
            rep(xy$y, each=n) + sin(theta)*yo);
         outlineLabels <- matrix(ncol=n, byrow=TRUE,
            rep(labels, each=n));
         outlineColors <- matrix(ncol=n, nrow=length(labels), byrow=TRUE,
            rep(alpha2col(bg, alpha=alphaOutline), each=n));
      } else {
         outlineX <- outlineY <- outlineLabels <- outlineColors <- NULL;
      }

      ## Shadow has offset
      if (shadow) {
         ## Make a matrix of coordinates per label
         shadowX <- matrix(ncol=n, byrow=TRUE,
            rep(xy$x + offsetX, each=n) + cos(theta)*xo*1.5);
         shadowY <- matrix(ncol=n, byrow=TRUE,
            rep(xy$y + offsetY, each=n) + sin(theta)*yo*1.5);
         shadowLabels <- matrix(ncol=n, byrow=TRUE,
            rep(labels, each=n));
         shadowColors <- matrix(ncol=n, nrow=length(labels), byrow=TRUE,
            rep(alpha2col(shadowColor, alpha=alphaShadow), each=n));
      } else {
         shadowX <- shadowY <- shadowLabels <- shadowColors <- NULL;
      }

      ## Append label coordinates to shadow coordinates so the shadows
      ## are drawn first, for each label in order. This order ensures
      ## that overlaps are respected without any labels appearing above
      ## another label shadow out of order.
      allX <- cbind(shadowX, outlineX, xy$x);
      allY <- cbind(shadowY, outlineY, xy$y);
      allColors <- cbind(shadowColors, outlineColors,
         rep(col, length.out=length(labels)));
      allLabels <- cbind(shadowLabels, outlineLabels, labels);
      #allCex <- rep(cex, n+1);
      #allFont <- rep(font, n+1);
      #allSrt <- rep(srt, n+1);

      ## Draw labels with one text() call to make it vectorized
      graphics::text(x=c(allX),
         y=c(allY),
         labels=c(allLabels),
         col=c(allColors),
         cex=cex, font=font,
         ...);
      invisible(list(allX=allX, allY=allY, allColors=allColors,
         allLabels=allLabels));
   }
}

#' Adjust axis label margins
#'
#' Adjust axis label margins
#'
#' This function takes a vector of axis labels, and the margin where they
#' will be used, and adjusts the relevant axis margin to accomodate the
#' label size, up to a maximum fraction of the figure size as defined by
#' \code{maxFig}. It currently assumes labels are placed perpendicular to
#' the axis, e.g. \code{las=2} when using \code{\link[graphics]{text}}.
#'
#' Note this function does not render labels in the figure.
#'
#' @param x vector of axis labels
#' @param margin single integer value indicating which margin to adjust,
#'    using the order by \code{par("mar")}, 1=bottom, 2=left, 3=top,
#'    4=right.
#' @param maxFig fraction less than 1, indicating the maximum size of margin
#'    relative to the figure size. Setting margins too large results in an
#'    error otherwise.
#' @param cex numeric or NULL, sent to \code{\link[graphics]{strwidth}} when
#'    calculating the string width of labels in inches.
#' @param prefix character string used to add whitespace around the axis label.
#' @param ... additional parameters are ignored.
#'
#' @examples
#' xlabs <- paste0("item_", (1:20));
#' ylabs <- paste0("rownum_", (1:20));
#' adjustAxisLabelMargins(xlabs, 1);
#' adjustAxisLabelMargins(ylabs, 2);
#' nullPlot(xlim=c(1,20), ylim=c(1,20), doMargins=FALSE);
#' axis(1, at=1:20, labels=xlabs, las=2);
#' axis(2, at=1:20, labels=ylabs, las=2);
#'
#' par("mar"=c(5,4,4,2));
#' adjustAxisLabelMargins(xlabs, 3);
#' adjustAxisLabelMargins(ylabs, 4);
#' nullPlot(xlim=c(1,20), ylim=c(1,20), doMargins=FALSE);
#' axis(3, at=1:20, labels=xlabs, las=2);
#' axis(4, at=1:20, labels=ylabs, las=2);
#'
#' @export
adjustAxisLabelMargins <- function
(x,
 margin=1,
 maxFig=1/2,
 cex=NULL,
 prefix="-- -- ",
 ...)
{
   ## Purpose is to adjust figure margins to accomodate label string length
   ## but no greater than the maxFig proportion of figure size.
   ##
   ## x is a vector of axis labels
   ##
   ## par("mai") and par("fin") are used, with units="inches", which allows
   ## the calculations to remain unaware of plot coordinates.
   ##
   ## The margin values refer to the order from par("mar"),
   ## 1-bottom, 2-left, 3-top, 4-right
   ## Note: If a plot device is not already open, the call to strwidth()
   ## will open one if possible. If not possible, an error will be thrown
   ## from strwidth().
   if (!margin %in% c(1,2,3,4)) {
      stop("adjustAxisLabelMargins() requires margin to be one of c(1,2,3,4).");
   }

   ## Get plot and figure sizes in inches
   parMai <- par("mai");
   parFin <- par("fin");
   maxWidth <- max(strwidth(paste(prefix, x),
      units="inches",
      cex=cex), na.rm=TRUE);

   ## Make sure label margins are not more than 1/2 the figure size
   refMargin <- 2-(margin %% 2);
   parMaiNew <- min(c(maxWidth, parFin[refMargin]*maxFig));
   parMai[margin] <- parMaiNew;
   par("mai"=parMai);
   invisible(parMaiNew);
}

#' Show colors from a vector or list
#'
#' Show colors from a vector or list
#'
#' This function simply displays colors for review, using
#' \code{\link{imageByColors}} to display colors and labels across the
#' plot space.
#'
#' @param x vector of colors, or list of color vectors.
#' @param labelCells logical whether to label colors atop the color itself.
#'    If NULL (default) it will only display labels with 40 or fewer items
#'    on either axis.
#' @param transpose logical whether to transpose the colors to display
#'    top-to-bottom, instead of left-to-right.
#' @param srtCellnote numerical angle to rotate text when
#'    \code{labelCells=TRUE}. When set to NULL, labels are vertical
#'    srtCellnote=90 when \code{transpose=FALSE} and horizontal
#'    srtCellnote=0 when \code{transpose=TRUE}.
#' @param adjustMargins logical indicating whether to call
#'    \code{\link{adjustAxisLabelMargins}} to adjust the x- and y-axis
#'    label margins to accomodate the label size.
#' @param ... additional parameters are sent to \code{\link{imageByColors}}.
#'
#' @return invisible color matrix used in \code{\link{imageByColors}}.
#'
#' @examples
#' x <- color2gradient(list(Reds=c("red"), Blues=c("blue")), n=c(4,7));
#' showColors(x);
#'
#' if (suppressPackageStartupMessages(require(RColorBrewer))) {
#'    y <- lapply(nameVector(RColorBrewer:::namelist), function(i){
#'       brewer.pal(20,i);
#'    });
#'    showColors(y, cexCellnote=0.6, cex.axis=0.7, main="Brewer Colors");
#' }
#' if (suppressPackageStartupMessages(require(viridis))) {
#'    # Grab the full viridis color map
#'    z <- rgb2col(viridis.map[,c("R","G","B")]);
#'    # label the colors using viridis.map$opt
#'    names(z) <- viridis.map$opt;
#'    showColors(z, labelCells=TRUE, xaxt="n", main="viridis.map colors");
#'    # or split the colors into a list
#'    zl <- split(z, viridis.map[,"opt"]);
#'    showColors(zl, labelCells=TRUE, srtCellnote=0, xaxt="n",
#'       main="Viridis colors as a list");
#' }
#'
#' @export
showColors <- function
(x,
 labelCells=NULL,
 transpose=FALSE,
 srtCellnote=NULL,
 adjustMargins=TRUE,
 ...)
{
   ## Purpose is to show a vector of colors, or display a list of
   ## color vectors in a table view
   ## x <- color2gradient(list(Reds=c("red"), Blues=c("blue")), n=c(4,7))

   if (igrepHas("list", class(x))) {
      xM <- rbindList(x);
      colnames(xM) <- paste0("item_", padInteger(seq_len(ncol(xM))));
      if (length(labelCells)==0) {
         if (max(dim(xM)) > 40) {
            labelCells <- FALSE;
         } else {
            labelCells <- TRUE;
         }
      }
      if (labelCells) {
         xMnames <- rbindList(lapply(x, function(i){
            if (is.null(names(i))) {
               rep("", length(i));
            } else {
               names(i);
            }
         }));
         if (length(xMnames) == 0) {
            xMnames <- xM;
         } else {
            colnames(xMnames) <- colnames(xM);
         }
      } else {
         xMnames <- xM;
         xMnames[] <- "";
      }
   } else {
      xM <- matrix(x, nrow=1);
      if (!is.null(names(x))) {
         colnames(xM) <- names(x);
      }
      if (length(labelCells)==0) {
         if (max(dim(xM)) > 40) {
            labelCells <- FALSE;
         } else {
            labelCells <- TRUE;
         }
      }
      if (labelCells) {
         if (!is.null(names(x))) {
            xMnames <- matrix(names(x), nrow=1);
         } else {
            xMnames <- xM;
         }
      } else {
         xMnames <- xM;
         xMnames[] <- "";
      }
   }

   if (adjustMargins &&
       length(colnames(xM)) == 0 &&
       length(rownames(xM)) == 0) {
      adjustMargins <- FALSE;
   }
   if (transpose) {
      if (adjustMargins) {
         parMar <- par("mar");
         ## Detect string width to adjust margins
         adjustAxisLabelMargins(rownames(xM), 1);
         adjustAxisLabelMargins(colnames(xM), 2);
      }
      if (length(srtCellnote)==0) {
         if (nrow(xM) > ncol(xM)) {
            srtCellnote <- 90;
         } else {
            srtCellnote <- 0;
         }
      }
      imageByColors(t(xM),
         cellnote=t(xMnames),
         flip="y",
         srtCellnote=srtCellnote,
         ...);
   } else {
      if (adjustMargins) {
         parMar <- par("mar");
         ## Detect string width to adjust margins
         adjustAxisLabelMargins(colnames(xM), 1);
         adjustAxisLabelMargins(rownames(xM), 2);
      }
      if (is.null(srtCellnote)) {
         if (nrow(xM) > ncol(xM)) {
            srtCellnote <- 0;
         } else {
            srtCellnote <- 90;
         }
      }
      imageByColors(xM,
         cellnote=xMnames,
         flip="y",
         srtCellnote=srtCellnote,
         ...);
   }
   if (adjustMargins) {
      par("mar"=parMar);
   }
   invisible(xM);
}

#' Plot distribution and histogram overlay
#'
#' Plot distribution and histogram overlay
#'
#' This function is a wrapper around `graphics::hist()` and
#' `stats::density()`, with enough customization to cover
#' most of the situations that need customization.
#'
#' For example `log="x"` will automatically log-scale the x-axis,
#' keeping the histogram bars uniformly sized. Alternatively,
#' `xScale="sqrt"` will square root transform the data, and
#' transform the x-axis while keeping the numeric values constant.
#'
#' It also takes
#' care of scaling the density height to be reasonably similar to
#' the histogram bar height, using the 99th quantile of the y-axis
#' value, which helps prevent outlier peaks from dominating the
#' y-axis range, thus obscuring interesting smaller features.
#'
#' If supplied with a data matrix, this function will create a layout
#' with ncol(x) panels, and plot the distribution of each column
#' in its own panel, using categorical colors from
#' `colorjam::rainbowJam()`.
#'
#' By default NA values are ignored, and the distributions are the
#' non-NA values.
#'
#' Colors can be controlled using the parameter `col`, but can
#' be specifically defined for bars with `barCol` and the polygon
#' with `polyCol`.
#'
#' @param x numeric vector, or numeric matrix.
#' @param doHistogram logical indicating whether to plot histogram bars.
#' @param doPolygon logical indicating whether to plot the density polygon.
#' @param col color or vector of colors to apply to plot panels.
#' @param barCol,polyCol,polyBorder,histBorder colors used when `col` is
#'    not supplied. They define colors for the histogram bars, polygon
#'    fill, polygon border, and histogram bar border, respectively.
#' @param breaks numeric breaks sent to `hist` to define the number of
#'    histogram bars.
#' @param u5.bias,pretty.n parameters sent to `base::pretty()` for axis
#'    label positioning.
#' @param bw text bandwidth name, used in the density calculation, sent
#'    to `jamba::breakDensity()`. By default `stats::density()` calls a
#'    very smooth density kernel, which obscures finer details. By default,
#'    `jamba::breakDensity()` uses a more detailed kernel.
#' @param densityBreaksFactor numeric factor controlling the level of
#'    detail in the density, sent to `jamba::breakDensity()`.
#' @param xScale character string defining the x-axis transformation:
#'    "default" applies no transform; "log10" applies a log10 transform;
#'    "sqrt" applies a sqrt transform.
#' @param log character vector, optionally containing "x" and/or "y" to
#'    apply the appropriate transformation. If "x" then it sets
#'    `xScale="log10"`.
#' @param usePanels logical indicating whether to separate
#'    the density plots into panels when `x` contains multiple columns.
#'    When `useOnePanel=FALSE` the panels will be defined so that all
#'    columns will fit on one page.
#' @param useOnePanel logical indicating whether to define multiple panels
#'    on one page. Therefore `useOnePanel=TRUE` will create multiple
#'    pages with one panel on each page, which may work well for
#'    output in multi-page PDF files.
#' @param ylimQuantile numeric value between 0 and 1, indicating the
#'    quantile value of the density `y` values to use for the ylim. This
#'    threshold is only applied when `ylim` is NULL.
#' @param ylim,xlim numeric y-axis and x-axis ranges, respectively. When NULL,
#'    the x-axis range is determined for each plot panel.
#' @param removeNA logical indicating whether to remove NA values
#'    prior to running histogram and density calculations. Presence
#'    of NA values generally causes both functions to fail.
#' @param ablineV,ablineH abline vertical and horizontal positions,
#'    respectively. These values are mostly helpful in multi-panel plots,
#'    since they draw consistent lines on each panel.
#' @param verbose logical indicating whether to print verbose output.
#'
#'
#' @export
plotPolygonDensity <- function
(x,
 doHistogram=TRUE,
 doPolygon=TRUE,
 barCol="#00337799",
 col=NULL,
 histBorder=makeColorDarker(barCol, darkFactor=1.5),
 colAlphas=c(0.8,0.6,0.9),
 hueShift=-0.1,
 darkFactors=c(-1.3, 1, 3),
 polyCol="#00449977",
 polyBorder=makeColorDarker(polyCol),
 lwd=2,
 las=2,
 u5.bias=0,
 pretty.n=10,
 bw=NULL,
 breaks=100,
 width=NULL,
 densityBreaksFactor=3,
 axisFunc=axis,
 bty="l",
 cex.axis=1.5,
 doPar=TRUE,
 heightFactor=0.95,
 weightFactor=NULL,#weightFactor=0.22*(100/breaks),
 main="Histogram distribution",
 xaxs="i",
 yaxs="i",
 log=NULL,
 xScale=c("default","log10","sqrt"),
 logFloorMethod=c("detect", "+1", "floor1", "none"),
 usePanels=TRUE,
 useOnePanel=FALSE,
 ablineV=NULL,
 ablineH=NULL,
 ablineVcol="#44444499",
 ablineHcol="#44444499",
 ablineVlty="solid",
 ablineHlty="solid",
 removeNA=TRUE,
 add=FALSE,
 ylimQuantile=0.99,
 ylim=NULL,
 verbose=FALSE,
 ...)
{
   ## Purpose is to wrapper a plot(density(x)) and polygon() method
   ## for pretty filled density plots
   ##
   ## log="x" will impose log10 scale to the x-axis, and display log-axis tick marks
   ##
   ## xScale is an extension to log="x" which allows setting the scale
   ## to other transforms, e.g. sqrt.
   ##
   ## TODO: fix the density-to-histogram visual scaling
   ## "Rescale density to histogram1"
   ##
   ## ylimQuantile is used to scale the y-axis such that one giant bar
   ## does not shrink the remaining visible y-axis scale so much that detail
   ## is lost. To show the full y-axis range, use ylimQuantile=1.
   ## This value is only applied when ylim=NULL.
   ##
   xScale <- match.arg(xScale);

   ## ablineV will include abline(s) in each panel
   if (!is.null(ablineV)) {
      ablineVcol <- rep(ablineVcol, length.out=length(ablineV));
      ablineVlty <- rep(ablineVlty, length.out=length(ablineV));
   }
   if (!is.null(ablineH)) {
      ablineHcol <- rep(ablineHcol, length.out=length(ablineH));
      ablineHlty <- rep(ablineHlty, length.out=length(ablineH));
   }

   ## Optionally, if the input data is a multi-color matrix, split into separate panels
   if (igrepHas("matrix|data.*frame", class(x)) && ncol(x) > 1 && usePanels) {
      ## ablineV will include abline(s) in each panel
      if (!is.null(ablineV)) {
         ablineV <- rep(ablineV, length.out=ncol(x));
      }

      newMfrow <- rev(decideMfrow(ncol(x)));
      if (useOnePanel) {
         newMfrow <- c(1,1);
      }
      if (doPar) {
         origMfrow <- par("mfrow");
         par("mfrow"=newMfrow);
      }
      if (length(barCol) == ncol(x)) {
         panelColors <- barCol;
      } else {
         if (suppressPackageStartupMessages(require(colorjam))) {
            panelColors <- rainbowJam(ncol(x));
         } else {
            panelColors <- sample(colors(), ncol(x));
         }
      }
      if (is.null(colnames(x))) {
         colnames(x) <- makeNames(rep("column", ncol(x)), suffix="_");
      }
      ## Get common set of breaks
      #hx <- hist(x, breaks=breaks, plot=FALSE, ...);
      #breaks <- hx$breaks;
      ## Iterate each column
      d1 <- lapply(nameVector(1:ncol(x), colnames(x)), function(i){
         if (useOnePanel) {
            add <- (i > 1);
            mainTitle <- "";
         } else {
            mainTitle <- colnames(x)[i];
         }
         if (removeNA) {
            xi <- rmNA(x[,i]);
         } else {
            xi <- x[,i];
         }
         d2 <- plotPolygonDensity(xi,
            main=mainTitle,
            barCol=alpha2col(panelColors[i], alpha=colAlphas[1]),
            polyCol=alpha2col(panelColors[i], alpha=colAlphas[2]),
            doPolygon=doPolygon,
            polyBorder=alpha2col(panelColors[i], alpha=colAlphas[3]),
            doHistogram=doHistogram,
            add=add,
            colAlphas=colAlphas,
            doPar=FALSE,
            col=col,
            lwd=lwd,
            las=las,
            u5.bias=u5.bias,
            pretty.n=pretty.n,
            bw=bw,
            breaks=breaks,
            width=width,
            axisFunc=axisFunc,
            bty=bty,
            cex.axis=cex.axis,
            heightFactor=heightFactor,
            weightFactor=weightFactor,
            xaxs=xaxs,
            yaxs=yaxs,
            log=log,
            xScale=xScale,
            logFloorMethod=logFloorMethod,
            verbose=verbose,
            ablineV=ablineV,
            ablineVcol=ablineVcol,
            ablineVlty=ablineVlty,
            ablineH=ablineH,
            ablineHcol=ablineHcol,
            ablineHlty=ablineHlty,
            ylimQuantile=ylimQuantile,
            ylim=ylim,
            ...);
         d2;
      });
      if (useOnePanel && ncol(x) > 1) {
         legend("top",
            inset=c(0,0.05),
            legend=colnames(x),
            fill=alpha2col(panelColors, alpha=colAlphas[2]),
            border=alpha2col(panelColors, alpha=colAlphas[3]));
      }
      if (doPar) {
         par("mfrow"=origMfrow);
      }
      invisible(d1);
   } else {
      ##
      oPar <- par();
      par("xaxs"=xaxs);
      par("yaxs"=yaxs);
      #if (is.null(bw) & is.null(width)) {
      #   bw <- "ucv";
      #}
      if (verbose) {
         printDebug("plotPolygonDensity(): ",
            "barCol, polyCol:",
            c(barCol, polyCol),
            fgText=list("orange", "dodgerblue", c(barCol, polyCol)));
      }
      #if (barCol %in% "#00337799" && polyCol == "#00449977" && length(col) > 0) {
      if (barCol %in% formals(plotPolygonDensity)$barCol &&
            polyCol %in% formals(plotPolygonDensity)$polyCol &&
            length(col) > 0) {
         barCol <- makeColorDarker(changeHue(col, hueShift=hueShift),
            fixAlpha=colAlphas[1],
            darkFactor=darkFactors[1]);
         polyCol <- makeColorDarker(col,
            fixAlpha=colAlphas[2],
            darkFactor=darkFactors[2]);
         polyBorder <- makeColorDarker(col,
            fixAlpha=colAlphas[3],
            darkFactor=darkFactors[3]);
      }

      if (xScale %in% "default") {
         if ("x" %in% log) {
            xScale <- "log10";
         }
      }
      ## Optional visual log-transformation
      #if ("x" %in% log) {
      if ("log10" %in% xScale) {
         x <- sensibleLog(x, logFloorMethod=logFloorMethod, verbose=verbose, ...);
         xLogAxisType <- attr(x, "xLogAxisType");
      } else if ("sqrt" %in% xScale) {
         x1 <- x;
         ## use square root of absolute value, multiplied by the sign
         x <- sqrt(abs(x1)) * sign(x1);
      }

      if (doHistogram) {
         if (removeNA) {
            x <- rmNA(x);
         }
         if (add) {
            hx <- hist(x,
               breaks=breaks,
               col=barCol,
               main=main,
               border=histBorder,
               xaxt="n",
               yaxt="n",
               las=las,
               ylab="",
               cex.axis=cex.axis*0.8,
               add=add,
               ...);
         } else {
            hx <- hist(x,
               breaks=breaks,
               col=barCol,
               main=main,
               border=histBorder,
               xaxt="n",
               las=las,
               ylab="",
               cex.axis=cex.axis*0.8,
               add=add,
               plot=FALSE,
               ...);
            ## Optionally define the y-axis scale
            if (is.null(ylim) &&
               !is.null(ylimQuantile) &&
               ylimQuantile < 1 &&
               ylimQuantile > 0 &&
               max(hx$counts) > 0) {
               ylim <- c(0,
                  quantile(hx$counts, c(ylimQuantile)));
            }
            plot(hx,
               col=barCol,
               main=main,
               border=histBorder,
               xaxt="n",
               las=las,
               ylab="",
               cex.axis=cex.axis*0.8,
               add=add,
               ylim=ylim,
               ...);
         }
         if (verbose) {
            printDebug("plotPolygonDensity(): ",
               "hx$breaks:",
               format(trim=TRUE, digits=2, ht(hx$breaks)));
         }
         if (!add) {
            if ("log10" %in% xScale) {
               if (verbose) {
                  printDebug("plotPolygonDensity(): ",
                     "log10 x-axis scale.");
               }
               minorLogTicksAxis(1,
                  doMinorLabels=TRUE,
                  logAxisType=xLogAxisType,
                  ...);
            } else if ("sqrt" %in% xScale) {
               if (verbose) {
                  printDebug("plotPolygonDensity(): ",
                     "sqrt xScale");
               }
               atPretty <- sqrtAxis(1);
               axisFunc(1,
                  at=sqrt(abs(atPretty))*sign(atPretty),
                  labels=names(atPretty),
                  las=las,
                  cex.axis=cex.axis,
                  ...);
            } else {
               #atPretty <- pretty(hx$breaks, u5.bias=u5.bias, n=pretty.n, ...);
               ## Slight change to use the plot region instead of the histogram region
               atPretty <- pretty(par("usr")[1:2],
                  u5.bias=u5.bias,
                  n=pretty.n,
                  ...);
               #axis(1, at=atPretty, labels=atPretty, las=las, ...);
               axisFunc(1,
                  at=atPretty,
                  las=las,
                  cex.axis=cex.axis,
                  ...);
               if (verbose) {
                  printDebug("plotPolygonDensity(): ",
                     "atPretty: ",
                     atPretty);
               }
            }
            box(bty=bty);
         }
      } else {
         hx <- NULL;
      }

      ## Calculate density
      dx <- breakDensity(x=x,
         bw=bw,
         width=width,
         breaks=breaks,
         densityBreaksFactor=densityBreaksFactor,
         weightFactor=weightFactor,
         addZeroEnds=TRUE,
         ...);

      if (doHistogram) {
         maxHistY <- max(hx$counts, na.rm=TRUE);
         maxHistY <- par("usr")[4];
         if (verbose) {
            printDebug("plotPolygonDensity(): ",
               "Re-scaled y to histogram height maxHistY:",
               maxHistY);
         }
         ## Scale the y-axis to match the histogram
         dx$y <- normScale(dx$y,
            from=0,
            to=maxHistY*heightFactor);
      }
      if (verbose) {
         printDebug("Completed density calculations.");
      }
      if (!doHistogram) {
         plot(dx,
            col="transparent",
            main=main,
            xaxt="n",
            ...);
         if ("x" %in% log) {
            minorLogTicksAxis(1,
               doMinorLabels=TRUE,
               logAxisType=xLogAxisType,
               ...);
         } else {
            #atPretty <- pretty(hx$breaks, u5.bias=u5.bias, n=pretty.n, ...);
            ## Slight change to use the plot region instead of the histogram region
            atPretty <- pretty(par("usr")[1:2],
               u5.bias=u5.bias,
               n=pretty.n,
               ...);
            axisFunc(1,
               at=atPretty,
               las=las,
               cex.axis=cex.axis,
               ...);
            if (verbose) {
               printDebug("plotPolygonDensity(): ",
                  "atPretty: ",
                  atPretty);
            }
         }
      }
      if (doPolygon) {
         polygon(dx, col=polyCol, border=polyBorder, lwd=lwd, ...);
      }
      if (!is.null(ablineV)) {
         abline(v=ablineV, col=ablineVcol, lty=ablineVlty, ...);
      }
      if (!is.null(ablineH)) {
         abline(h=ablineH, col=ablineHcol, lty=ablineHlty, ...);
      }
      #par(oPar);
      invisible(list(d=dx,
         hist=hx,
         barCol=barCol,
         polyCol=polyCol,
         polyBorder=polyBorder,
         histBorder=histBorder));
   }
}

#' Calculate more detailed density of numeric values
#'
#' Calculate more detailed density of numeric values
#'
#' This function is a drop-in replacement for `stats::density()`,
#' simply to provide a quick alternative that defaults to a higher
#' level of detail. Detail can be adjusted using `densityBreaksFactor`,
#' where higher values will use a wider step size, thus lowering
#' the detail in the output.
#'
#' @param x numeric vector
#' @param breaks numeric breaks as described for `stats::density()` except
#'    that single integer value is multiplied by `densityBreaksFactor`.
#' @param bw character name of a bandwidth function, or NULL.
#' @param width NULL or numeric value indicating the width of breaks to
#'    apply.
#' @param densityBreaksFactor numeric factor to adjust the width of
#'    density breaks, where higher values result in less detail.
#' @param weightFactor optional vector of weights `length(x)` to apply
#'    to the density calculation.
#' @param addZeroEnds logical indicating whether the start and end value
#'    should always be zero, which can be helpful for creating a polygon.
#' @param baseline optional numeric value indicating the expected baseline,
#'    which is typically zero, but can be set to a higher value to indicate
#'    a "noise floor".
#' @param floorBaseline logical indicating whether to apply a noise floor
#'    to the output data.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional parameters are sent to `stats::density()`.
#'
#' @export
breakDensity <- function
(x,
 breaks=length(x)/3,
 bw=NULL,
 width=NULL,
 densityBreaksFactor=3,
 weightFactor=1,
 addZeroEnds=TRUE,
 baseline=0,
 floorBaseline=FALSE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to provide slightly more granular density() than
   ## the default provides
   ##
   ## bw can be a custom density kernel, see density() for details
   ##
   ## width can be the width supplied to density() but if NULL
   ## then this function calculates a reasonable width based upon
   ## the data content
   ##
   ## densityBreaksFactor is used to tune the level of detail, roughly
   ## defines a smoothing window of roughly this many fold above the
   ## distance between breaks, using breaks either as an integer number
   ## of breaks, or as a discrete set of breakpoints.
   ## Set to 0.01 to see discrete values as sharp peaks, or
   ## set to 10 to see a smooth gradient.
   ##
   ## addZeroEnds=TRUE will add a baseline y-value to the beginning and end
   ## which helps when used to draw a polygon, using the baseline parameter.
   ##
   ## floorBaseline=TRUE will change any value below the baseline to the baseline
   if (is.null(bw)) {
      if (is.null(width)) {
         if (length(breaks) == 1) {
            width <- diff(range(x))/(breaks) * densityBreaksFactor;
         } else {
            width <- diff(range(x))/(median(diff(breaks))) * densityBreaksFactor;
         }
      }
      if (verbose) {
         printDebug("breakDensity(): ",
            "width:",
            formatC(width, format="g", digits=3));
         printDebug("breakDensity(): ",
            "breaks:",
            formatC(breaks, format="g", digits=3));
      }
      dx <- density(x,
         width=width,
         weight=rep(weightFactor, length.out=length(x)),
         ...);
   } else {
      dx <- density(x,
         width=width,
         weight=rep(weightFactor, length.out=length(x)),
         bw=bw,
         ...);
   }

   ## Optionally add a y-value of zero to the beginning and end
   if (addZeroEnds) {
      if (verbose) {
         printDebug("breakDensity(): ",
            "Adding baseline:", baseline, " to ends.");
      }
      dx$x <- c(head(dx$x, 1), dx$x, tail(dx$x, 1));
      dx$y <- c(baseline, dx$y, baseline);
   }
   ## Optionally floor data at the baseline
   if (floorBaseline && any(dx$y) < baseline) {
      if (verbose) {
         printDebug("breakDensity(): ",
            "Enforcing a noise floor:", baseline);
      }
      dx$y[dx$y < baseline] <- baseline;
   }

   return(dx);
}


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
#' features of `graphics::smoothScatter()` plots much easier
#' to customize. For example bandwidthN allows defining the number of
#' bandwidth steps used by the kernel density function, and importantly
#' bases the number of steps on the visible plot window, and not the range
#' of data, which can differ substantially. The `nbin` argument is related,
#' but is used to define the level of detail used in the image function,
#' which when plotting numerous smaller panels, can be useful to reduce
#' unnecessary visual details.
#'
#' This function also by default produces a raster image plot
#' with `useRaster=TRUE`, which adjusts the x- and y-bandwidth to
#' produce visually round density even when the x- and y-ranges
#' are very different.
#'
#' Comments:
#'
#' * `asp=1` will define an aspect ratio 1, meaning the x-axis and y-axis
#' units will be the same physical size in the output device.
#' When this is true, and `fillBackground=TRUE` the `xlim` and `ylim`
#' values follow logic for `plot.default()` and `plot.window()` such that
#' each axis will include at least the `xlim` and `ylim` ranges, with
#' additional range included in order to maintain the plot aspect ratio.
#' * When `asp`, and any of `xlim` or `ylim`, are defined, the data will
#' be "cropped" to respective `xlim` and `ylim` values as relevant,
#' after which the plot is drawn with the appropriate plot aspect ratio.
#' When `applyRangeCeiling=TRUE`, points outside the fixed `xlim` and `ylim`
#' range are fixed to the edge of the range, after which the plot is drawn
#' with the requested plot aspect ratio. It is recommended not to define
#' `xlim` and `ylim` when also defining `asp`.
#' * When `add=TRUE` the `xlim` and `ylim` values are already defined
#' by the plot device. It is recommended not to define `xlim` and `ylim`
#' when `add=TRUE`.
#'
#' @family jam plot functions
#'
#' @param x numeric vector, or data matrix with two or  more columns.
#' @param y numeric vector, or if data is supplied via x as a matrix, y
#'    is NULL.
#' @param bwpi `numeric` value indicating the bandwidth "per inch"
#'    to scale the bandwidth based upon visual space available.
#'    This argument is used to define `bandwidthN`, however `bwpi`
#'    is only used when `bandwidthN=NULL`.
#'    The bandwidth is used to define the 2-dimensional point density.
#' @param binpi `numeric` value indicating the number of bins "per inch",
#'    to scale based upon visual space available.
#'    This argument is used to define `nbin`, however `binpi`
#'    is only used when `nbin=NULL`.
#' @param bandwidthN `integer` number of bandwidth steps to use across the
#'    visible plot window. Note that this bandwidth differs from default
#'    `graphics::smoothScatter()` in that it uses the visible
#'    plot window instead of the data range, so if the plot window is not
#'    sufficiently similar to the data range, the resulting smoothed
#'    density will not be visibly distorted. This parameter also permits
#'    display of higher (or lower) level of detail.
#' @param nbin `integer` number of bins to use when converting the kernel
#'    density result (which uses bandwidthN above) into a usable image.
#'    This setting is effectively the resolution of rendering the
#'    bandwidth density in terms of visible pixels. For example
#'    `nbin=256` will create 256 visible pixels wide and tall in each
#'    plot panel; and `nbin=32` will create 32 visible pixels, with
#'    lower detail which may be suitable for multi-panel plots.
#'    To use a variable number of bins, try `binpi`.
#' @param expand `numeric` value indicating the fraction of the x-axis
#'    and y-axis ranges to add to create an expanded range,
#'    used when `add=FALSE`. The default `expand=c(0.04, 0.04)` mimics
#'    the R base plot default which adds 4 percent total, therefore 2 percent
#'    to each side of the visible range.
#' @param transFactor `numeric` value used by the default `transformation`
#'    function, which effectively scales the density of points to
#'    a reasonable visible distribution. This argument is a convenience
#'    method to avoid having to type out the full `transformation` function.
#' @param transformation `function` which converts point density to a number,
#'    typically related to square root or cube root transformation. Note
#'    that the default uses `transFactor` but if a custom function is
#'    supplied, it will not use `transFactor` unless specified.
#' @param xlim `numeric` x-axis range, or `NULL` to use the data range.
#' @param ylim `numeric` y-axis range, or `NULL` to use the data range.
#' @param xlab,ylab `character` labels for x- and y-axis, respectively.
#' @param nrpoints `integer` number of outlier datapoints to display,
#'    as defined by `graphics::smoothScatter()`, however the default here
#'    is `nrpoints=0` to avoid additional clutter in the output,
#'    and because the default arguments `bwpi`, `binpi` usually indicate all
#'    individual points.
#' @param colramp any input recognized by `getColorRamp()`:
#'    * `character` vector with multiple colors
#'    * `character` string length 1, with valid R color used to create
#'    a linear color gradient
#'    * `character` name of a known color gradient from `RColorBrewer`
#'    or `viridis`
#'    * `function` that itself produces vector of colors,
#'    in the form `function(n)` where `n` defines the number of colors.
#' @param col `character` string with R color used when `nrpoints` is
#'    non-zero, this color defines the color of those points.
#' @param doTest `logical` indicating whether to create a visual set of test
#'    plots to demonstrate the utility of this function.
#' @param fillBackground `logical` indicating whether to fill the
#'    background of the plot panel with the first color in `colramp`.
#'    The default `fillBackground=TRUE` is useful since the plot panel
#'    may be slightly wider than the range of data being displayed, and
#'    when the first color in `colramp` is not the same as the plot device
#'    background color.
#'    Run a test using:
#'    `plotSmoothScatter(doTest=TRUE, fillBackground=FALSE, colramp="viridis")`
#'    and compare with:
#'    `plotSmoothScatter(doTest=TRUE, colramp="viridis")`
#' @param naAction `character` string indicating how to handle NA values,
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
#' @param xaxt `character` value compatible with par(xaxt), used to control
#'    the x-axis range, similar to its use in `plot()` generic functions.
#' @param yaxt `character` value compatible with par(yaxt), used to control
#'    the y-axis range, similar to its use in `plot()` generic functions.
#' @param add `logical` whether to add to an existing active R plot, or create
#'    a new plot window.
#' @param asp `numeric` with optional aspect ratio, as described in
#'    `graphics::plot.window()`, where `asp=1` defines x- and y-axis
#'    coordinate ranges such that distances between points are rendered
#'    accurately. One data unit on the y-axis is equal in length to
#'    `asp` multiplied by one data unit on the x-axis.
#'    Notes:
#'    * When `add=TRUE`, the value `asp` is ignored, because
#'    the existing plot device is re-used.
#'    * When `add=FALSE` and `asp` is defined with `numeric` value,
#'    a new plot device is opened using `plot.window()`, and the `xlim`
#'    and `ylim` values are passed to that function. As a result the
#'    `par("usr")` values are used to define `xlim` and `ylim` for the
#'    purpose of determining visible points, relevant to `applyRangeCeiling`.
#' @param applyRangeCeiling `logical` indicating how to handle points outside
#'    the visible plot range. Valid values:
#'    \describe{
#'       \item{TRUE}{Points outside the viewing area are fixed to the
#'       plot boundaries, in order to represent that there are additional
#'       points outside the boundary. This setting is recommended when
#'       the reasonable viewing area is smaller than the actual data,
#'       for example to be consistent across plot panels, but where
#'       you want to indicate that points may be outside the range.}
#'       \item{FALSE}{Points outside the viewing area is not displayed,
#'       with no special visual indication. This setting is useful when
#'       data may contain a large number of points at `c(0, 0)` and the
#'       density overwhelms the detail in the rest of the plot. In that
#'       case setting `xlim=c(1e-10, 10)` and `applyRangeCeiling=FALSE`
#'       would obscure these points.}
#'    }
#' @param useRaster `logical` indicating whether to produce plots using the
#'    `graphics::rasterImage()` function which produces a plot
#'    raster image offline then scales this image to visible plot space.
#'    This technique has two benefits:
#'    1. It produces substantially faster plot output.
#'    2. Output contains substantially fewer plot objects, which results
#'    in much smaller file sizes when saving in PDF or SVG format.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are passed to called functions,
#'    including `getColorRamp()`, `nullPlot()`, `smoothScatterJam()`.
#'
#' @examples
#' # doTest=TRUE invisibly returns the test data
#' x <- plotSmoothScatter(doTest=TRUE);
#'
#' # so it can be plotted again with different settings
#' colnames(x) <- c("column_1", "column_2")
#' plotSmoothScatter(x, colramp="inferno");
#'
#' plotPolygonDensity(x);
#'
#' @export
plotSmoothScatter <- function
(x,
 y=NULL,
 bwpi=50,
 binpi=50,
 bandwidthN=NULL,
 nbin=NULL,
 expand=c(0.04, 0.04),
 transFactor=0.25,
 transformation=function(x)x^transFactor,
 xlim=NULL,
 ylim=NULL,
 xlab=NULL,
 ylab=NULL,
 nrpoints=0,
 colramp=c("white", "lightblue", "blue", "orange", "orangered2"),
 col="black",
 doTest=FALSE,
 fillBackground=TRUE,
 naAction=c("remove", "floor0", "floor1"),
 xaxt="s",
 yaxt="s",
 add=FALSE,
 asp=NULL,
 applyRangeCeiling=TRUE,
 useRaster=TRUE,
 verbose=FALSE,
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

   # make sure colramp is a function
   if (length(colramp) == 0) {
      colramp <- eval(formals(plotSmoothScatter)$colramp);
   } else {
      colramp <- getColorRamp(colramp,
         n=NULL,
         ...);
   }

   # validate expand for x and y axis limit expansion
   if (length(expand) == 0) {
      expand <- c(0, 0);
   }
   expand <- rep(expand,
      length.out=2);

   # optional visual test
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
      #oPar <- par(no.readonly=TRUE);
      oPar <- par("mfrow"=c(2,2), "mar"=c(2,3,4,1));
      on.exit(par(oPar));
      smoothScatter(x2,
         main="smoothScatter default (using colramp blues9)",
         asp=asp,
         ylab="",
         xlab="");
      plotSmoothScatter(x2,
         colramp=colramp,
         fillBackground=fillBackground,
         main="plotSmoothScatter",
         asp=asp,
         ...);
      plotSmoothScatter(x2,
         colramp=c("white", blues9),
         fillBackground=fillBackground,
         main="plotSmoothScatter (using colramp blues9)",
         asp=asp,
         ...);
      xy <- plotSmoothScatter(x2,
         colramp=colramp,
         bwpi=bwpi * 1.5,
         bandwidthN=bandwidthN * 1.5,
         binpi=binpi * 1.5,
         fillBackground=fillBackground,
         main="plotSmoothScatter with increased bandwidth and bin",
         asp=asp,
         ...);
      return(invisible(x2));
   }

   ## use xy.coords()
   xlabel <- if (!missing(x))
      deparse(substitute(x));
   ylabel <- if (length(y) > 0)
      deparse(substitute(y));
   xy <- xy.coords(x=x,
      y=y,
      xlab=xlabel,
      ylab=ylabel,
      recycle=TRUE,
      setLab=TRUE);
   if (length(xlab) == 0) {
      xlab <- xy$xlab;
   }
   if (length(ylab) == 0) {
      ylab <- xy$ylab;
   }
   x <- xy$x;
   y <- xy$y;

   ## Deal with NA values
   if (naAction == "remove") {
      naValues <- (is.na(x) | is.na(y));
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

   if (length(xlim) == 0) {
      if (TRUE %in% add && exists(".Devices")) {
         xlim <- range(par("usr")[1:2], na.rm=TRUE);
      } else {
         xlim <- range(x, na.rm=TRUE);
      }
   }
   if (length(ylim) == 0) {
      if (TRUE %in% add && exists(".Devices")) {
         ylim <- range(par("usr")[3:4], na.rm=TRUE);
      } else {
         ylim <- range(y, na.rm=TRUE);
      }
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

   # expand xlim and ylim viewing range
   xlim4 <- sort((c(-1,1) * diff(xlim) * expand[1]/2) + xlim);
   ylim4 <- sort((c(-1,1) * diff(ylim) * expand[2]/2) + ylim);
   if (TRUE) {
      printDebug("plotSmoothScatter(): ",
         "xlim: ", xlim4);
      printDebug("plotSmoothScatter(): ",
         "ylim: ", ylim4);
   }

   ## Adjust for uneven plot aspect ratio, by using the plot par("pin")
   ## which contains the actual dimensions.
   ## Note that it does not require the actual coordinates of the plot,
   ## just the relative size of the display
   if (!TRUE %in% add) {
      if (TRUE %in% fillBackground) {
         nullPlot(doBoxes=FALSE,
            doUsrBox=FALSE,
            fill=head(colramp(11),1),
            xaxs="i",
            yaxs="i",
            xaxt="n",
            yaxt="n",
            xlim=xlim4,
            ylim=ylim4,
            add=add,
            asp=asp,
            border="transparent",
            ...);
         # use grid.rect since it scales when plot is resized
         abline(h=mean(ylim4), col="transparent")
         grid::grid.rect(gp=grid::gpar(
            fill=head(colramp(11), 1)))
      } else {
         nullPlot(doBoxes=FALSE,
            xaxs="i",
            yaxs="i",
            xaxt="n",
            yaxt="n",
            xlim=xlim4,
            ylim=ylim4,
            add=add,
            asp=asp,
            ...);
      }
      if (length(asp) == 1) {
         parUsr <- par("usr");
         xlim4 <- parUsr[1:2]
         ylim4 <- parUsr[3:4]
         if (TRUE) {
            printDebug("plotSmoothScatter(): ",
               "parUsr: ", parUsr);
            printDebug("plotSmoothScatter(): ",
               "xlim: ", xlim4);
            printDebug("plotSmoothScatter(): ",
               "ylim: ", ylim4);
         }
      }
      axis(1, las=1, xaxt=xaxt);
      axis(2, las=2, yaxt=yaxt);
      if ((length(xlab) > 0 && nchar(xlab) > 0) ||
            (length(ylab) > 0 && nchar(ylab) > 0)) {
         title(xlab=xlab,
            ylab=ylab,
            ...);
      }
   } else {
      # add=TRUE
      # so xlim,ylim are already defined
      if (fillBackground) {
         abline(h=mean(ylim4), col="transparent")
         grid::grid.rect(
            gp=grid::gpar(
               fill=head(colramp(11), 1)))
      }
      parUsr <- par("usr");
      xlim4 <- parUsr[1:2]
      ylim4 <- parUsr[3:4]
      if (TRUE) {
         printDebug("plotSmoothScatter(): ",
            "parUsr: ", parUsr);
         printDebug("plotSmoothScatter(): ",
            "xlim: ", xlim4);
         printDebug("plotSmoothScatter(): ",
            "ylim: ", ylim4);
      }
   }


   ## Determine resolution of 2D density, and of pixel display
   pin1 <- par("pin")[1] / par("pin")[2];
   if (length(bandwidthN) > 0) {
      bandwidthN <- rep(bandwidthN, length.out=2);
      bandwidthXY <- c(diff(xlim4)/bandwidthN[1],
         diff(ylim4)/bandwidthN[2]*pin1);
   } else {
      ## Alternate method using breaks per inch
      if (length(bwpi) == 0) {
         bwpi <- 30;
      }
      bandwidthXY <- c(
         diff(xlim4) / (par("pin")[1] * bwpi),
         diff(ylim4) / (par("pin")[2] * bwpi));
   }
   if (length(nbin) == 0) {
      if (length(binpi) == 0) {
         binpi <- 50;
      }
      nbin <- c(
         round(par("pin")[1] * binpi),
         round(par("pin")[2] * binpi));
   }
   if (verbose) {
      jamba::printDebug("plotSmoothScatter(): ",
         "bandwidthXY: ",
         bandwidthXY);
      jamba::printDebug("nbin: ", nbin);
   }

   ssj <- smoothScatterJam(x=x,
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
      col=col,
      useRaster=useRaster,
      ...);

   return(invisible(ssj));
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
      colramp=colramp,
      nrpoints=nrpoints,
      col=col));
}

#' Smooth scatter plot, Jam style
#'
#' Produce smooth scatter plot, a helper function called by
#' `plotSmoothScatter()`.
#'
#' For general purposes, use `plotSmoothScatter()` as a replacement
#' for `graphics::smoothScatter()`, which produces better default
#' settings for pixel size and density bandwidth.
#'
#' This function is only necessary in order to override the
#' `graphics::smoothScatter()` function which calls
#' `graphics::image.default()`.
#' Instead, this function calls `imageDefault()` which is required
#' in order to utilize custom raster image scaling, particularly important
#' when the x- and y-axis ranges are not similar, e.g. where the x-axis spans
#' 10 units, but the y-axis spans 10,000 units.
#'
#' @family jam plot functions
#'
#' @param x `numeric` vector, or data matrix with two or  more columns.
#' @param y `numeric` vector, or if data is supplied via x as a matrix, y
#'    is NULL.
#' @param nbin `integer` number of bins to use when converting the kernel
#'    density result (which uses bandwidthN above) into a usable image.
#'    For example, nbin=123 is the default used by
#'    `graphics::smoothScatter()`, however the
#'    `plotSmoothScatter()` function default is higher (256).
#' @param bandwidth `numeric` vector used to define the y- and x-axis
#'    bandwidths, respectively, passed to `KernSmooth::bkde2D()`,
#'    which calculates the underlying 2-dimensional kernel density.
#'    The `plotSmoothScatter()` function was motivated by never wanting
#'    to define this number directly, instead auto-calculation suitable
#'    values.
#' @param colramp `function` that takes one `numeric` argument and returns
#'    that integer number of colors, by default 256.
#' @param nrpoints `integer` number of outlier datapoints to display,
#'    as defined by `graphics::smoothScatter()`, however the default here
#'    is `nrpoints=0` to avoid additional clutter in the output,
#'    and because the default argument `bandwidthN` usually indicates all
#'    individual points.
#' @param pch point shape used when `nrpoints>0`.
#' @param cex `numeric` point size expansion factor used when `nrpoints>0`.
#' @param col `character` R color used when `nrpoints>0`.
#' @param transformation `function` which converts point density to a number,
#'    typically related to square root or cube root transformation.
#' @param postPlotHook is `NULL` for no post-plot hook, or a `function` which
#'    is called after producing the image plot. By default it is simply used
#'    to draw a box around the image, but could be used to layer additional
#'    information atop the image plot, for example contours, labels, etc.
#' @param xlab `character` x-axis label
#' @param ylab `character` y-axis label
#' @param xlim `numeric` x-axis range for the plot
#' @param ylim `numeric` y-axis range for the plot
#' @param add `logical` whether to add to an existing active R plot, or create
#'    a new plot window.
#' @param xaxs `character` value compatible with `par("xaxs")`, mainly useful
#'    for suppressing the x-axis, in order to produce a custom x-axis
#'    range, most useful to restrict the axis range expansion done by R
#'    by default.
#' @param yaxs `character` value compatible with `par("yaxs")`, mainly useful
#'    for suppressing the y-axis, in order to produce a custom y-axis
#'    range, most useful to restrict the axis range expansion done by R
#'    by default.
#' @param xaxt `character` value compatible with `par("xaxt")`, mainly useful
#'    for suppressing the x-axis, in order to produce a custom x-axis
#'    by other mechanisms, e.g. log-scaled x-axis tick marks.
#' @param yaxt `character` value compatible with `par("yaxt")`, mainly useful
#'    for suppressing the y-axis, in order to produce a custom y-axis
#'    by other mechanisms, e.g. log-scaled y-axis tick marks.
#' @param useRaster `NULL` or `logical` indicating whether to invoke
#'    `graphics::rasterImage()` to produce a raster image.
#'    If NULL, it determines whether to produce a raster image within the
#'    `imageDefault()` function, which checks the options
#'    using `getOption("preferRaster", FALSE)` to determine among
#'    other things, whether the user prefers raster images, and if the
#'    `dev.capabilities()` supports raster.
#' @param ... additional arguments are passed to `imageDefault()` and
#'    optionally to `plotPlotHook()` when supplied.
#'
#' @seealso `graphics::smoothScatter()`
#'
#' @export
smoothScatterJam <- function
(x,
 y=NULL,
 nbin=256,
 bandwidth,
 colramp=colorRampPalette(c("white",
    "lightblue",
    "blue",
    "orange",
    "orangered2")),
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

   # calculate point density
   map <- jamCalcDensity(x=x,
      nbin=nbin,
      bandwidth=bandwidth);
   xm <- map$x1;
   ym <- map$x2;
   dens <- map$fhat;
   dens[] <- transformation(dens);

   # initialize the plot
   imageDefault(xm,
      ym,
      z=dens,
      col=colramp(256),
      xlab=xlab,
      add=add,
      ylab=ylab,
      xlim=xlim,
      ylim=ylim,
      xaxs=xaxs,
      yaxs=yaxs,
      xaxt=xaxt,
      yaxt=yaxt,
      useRaster=useRaster,
      ...);

   # create list with data used
   imageL <- list(xm=xm,
      ym=ym,
      z=dens,
      col=colramp(256),
      xlab=xlab,
      add=add,
      ylab=ylab,
      xlim=xlim,
      ylim=ylim,
      xaxs=xaxs,
      yaxs=yaxs,
      xaxt=xaxt,
      yaxt=yaxt);
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
      points(x[sel,, drop=FALSE],
         pch=pch,
         cex=cex,
         col=col);
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
#' @family jam plot functions
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
#' nullPlot(doBoxes=FALSE)
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
 marginUnit=c("lines",
    "inches"),
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

   # validate arguments
   marginUnit <- match.arg(marginUnit);

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
         if ("lines" %in% marginUnit) {
            # lines
            Margins <- par("mar");
            MarginTerm <- "mar";
            OMargins <- par("oma");
            OMarginTerm <- "oma";
         } else {
            # inches
            Margins <- par("mai");
            MarginTerm <- "mai";
            OMargins <- par("omi");
            OMarginTerm <- "omi";
         }
         MarginsN <- Margins;
         # Margins <- substr(Margins, 5, nchar(Margins));
         # MarginsN <- as.numeric(unlist(strsplit(Margins, "[ ]+")));
         MarginsV <- format(MarginsN,
            nsmall=0,
            scientific=FALSE,
            digits=3);
         OMarginsV <- format(OMargins,
            nsmall=0,
            scientific=FALSE,
            digits=3);
         MarginsText <- paste0("  ", MarginTerm,
            "=c(", paste(MarginsV, collapse=", "), ")",
            plotNumPrefix);
         OMarginsText <- paste0("  ", OMarginTerm,
            "=c(", paste(OMarginsV, collapse=", "), ")",
            plotNumPrefix);
         if (plotSrt == 90) {
            plotLas <- 2;
         } else {
            plotLas <- 1;
         }
         if (MarginsN[3] < 1) {
            mtext(paste0("Margin", MarginsText),
               NORTH<-3,
               line=-1,
               cex=0.7,
               col="navy",
               las=plotLas,
               adj=plotLas-1);
         } else {
            mtext(paste0("Margin", MarginsText),
               SOUTH<-3,
               line=1,
               cex=0.7,
               col="navy",
               las=plotLas,
               adj=plotLas-1);
         }

         box("inner", lty="dotted", col="darkgreen", lwd=3);
         if (any(OMargins > 0)) {
            if (OMargins[1] >= 1) {
               mtext(paste0("Outer Margin Area", OMarginsText),
                  SOUTH<-1,
                  line=0,
                  adj=1.0,
                  cex=0.7,
                  col="darkgreen",
                  outer=TRUE,
                  las=plotLas);
            } else {
               mtext(paste0("Outer Margin Area", OMarginsText),
                  SOUTH<-1,
                  line=-1,
                  adj=1.0,
                  cex=0.7,
                  col="darkgreen",
                  outer=TRUE,
                  las=plotLas);
            }
         }
         box("outer", lty="solid", col="darkgreen");

         ## Text: vector of strings in mtext call
         lapply(1:4, function(i){
            if (MarginsV[i] > 0) {
               newLas <- as.integer(3 - i%%2*2);
               par("las"=newLas);
               mtext(paste0(MarginTerm, "[", i, "]",
                     plotNumPrefix, "=",
                     MarginsV[i]),
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
            if (OMarginsV[i] > 0) {
               newLas <- as.integer(3 - i%%2*2);
               par("las"=newLas);
               mtext(paste0("oma[", i, "]",
                     plotNumPrefix, "=",
                     OMarginsV[i]),
                  i,
                  adj=0.5,
                  padj=0,
                  line=-0.1,
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
#'
#' @family jam plot functions
#'
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
#' @family jam plot functions
#'
#' @seealso \code{\link[graphics]{image}}
#'
#' @examples
#' ps <- plotSmoothScatter(doTest=TRUE)
#'
#' @export
imageDefault <- function
(x=seq_len(nrow(z)+1)-0.5,
 y=seq_len(ncol(z)+1)-0.5,
 z,
 zlim=range(z[is.finite(z)]),
 xlim=range(x),
 ylim=range(y),
 col=grDevices::hcl.colors(12, "YlOrRd",  rev=TRUE),
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
 maxRatioFix=10,
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
            # 14dec2022
            y <- seq.int(0, 1, length.out=ncol(z));
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
         jamba::printDebug("imageDefault(): ",
            c("!missing(z)","is.list(x)"));
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
            c("!missing(z)","!is.list(x)"));
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
      if (verbose) {
         printDebug("imageDefault(): ",
            "redefined y using diff(y)/2",
            ", length(y):",
            length(y));
      }
   } else {
      if (verbose) {
         printDebug("imageDefault(): ",
            "did not redefine y using diff(y)/2",
            ", length(y):",
            length(y));
      }
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
      # remove check for R version < 3
      zi <- .bincode(x=z,
         breaks=breaks,
         right=TRUE,
         include.lowest=TRUE) - 1L;
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
   if (length(minRasterMultiple) == 0) {
      if (!is.null(rasterTarget)) {
         minRasterMultiple <- c(ceiling(rasterTarget[1]/ncol(z)),
            ceiling(rasterTarget[2]/nrow(z)));
      } else {
         minRasterMultiple <- c(1, 1);
      }
   } else {
      minRasterMultiple <- rep(minRasterMultiple, length.out=2);
   }
   if (verbose) {
      printDebug("minRasterMultiple:", minRasterMultiple);
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
         if (verbose) {
            printDebug("imageDefault(): ",
               c("Fixing the raster ratio."));
         }
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
            printDebug("imageDefault(): ",
               "dupColX: ",
               dupColX);
            printDebug("imageDefault(): ",
               "dupRowX: ",
               dupRowX);
         }

         ## Ensure that minRasterMultiple is applied
         dupColX <- min(c(maxRatioFix[2],
            max(c(dupColX, minRasterMultiple[2]))));
         dupRowX <- min(c(maxRatioFix[1],
            max(c(dupRowX, minRasterMultiple[1]))));

         if (verbose) {
            printDebug("imageDefault(): ",
               "maxRatioFix:",
               maxRatioFix);
            printDebug("imageDefault(): ",
               "dupColX: ",
               dupColX);
            printDebug("imageDefault(): ",
               "dupRowX: ",
               dupRowX);
         }
         newCols <- rep(1:(length(x)-1), each=dupColX);
         newRows <- rep(1:(length(y)-1), each=dupRowX);

         ## Column processing
         xNcolSeq <- seq(from=0.5,
            to=(length(x)-1)+0.5,
            length.out=length(newCols)+1);
         ## 29oct2018 modify so the x range is same as before
         xNcolSeq <- normScale(xNcolSeq,
            from=min(x),
            to=max(x));
         newColBreaks <- breaksByVector(newCols);
         newColLabels <- newColBreaks$newLabels;

         ## Row processing
         yNrowSeq <- seq(from=0.5, to=(length(y)-1)+0.5,
            length.out=length(newRows)+1);
         ## 29oct2018 modify so the x range is same as before
         yNrowSeq <- normScale(yNrowSeq,
            from=min(y),
            to=max(y));
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
         ## 14dec2022 modify so the y range is same as before
         # yNrowSeq <- normScale(yNrowSeq,
         #    from=min(y),
         #    to=max(y));

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
      rasterImage(image=as.raster(zc),
         xleft=min(x),
         ybottom=min(y),
         xright=max(x),
         ytop=max(y),
         interpolate=interpolate);
      return(invisible(list(
         x=x,
         y=y,
         zi=zi,
         col=col,
         zc=zc)))
   } else {
      # call image.default() and let it render non-rasterized output
      graphics::image.default(x=x,
         y=y,
         z=zi,
         col=col,
         add=TRUE,
         breaks=breaks,
         oldstyle=oldstyle,
         useRaster=FALSE,
         ...)
      # .External.graphics(graphics:::C_image, x, y, zi, col);
      return(invisible(list(
         x=x,
         y=y,
         zi=zi,
         col=col,
         zc=NULL)))
   }
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
#' @family jam plot functions
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
#' @param shadowOrder `character` value indicating when shadows are drawn
#'    relative to drawing labels: `"each"` draws each shadow with each label,
#'    so that shadows will overlap previous labels; `"all"` draws all shadows
#'    first then all labels, so labels will always appear above all
#'    shadows. See examples.
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
#' # example showing labels with overlapping shadows
#' opar <- par("mfrow"=c(1, 2))
#' nullPlot(doBoxes=FALSE);
#' title(main="shadowOrder='each'");
#' shadowText(x=c(1.5, 1.65), y=c(1.5, 1.55),
#'    labels=c("one", "two"), cex=c(2, 4), shadowOrder="each")
#' nullPlot(doBoxes=FALSE);
#' title(main="shadowOrder='all'");
#' shadowText(x=c(1.5, 1.65), y=c(1.5, 1.55),
#'    labels=c("one", "two"), cex=c(2, 4), shadowOrder="all")
#' par(opar)
#'
#' @export
shadowText <- function
(x,
 y=NULL,
 labels=NULL,
 col="white",
 bg=setTextContrastColor(col),
 r=getOption("jam.shadow.r", 0.15),
 offset=c(0.15, -0.15),
 n=getOption("jam.shadow.n", 8),
 outline=getOption("jam.outline", TRUE),
 alphaOutline=getOption("jam.alphaOutline", 0.4),
 shadow=getOption("jam.shadow", FALSE),
 shadowColor=getOption("jam.shadowColor", "black"),
 alphaShadow=getOption("jam.alphaShadow", 0.2),
 shadowOrder=c("each", "all"),
 cex=par("cex"),
 font=par("font"),
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
   shadowOrder <- match.arg(shadowOrder);

   if (length(alphaOutline) == 0) {
      alphaOutline <- 0.7;
      options("jam.alphaOutline"=0.7);
   }
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
      return(invisible(list(st1=st1, st2=st2, st3=st3)));
   }

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
   if ("each" %in% shadowOrder) {
      allX <- t(allX);
      allY <- t(allY);
      allColors <- t(allColors);
      allLabels <- t(allLabels);
      cex <- rep(cex, each=nrow(allX));
      font <- rep(font, each=nrow(allX));
   }
   #allCex <- rep(cex, n+1);
   #allFont <- rep(font, n+1);
   #allSrt <- rep(srt, n+1);

   ## Draw labels with one text() call to make it vectorized
   graphics::text(x=c(allX),
      y=c(allY),
      labels=c(allLabels),
      col=c(allColors),
      cex=cex,
      font=font,
      ...);
   return(invisible(list(allX=allX, allY=allY, allColors=allColors,
      allLabels=allLabels)));
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
#' @family jam plot functions
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
 cex=par("cex"),
 cex.axis=par("cex.axis"),
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
   cex_use <- cex * cex.axis;
   maxWidth <- max(
      strwidth(paste(prefix, x),
         units="inches",
         cex=cex_use) + 0.2,
      na.rm=TRUE);

   ## Make sure label margins are not more than 1/2 the figure size
   refMargin <- 2-(margin %% 2);
   parMaiNew <- min(c(maxWidth, parFin[refMargin]*maxFig));
   parMai[margin] <- parMaiNew;
   par("mai"=parMai);
   invisible(parMaiNew);
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
#' @family jam plot functions
#'
#' @param x `numeric` vector, or `numeric` matrix. When a matrix is
#'    provided, each column in the matrix is used as its own data source.
#' @param doHistogram `logical` indicating whether to plot histogram bars.
#' @param doPolygon `logical` indicating whether to plot the density polygon.
#' @param col `character` color, or when `x` is supplied as a matrix,
#'    a vector of colors is applied to across plot panels.
#'    Note that `col` will override all colors defined for `barCol`, `polyCol`,
#'    `histBorder`, `polyBorder`.
#' @param barCol,polyCol,polyBorder,histBorder `character` colors used
#'    when `col` is not supplied.
#'    They define colors for the histogram bars, polygon fill,
#'    polygon border, and histogram bar border, respectively.
#' @param colAlphas `numeric` vector with length 3, indicating the alpha
#'    transparency to use for histogram bar fill, polygon density fill,
#'    and border color, respectively.
#'    Alpha transparency should be scaled between 0 (fully transparent)
#'    and 1 (fully opaque).
#'    These alpha transparency values are applied to each color in `col`
#'    when `col` is defined.
#' @param darkFactors `numeric` used to adjust colors when `col` is defined.
#'    Values are applied to histogram bar fill, polygon density fill,
#'    and border color, respectively, by calling `makeColorDarker()`.
#' @param lwd `numeric` line width.
#' @param las `integer` used to define axis label orientation.
#' @param u5.bias,pretty.n `numeric` arguments passed to to `base::pretty()`
#'    to define pretty axis label positions.
#' @param bw `character` string of the bandwidth name to use in the
#'    density calculation, passed to `jamba::breakDensity()`.
#'    By default `stats::density()` calls a very smooth density kernel,
#'    which obscures finer details, so the default in
#'    `jamba::breakDensity()` uses a more detailed kernel.
#' @param breaks `numeric` breaks sent to `hist` to define the number of
#'    histogram bars. It can be in the form of a single `integer` number
#'    of equidistant breaks, or a `numeric` vector with specific break
#'    positions, but remember to include a starting value lower the the
#'    lowest value in `x`, and an ending value higher than the highest
#'    value in `x`. Passed to `breakDensity()`.
#' @param width `numeric` passed to `breakDensity()`.
#' @param densityBreaksFactor `numeric` scaling factor to control
#'    the level of detail in the density, passed to `breakDensity()`.
#' @param axisFunc `function` optionally used in place of `axis()` to define
#'    axis labels.
#' @param bty `character` string used to define the plot box shape,
#'    see `box()`.
#' @param cex.axis `numeric` scalar to adjust axis label font size.
#' @param doPar `logical` indicating whether to apply `par()`, specifically
#'    when `x` is supplied as a multi-column matrix. When `doPar=FALSE`,
#'    no panels nor margin adjustments are made at all.
#' @param heightFactor `numeric` value indicating the height of the y-axis
#'    plot scale to use when scaling the histogram and polygon density
#'    within each plot panel.
#' @param weightFactor `numeric` passed to `breakDensity()`.
#' @param main `character` title to display above the plot, used only when
#'    `x` is supplied as a single `numeric` vector. Otherwise each plot
#'    title uses the relevant `colnames(x)` value.
#' @param xaxs,yaxs `character` string indicating the type of x-axis and
#'    y-axis to render, see `par()`.
#' @param xlab,ylab `character` labels for x-axis and y-axis, respectively.
#' @param log `character` vector, optionally containing `"x"` and/or `"y"` to
#'    to indicate which axes are log-transformed. If `"x" %in% log`
#'    then it sets `xScale="log10"`, both methods are equivalent in
#'    defining the log-transformation of the x-axis.
#' @param xScale `character` string to define the x-axis transformation:
#'    * `"default"` applies no transform;
#'    * `"log10"` applies a log10 transform, specifically `log10(x + 1)`
#'    * `"sqrt"` applies a sqrt transform.
#' @param usePanels `logical` indicating whether to separate
#'    the density plots into panels when `x` contains multiple columns.
#'    When `useOnePanel=FALSE` the panels will be defined so that all
#'    columns will fit on one page.
#' @param useOnePanel `logical` indicating whether to define multiple panels
#'    on one page. Therefore `useOnePanel=TRUE` will create multiple
#'    pages with one panel on each page, which may work well for
#'    output in multi-page PDF files.
#' @param ablineV,ablineH `numeric` vector representing abline
#'    vertical and horizontal positions, respectively.
#'    These values are mostly helpful in multi-panel plots,
#'    to draw consistent reference lines on each panel.
#' @param ablineVlty,ablineHlty `numeric` or `character` indicating the
#'    line type to use for `ablineV` and `ablineH`, respectively.
#' @param removeNA `logical` indicating whether to remove NA values
#'    prior to running histogram and density calculations. Presence
#'    of NA values generally causes both functions to fail.
#' @param add `logical` indicating whether to add the plot to an existing
#'    visualization.
#' @param ylimQuantile `numeric` value between 0 and 1, indicating the
#'    quantile value of the density `y` values to use for the ylim. This
#'    threshold is only applied when `ylim` is NULL.
#' @param ylim,xlim `numeric` y-axis and x-axis ranges, respectively.
#'    When either is `NULL`, the axis range is determined independently
#'    for each plot panel. Either value can be supplied as a `list`
#'    to control the numeric range for each individual plot, relevant
#'    only when `x` is supplied as a multi-column matrix.
#' @param highlightPoints `character` vector of optional rownames,
#'    or `integer` values with row indices, for rows to be highlighted.
#'    When `x` is supplied as a `matrix`, `highlightPoints` can
#'    be supplied as a `list` of vectors, referring to each column in `x`.
#'    When rows are highlighted, the plot is drawn with all points,
#'    then the highlighted points are drawn again over the histogram bars,
#'    and polygon density, as relevant.
#' @param highlightCol `character` vector of colors to
#'    use to fill the histogram when `highlightPoints` is supplied.
#'    Multiple values are recycled one per column in `x`,
#'    if `x` is supplied as a multi-column matrix.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are passed to relevant internal
#'    functions.
#'
#' @examples
#' # basic density plot
#' set.seed(123);
#' x <- rnorm(2000);
#' plotPolygonDensity(x, main="basic polygon density plot");
#'
#' # fewer breaks
#' plotPolygonDensity(x,
#'    breaks=20,
#'    main="breaks=20");
#'
#' # log-scaled x-axis
#' plotPolygonDensity(10^(3+rnorm(2000)), log="x",
#'    breaks=50,
#'    main="log-scaled x-axis");
#'
#' # highlighted points
#' set.seed(123);
#' plotPolygonDensity(x,
#'    highlightPoints=sample(which(abs(x) > 1), size=200),
#'    breaks=40,
#'    main="breaks=40");
#'
#' @export
plotPolygonDensity <- function
(x,
 doHistogram=TRUE,
 doPolygon=TRUE,
 col=NULL,
 barCol="#00337799",
 polyCol="#00449977",
 polyBorder=makeColorDarker(polyCol),
 histBorder=makeColorDarker(barCol, darkFactor=1.5),
 colAlphas=c(0.8,0.6,0.9),
 darkFactors=c(-1.3, 1, 3),
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
 xlab="",
 ylab="",
 log=NULL,
 xScale=c("default","log10","sqrt"),
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
 xlim=NULL,
 highlightPoints=NULL,
 highlightCol="gold",
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
   if (igrepHas("matrix|data.*frame|tibble|data.table", class(x)) && ncol(x) > 1 && usePanels) {
      ## ablineV will include abline(s) in each panel
      if (!is.null(ablineV)) {
         ablineV <- rep(ablineV, length.out=ncol(x));
      }

      newMfrow <- decideMfrow(ncol(x));
      if (useOnePanel) {
         newMfrow <- c(1,1);
      }
      if (doPar) {
         origMfrow <- par("mfrow"=newMfrow);
      }
      if (length(barCol) == ncol(x)) {
         panelColors <- barCol;
      } else {
         if (check_pkg_installed("colorjam")) {
            panelColors <- colorjam::rainbowJam(ncol(x));
         } else {
            panelColors <- sample(unvigrep("gr[ae]y|white|black|[34]$", colors()),
               size=ncol(x));
         }
      }
      if (length(colnames(x)) == 0) {
         colnames(x) <- makeNames(rep("column", ncol(x)), suffix="_");
      }

      ## Handle highlightPoints
      if (length(highlightPoints) > 0) {
         if (!is.list(highlightPoints)) {
            highlightPoints <- list(highlightPoints);
         }
         if (length(highlightPoints) < ncol(x)) {
            highlightPoints <- rep(highlightPoints, length.out=ncol(x));
         }
         if (length(highlightCol) == 0) {
            highlightCol <- "gold";
         }
         highlightCol <- rep(highlightCol, length.out=ncol(x));
      }

      ## Get common set of breaks
      #hx <- hist(x, breaks=breaks, plot=FALSE, ...);
      #breaks <- hx$breaks;
      ## Iterate each column
      # recycle xlim if present
      if (length(xlim) > 0) {
         if (!is.list(xlim)) {
            xlim <- list(range(xlim, na.rm=TRUE));
         }
         xlim <- rep(xlim, length.out=ncol(x));
      } else {
         xlim <- NULL
      }
      # recycle ylim if present
      if (length(ylim) > 0) {
         if (!is.list(ylim)) {
            ylim <- list(range(ylim, na.rm=TRUE));
         }
         ylim <- rep(ylim, length.out=ncol(x));
      } else {
         ylim <- NULL
      }

      d1 <- lapply(nameVector(seq_len(ncol(x)), colnames(x)), function(i){
         if (useOnePanel) {
            add <- (i > 1);
            mainTitle <- "";
         } else {
            mainTitle <- colnames(x)[i];
         }
         xi <- x[,i];
         if (length(rownames(x)) > 0) {
            names(xi) <- rownames(x);
         }
         if (removeNA) {
            xi <- rmNA(xi);
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
            verbose=verbose,
            ablineV=ablineV,
            ablineVcol=ablineVcol,
            ablineVlty=ablineVlty,
            ablineH=ablineH,
            ablineHcol=ablineHcol,
            ablineHlty=ablineHlty,
            ylimQuantile=ylimQuantile,
            ylim=ylim[[i]],
            xlim=xlim[[i]],
            highlightPoints=highlightPoints[[i]],
            highlightCol=highlightCol[[i]],
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
         par(newMfrow);
      }
      invisible(d1);
   } else {
      ##
      oPar <- par("xaxs"=xaxs, "yaxs"=yaxs);
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
         barCol <- makeColorDarker(col,
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
         x <- log10(abs(x) + 1) * sign(x);
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
            hx <- call_fn_ellipsis(hist.default,
               x=x,
               breaks=breaks,
               col=barCol,
               main=main,
               border=histBorder,
               xaxt="n",
               yaxt="n",
               las=las,
               # xlab="",
               ylab="",
               cex.axis=cex.axis*0.8,
               add=add,
               ...);
         } else {
            hx <- call_fn_ellipsis(hist.default,
               x=x,
               breaks=breaks,
               # xlab=xlab,
               #col=barCol,
               #main=main,
               #border=histBorder,
               #xaxt="n",
               #las=las,
               #ylab="",
               #cex.axis=cex.axis*0.8,
               #add=add,
               plot=FALSE,
               ...);
            ## Optionally define the y-axis scale
            if (length(ylim) == 0 &&
               length(ylimQuantile) > 0 &&
               ylimQuantile < 1 &&
               ylimQuantile > 0 &&
               max(hx$counts) > 0) {
               ylim <- c(0,
                  quantile(hx$counts, c(ylimQuantile)));
            }
            if (length(xlim) == 0) {
               xlim <- range(hx$breaks, na.rm=TRUE);
            }
            plot(hx,
               col=barCol,
               main=main,
               border=histBorder,
               xaxt="n",
               las=las,
               ylab=ylab,
               xlab=xlab,
               cex.axis=cex.axis*0.8,
               add=add,
               ylim=ylim,
               xlim=xlim,
               ...);
         }

         if (verbose) {
            printDebug("plotPolygonDensity(): ",
               "hx$breaks:",
               format(trim=TRUE,
                  digits=2,
                  c(head(hx$breaks),
                     NA,
                     tail(hx$breaks))));
         }
         if (!add) {
            if ("log10" %in% xScale) {
               if (verbose) {
                  printDebug("plotPolygonDensity(): ",
                     "log10 x-axis scale.");
               }
               minorLogTicksAxis(1,
                  doMinorLabels=TRUE,
                  logBase=10,
                  displayBase=10,
                  offset=1,
                  logAxisType=xLogAxisType,
                  ...);
            } else if ("sqrt" %in% xScale) {
               if (verbose) {
                  printDebug("plotPolygonDensity(): ",
                     "sqrt xScale");
               }
               atPretty <- sqrtAxis(side=1,
                  plot=FALSE);
               axisFunc(1,
                  at=atPretty,
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
      if (doPolygon) {
         dx <- breakDensity(x=x,
            bw=bw,
            width=width,
            breaks=breaks,
            densityBreaksFactor=densityBreaksFactor,
            weightFactor=weightFactor,
            addZeroEnds=TRUE,
            verbose=verbose,
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
            xout <- (head(hx$breaks, -1) + tail(hx$breaks, -1))/2;
            xu <- match(unique(dx$x), dx$x);
            dy <- approx(x=dx$x[xu], y=dx$y[xu], xout=xout)$y;
            dScale <- median(hx$counts[hx$counts > 0] / dy[hx$counts > 0]);
            dx$y <- dx$y * dScale;
            #dx$y <- normScale(dx$y,
            #   from=0,
            #   to=maxHistY*heightFactor);
         }
         if (verbose) {
            printDebug("plotPolygonDensity(): ",
               "Completed density calculations.");
         }
      } else {
         dx <- NULL;
      }
      if (!doHistogram) {
         plot(dx,
            col="transparent",
            main=main,
            xaxt="n",
            ...);
         if ("log10" %in% xScale) {
            minorLogTicksAxis(1,
               logBase=10,
               displayBase=10,
               offset=1,
               doMinorLabels=TRUE,
               logAxisType=xLogAxisType,
               ...);
         } else if ("sqrt" %in% xScale) {
            if (verbose) {
               printDebug("plotPolygonDensity(): ",
                  "sqrt xScale");
            }
            atPretty <- sqrtAxis(side=1,
               plot=FALSE);
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
         polygon(dx,
            col=polyCol,
            border=polyBorder,
            lwd=lwd,
            ...);
      }

      ## Optionally plot highlightPoints
      if (doHistogram) {
         if (length(highlightPoints) > 0) {
            if (verbose) {
               printDebug("plotPolygonDensity(): ",
                  "Plotting highlightPoints.");
            }
            hxh <- call_fn_ellipsis(hist.default,
               x=x[highlightPoints],
               breaks=hx$breaks,
               col=highlightCol,
               main="",
               border=makeColorDarker(highlightCol),
               xaxt="n",
               yaxt="n",
               ylab="",
               # xlab="",
               add=TRUE,
               ...);
         }
      }

      if (!is.null(ablineV)) {
         abline(v=ablineV, col=ablineVcol, lty=ablineVlty, ...);
      }
      if (!is.null(ablineH)) {
         abline(h=ablineH, col=ablineHcol, lty=ablineHlty, ...);
      }
      if (doPar) {
         par(oPar);
      }
      invisible(list(d=dx,
         hist=hx,
         barCol=barCol,
         polyCol=polyCol,
         polyBorder=polyBorder,
         histBorder=histBorder));
   }
}

#' Determine square root axis tick mark positions
#'
#' Determine square root axis tick mark positions
#'
#' This function calculates positions for tick marks for data
#' that has been transformed with `sqrt()`, specifically a directional
#' transformation like `sqrt(abs(x)) * sign(x)`.
#'
#' The main goal of this function is to provide reasonably placed
#' tick marks using integer values.
#'
#' @return
#' Invisibly returns a numeric vector of axis tick positions,
#' named by the display label.
#' The axis values are in square root space while the labels represent
#' the normal space values.
#'
#' @family jam plot functions
#'
#' @param side integer value indicating the axis position, as used
#'    by `axis()`, 1=bottom, 2=left, 3=top, 4=right.
#' @param x optional numeric vector representing the numeric range
#'    to be labeled.
#' @param pretty.n numeric value indicating the number of desired
#'    tick marks, passed to `pretty()`.
#' @param u5.bias numeric value passed to `pretty()` to influence the
#'    frequency of intermediate tick marks.
#' @param big.mark character value passed to `format()` which helps
#'    visually distinguish numbers larger than 1000.
#' @param plot logical indicating whether to plot the axis tick
#'    marks and labels.
#' @param las,cex.axis numeric values passed to `axis()` when drawing
#'    the axis, by default `las=2` plots labels rotated
#'    perpendicular to the axis.
#' @param ... additional parameters are passed to `pretty()`.
#'
#' @export
sqrtAxis <- function
(side=1,
 x=NULL,
 pretty.n=10,
 u5.bias=0,
 big.mark=",",
 plot=TRUE,
 las=2,
 cex.axis=0.6,
 ...)
{
   ## Purpose is to generate a set of tick marks for sqrt
   ## transformed data axes.  It assumes data is already sqrt-transformed,
   ## and that negative values have been treated like:
   ## sqrt(abs(x))*sign(x)
   if (length(side) > 2) {
      x <- side;
      side <- 0;
   }
   if (length(side) == 0) {
      side <- 0;
   }
   if (1 %in% side) {
      xRange <- par("usr")[1:2];
   } else if (2 %in% side) {
      xRange <- par("usr")[3:4];
   } else if (length(x) > 0) {
      xRange <- range(x, na.rm=TRUE);
   }

   subdivideSqrt <- function(atPretty1, n=pretty.n, ...) {
      ## Purpose is to take x in form of 0,x1,
      ## and subdivide using pretty()
      atPretty1a <- unique(sort(abs(atPretty1)));
      atPretty1b <- tail(atPretty1a, -2);
      atPretty2a <- pretty(head(atPretty1a,2), n=n, ...);
      return(unique(sort(c(atPretty2a, atPretty1b))));
   }

   ## Determine tick positions
   nSubFactor <- 2.44;

   atPretty1 <- pretty(xRange^2*sign(xRange),
      u5.bias=u5.bias,
      n=(pretty.n)^(1/nSubFactor));

   atPretty1old <- atPretty1;
   while (length(atPretty1) <= pretty.n) {
      atPretty1new <- subdivideSqrt(atPretty1,
         n=noiseFloor(minimum=2, (pretty.n)^(1/nSubFactor)));
      atPretty1 <- atPretty1new[atPretty1new <= max(abs(xRange^2))];
      atPretty1old <- atPretty1;
   }
   atPretty3 <- unique(sort(
      rep(atPretty1,
         each=length(unique(sign(xRange)))) * sign(xRange)));
   atPretty <- atPretty3[
      (atPretty3 >= head(xRange,1)^2*sign(head(xRange,1)) &
       atPretty3 <= tail(xRange, 1)^2*sign(tail(xRange, 1)))];

   xLabel <- sapply(atPretty, function(i){
      format(i,
         trim=TRUE,
         digits=2,
         big.mark=big.mark);
   });
   ## Transform to square root space
   atSqrt <- sqrt(abs(atPretty))*sign(atPretty);
   if (plot) {
      axis(side=side,
         at=atSqrt,
         labels=xLabel,
         las=las,
         cex.axis=cex.axis,
         ...);
   }

   invisible(nameVector(atSqrt, xLabel));
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
#' @family jam practical functions
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
   if (length(weightFactor) > 0) {
      weightFactor <- rep(weightFactor, length.out=length(x));
   }

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
         weight=weightFactor,
         ...);
   } else {
      dx <- density(x,
         width=width,
         weight=weightFactor,
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

#' Display major and minor tick marks for log-scale axis
#'
#' Display major and minor tick marks for log-scale axis,
#' with optional offset for proper labeling of `log2(1+x)`.
#'
#' This function displays log units on the axis of an
#' existing base R plot. It calls `jamba::minorLogTicks()` which
#' calculates appropriate tick and label positions.
#'
#' Note: This function assumes the axis values have already been
#' log-transformed. Make sure to adjust the `offset` to reflect
#' the method of log-transformation, for example:
#'
#' * `log2(1+x)` would require `logBase=2` and `offset=1` in order
#' to represent values properly at or near zero.
#' * `log(0.5+x)` would require `logBase=exp(1)` and `offset=0.5`.
#' * `log10(x)` would require `logBase=10` and `offset=0`.
#'
#' The defaults `logBase=2` and `displayBase=10` assume data
#' has been log2-transformed, and displays tick marks using the
#' common base of 10. To display tick marks at two-fold intervals,
#' use `displayBase=2`.
#'
#' This function was motivated in order to label log-transformed
#' data properly in some special cases, like using `log2(1+x)`
#' where the resulting values are shifted "off by one" using
#' standard log-scaled axis tick marks and labels.
#'
#' For log fold changes, set `symmetricZero=TRUE`, which will
#' create negative log scaled fold change values as needed for
#' negative values. For example, this option would label a
#' `logBase=2` value of `-2` as `-4` and not as `0.25`.
#'
#' Note that by default, whenever `offset > 0` the argument
#' `symmetricZero=TRUE` is also defined, since a negative value in
#' that scenario has little meaning. This behavior can be turned
#' off by setting `symmetricZero=FALSE`.
#'
#' @return
#' A list with vectors of majorLabels, majorTicks, minorLabels,
#' minorTicks, and allLabelsDF which is a `data.frame` containing
#' all axis tick positions, with corresponding labels.
#'
#' @family jam plot functions
#'
#' @param side integer indicating the axis side, 1=bottom, 2=left,
#'    3=top, 4=right.
#' @param lims NULL or numeric range for which the axis tick marks
#'    will be determined. If NULL then the corresponding `par("usr")`
#'    will be used.
#' @param logBase numeric value indicating the log base units, which
#'    will be used similar to how `base` is used in `log(x, base)`.
#' @param displayBase numeric value indicating the log base units to
#'    use when determining the numeric label position. For example,
#'    data may be log2 scaled, and yet it is visually intuitive to
#'    show log transformed axis units in base 10 units. See examples.
#' @param offset numeric offset used in transforming the
#'    numeric data displayed on this axis. For example, a common
#'    technique is to transform data using `log2(1+x)` which adds
#'    `1` to values prior to the log2 transformation. In this case,
#'    `offset=1`, which ensures the axis labels exactly
#'    match the initial numeric value prior to the log2 transform.
#' @param symmetricZero logical indicating whether numeric values
#'    are symmetric around zero. For example, log fold changes should
#'    use `symmetricZero=TRUE` which ensures a log2 value of `-2` is
#'    labeled `-4` to indicate a negative four fold change. If
#'    `symmetricZero=FALSE` a log2 value of `-2` would be labeled
#'    `0.0625`.
#' @param padj numeric vector length 2, which is used to position
#'    axis labels for the minor and major labels, respectively. For
#'    example, `padj=c(0,1)` will position minor labels just to the
#'    left of the tick marks, and major labels just to the right
#'    of tick marks. This example is helpful when minor labels bunch
#'    up on the right side of each section.
#' @param doFormat logical indicating whether to apply `base::format()` to
#'    format numeric labels.
#' @param big.mark,scipen parameters passed to `base::format()` when
#'    `doFormat=TRUE`.
#' @param minorWhich integer vector indicating which of the minor tick
#'    marks should be labeled. Labels are generally numbered from `2`
#'    to `displayBase-1`. So by default, log 10 units would add
#'    minor tick marks and labels to the `c(2,5)` position. For log2
#'    units only, the second label is defined at 1.5, which shows
#'    minor labels at `c(3, 6, 12)`, which are `1.5 * c(2, 4, 8)`.
#' @param minorLogTicksData a list object created by running
#'    `jamba::minorLogTicks()`, which allows inspecting and modifying
#'    the content for custom control.
#' @param majorCex,minorCex the base text size factors, relative
#'    to cex=1 for default text size. These factors are applied in
#'    addition to existing `par("cex")` values, preserving any
#'    global text size defined there.
#' @param cex,col,col.ticks,las parameters used for axis label size,
#'    axis label colors,
#'    axis tick mark colors, and label text orientation, respectively.
#' @param verbose logical indicating whether to print verbose output.
#'
#' @examples
#' plotPolygonDensity(0:100, breaks=100);
#'
#' plotPolygonDensity(0:100, breaks=100, log="x",
#'    main="plotPolygonDensity() uses minorLogTicksAxis()",
#'    xlab="x (log-scaled)");
#'
#' plotPolygonDensity(log2(1+0:100), breaks=100,
#'    main="manually called minorLogTicksAxis(logBase=2)",
#'    xaxt="n",
#'    xlab="x (log-scaled)");
#' minorLogTicksAxis(1, offset=1, logBase=2);
#'
#' plotPolygonDensity(log10(1+0:100), breaks=100,
#'    main="manually called minorLogTicksAxis(logBase=10)",
#'    xaxt="n",
#'    xlab="x (log-scaled)");
#' minorLogTicksAxis(1, offset=1, logBase=10);
#'
#' plotPolygonDensity(log10(1+0:100), breaks=100,
#'    main="using 'minorWhich=2:9'",
#'    xaxt="n",
#'    xlab="x (log-scaled)");
#' minorLogTicksAxis(1, offset=1, logBase=10,
#'    minorWhich=2:9);
#'
#' @export
minorLogTicksAxis <- function
(side=NULL,
 lims=NULL,
 logBase=2,
 displayBase=10,
 offset=0,
 symmetricZero=(offset > 0),
 majorCex=1,
 minorCex=0.65,
 doMajor=TRUE,
 doLabels=TRUE,
 doMinorLabels=TRUE,
 asValues=TRUE,
 padj=NULL,
 doFormat=TRUE,
 big.mark=",",
 scipen=10,
 minorWhich=c(2,5),
 logStep=1,
 cex=1,
 las=2,
 col="black",
 col.ticks=col,
 minorLogTicksData=NULL,
 verbose=FALSE,
 ...)
{
   ## Kindly posted by Joris Meys on Stack Overflow, adapted for use in log axes
   ## http://stackoverflow.com/questions/6955440/displaying-minor-logarithmic-ticks-in-x-axis-in-r
   ##
   ## padj can take two values, for the minor and major ticks, respectively,
   ## and is recycled if too short.
   ##
   ## padj <- c(0,1) will align minor tick labels just to the left of the ticks, and major
   ## labels just to the right, since log-scaled labels tend to bunch up on the left side
   ## of each major label
   ##
   ## To define a set of minor tick positions, send a list object minorLogTicksData
   ## with (majorTicks, majorLabels, minorTicks, minorLabels)
   if (is.null(padj)) {
      if (side %in% c(1, 3)) {
         padj <- c(0.3,0.7);
      } else {
         padj <- c(0.7,0.3);
      }
   } else {
      padj <- rep(padj, length.out=2);
   }

   if (!is.null(minorLogTicksData)) {
      mlt <- minorLogTicksData;
   } else {
      mlt <- minorLogTicks(side=side,
         lims=lims,
         logBase=logBase,
         displayBase=displayBase,
         offset=offset,
         symmetricZero=symmetricZero,
         minorWhich=minorWhich,
         logStep=logStep,
         asValues=asValues,
         verbose=verbose,
         ...);
   }
   majorTicks <- mlt$majorTicks;
   majorLabels <- mlt$majorLabels;
   minorTicks <- mlt$minorTicks;
   minorLabels <- mlt$minorLabels;

   ## Optionally format numbers, mostly to add commas per thousands place
   NAmajor <- is.na(majorLabels);
   NAminor <- is.na(minorLabels);
   if (doFormat) {
      if (verbose) {
         printDebug("minorLogTicksAxis(): ",
            "Formatting numerical labels.");
      }
      if (is.numeric(scipen)) {
         scipenO <- getOption("scipen");
         options("scipen"=scipen);
      }
      majorLabels <- sapply(majorLabels,
         format,
         big.mark=big.mark,
         trim=TRUE,
         ...);
      minorLabels <- sapply(minorLabels,
         format,
         big.mark=big.mark,
         trim=TRUE,
         ...);
      if (is.numeric(scipen)) {
         options("scipen"=scipenO);
      }
   }
   if (any(NAmajor)) {
      majorLabels[NAmajor] <- "";
   }
   if (any(NAminor)) {
      minorLabels[NAminor] <- "";
   }

   ## By default display the major tick labels
   if (doMajor && length(majorTicks) > 0) {
      if (!doLabels) {
         majorLabels <- FALSE;
      }
      axis(side,
         at=majorTicks,
         tcl=par("tcl")*majorCex*cex,
         labels=majorLabels,
         padj=padj[2],
         cex.axis=majorCex*cex,
         col="transparent",
         col.ticks=col.ticks,
         las=las,
         ...);
   }
   if (!doMinorLabels) {
      minorLabels <- FALSE;
   }
   axis(side,
      at=minorTicks,
      tcl=par("tcl")*minorCex*cex,
      labels=minorLabels,
      padj=padj[1],
      cex.axis=minorCex*cex,
      col="transparent",
      col.ticks=col.ticks,
      las=las,
      ...);
   axis(side,
      at=range(c(majorTicks, minorTicks)),
      labels=FALSE,
      col=col,
      col.ticks="transparent",
      ...);

   invisible(mlt);
}

#' Calculate major and minor tick marks for log-scale axis
#'
#' Calculate major and minor tick marks for log-scale axis
#'
#' This function calculates log units for the axis of an
#' existing base R plot. It
#' calculates appropriate tick and label positions for major
#' steps, which are typically in log steps; and minor steps, whic
#' are typically a subset of steps at one lower log order.
#' For example, log 10 steps would be: `c(1, 10, 100, 1000)`,
#' and minor steps would be `c(2, 5, 20, 50, 200, 500, 2000, 5000)`.
#'
#' This function was motivated in order to label log-transformed
#' data properly in some special cases, like using `log2(1+x)`
#' where the resulting values are shifted "off by one" using
#' standard log-scaled axis tick marks and labels.
#'
#' Also, when using log fold change values, this function
#' creates axis labels which indicate negative fold change
#' values, for example `-2` in log2 fold change units would
#' be labeled with fold change `-4`, and not `0.0625` which
#' represents a fractional value.
#'
#' Use the argument `symmetricZero=TRUE` when using directional
#' log fold change values.
#'
#' @return
#' List of axis tick positions, and corresponding labels, for major
#' and minor ticks. Major ticks are defined as one tick per log10
#' unit, exponentiated. For example, 1, 10, 100, 1000.
#'
#' @family jam practical functions
#'
#' @examples
#' ## This example shows how to draw axis labels manually,
#' ## but the function minorLogTicksAxis() is easier to use.
#' xlim <- c(0,4);
#' nullPlot(xlim=xlim, doMargins=FALSE);
#' mlt <- minorLogTicks(1,
#'    logBase=10,
#'    offset=1,
#'    minTick=0);
#' maj <- subset(mlt$allLabelsDF, type %in% "major");
#' axis(1, las=2,
#'    at=maj$tick, label=maj$text);
#' min <- subset(mlt$allLabelsDF, type %in% "minor");
#' axis(1, las=2, cex.axis=0.7,
#'    at=min$tick, label=min$text,
#'    col="blue");
#' text(x=log10(1+c(0,5,50,1000)), y=rep(1.7, 4),
#'    label=c(0,5,50,1000), srt=90);
#'
#' nullPlot(xlim=c(-4,10), doMargins=FALSE);
#' axis(3, las=2);
#' minorLogTicksAxis(1, logBase=2, displayBase=10, symmetricZero=TRUE);
#'
#' nullPlot(xlim=c(-4,10), doMargins=FALSE);
#' axis(3, las=2);
#' minorLogTicksAxis(1, logBase=2, displayBase=10, offset=1);
#' x2 <- rnorm(1000) * 40;
#' d2 <- density(log2(1+abs(x2)) * ifelse(x2<0, -1, 1));
#' lines(x=d2$x, y=normScale(d2$y)+1, col="green4");
#'
#' nullPlot(xlim=c(0,10), doMargins=FALSE);
#' axis(3, las=2);
#' minorLogTicksAxis(1, logBase=2, displayBase=10, offset=1);
#' x1 <- c(0, 5, 15, 200);
#' text(y=rep(1.0, 4), x=log2(1+x1), label=x1, srt=90, adj=c(0,0.5));
#' points(y=rep(0.95, 4), x=log2(1+x1), pch=20, cex=2, col="blue");
#'
#' @param side integer value indicating which axis to produce tick
#'    marks, 1=bottom, 2=left, 3=top, 4=right.
#' @param lims numeric vector length=2, indicating specific numeric
#'    range to use for tick marks.
#' @param logBase numeric value indicating the logarithmic base, assumed
#'    to be applied to the numeric `lims` limits, or the axis range,
#'    previously.
#' @param displayBase numeric value indicating the base used to position
#'    axis labels, typically `displayBase=10` is used to draw labels
#'    at typical positions.
#' @param logStep integer value indicating the number of log steps
#'    between major axis label positions. Typically `logStep=1` will
#'    draw a label every log position based upon `displayBase`, for
#'    example `displayBase=10` and `logStep=1` will use `c(1,10,100,1000)`;
#'    and `displayBase=10` and `logStep=2` would use `c(1,100,10000)`.
#' @param minorWhich integer vector of values to label, where those
#'    integer values are between 1 and `displayBase`, for example
#'    `displayBase=10` may label only `c(2,5)`, which implies minor
#'    tick labels at `c(2, 5, 20, 50, 200, 500)`. Any minor labels
#'    which would otherwise equal a major tick position are removed.
#'    By default, when `displayBase=2`, `minorWhich=c(1.5)` which has the
#'    effect of drawing one minor label between each two-fold
#'    major tick label.
#' @param asValues logical indicating whether to create exponentiated
#'    numeric labels. When `asValues=FALSE`, it creates `expression` objects
#'    which include the exponential value. Use `asValues=FALSE` and
#'    `logAxisType="pvalue"` to draw P-value labels.
#' @param offset numeric value added during log transformation, typically
#'    of the form `log(1 + x)` where `offset=1`. The offset is used to
#'    determine the accurate numeric label such that values of `0` are
#'    properly labeled by the original numeric value.
#' @param symmetricZero logical indicating whether numeric values
#'    are symmetric around zero. For example, log fold changes should
#'    use `symmetricZero=TRUE` which ensures a log2 value of `-2` is
#'    labeled `-4` to indicate a negative four fold change. If
#'    `symmetricZero=FALSE` a log2 value of `-2` would be labeled
#'    `0.0625`.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional parameters are ignored.
#'
#' @export
minorLogTicks <- function
(side=NULL,
 lims=NULL,
 logBase=2,
 displayBase=10,
 logStep=1,
 minorWhich=c(2,5),
 asValues=TRUE,
 offset=0,
 symmetricZero=(offset>0),
 col="black",
 col.ticks=col,
 combine=FALSE,
 logAxisType=c("normal", "flipped", "pvalue"),
 verbose=FALSE,
 ...)
{
   ## Kindly posted by Joris Meys on Stack Overflow, adapted for use in log axes
   ## http://stackoverflow.com/questions/6955440/displaying-minor-logarithmic-ticks-in-x-axis-in-r
   ## This function simply returns the tick mark positions.
   ##
   ## minTick and maxTick (optional) simply restrict the range of values returned.
   ##
   ## Returns a list of majorTicks, minorTicks, majorLabels, and minorLabels.
   ##
   ## logAxisType="flipped" will flip negative values so they are like fold changes, e.g.
   ## "-1" will become "-10" instead of "0.1"
   ##
   if (length(offset) == 0) {
      offset <- 0;
   }
   offset <- head(offset, 1);

   if (logStep > 1) {
      minorWhich <- c(1);
   }

   if (length(lims) == 0) {
      if (length(side) == 0) {
         stop("minorLogTicks requires either axis (which axis), or lims (range of values) to be defined.");
      }
      lims <- par("usr");
      if(side %in% c(1,3)) {
         lims <- lims[1:2];
      } else {
         lims <- lims[3:4];
      }
   } else {
      lims <- range(lims);
   }
   logAxisType <- match.arg(logAxisType);
   ## Now set the floor and raise the roof to the nearest integer at or
   ## just beyond the given range of values.
   lims <- c(floor(lims[1]), ceiling(lims[2]));
   if (verbose) {
      printDebug("minorLogTicks(): ",
         "lims:",
         format(lims, digits=3, scientific=FALSE, trim=TRUE));
   }

   ## Define integer sequence of steps
   ## Define the intended labels based upon integer sequence in log units
   ## (prior to adjustments with offset)
   if (displayBase != logBase) {
      if (verbose) {
         printDebug("minorLogTicks(): ",
            "adjusting logBase to displayBase.");
      }
      displayLims1 <- c(logBase^abs(lims[1])*ifelse(lims[1] < 0, -1, 1),
         logBase^abs(lims[2])*ifelse(lims[2] < 0, -1, 1));
      displayLims2 <- (
         log(offset + abs(displayLims1),
            base=displayBase) *
         ifelse(displayLims1 < 0, -1, 1));
      displayLims <- c(floor(displayLims2[1]), ceiling(displayLims2[2]));
      majorTicks <- seq(from=displayLims[1],
         to=displayLims[2],
         by=logStep);
      #logBase <- displayBase;
   } else {
      majorTicks <- seq(from=lims[1], to=lims[2], by=1);
   }
   majorLabels <- sapply(majorTicks, function(i) {
      iX <- getAxisLabel(i,
         asValues,
         logAxisType,
         logBase=displayBase,
         offset=offset,
         symmetricZero=symmetricZero);
      iX;
   });
   ## majorLabels represents the numeric value associated with each
   ## axis position desired
   ##
   ## However, when offset == 1, it means the actual axis
   ## position for value=10 was calculated using log10(10+1),
   ## which slightly shifts the actual axis position to the right.
   ## Therefore, in that case we must re-calculate majorTicks using
   ## the new axis space.
   if (offset > 0 || symmetricZero) {
      if (verbose) {
         printDebug("minorLogTicks(): ",
            "adjusted axis position for log base ",
            logBase,
            " labels using offset:",
            offset);
         printDebug("minorLogTicks(): ",
            "majorTicks:",
            format(digits=2, trim=TRUE, majorTicks));
      }
      if (any(majorLabels < 0) && any(majorLabels) > 0) {
         if (verbose) {
            printDebug("minorLogTicks(): ",
               "Included zero with majorLabels since offset is non-zero");
         }
         majorLabels <- sort(unique(c(majorLabels, -1, 0)));
      }
      if (symmetricZero) {
         iUse <- noiseFloor(abs(majorLabels) + offset,
            minimum=1);
         majorTicks <- (log(iUse, base=logBase) *
               ifelse(majorLabels < 0, -1, 1));
      } else {
         majorTicks <- log(abs(majorLabels) + offset,
            base=logBase) * ifelse(majorLabels < 0, -1, 1);
      }
   } else {
      majorTicks <- log(majorLabels + offset, base=logBase);
   }
   majorLabelsDF <- data.frame(label=majorLabels,
      type="major",
      use=TRUE,
      tick=majorTicks);
   ## Confirm that the labels are unique
   cleanLTdf <- function(df) {
      df <- df[rev(seq_len(nrow(df))),,drop=FALSE];
      df <- subset(df, !(is.infinite(tick) | is.na(tick)));
      df <- df[match(unique(df$label), df$label),,drop=FALSE];
      df <- df[match(unique(df$tick), df$tick),,drop=FALSE];
      df <- df[rev(seq_len(nrow(df))),,drop=FALSE];
      df;
   }
   majorLabelsDF <- cleanLTdf(majorLabelsDF);
   majorTicks <- majorLabelsDF$tick;
   majorLabels <- majorLabelsDF$majorLabels;
   if (verbose) {
      printDebug("minorLogTicks(): ",
         "majorLabels:",
         majorLabels);
      printDebug("minorLogTicks(): ",
         "majorTicks:",
         format(digits=2, trim=TRUE, majorTicks));
      print(majorLabelsDF);
   }

   ## Define the minor Ticks by the first two values from pretty()
   if (displayBase == 2) {
      minorSet <- c(`2`=1.5);
   } else {
      minorSet <- setdiff(
         seq(from=1, to=displayBase, length.out=displayBase),
         c(1, displayBase));
      names(minorSet) <- nameVector(minorSet);
   }
   if (length(minorWhich) > 0) {
      ## Make sure minorWhich is contained in minorSet
      minorWhich <- minorSet[names(minorSet) %in% as.character(minorWhich) |
            minorSet %in% minorWhich];
   } else {
      minorWhich <- minorSet;
   }
   if (verbose) {
      printDebug("minorLogTicks(): ",
         "minorSet:",
         minorSet);
      printDebug("minorLogTicks(): ",
         "minorWhich:",
         minorWhich);
   }

   ## Calculate minor labels
   minorLabelsDF <- as.data.frame(rbindList(
      lapply(majorTicks, function(i){
         if (verbose) {
            printDebug("minorLogTicks(): ",
               "Calculating minor ticks based upon majorTick:",
               i);
         }
         if ((offset > 0 && i < 0) ||
               symmetricZero ||
               igrepHas("flip", logAxisType)) {
            iBase <- logBase^i - offset;
            iBaseAbs <- (logBase^abs(i) - offset) * ifelse(i < 0, -1, 1);
            if (iBase == 0 ||
                  (symmetricZero && i == 0)) {
               iSeries <- unique(sort(c(-1 * minorSet * iBase,
                  minorSet *iBase)));
               iSeriesLab <- unique(sort(c(-1 * minorWhich * iBase,
                  minorWhich * iBase)));
               if (verbose) {
                  printDebug("   1 iSeries:",
                     format(scientific=FALSE, trim=TRUE, iSeries));
                  printDebug("   1 iSeriesLab:",
                     format(scientific=FALSE, trim=TRUE, iSeriesLab));
               }
            } else {
               iSeries <- unique(sort(minorSet * iBaseAbs));
               iSeriesLab <- unique(sort(minorWhich * iBaseAbs));
               if (iBase < 0) {
                  iSeries <- rev(iSeries);
               }
               if (offset == 0 &&
                     symmetricZero &&
                     iBase == 1) {
                  if (verbose) {
                     printDebug("   inserted positive/negative values:",
                        format(scientific=FALSE, trim=TRUE, iSeries));
                  }
                  iSeries <- iSeries[iSeries >= 1];
                  iSeries <- sort(unique(c(iSeries, -1*iSeries)));
                  iSeriesLab <- iSeriesLab[iSeriesLab >= 1];
                  iSeriesLab <- sort(unique(c(iSeriesLab, -1*iSeriesLab)));
               }
               if (verbose) {
                  printDebug("   2 iSeries:",
                     format(scientific=FALSE, trim=TRUE, iSeries));
                  printDebug("   2 iSeriesLab:",
                     format(scientific=FALSE, trim=TRUE, iSeriesLab));
               }
            }
            iSet <- unique(log(abs(iSeries), base=logBase)*ifelse(sign(iSeries)<0,-1,1));
         } else {
            iBase <- logBase^i - offset;
            iSeries <- unique(sort(minorSet * iBase));
            iSeriesLab <- unique(sort(minorWhich * iBase));
            iSet <- log(iSeries, base=logBase);
            if (verbose) {
               printDebug("   3 iSeries:",
                  format(scientific=FALSE, trim=TRUE, iSeries));
            }
         }
         data.frame(label=iSeries,
            type="minor",
            use=(iSeries %in% iSeriesLab));
      })
   ));
   ## Remove any minor labels which overlap major labels
   minorLabelsDF <- minorLabelsDF[!minorLabelsDF$label %in% majorLabelsDF$label,,drop=FALSE];
   #minorLabelsUse <- minorLabelsAll[minorLabelsAll[,"label"],"series"];

   ## Calculate minor ticks
   if (offset > 0 ||
         symmetricZero ||
         igrepHas("flip", logAxisType)) {
      #minorTicksAll <- (log(abs(minorLabels)+offset, logBase) *
      #      ifelse(minorLabels < 0, -1, 1));
      minorTicksAll <- (log(abs(minorLabelsDF$label)+offset, logBase) *
            ifelse(minorLabelsDF$label < 0, -1, 1));
      minorLabelsDF$tick <- minorTicksAll;
   } else if (igrepHas("flip", logAxisType)) {
      minorTicksAll <- (log(abs(minorLabelsDF$label)+offset, logBase) *
            ifelse(minorLabelsDF$label < 0, -1, 1));
      minorLabelsDF$tick <- minorTicksAll;
   } else {
      minorTicksAll <- log(minorLabelsDF$label+offset, logBase);
      minorLabelsDF$tick <- minorTicksAll;
   }
   minorLabelsDF <- minorLabelsDF[!minorLabelsDF$tick %in% majorLabelsDF$tick,,drop=FALSE];
   minorLabelsDF <- cleanLTdf(minorLabelsDF);

   minorTicks <- minorLabelsDF$tick;
   allLabelsDF <- rbind(majorLabelsDF,
      minorLabelsDF);
   allLabelsDF <- cleanLTdf(allLabelsDF);
   allLabelsDF$text <- ifelse(allLabelsDF$use, allLabelsDF$label, NA);

   majorLabels <- subset(allLabelsDF, type %in% "major")$text;
   majorTicks <- subset(allLabelsDF, type %in% "major")$tick;
   minorLabels <- subset(allLabelsDF, type %in% "minor")$text;
   minorTicks <- subset(allLabelsDF, type %in% "minor")$tick;

   if (combine) {
      majorTicks <- sort(unique(c(majorTicks, minorTicks)));
      minorTicks <- majorTicks;
   }
   minorLabels1 <- sapply(names(minorTicks), function(iName) {
      if (offset > 0 || symmetricZero) {
         i <- (logBase^abs(minorTicks[iName]) - offset) * ifelse(minorTicks[iName] < 0, -1, 1);
      } else {
         i <- minorTicks[iName];
      }
      if (!igrepHas("^TRUE", iName)) {
         iX <- "";
      } else {
         iX <- getAxisLabel(i,
            asValues,
            logAxisType,
            logBase,
            offset=offset);
      }
      iX;
   });
   minorLabels1 <- unname(minorLabels1);
   minorTicks <- unname(minorTicks);

   ## if the axis is log transformed, we must exponentiate the values for plotting to work properly
   if ((side %in% c(1,3) && par("xlog")) ||
         (side %in% c(2,4) && par("ylog"))) {
      if (verbose) {
         printDebug("minorLogTicks(): ",
            "Exponentiating axis coordinates.");
      }
      allLabelsDF$base_tick <- allLabelsDF$tick;
      allLabelsDF$tick <- 10^allLabelsDF$tick;
      majorTicks <- 10^majorTicks;
      minorTicks <- 10^minorTicks;
   }
   ## Optionally combine labels
   if (combine) {
      majorLabels <- minorLabels;
      minorTicks <- numeric(0);
      minorLabels <- character(0);
   }
   retVals <- list(majorTicks=majorTicks,
      minorTicks=minorTicks,
      allTicks=sort(c(minorTicks, majorTicks)),
      majorLabels=majorLabels,
      minorLabels=minorLabels,
      minorLabels1=minorLabels1,
      minorSet=minorSet,
      minorWhich=minorWhich,
      allLabelsDF=allLabelsDF);
   return(retVals);
}

#' Get axis label for minorLogTicks
#'
#' Get axis label for minorLogTicks
#'
#' This function is intended to be called internally by
#' `jamba::minorLogTicks()`.
#'
#' @family jam practical functions
#'
getAxisLabel <- function
(i,
 asValues,
 logAxisType=c("normal", "flip", "pvalue"),
 logBase,
 base_limit=2,
 offset=0,
 symmetricZero=(offset > 0),
 ...)
{
   ## This function takes an axis coordinate and transforms into the
   ## corresponding label. Note that it does NOT apply offset,
   ## since this function serves a specific purpose within the
   ## minorLogTicks() parent function.
   logAxisType <- match.arg(logAxisType);

   if (asValues) {
      if (igrepHas("flip", logAxisType)) {
         #iX <- (logBase^abs(i) - offset) * ifelse(sign(i)<0,-1,1);
         iX <- (logBase^abs(i)) * ifelse(i < 0, -1, 1);
      } else if (offset > 0 || symmetricZero) {
         iX <- (logBase^abs(i)) * ifelse(i < 0, -1, 1);
      } else {
         iX <- logBase^i;
      }
   } else {
      if (length(logAxisType) > 0 && igrepHas("pvalue", logAxisType)) {
         iSign <- sign(i);
         if (iSign > 0) {
            iBase <- floor(i);
            iExtra <- abs(i) %% 1;

            if (iExtra > 0) {
               ## Handle 2x10^-3
               i <- -1 * abs(iBase) - 1;
               iExtra <- 11 - signif(10^(iExtra), digits=2);
               if (i == 0) {
                  ## Print the straight label
                  iX <- as.expression(bquote(.(iExtra)));
               } else if (abs(i) <= base_limit) {
                  ## Print the label without exponent
                  iVal <- iExtra * 10^i;
                  iX <- as.expression(bquote(.(iVal)));
               } else {
                  xsep <- "x";
                  iX <- as.expression(bquote(.(iExtra) * .(xsep) * 10^ .(i)));
               }
            } else {
               i <- -1 * abs(i);
               if (i == 0) {
                  iVal <- 1;
                  iX <- as.expression(bquote(.(iVal)));
               } else if (abs(i) <= base_limit) {
                  ## Print the label without exponent
                  iVal <- 10^i;
                  iX <- as.expression(bquote(.(iVal)));
               } else {
                  iX <- as.expression(bquote(10^ .(i)));
               }
            }
         } else {
            iX <- as.expression(bquote(-10^ .(i)));
         }
      } else {
         if (logBase == 2) {
            iX <- as.expression(bquote(2^ .(i)));
         } else if (logBase == 10) {
            iX <- as.expression(bquote(10^ .(i)));
         } else {
            iX <- as.expression(bquote(.(exp(1))^ .(i)));
         }
      }
   }
   iX;
}



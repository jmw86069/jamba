
#' Get axis label for minorLogTicks
#'
#' Get axis label for minorLogTicks
#'
#' This function is intended to be called internally by
#' `jamba::minorLogTicks()`.
#'
#' @returns `character` or `expression` axis label as appropriate.
#'
#' @param i `numeric` axis value
#' @param asValues `logical` indicating whether the value should be
#'    evaluated.
#' @param logAxisType `character` string with the type of axis values:
#'    * `"normal"`: axis values as-is.
#'    * `"flip"`: inverted axis values, for example where negative values
#'    should be displayed as negative log-transformed values.
#'    * `"pvalue"`: for values transformed as `-log10(pvalue)`
#' @param logBase `numeric` logarithmic base
#' @param base_limit `numeric` value indicating the minimum value that
#'    should be written as an exponential.
#' @param offset `numeric` value of offset used for log transformation.
#' @param symmetricZero `logical` indicating whether negative values
#'    should be displayed as negative log-transformed values.
#' @param ... additional arguments are ignored.
#'
#' @family jam practical functions
#'
#' @examples
#' x <- log10(c(1, 2, 5, 10, 20, 50, 100, 200, 500))
#' getAxisLabel(x, asValues=TRUE, logBase=10)
#'
#' x1exp <- c(1, 2, 3, 4, 5)
#' plot(1:6, main="exponential values")
#' for (i in seq_along(x1exp)) {
#'    text(x=i, y=i + 0.2,
#'       getAxisLabel(x1exp[i], asValues=FALSE, logBase=10))
#' }
#'
#' x1exp <- c(-3:3)
#' plot(-3:3, main="log2 fold change values")
#' for (i in seq_along(x1exp)) {
#'    text(x=i, y=i + 0.3 - 4,
#'       getAxisLabel(x1exp[i],
#'          logAxisType="flip",
#'          asValues=TRUE, logBase=2))
#' }
#'
#' x1exp <- c(1, 2, 3, 4, 5)
#' plot(1:6, main="P-value style")
#' for (i in seq_along(x1exp)) {
#'    text(x=i, y=i + 0.2,
#'       getAxisLabel(x1exp[i],
#'       logAxisType="pvalue", asValues=FALSE, logBase=10))
#' }
#'
#' @export
getAxisLabel <- function
(i,
 asValues,
 logAxisType=c("normal",
    "flip",
    "pvalue"),
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
         } else if (iSign == 0) {
            iX <- as.expression(1);
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


#' Display major and minor tick marks for log-scale axis
#'
#' Display major and minor tick marks for log-scale axis,
#' with optional offset for proper labeling of `log2(1+x)`
#' with numeric offset.
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
#' @returns `list` with vectors:
#'    * `majorLabels`: `character` vector of major axis labels
#'    * `majorTicks`: `numeric` vector of major axis tick positions
#'    * `minorLabels`: `character` vector of minor axis labels
#'    * `minorTicks`: `numeric` vector of minor axis tick positions
#'    * `allLabelsDF`: `data.frame` containing all axis tick
#'    positions and corresponding labels.
#'
#' @family jam plot functions
#'
#' @param side `integer` indicating the axis side, 1=bottom, 2=left,
#'    3=top, 4=right.
#' @param lims NULL or `numeric` range for which the axis tick marks
#'    will be determined.
#'    If NULL then the corresponding `graphics::par("usr")`
#'    will be used.
#' @param logBase `numeric` value indicating the log base units, which
#'    will be used similar to how `base` is used in `log(x, base)`.
#' @param displayBase `numeric` value indicating the log base units to
#'    use when determining the numeric label position. For example,
#'    data may be log2 scaled, and yet it is visually intuitive to
#'    show log transformed axis units in base 10 units. See examples.
#' @param offset `numeric` offset used in transforming the
#'    numeric data displayed on this axis. For example, a common
#'    technique is to transform data using `log2(1+x)` which adds
#'    `1` to values prior to the log2 transformation. In this case,
#'    `offset=1`, which ensures the axis labels exactly
#'    match the initial numeric value prior to the log2 transform.
#' @param symmetricZero `logical` indicating whether numeric values
#'    are symmetric around zero. For example, log fold changes should
#'    use `symmetricZero=TRUE` which ensures a log2 value of `-2` is
#'    labeled `-4` to indicate a negative four fold change. If
#'    `symmetricZero=FALSE` a log2 value of `-2` would be labeled
#'    `0.0625`.
#' @param padj `numeric` vector length 2, which is used to position
#'    axis labels for the minor and major labels, respectively. For
#'    example, `padj=c(0,1)` will position minor labels just to the
#'    left of the tick marks, and major labels just to the right
#'    of tick marks. This example is helpful when minor labels bunch
#'    up on the right side of each section.
#' @param doFormat `logical` indicating whether to apply `base::format()` to
#'    format numeric labels.
#' @param big.mark,scipen arguments passed to `base::format()` when
#'    `doFormat=TRUE`.
#' @param minorWhich `integer` vector indicating which of the minor tick
#'    marks should be labeled. Labels are generally numbered from `2`
#'    to `displayBase-1`. So by default, log 10 units would add
#'    minor tick marks and labels to the `c(2,5)` position. For log2
#'    units only, the second label is defined at 1.5, which shows
#'    minor labels at `c(3, 6, 12)`, which are `1.5 * c(2, 4, 8)`.
#' @param minorLogTicksData `list` object created by running
#'    `jamba::minorLogTicks()`, which allows inspecting and modifying
#'    the content for custom control.
#' @param majorCex,minorCex `numeric` base text size factors, relative
#'    to cex=1 for default text size. These factors are applied in
#'    addition to existing `graphics::par("cex")` values, preserving any
#'    global text size defined there.
#' @param doMajor,doMinor,doLabels,doMinorLabels `logical`, default TRUE,
#'    whether to display each type of tick and label.
#'    * `doMajor` display major ticks, at `displayBase` positions
#'    * `doMinor` display minor ticks at intermediate positions
#'    * `doLabels` display any labels
#'    * `doMinorLabels` display minor labels
#' @param asValues `logical`, default TRUE, whether to print the
#'    exponentiated value, otherwise FALSE will print the log value.
#' @param logAxisType `character` string with the type of axis values:
#'    * `"normal"`: axis values as-is.
#'    * `"flip"`: inverted axis values, for example where negative values
#'    should be displayed as negative log-transformed values.
#'    * `"pvalue"`: for values transformed as `-log10(pvalue)`
#' @param logStep `integer` the number of log units per "step", typically `1`.
#' @param cex,col,col.ticks,las parameters used for axis label size,
#'    axis label colors,
#'    axis tick mark colors, and label text orientation, respectively.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... Additional arguments are ignored.
#'
#' @examples
#' plotPolygonDensity(0:100, breaks=100);
#'
#' plotPolygonDensity(0:100, breaks=50, log="x",
#'    main="plotPolygonDensity() uses minorLogTicksAxis()",
#'    xlab="x (log-scaled)");
#'
#' plotPolygonDensity(log2(1+0:100), breaks=50,
#'    main="manually called minorLogTicksAxis(logBase=2)",
#'    xaxt="n",
#'    xlab="x (log-scaled)");
#' minorLogTicksAxis(1, offset=1, logBase=2);
#'
#' plotPolygonDensity(log10(1+0:100), breaks=50,
#'    main="manually called minorLogTicksAxis(logBase=10)",
#'    xaxt="n",
#'    xlab="x (log-scaled)");
#' minorLogTicksAxis(1, offset=1, logBase=10);
#'
#' # example with log fold axes
#' k <- c(-5:5)
#' plot(x=k, y=k, xaxt="n", yaxt="n",
#'    xlab="log2 base, displaying tick marks with log10 intervals",
#'    ylab="log2 base, displaying tick marks with log2 intervals")
#' axis(3, las=2)
#' axis(4, las=2)
#' lfax <- logFoldAxis(side=1, logBase=2, displayBase=2)
#' lfay <- logFoldAxis(side=2, logBase=2, displayBase=10)
#' # optionally add x-axis ablines
#' abline(v=lfax$allTicks, lty="dotted", col="grey88")
#' abline(v=lfax$majorTicks, lty="dashed", col="grey82")
#' # optionally add y-axis ablines
#' abline(h=lfay$allTicks, lty="dotted", col="grey88")
#' abline(h=lfay$majorTicks, lty="dashed", col="grey82")
#'
#' # example showing volcano plot features
#' set.seed(123);
#' n <- 1000;
#' vdf <- data.frame(lfc=rnorm(n) * 2)
#' vdf$`-log10 (padj)` <- abs(vdf$lfc) * abs(rnorm(n))
#' plotSmoothScatter(vdf, xaxt="n", yaxt="n", xlab="Fold change",
#'    main="Volcano plot\ndisplayBase=2")
#' logFoldAxis(1)
#' pvalueAxis(2)
#'
#' plotSmoothScatter(vdf, xaxt="n", yaxt="n", xlab="Fold change",
#'    main="Volcano plot\ndisplayBase=10")
#' logFoldAxis(1, displayBase=10)
#' pvalueAxis(2)
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
 doMinor=TRUE,
 doLabels=TRUE,
 doMinorLabels=TRUE,
 asValues=TRUE,
 logAxisType=c("normal",
    "flip",
    "pvalue"),
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
   ## padj can take two values, for the minor and major ticks, respectively,
   ## and is recycled if too short.
   ##
   ## padj <- c(0,1) will align minor tick labels just to the left
   ## of the ticks, and major labels just to the right,
   ## since log-scaled labels tend to bunch up on the left side
   ## of each major label.
   ##
   ## To define a set of minor tick positions, send a list object
   ## minorLogTicksData with (majorTicks, majorLabels, minorTicks, minorLabels)
   if (length(padj) == 0) {
      if (side %in% c(1, 3)) {
         padj <- c(0.3,0.7);
      } else {
         padj <- c(0.7,0.3);
      }
   } else {
      padj <- rep(padj, length.out=2);
   }
   if (FALSE %in% doMinor) {
      padj <- c(0.5, 0.5);
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
         logAxisType=logAxisType,
         verbose=verbose,
         ...);
   }
   majorTicks <- mlt$majorTicks;
   majorLabels <- mlt$majorLabels;
   mltMinor <- subset(mlt$allLabelsDF,
      mlt$allLabelsDF$type %in% "minor" &
      mlt$allLabelsDF$use %in% TRUE);
   minorTicks <- mltMinor$tick;
   minorLabels <- mltMinor$text;

   ## Optionally format numbers, mostly to add commas per thousands place
   if (doFormat) {
      if (verbose) {
         printDebug("minorLogTicksAxis(): ",
            "Formatting numerical labels.");
      }
      if (is.numeric(scipen)) {
         withr::local_options("scipen"=scipen)
      }
      if (!"expression" %in% class(majorLabels) && length(majorLabels) > 0) {
         majorLabels <- sapply(majorLabels,
            format,
            big.mark=big.mark,
            trim=TRUE,
            ...);
      }
      if (!"expression" %in% class(minorLabels) && length(minorLabels) > 0) {
         minorLabels <- sapply(minorLabels,
            format,
            big.mark=big.mark,
            trim=TRUE,
            ...);
      }
   }
   if (!inherits(majorLabels, "expression")) {
      NAmajor <- is.na(majorLabels);
      if (any(NAmajor)) {
         majorLabels[NAmajor] <- "";
      }
   }
   if (!inherits(minorLabels, "expression")) {
      NAminor <- is.na(minorLabels);
      if (any(NAminor)) {
         minorLabels[NAminor] <- "";
      }
   }

   ## By default display the major tick labels
   if (doMajor && length(majorTicks) > 0) {
      if (!doLabels) {
         majorLabels <- FALSE;
      }
      if ("expression" %in% class(majorLabels)) {
         if (verbose) {
            printDebug("minorLogTicksAxis(): ",
               "Axis labels displayed as expression.")
         }
         for (i in seq_along(majorLabels)) {
            graphics::axis(side,
               at=majorTicks[i],
               tcl=graphics::par("tcl")*majorCex*cex,
               labels=majorLabels[i],
               padj=padj[2],
               cex.axis=majorCex*cex,
               col="transparent",
               col.ticks=col.ticks,
               las=las,
               ...);
         }
      } else {
         graphics::axis(side,
            at=majorTicks,
            tcl=graphics::par("tcl")*majorCex*cex,
            labels=majorLabels,
            padj=padj[2],
            cex.axis=majorCex*cex,
            col="transparent",
            col.ticks=col.ticks,
            las=las,
            ...);
      }
   }
   if (doMinor && length(minorTicks) > 0) {
      if (!doMinorLabels) {
         minorLabels <- FALSE;
      }
      if ("expression" %in% class(minorLabels)) {
         for (i in seq_along(minorLabels)) {
            graphics::axis(side,
               at=minorTicks[i],
               tcl=graphics::par("tcl")*minorCex*cex,
               labels=minorLabels[i],
               padj=padj[1],
               cex.axis=minorCex*cex,
               col="transparent",
               col.ticks=col.ticks,
               las=las,
               ...);
         }
      } else {
         graphics::axis(side,
            at=minorTicks,
            tcl=graphics::par("tcl")*minorCex*cex,
            labels=minorLabels,
            padj=padj[1],
            cex.axis=minorCex*cex,
            col="transparent",
            col.ticks=col.ticks,
            las=las,
            ...);
      }
   }
   graphics::axis(side,
      at=range(c(majorTicks, minorTicks), na.rm=TRUE),
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
#' This function is called by `minorLogTicksAxis()`, and
#' it may be better to use that function, or `logFoldAxis()`
#' or `pvalueAxis()` which has better preset options.
#'
#' This function calculates log units for the axis of an
#' existing base R plot.
#' It calculates appropriate tick and label positions for:
#' * major steps, which are typically in log steps; and
#' * minor steps, which are typically a subset of steps at one
#' lower log order.
#'
#' For example, log 10 steps would be: `c(1, 10, 100, 1000)`,
#' and minor steps would be `c(2, 5, 20, 50, 200, 500, 2000, 5000)`.
#'
#' ## Motivation
#'
#' This function is motivated to fill a few difficult cases:
#'
#' 1. Label axis ticks properly
#' when used together with `offset`. For example `log2(1 + x)`
#' uses `offset=1`. Other offsets can be used as relevant.
#'
#' 2. Create axis labels which indicate negative fold change
#' values, for example `-2` in log2 fold change units would
#' be labeled with fold change `-4`, and not `0.0625`.
#'
#' 3. Use symmetric tick marks around x=0 when applied to log fold changes.
#'
#' 4. Display actual P-values when plotting `log10(Pvalue)`, which
#' is common for volcano plots.
#'
#' @returns `list` of axis tick positions, and corresponding labels,
#'    for major and minor ticks. Note that labels may be `numeric`,
#'    `character`, or `expression`. Specifically when `expression`
#'    the `graphics::axis()` must be called once per label.
#'    * majorTicks: `numeric` position of each major tick mark
#'    * minorTicks: `numeric` position of each minor tick mark
#'    * allTicks: `numeric` position of each major tick mark
#'    * majorLabels: label to show for each tick mark
#'    * minorLabels: label to show for each tick mark
#'    * minorSet: the `numeric` steps requested for minor ticks
#'    * minorWhich: the `numeric` steps requested for minor labels
#'    * allLabelsDF: `data.frame` with all tick marks and labels, with
#'    colname `"use"` indicating whether the label is displayed beside
#'    each tick mark.
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
#' graphics::axis(1, las=2,
#'    at=maj$tick, label=maj$text);
#' min <- subset(mlt$allLabelsDF, type %in% "minor");
#' graphics::axis(1, las=2, cex.axis=0.7,
#'    at=min$tick, label=min$text,
#'    col="blue");
#' graphics::text(x=log10(1+c(0,5,50,1000)), y=rep(1.7, 4),
#'    label=c(0,5,50,1000), srt=90);
#'
#' nullPlot(xlim=c(-4,10), doMargins=FALSE);
#' abline(v=0, lty=2)
#' graphics::axis(3, las=2);
#' minorLogTicksAxis(1, logBase=2, displayBase=10, symmetricZero=TRUE);
#'
#' nullPlot(xlim=c(-4,10), doMargins=FALSE);
#' graphics::axis(3, las=2);
#' minorLogTicksAxis(1, logBase=2, displayBase=10, offset=1);
#' x2 <- stats::rnorm(1000) * 40;
#' d2 <- stats::density(log2(1+abs(x2)) * ifelse(x2<0, -1, 1));
#' lines(x=d2$x, y=normScale(d2$y)+1, col="green4");
#'
#' nullPlot(xlim=c(0,10), doMargins=FALSE);
#' graphics::axis(3, las=2);
#' minorLogTicksAxis(1, logBase=2, displayBase=10, offset=1);
#' x1 <- c(0, 5, 15, 200);
#' graphics::text(y=rep(1.0, 4), x=log2(1+x1), label=x1, srt=90, adj=c(0,0.5));
#' graphics::points(y=rep(0.95, 4), x=log2(1+x1), pch=20, cex=2, col="blue");
#'
#' @param side `integer` value indicating which axis to produce tick
#'    marks, 1=bottom, 2=left, 3=top, 4=right.
#' @param lims `numeric` vector length=2, indicating specific numeric
#'    range to use for tick marks.
#' @param logBase `numeric` value indicating the logarithmic base, assumed
#'    to be applied to the numeric `lims` limits, or the axis range,
#'    previously.
#' @param displayBase `numeric` value indicating the base used to position
#'    axis labels, typically `displayBase=10` is used to draw labels
#'    at typical positions.
#' @param logStep `integer` value indicating the number of log steps
#'    between major axis label positions. Typically `logStep=1` will
#'    draw a label every log position based upon `displayBase`, for
#'    example `displayBase=10` and `logStep=1` will use `c(1,10,100,1000)`;
#'    and `displayBase=10` and `logStep=2` would use `c(1,100,10000)`.
#' @param minorWhich `integer` vector of values to label, where those
#'    integer values are between 1 and `displayBase`, for example
#'    `displayBase=10` may label only `c(2,5)`, which implies minor
#'    tick labels at `c(2, 5, 20, 50, 200, 500)`. Any minor labels
#'    which would otherwise equal a major tick position are removed.
#'    By default, when `displayBase=2`, `minorWhich=c(1.5)` which has the
#'    effect of drawing one minor label between each two-fold
#'    major tick label.
#' @param asValues `logical` indicating whether to create exponentiated
#'    numeric labels. When `asValues=FALSE`, it creates `expression` objects
#'    which include the exponential value. Use `asValues=FALSE` and
#'    `logAxisType="pvalue"` to draw P-value labels.
#' @param offset `numeric` value added during log transformation, typically
#'    of the form `log(1 + x)` where `offset=1`. The offset is used to
#'    determine the accurate numeric label such that values of `0` are
#'    properly labeled by the original numeric value.
#' @param symmetricZero `logical` indicating whether numeric values
#'    are symmetric around zero. For example, log fold changes should
#'    use `symmetricZero=TRUE` which ensures a log2 value of `-2` is
#'    labeled `-4` to indicate a negative four fold change. If
#'    `symmetricZero=FALSE` a log2 value of `-2` would be labeled
#'    `0.0625`.
#' @param col,col.ticks `character` color used for the axis label, and
#'    axis tick marks, respectively, default "black".
#' @param combine `logical`, default FALSE, whether to combine major and
#'    minor ticks into one continuous set of major tick marks.
#' @param logAxisType `character` string indicating the type of log axis:
#'    * normal: typical axis style and orientation
#'    * flipped: used for reverse orientation
#'    * pvalue: used for `-log10(pvalue)` orientation.
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
 minorWhich=c(2, 5),
 asValues=TRUE,
 offset=0,
 symmetricZero=(offset>0),
 col="black",
 col.ticks=col,
 combine=FALSE,
 logAxisType=c("normal",
    "flip",
    "pvalue"),
 verbose=FALSE,
 ...)
{
   ## Returns a list of majorTicks, minorTicks, majorLabels, and minorLabels.
   ##
   ## logAxisType="flipped" will flip negative values so they are like fold changes, e.g.
   ## "-1" will become "-10" instead of "0.1"
   ##
   if (length(offset) == 0) {
      offset <- 0;
   }
   offset <- head(offset, 1);
   displayBase <- head(displayBase, 1);
   logBase <- head(logBase, 1);
   logAxisType <- match.arg(logAxisType);
   if ("pvalue" %in% logAxisType) {
      offset <- 0;
   }

   if (logStep > 1) {
      minorWhich <- c(1);
   }

   if (length(lims) == 0) {
      if (length(side) == 0) {
         stop(paste0("minorLogTicks requires either axis (which axis), ",
            "or lims (range of values) to be defined."));
      }
      lims <- graphics::par("usr");
      if (side %in% c(1, 3)) {
         lims <- lims[1:2];
      } else {
         lims <- lims[3:4];
      }
   } else {
      lims <- range(lims, na.rm=TRUE);
   }
   ## Now set the floor and raise the roof to the nearest integer at or
   ## just beyond the given range of values.
   if ("pvalue" %in% logAxisType) {
      lims <- c(ceiling(lims[1]), floor(lims[2]));
   } else {
      lims <- c(floor(lims[1]), ceiling(lims[2]));
   }
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

   # convert to text labels
   majorLabels <- sapply(majorTicks, function(i) {
      iX <- getAxisLabel(i=i,
         asValues=asValues,
         logAxisType=logAxisType,
         logBase=displayBase,
         offset=offset,
         symmetricZero=symmetricZero);
      iX;
   });
   if (verbose) {
      printDebug("minorLogTicks(): ",
         "asValues:", asValues);
      printDebug("minorLogTicks(): ",
         "getAxisLabel() majorLabels:");
      print(majorLabels);
   }

   ## majorLabels represents the numeric value associated with each
   ## axis position desired
   ##
   ## However, when offset == 1, it means the actual axis
   ## position for value=10 was calculated using log10(10+1),
   ## which slightly shifts the actual axis position to the right.
   ## Therefore, in that case we must re-calculate majorTicks using
   ## the new axis space.
   if (offset > 0 || TRUE %in% symmetricZero) {
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
      if (any(majorLabels < 0) && any(majorLabels > 0)) {
         if (verbose) {
            printDebug("minorLogTicks(): ",
               "Included zero with majorLabels since offset is non-zero");
         }
         majorLabels <- sort(unique(c(majorLabels, -1, 0)));
      }
      if (TRUE %in% symmetricZero) {
         iUse <- noiseFloor(abs(majorLabels) + offset,
            minimum=1);
         majorTicks <- (log(iUse, base=logBase) *
               ifelse(majorLabels < 0, -1, 1));
      } else {
         majorTicks <- log(abs(majorLabels) + offset,
            base=logBase) * ifelse(majorLabels < 0, -1, 1);
      }
   } else {
      if (offset > 0) {
         majorTicks <- log(majorLabels + offset, base=logBase);
      } else if (!"pvalue" %in% logAxisType) {
         # for pvalue we do not log-transform the ticks
         majorTicks <- log(majorTicks, base=logBase);
      }
   }
   if ("pvalue" %in% logAxisType) {
      majorLabelsDF <- data.frame(
         check.names=FALSE,
         stringsAsFactors=FALSE,
         label=I(majorLabels),
         type="major",
         use=TRUE,
         tick=majorTicks);
   } else {
      majorLabelsDF <- data.frame(
         check.names=FALSE,
         stringsAsFactors=FALSE,
         label=majorLabels,
         type="major",
         use=TRUE,
         tick=majorTicks);
   }

   ## Confirm that the labels are unique
   cleanLTdf <- function(df) {
      df <- df[rev(seq_len(nrow(df))),,drop=FALSE];
      df <- subset(df, !(is.infinite(df$tick) | is.na(df$tick)));
      if (!igrepHas("asis|expression", class(df$label))) {
         df <- subset(df, !duplicated(df$label))
      }
      df <- subset(df, !duplicated(df$tick))
      df <- df[rev(seq_len(nrow(df))), , drop=FALSE];
      df;
   }
   majorLabelsDF <- cleanLTdf(majorLabelsDF);
   majorTicks <- majorLabelsDF$tick;
   majorLabels <- majorLabelsDF$majorLabels;
   if (verbose) {
      printDebug("minorLogTicks(): ",
         "final majorLabels:");
         print(majorLabels);
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
   if ("pvalue" %in% logAxisType) {
      minorSet <- numeric(0);
      minorWhich <- numeric(0);
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
   if (length(minorSet) == 0 && length(minorWhich) == 0) {
      minorLabelsDF <- data.frame(label=numeric(0),
         type=character(0),
         use=logical(0));
   } else {
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
               type=head("minor", length(iSeries)),
               use=(iSeries %in% iSeriesLab));
         })
      ));
      ## Remove any minor labels which overlap major labels
      minorLabelsDF <- subset(minorLabelsDF,
         !minorLabelsDF$label %in% majorLabelsDF$label);
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
      minorLabelsDF <- subset(minorLabelsDF,
         !minorLabelsDF$tick %in% majorLabelsDF$tick)
      minorLabelsDF <- cleanLTdf(minorLabelsDF);
   }

   minorTicks <- minorLabelsDF$tick;
   allLabelsDF <- rbind(majorLabelsDF,
      minorLabelsDF);
   allLabelsDF <- cleanLTdf(allLabelsDF);
   allLabelsDF$text <- ifelse(allLabelsDF$use, allLabelsDF$label, NA);

   type <- NULL;
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
   if ((side %in% c(1,3) && graphics::par("xlog")) ||
         (side %in% c(2,4) && graphics::par("ylog"))) {
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


#' Determine square root axis tick mark positions
#'
#' Determine square root axis tick mark positions, including positive
#' and negative range values.
#'
#' This function calculates positions for tick marks for data
#' that has been transformed with `sqrt()`, specifically a directional
#' transformation like `sqrt(abs(x)) * sign(x)`.
#'
#' If `x` is supplied, it is used to define the numeric range, otherwise
#' the observed range is taken based upon `side`. If neither `x` nor `side`
#' is supplied, or if the numeric range is empty or zero width,
#' it returns `NULL`.
#'
#' The main goal of this function is to provide reasonably placed
#' tick marks using integer values.
#'
#' @family jam plot functions
#'
#' @returns invisible `numeric` vector with axis positions, named
#'    by normal space numeric labels. The primary use is to
#'    add numeric axis tick marks and labels.
#'
#' @param side `integer` value indicating the axis position, as used
#'    by `graphics::axis()`, 1=bottom, 2=left, 3=top, 4=right.
#'    Note that when `x` is supplied, the numeric range is defined
#'    using values in `x` and not the axis side.
#' @param x optional `numeric` vector representing the numeric range
#'    to be labeled. When supplied, the numeric range of `x` is used
#'    and not the axis side.
#' @param pretty.n `numeric` value indicating the number of desired
#'    tick marks, passed to `pretty()`.
#' @param u5.bias `numeric` value passed to `pretty()` to influence the
#'    frequency of intermediate tick marks.
#' @param big.mark `character` value passed to `format()` which helps
#'    visually distinguish numbers larger than 1000.
#' @param plot `logical` indicating whether to plot the axis tick
#'    marks and labels.
#' @param las,cex.axis `numeric` values passed to `graphics::axis()`
#'    when drawing the axis. The custom default `las=2` plots labels rotated
#'    perpendicular to the axis.
#' @param ... additional parameters are passed to `pretty()`, and to
#'    `graphics::axis()` when `plot=TRUE`.
#'
#' @examples
#' plot(-3:3*10, -3:3*10, xaxt="n")
#' x <- sqrtAxis(1)
#' abline(v=x, col="grey", lty="dotted")
#' abline(h=pretty(par("usr")[3:4]), col="grey", lty="dotted")
#'
#' # slightly different label placement with u5.bias=0
#' plot(-3:3*10, -3:3*10, xaxt="n")
#' x <- sqrtAxis(1, u5.bias=0)
#' abline(v=x, col="grey", lty="dotted")
#' abline(h=pretty(par("usr")[3:4]), col="grey", lty="dotted")
#'
#' @export
sqrtAxis <- function
(side=1,
 x=NULL,
 pretty.n=10,
 u5.bias=1,
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
   if (length(side) > 2 && length(x) == 0) {
      x <- side;
      side <- 0;
      plot <- FALSE;
   }
   if (length(side) == 0 && length(x) == 0) {
      return(NULL)
   }

   if (length(x) > 0) {
      xRange <- range(x, na.rm=TRUE);
   } else if (1 %in% side) {
      xRange <- graphics::par("usr")[1:2];
   } else if (2 %in% side) {
      xRange <- graphics::par("usr")[3:4];
   } else if (length(x) > 0) {
      xRange <- range(x, na.rm=TRUE);
   }

   # if xRange is empty or length 1, return NULL
   if (length(xRange) < 2) {
      return(NULL)
   }

   subdivideSqrt <- function(atPretty1, n=pretty.n, ...) {
      ## Purpose is to take x in form of 0,x1,
      ## and subdivide using pretty()
      atPretty1a <- unique(sort(abs(atPretty1)));
      atPretty1b <- tail(atPretty1a, -2);
      atPretty2a <- call_fn_ellipsis(pretty.default,
         x=head(atPretty1a, 2),
         n=n,
         u5.bias=u5.bias,
         ...);
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
   if (TRUE %in% plot) {
      graphics::axis(side=side,
         at=atSqrt,
         labels=xLabel,
         las=las,
         cex.axis=cex.axis,
         ...);
   }

   invisible(nameVector(atSqrt, xLabel));
}


#' Log fold axis
#'
#' Log fold axis
#'
#' @rdname minorLogTicksAxis
#'
#' @export
logFoldAxis <- function
(side=NULL,
 lims=NULL,
 logBase=2,
 displayBase=2,
 offset=0,
 symmetricZero=TRUE,
 asValues=TRUE,
 minorWhich=NULL,
 doMinor=TRUE,
 doMinorLabels=NULL,
 scipen=1,
 ...)
{
   #
   if (displayBase == 10) {
      if (length(minorWhich) == 0) {
         minorWhich <- c(2, 5)
      }
      if (length(doMinorLabels) == 0) {
         if (doMinor) {
            doMinorLabels <- TRUE
         } else {
            doMinorLabels <- FALSE
         }
      }
   }
   if (displayBase == 2) {
      if (length(minorWhich) == 0) {
         minorWhich <- c(1.5)
      }
      if (length(doMinorLabels) == 0) {
         doMinorLabels <- FALSE
      }
   }

   minorLogTicksAxis(side=side,
      logBase=logBase,
      displayBase=displayBase,
      offset=0,
      symmetricZero=symmetricZero,
      asValues=asValues,
      minorWhich=minorWhich,
      doMinor=doMinor,
      doMinorLabels=doMinorLabels,
      scipen=scipen,
      ...);
}

#' P-value axis labels
#'
#' @rdname minorLogTicksAxis
#'
#' @export
pvalueAxis <- function
(side=2,
 lims=NULL,
 displayBase=10,
 logBase=10,
 logAxisType="pvalue",
 asValues=FALSE,
 doMinor=FALSE,
 doMinorLabels=FALSE,
 scipen=1,
 ...)
{
   #
   minorLogTicksAxis(side=side,
      displayBase=displayBase,
      logBase=logBase,
      padj=TRUE,
      logAxisType=logAxisType,
      asValues=asValues,
      doMinor=doMinor,
      doMinorLabels=doMinorLabels,
      scipen=scipen,
      ...)
}

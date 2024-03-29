% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba-plots.r
\name{imageDefault}
\alias{imageDefault}
\title{Display a color raster image}
\usage{
imageDefault(
  x = seq_len(nrow(z) + 1) - 0.5,
  y = seq_len(ncol(z) + 1) - 0.5,
  z,
  zlim = range(z[is.finite(z)]),
  xlim = range(x),
  ylim = range(y),
  col = grDevices::hcl.colors(12, "YlOrRd", rev = TRUE),
  add = FALSE,
  xaxs = "i",
  yaxs = "i",
  xaxt = "n",
  yaxt = "n",
  xlab,
  ylab,
  breaks,
  flip = c("none", "x", "y", "xy"),
  oldstyle = TRUE,
  useRaster = NULL,
  fixRasterRatio = TRUE,
  maxRatioFix = 10,
  minRasterMultiple = NULL,
  rasterTarget = 200,
  interpolate = getOption("interpolate", TRUE),
  verbose = FALSE,
  ...
)
}
\arguments{
\item{x}{location of grid lines at which the intervals in z are measured.}

\item{y}{location of grid lines at which the intervals in z are measured.}

\item{z}{numeric or logical matrix containing the values to be plotted,
where NA values are allowed.}

\item{zlim}{numeric range allowed for values in z.}

\item{xlim}{numeric range to plot on the x-axis, by default the x range.}

\item{ylim}{numeric range to plot on the y-axis, by default the y range.}

\item{col}{character vector of colors to be mapped to values in z.}

\item{add}{logical whether to add to an existing active R plot, or create
a new plot window.}

\item{xaxs}{character value compatible with par(xaxs), mainly useful
for suppressing the x-axis, in order to produce a custom x-axis
range, most useful to restrict the axis range expansion done by R
by default.}

\item{yaxs}{character value compatible with par(yaxs), mainly useful
for suppressing the y-axis, in order to produce a custom y-axis
range, most useful to restrict the axis range expansion done by R
by default.}

\item{xaxt}{character value compatible with par(xaxt), mainly useful
for suppressing the x-axis, in order to produce a custom x-axis
by other mechanisms, e.g. log-scaled x-axis tick marks.}

\item{yaxt}{character value compatible with par(yaxt), mainly useful
for suppressing the y-axis, in order to produce a custom y-axis
by other mechanisms, e.g. log-scaled y-axis tick marks.}

\item{xlab}{character label for the x-axis}

\item{ylab}{character label for the y-axis}

\item{breaks}{numeric vector of breakpoints for colors.}

\item{oldstyle}{logical whether to delineate axis coordinates with an
integer spacing for each column and row. Note: the only allowed parameter
is TRUE, since useRaster=TRUE requires it. Therefore, this function
for consistency will only output this format.}

\item{useRaster}{logical whether to force raster image scaling, which
is especially useful for large data matrices. In this case a bitmap
raster image is created instead of polygons, then the bitmap is scaled
to fit the plot space. Otherwise, individual polygons can be obscured
on monitor screens, or may result in an extremely large file size when
writing to vector image format such as PDF or SVG.}

\item{fixRasterRatio}{logical whether to implement a simple workaround
to the requirement for square pixels, in the event the x- and y-axis
dimensions are not roughly equal.}

\item{maxRatioFix}{integer maximum number of times any axis may be
replicated to create a matrix of roughly equal x- and y-axis dimensions.}

\item{minRasterMultiple}{integer minimum number of times the x- and y-axis
will be duplicated, which is mostly useful when creating useRaster=TRUE
for small matrix sizes, otherwise the result will be quite blurry. For
example, minRasterMultiple=10 will duplicate each axis 10 times. Values
are aplied to rows then columns. These values are automatically defined
if minRasterMultiple is NULL and rasterTarget is not NULL.}

\item{rasterTarget}{integer number of cells below which cells are duplicated
in order to maintain detail. The default 200 defines
minRasterMultiple=c(1,1) if there are 200 rows and 200 columns, or
minRasterMultiple=c(1,100) if there are 200 rows but 2 columns.}

\item{interpolate}{logical whether to implement image interpolation,
by default TRUE when useRaster=TRUE.}

\item{verbose}{logical whether to enable verbose output, useful for
debugging.}
}
\value{
\code{list} composed of elements suitable to call
\code{graphics::image.default()}.
}
\description{
Display a color raster image
}
\details{
This function augments the \code{\link[graphics]{image}} function, in
that it handles the useRaster parameter for non-symmetric data matrices,
in order to minimize the distortion from image-smoothing when pixels are
not square.

The function also by default creates the image map using coordinates where
each integer represents the center point of one column or row of data,
known in the default \code{\link[graphics]{image}} function as \code{oldstyle=TRUE}.
For consistency, \code{imageDefault} will only accept \code{oldstyle=TRUE}.
}
\examples{
ps <- plotSmoothScatter(doTest=TRUE)

}
\seealso{
\code{\link[graphics]{image}}

Other jam plot functions: 
\code{\link{adjustAxisLabelMargins}()},
\code{\link{coordPresets}()},
\code{\link{decideMfrow}()},
\code{\link{drawLabels}()},
\code{\link{getPlotAspect}()},
\code{\link{groupedAxis}()},
\code{\link{imageByColors}()},
\code{\link{minorLogTicksAxis}()},
\code{\link{nullPlot}()},
\code{\link{plotPolygonDensity}()},
\code{\link{plotRidges}()},
\code{\link{plotSmoothScatter}()},
\code{\link{shadowText_options}()},
\code{\link{shadowText}()},
\code{\link{showColors}()},
\code{\link{smoothScatterJam}()},
\code{\link{sqrtAxis}()},
\code{\link{usrBox}()}
}
\concept{jam plot functions}

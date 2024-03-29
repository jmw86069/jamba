% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba.r
\name{noiseFloor}
\alias{noiseFloor}
\title{Apply noise floor and ceiling to numeric vector}
\usage{
noiseFloor(
  x,
  minimum = 0,
  newValue = minimum,
  adjustNA = FALSE,
  ceiling = NULL,
  newCeiling = ceiling,
  ...
)
}
\arguments{
\item{x}{\code{numeric} vector or matrix}

\item{minimum}{\code{numeric} floor value}

\item{newValue}{\code{numeric}, by default the same as the floor value. Sometimes
it can be useful to define a different value, one example is to define
values as \code{NA}, or another distinct number away from the floor.}

\item{adjustNA}{\code{logical} whether to change \code{NA} values to the \code{newValue.}}

\item{ceiling}{\code{numeric} value, optionally a ceiling. If defined, then values
above the ceiling value are set to \code{newCeiling.}}

\item{newCeiling}{\code{numeric} value when ceiling is defined, values above the
ceiling are set to this \code{numeric} value.}

\item{...}{additional parameters are ignored.}
}
\value{
A \code{numeric} vector or \code{matrix}, matching the input type \code{x} where numeric
values are fixed to the \code{minimum} and \code{ceiling} values as defined
by \code{newValue} and \code{newCeiling}, respectively.
}
\description{
Apply noise floor and ceiling to numeric vector
}
\details{
A noise floor is useful when detected numeric values are sometimes below
a clear noise threshold, and where some downstream ratio may be calculated
using these values. Applying a noise floor ensures the ratios and not
artificially higher, especially in cases where the values involved are
least reliable. This procedure is expected to produce more conservative
and appropriate ratios in that scenario.

A ceiling is similar, values above the ceiling are set to the ceiling,
which is practical when values above a certain threshold are conceptually
similar to those at the threshold. One clear example is plotting
\code{-log10(Pvalue)} when the range of P-values might approach 1e-1000.
In this case, setting a ceiling of 50 conceptually equates P-values
below 1e-50, while also restricting the axis range of a plot.

The ability to set values at the floor to a different value, using
\code{newValue} different from \code{minimum}, is intended to allow separation
of numeric values from the floor for illustrative purposes.
}
\examples{
# start with some random data
n <- 2000;
x1 <- rnorm(n);
y1 <- rnorm(n);

# apply noise floor and ceiling
x2 <- noiseFloor(x1, minimum=-2, ceiling=2);
y2 <- noiseFloor(y1, minimum=-2, ceiling=2);

# apply noise floor and ceiling with custom replacement values
xm <- cbind(x=x1, y=y1);
xm3 <- noiseFloor(xm,
   minimum=-2, newValue=-3,
   ceiling=2, newCeiling=3);

parMfrow <- par("mfrow");
par("mfrow"=c(2,2));
plotSmoothScatter(x1, y1);
plotSmoothScatter(x2, y2);
plotSmoothScatter(xm3);
par("mfrow"=parMfrow);

}
\seealso{
Other jam numeric functions: 
\code{\link{deg2rad}()},
\code{\link{fix_matrix_ratio}()},
\code{\link{normScale}()},
\code{\link{rad2deg}()},
\code{\link{rowGroupMeans}()},
\code{\link{rowRmMadOutliers}()},
\code{\link{warpAroundZero}()}
}
\concept{jam numeric functions}

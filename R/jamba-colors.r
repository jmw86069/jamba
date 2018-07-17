##
## jamba-colors.r
##
## setTextContrastColor
## col2hcl
## col2alpha
## alpha2col
## hsv2col
## rgb2col
## makeColorDarker
## getColorRamp
## isColor

#' Define visible text color
#'
#' Given a vector or colors, define a contrasting color for text,
#' typically using either white or black.
#'
#' @param color character vector with one or more R-compatible colors.
#' @param colorModel Either 'hcl' or 'rgb' to indicate how the colors
#'    will be judged for overall brightness. The 'hcl' method uses the L
#'    value, which more reliably represents overall visible lightness.
#' @param rgbCutoff numeric threshold above which a color is judged to be
#'    bright, therefore requiring a dark text color. The mean r,g,b value is
#'    used.
#' @param hclCutoff numeric threshold above which a color is judged to be
#'    bright, therefore requiring a dark text color. This comparison uses the
#'    L value from the \code{\link{col2hcl}} function, which scales colors from
#'    1 to 100.
#' @param useGrey numeric threshold used to define dark and bright text colors,
#'    using the R greyscale gradient from 0 to 100. useGrey=10 implies
#'    'grey10' and 'grey90' for the contrasting text colors.
#'    useGrey=15 is useful if labels may also overlap white or black space,
#'    since the text will never be fully white or black.
#' @param keepAlpha logical indicates whether the input color alpha
#'    transparency should be maintained in the text color. By default, text
#'    alpha is not maintained, and instead is set to alpha=1, fully
#'    opaque.
#'
#' @seealso jamba color manipulation functions: \code{\link{hsv2col}},
#'    \code{\link{col2hsv}},\code{\link{col2hcl}},
#'    \code{\link{rgb2col}},
#'    \code{\link{alpha2col}},\code{\link{col2alpha}},
#'    \code{\link{makeColorDarker}},
#'    \code{\link{color2gradient}},
#'    \code{\link{setTextContrastColor}},
#'    \code{\link{showColors}}
#'
#' @examples
#' setTextContrastColor(c("red","yellow","lightblue","blue4"))
#'
#' @export
setTextContrastColor <- function
(color, hclCutoff=73, rgbCutoff=127, colorModel=c("hcl", "rgb"),
 useGrey=0, keepAlpha=FALSE,
 ...)
{
   ## Purpose is to provide a good contrasting text color, given a background color
   ## useGrey=TRUE will use slightly off-white and off-black in order to allow some
   ## visual contrast when labels slightly overlap the opposite color.
   ##
   ## useGrey may also be an integer between 0 and 50, defining how much the grey
   ## maxima differ from perfect black and white, as defined by a range of 0 to 100.
   ## For example, useGrey=20 will define the values to be grey20 through grey80.
   ##
   ## Finally, useGrey may be supplied with two values, indicating divergence from
   ## black and white, respectively.
   ##
   ## keepAlpha=TRUE will keep the original alpha value
   colorModel <- match.arg(colorModel);

   ## Apply the logic to useGrey
   useGrey <- rep(useGrey, length.out=2);
   useGrey[isTRUE(useGrey)] <- 15;
   useGrey[useGrey %in% c(FALSE,0)] <- 0;
   useGrey[useGrey > 100] <- 100;

   greyVals <- abs(c(0,-100) + useGrey);
   bwColors <- rgb2col(col2rgb(paste0("grey", greyVals)))

   if (colorModel %in% "rgb") {
      iColor <- ifelse(colMeans(col2rgb(color)) > rgbCutoff,
         bwColors[1],
         bwColors[2]);
   } else {
      iColor <- ifelse(col2hcl(color)["L",] > hclCutoff,
         bwColors[1],
         bwColors[2]);
   }
   if (keepAlpha) {
      iColor <- alpha2col(x=iColor, alpha=col2alpha(color));
   }
   iColor;
}

#' convert R color to HCL color matrix
#'
#' convert R color to HCL color matrix
#'
#' This function takes an R color and converts to an HCL matrix, using
#' the colorspace package, and \code{\link[colorspace]{RGB}} and
#' \code{\link[colorspace]{polarLUV}} functions. It is also used to
#' maintain alpha transparency, to enable interconversion via other
#' color manipulation functions as well.
#'
#' @param x R compatible color, either a color name, or hex value, or
#'    a mixture of the two. Any value compatible with \code{\link{col2rgb}}.
#' @param maxValue numeric maximum value to return, useful when the downstream
#'    alpha range should be 255. By default maxValue=1 is returned.
#'
#' @seealso jamba color manipulation functions: \code{\link{hsv2col}},
#'    \code{\link{col2hsv}},\code{\link{col2hcl}},
#'    \code{\link{rgb2col}},
#'    \code{\link{alpha2col}},\code{\link{col2alpha}},
#'    \code{\link{makeColorDarker}},
#'    \code{\link{color2gradient}},
#'    \code{\link{setTextContrastColor}},
#'    \code{\link{showColors}}
#'
#' @examples
#' col2hcl("#FF000044")
#'
#' @export
col2hcl <- function
(x,
 maxColorValue=255,
 ...)
{
   ## Purpose is to convert R color to HCL
   ## R color can be a hex string or color name from colors()
   if (!suppressPackageStartupMessages(require(colorspace))) {
      stop("The colorspace package is required.");
   }
   if (length(names(x)) == 0) {
      names(x) <- makeNames(x);
   }
   x1 <- col2rgb(x);
   a1 <- col2alpha(x);
   x2 <- RGB(t(x1)[,1:3,drop=FALSE]/maxColorValue);
   ## Note: spatstat overrides coords() with a generic function but
   ## the colorspace function is not properly dispatched for class RGB.
   ## Currently a weakness in R, that generic functions can be overwritten
   ## and the only workaround is to prefix the specific package name,
   ## which of course requires only that exact package to provide the
   ## function. Over time, these workarounds will break, as functions
   ## may be migrated to new packages.
   x3 <- rbind(t(colorspace::coords(as(x2, "polarLUV"))), "alpha"=a1);
   colnames(x3) <- names(x);
   x3[is.na(x3)] <- 0;
   x3;
}

#' convert HCL to R color
#'
#' Convert an HCL color matrix to vector of R hex colors
#'
#' @param x matrix of colors, with rownames `"H"`, `"C"`, `"L"`, or if not
#'    supplied it looks for vectors `H`, `C`, and `L` accordingly. It can
#'    alternatively be supplied as an object of class `polarLUV`.
#' @param H,C,L numeric vectors supplied as an alternative to `x`, with
#'    ranges 0 to 360, 0 to 100, and 0 to 100, respectively.
#' @param maxColorValue numeric value indicating the maximum RGB values,
#'    typically scaling values to a range of 0 to 255, from the default
#'    returned range of 0 to 1. In general, this value should not be
#'    modified.
#' @param ceiling numeric value indicating the maximum values allowed for
#'    `R`, `G`, and `B` after conversion by `colorspace::as(x, "RGB")`.
#'    This ceiling is applied after the `maxColorValue` is used to scale
#'    numeric values, and is intended to correct for the occurrence of
#'    values above 255, which would be outside the typical color gamut
#'    allowed for RGB colors used in R. In general, this value should not
#'    be modified.
#' @param alpha optional vector of alpha values. If not supplied, and if
#'    `x` is supplied as a matrix with rowname `"alpha"`, then values will
#'    be used from `x["alpha",]`.
#' @param fixup boolean indicating whether to use
#'    `colorspace::hex(...,fixup=TRUE)` for conversion to R hex colors,
#'    **which is not recommended** since this conversion applies some
#'    unknown non-linear transformation for colors outside the color gamut.
#'    It is here is an option for comparison, and if specifically needed.
#' @param ... other arguments are ignored.
#'
#' @return vector of R colors, or where the input was NA, then NA
#'    values are returned in the same order.
#'
#' @examples
#' # Prepare a basic HCL matrix
#' hclM <- col2hcl(c(red="red",
#'    blue="blue",
#'    yellow="yellow",
#'    orange="#FFAA0066"));
#' hclM;
#'
#' # Now convert back to R hex colors
#' colorV <- hcl2col(hclM);
#' colorV;
#'
#' showColors(colorV);
#'
#' @export
hcl2col <- function
(x=NULL,
 H=NULL,
 C=NULL,
 L=NULL,
 ceiling=255,
 maxColorValue=255,
 alpha=NULL,
 fixup=NULL,
 ...)
{
   ## Purpose is to convert HCL back to an R hex color string
   ## Note that this function uses the colorspace HCL, which differs from the
   ## used by the built-in R method hcl()
   if (!suppressPackageStartupMessages(require(colorspace))) {
      stop("The colorspace package is required.");
   }
   if (!suppressPackageStartupMessages(require(matrixStats))) {
      useMatrixStats <- TRUE;
   } else {
      useMatrixStats <- FALSE;
   }
   if (igrepHas("polarLUV", class(x))) {
      x <- t(colorspace::coords(x));
      xnames <- colnames(x);
   } else if (igrepHas("matrix", class(x))) {
      H <- x["H",];
      C <- x["C",];
      L <- x["L",];
      alpha <- x["alpha",];
      xnames <- colnames(x);
   } else if (length(x) == 0) {
      if (length(H) == 0 ||
            length(C) == 0 ||
            length(L) == 0) {
         stop("hcl2col() requires matrix x with rownames H, C, L; or vectors H, C, and L.");
      }
      x <- rbind(H=H, C=C, L=L);
      xnames <- names(H);
   }

   if (length(alpha) > 0) {
      a1 <- alpha;
   } else if ("alpha" %in% rownames(x)) {
      a1 <- x["alpha",,drop=TRUE];
   } else {
      a1 <- 1;
   }
   a1[is.na(a1)] <- 1;
   a1 <- rep(a1, length.out=ncol(x));

   a1 <- tryCatch({
      if (max(a1) <= 1) {
         a1 * 255;
         #a1;
      } else {
         a1;
         #a1 / 255;
      }
   }, error=function(e) {
      printDebug("Error: ", e, c("orange", "lightblue"));
      exit;
      a1;
   });

   ## Convert to HCL using colorspace::polarLUV()
   x2 <- polarLUV(H=t(x)[,c("H")],
      C=t(x)[,c("C")],
      L=t(x)[,c("L")]);

   ## fixup is an optional boolean, which uses colorspace hex() to
   ## repair any colors outside of normal RGB ranges (the color gamut),
   ## otherwise they become NA. Note that the fixup=TRUE method is lossy,
   ## as colorspace apparently applies a non-linear conversion strategy.
   if (length(fixup) > 0) {
      xCol <- hex(x2, fixup=fixup);
      xCol <- alpha2col(xCol, alpha=a1, maxValue=255);
      names(xCol) <- xnames;
   } else {
      ## use colorspace to convert to RGB, but cap the values at 255
      ## which keeps them in the color gamut, while not being lossy
      x3 <- round(
         noiseFloor(
            t(colorspace::coords(as(x2, "RGB")) * maxColorValue),
            minimum=0,
            ceiling=ceiling,
            adjustNA=TRUE),
         digits=3);
      if (useMatrixStats) {
         x3colMax <- colMaxs(x3);
      } else {
         x3colMax <- apply(x3, 2, max);
      }
      if (any(x3colMax) > 255) {
         x3[,x3colMax > 255] <- t(t(x3[,x3colMax > 255, drop=FALSE]) *
               (255 / x3colMax[x3colMax > 255]));
      }
      x3 <- rbind(x3, "alpha"=a1);
      xCol <- rgb2col(x3, maxColorValue=255);
      names(xCol) <- xnames;
   }

   xCol;
}


#' get R color alpha value
#'
#' Return the alpha transparency per R color
#'
#' @param x R compatible color, either a color name, or hex value, or
#'    a mixture of the two. Any value compatible with \code{\link{col2rgb}}.
#' @param maxValue numeric maximum value to return, useful when the downstream
#'    alpha range should be 255. By default maxValue=1 is returned.
#'
#' @seealso jamba color manipulation functions: \code{\link{hsv2col}},
#'    \code{\link{col2hsv}},\code{\link{col2hcl}},
#'    \code{\link{rgb2col}},
#'    \code{\link{alpha2col}},\code{\link{col2alpha}},
#'    \code{\link{makeColorDarker}},
#'    \code{\link{color2gradient}},
#'    \code{\link{setTextContrastColor}},
#'    \code{\link{showColors}}
#'
#' @export
col2alpha <- function
(x, maxValue=1, ...)
{
   ## Purpose is to extract the alpha value for a set of colors defined in hex space,
   ## for those R tools that use the inconsistent method of defining alpha separate from
   ## the RGB color, although most tools should really be using RGBA format instead...
   xNA <- is.na(x);
   alphaValues <- col2rgb(x, alpha=TRUE)["alpha",]/255*maxValue;
   alphaValues[xNA] <- 0;
   return(alphaValues);
}

#' set R color alpha value
#'
#' Define the alpha transparency per R color
#'
#' @param x R compatible color, either a color name, or hex value, or
#'    a mixture of the two. Any value compatible with \code{\link{col2rgb}}.
#' @param alpha numeric alpha transparency to use per x color. alpha is
#'    recycled to length(x) as needed.
#' @param maxValue numeric maximum value to return, useful when the downstream
#'    alpha range should be 255. By default maxValue=1 is returned.
#'
#' @seealso jamba color manipulation functions: \code{\link{hsv2col}},
#'    \code{\link{col2hsv}},\code{\link{col2hcl}},
#'    \code{\link{rgb2col}},
#'    \code{\link{alpha2col}},\code{\link{col2alpha}},
#'    \code{\link{makeColorDarker}},
#'    \code{\link{color2gradient}},
#'    \code{\link{setTextContrastColor}},
#'    \code{\link{showColors}}
#'
#' @export
alpha2col <- function
(x, alpha=1, maxValue=1, ...)
{
   ## Purpose is change the alpha of a vector of colors to the one given.
   ## Note that NA values are left as NA values
   xNA <- is.na(x);
   alpha <- rep(alpha, length.out=length(x));
   rgbx <- rgb2col(rbind(col2rgb(x, alpha=FALSE), alpha=alpha*(255/maxValue)),
      maxColorValue=255);
   if (!is.null(names(x))) {
      names(rgbx) <- names(x);
   }
   rgbx[xNA] <- x[xNA];
   return(rgbx);
}

#' Convert HSV matrix to R color
#'
#' Converts a HSV color matrix to R hex color
#'
#' This function augments the \code{\link{hsv}} function in that it handles
#' output from \code{\link{rgb2hsv}} or \code{\link{col2hsv}}, sufficient to
#' run a series of conversion functions, e.g. \code{hsv2col(col2hsv("red"))}.
#' This function also maintains alpha transparency, which is not maintained
#' by the \code{\link{hsv}} function.
#'
#' @param hsvValue HSV matrix, with rownames c("h","s","v") in any order,
#'    and optionally "alpha" rowname for alpha transparency.
#'
#' @seealso jamba color manipulation functions: \code{\link{hsv2col}},
#'    \code{\link{col2hsv}},\code{\link{col2hcl}},
#'    \code{\link{rgb2col}},
#'    \code{\link{alpha2col}},\code{\link{col2alpha}},
#'    \code{\link{makeColorDarker}},
#'    \code{\link{color2gradient}},
#'    \code{\link{setTextContrastColor}},
#'    \code{\link{showColors}}
#'
#' @examples
#' # start with a color vector
#' # red and blue with partial transparency
#' colorV <- c("#FF000055", "#00339999");
#'
#' # confirm the hsv matrix maintains transparency
#' col2hsv(colorV);
#'
#' # convert back to the original color
#' hsv2col(col2hsv(colorV));
#'
#' @export
hsv2col <- function
(hsvValue,
 ...)
{
   ## Purpose is to augment the hsv() function which does not handle
   ## output from rgb2hsv(). It should be possible to run hsv2col(rgb2hsv(x)).
   ##
   ## This function also handles alpha.
   ##
   ## This function also allows value above 1, which have the effect of reducing
   ## the saturation.
   if (all(is.null(dim(hsvValue)))) {
      do.call(hsv, hsvValue, ...);
   } else {
      if (!"alpha" %in% rownames(hsvValue)) {
         hsvValue <- rbind(hsvValue,
            matrix(nrow=1, rep(1, ncol(hsvValue)),
               dimnames=c("alpha",list(colnames(hsvValue)))));
      }
      hsv(h=hsvValue["h",],
         s=hsvValue["s",],
         v=hsvValue["v",],
         alpha=hsvValue["alpha",]);
   }
}

#' Convert R color to HSV matrix
#'
#' Convert R color to HSV matrix
#'
#' This function takes a valid R color and converts to a HSV matrix. The
#' output can be effectively returned to R color with
#' \code{\link{hsv2col}}, usually after manipulating the
#' HSV color matrix.
#'
#' @return matrix of HSV colors
#'
#' @param x R color
#' @param ... additional parameters are ignored
#'
#' @seealso jamba color manipulation functions: \code{\link{hsv2col}},
#'    \code{\link{col2hsv}},\code{\link{col2hcl}},
#'    \code{\link{rgb2col}},
#'    \code{\link{alpha2col}},\code{\link{col2alpha}},
#'    \code{\link{makeColorDarker}},
#'    \code{\link{color2gradient}},
#'    \code{\link{setTextContrastColor}},
#'    \code{\link{showColors}}
#'
#' @examples
#' # start with a color vector
#' # red and blue with partial transparency
#' colorV <- c("#FF000055", "#00339999");
#'
#' # confirm the hsv matrix maintains transparency
#' col2hsv(colorV);
#'
#' # convert back to the original color
#' hsv2col(col2hsv(colorV));
#'
#' @export
col2hsv <- function
(x, ...)
{
   ## Purpose is to use col2rgb and rgb2hsv2 to convert colors to an hsv matrix
   rgbColors <- col2rgb(x, alpha=TRUE);
   hsvValues <- rgb2hsv(rgbColors[1:3,,drop=FALSE]);
   return(rbind(
      rgb2hsv(rgbColors[1:3,,drop=FALSE]),
      rgbColors["alpha",,drop=FALSE]/255));
}

#' Convert RGB color matrix to R color
#'
#' Convert RGB color matrix to R color
#'
#' This function intends to augment the \code{\link{rgb}} function, which
#' does not handle output from \code{\link{col2rgb}}. The goal is to handle
#' multiple color conversions, e.g. \code{rgb2col(col2rgb("red"))}. This
#' function also maintains alpha transparency when supplied.
#'
#' The output is named either by names(red), rownames(red), or if supplied,
#' the value of the parameter \code{names}.
#'
#' @param red numeric vector of red values; or RGB numeric matrix with
#'    rownames c("red","green","blue") in any order, with optional rowname
#'    "alpha"; or character strings with comma-separated rgb values, in
#'    format "100,20,10". The latter input is designed to handle web rgb
#'    values.
#' @param green numeric vector, or when red is a matrix or comma-delimited
#'    character string, this parameter is ignored.
#' @param blue numeric vector, or when red is a matrix or comma-delimited
#'    character string, this parameter is ignored.
#' @param alpha numeric vector, or when red is a matrix or comma-delimited
#'    character string, this parameter is ignored.
#' @param alpha numeric vector, or when red is a matrix or comma-delimited
#'    character string, this parameter is ignored.
#' @param maxColorValue numeric maximum value for colors. If NULL then it
#'    defaults to 1 unless there are values above 1, in which case it defaults
#'    to 255.
#' @param keepNA logical whether to keep NA values, returning NA for any
#'    input where red, green, and/or blue are NA. If keepNA==FALSE then it
#'    substitutes 0 for any NA values.
#'
#' @seealso jamba color manipulation functions: \code{\link{hsv2col}},
#'    \code{\link{col2hsv}},\code{\link{col2hcl}},
#'    \code{\link{rgb2col}},
#'    \code{\link{alpha2col}},\code{\link{col2alpha}},
#'    \code{\link{makeColorDarker}},
#'    \code{\link{color2gradient}},
#'    \code{\link{setTextContrastColor}},
#'    \code{\link{showColors}}
#'
#' @examples
#' # start with a color vector
#' # red and blue with partial transparency
#' colorV <- c("#FF000055", "#00339999");
#'
#' # Show the output of rgb2col
#' # make sure to include alpha=TRUE to maintain alpha transparency
#' col2rgb(colorV, alpha=TRUE);
#'
#' # confirm we can convert from RGB back to the same colors
#' rgb2col(col2rgb(colorV, alpha=TRUE));
#'
#' @export
rgb2col <- function
(red,
 green=NULL,
 blue=NULL,
 alpha=NULL,
 names=NULL,
 maxColorValue=NULL,
 keepNA=TRUE,
 ...)
{
   ## Purpose is to augment the function rgb() which does not handle output
   ## from col2rgb().
   ## The goal is to be able to run rgb2col(col2rgb()) and have it return
   ## the original colors.
   ##
   ## input here can be a matrix with columns c("red", "green", "blue") or
   ## comma-delimited text strings in the form "10,10,10" for red, green,
   ## and blue, respectively.
   ##
   ## maxColorValue is the highest color value, by default 1, but can be
   ## set to 255 to handle 8-bit colors.

   if (length(red) == 0 || is.na(red)) {
      return(red);
   }
   if (length(green) == 0 && length(blue) == 0) {
      if (igrepHas("character", class(red)) && igrepHas(",.+,", red)) {
         red <- rbindList(lapply(strsplit(red, ","), as.numeric));
         red[is.na(red)] <- 0;
         redCols <- 1:min(c(ncol(red),4));
         colnames(red)[redCols] <- c("red", "green", "blue", "alpha")[redCols];
      }
      if (is.matrix(red) || is.data.frame(red) || class(red) %in% c("RGB")) {
         red <- data.matrix(red);
         red[is.na(red)] <- 0;
         ## Note we allow red, green, blue, and yellow, the latter is allowed
         ## so we can use output from rgb2ryb() as input here
         rgbRownames <- c("red", "blue", "green", "R", "B", "G",
            "yellow", "Y", "alpha");
         if (nrow(red) >= 3 && sum(rownames(red) %in% rgbRownames) >= 3) {
            red <- t(red);
         }
         if (is.null(names) && !is.null(rownames(red))) {
            names <- rownames(red);
         }
         if (ncol(red) < 3) {
            stop("at least 3 columns needed");
         }
         rCol <- vigrep("^R$|red", colnames(red));
         gCol <- vigrep("^G$|green", colnames(red));
         bCol <- vigrep("^B$|blue", colnames(red));
         if (length(rCol) == 0) { rCol <- 1; }
         if (length(gCol) == 0) { gCol <- 2; }
         if (length(bCol) == 0) { bCol <- 3; }
         green <- red[,gCol];
         blue <- red[,bCol];
         if (ncol(red) >= 4) {
            alpha <- red[, 4];
         }
         red <- red[,rCol];
      }
   }
   if (is.null(maxColorValue)) {
      if (max(c(red, green, blue)) > 1) {
         maxColorValue <- 255;
      } else {
         maxColorValue <- 1;
      }
   }

   if (is.null(alpha)) {
      alpha <- maxColorValue;
   }

   ## Gracefully handle NA by substituting with zero
   anyNA <- (is.na(red) | is.na(green) | is.na(green));
   if (any(anyNA)) {
      red <- rmNA(red, naValue=0);
      green <- rmNA(green, naValue=0);
      blue <- rmNA(blue, naValue=0);
   }
   result <- grDevices::rgb(red=red, green=green, blue=blue,
      alpha=alpha, maxColorValue=maxColorValue, names=names);

   ## Optionally revert back to NA instead of using the zeros
   if (keepNA && any(anyNA)) {
      result[anyNA] <- NA;
   }
   return(result);
}

#' make R colors darker (or lighter)
#'
#' Makes R colors darker or lighter based upon darkFactor
#'
#' This function was originally intended to create border colors, or to
#' create slightly darker colors used for labels. It is also useful for
#' for making colors lighter, in adjusting color saturation up or down,
#' or applying alpha transparency during the same step.
#'
#' Note when colors are brightened beyond value=1, the saturation is
#' gradually reduced in order to produce a visibly lighter color. The
#' saturation minimu is set to 0.2, to maintain at least some amount of
#' color.
#'
#' @seealso jamba color manipulation functions: \code{\link{hsv2col}},
#'    \code{\link{col2hsv}},\code{\link{col2hcl}},
#'    \code{\link{rgb2col}},
#'    \code{\link{alpha2col}},\code{\link{col2alpha}},
#'    \code{\link{makeColorDarker}},
#'    \code{\link{color2gradient}},
#'    \code{\link{setTextContrastColor}},
#'    \code{\link{showColors}}
#'
#' @examples
#' colorV <- c("red","orange","purple","blue");
#' colorVdark2 <- makeColorDarker(colorV, darkFactor=2);
#' colorVlite2 <- makeColorDarker(colorV, darkFactor=-2);
#' showColors(cexCellnote=0.7,
#'    list(
#'    `darkFactor=2`=colorVdark2,
#'    `original colors`=colorV,
#'    `darkFactor=-2`=colorVlite2
#'    ));
#'
#' # these adjustments work really well inside a network diagram
#' # when coloring nodes, and providing an outline of comparable
#' # color.
#' plot(x=c(1,2,1,2), y=c(1,2,2,1), pch=21,
#'    xaxt="n", yaxt="n", xlab="", ylab="",
#'    xlim=c(0.5,2.5), ylim=c(0.5,2.5),
#'    bg=colorV, col=colorVdark2, cex=4, lwd=2);
#' points(x=c(1,2,1,2), y=c(1,2,2,1), pch=20, cex=4,
#'    col=colorVlite2);
#'
#' # Making a color lighter can make it easier to add labels
#' # The setTextContrastColor() function also helps.
#' text(x=c(1,2,1,2), y=c(1,2,2,1), 1:4,
#'    col=setTextContrastColor(colorVlite2));
#'
#' @export
makeColorDarker <- function
(hexColor,
 darkFactor=2,
 sFactor=1,
 fixAlpha=NULL,
 verbose=FALSE,
 keepNA=FALSE,
 useMethod=1,
 ...)
{
   ## Purpose is to make any hex color darker, by lowering the HSV value.
   ## Default settings will generally create a suitably darker color.
   ## However, this function is also efficient for adjusting colors lighter
   ## or darker, similarly in adjusting color saturation up or down.
   ##
   ## darkFactor centers at zero, positive values make colors darker, negative
   ## values make colors lighter.
   ##
   ## sFactor centers at zero, positive values make colors more saturated,
   ## negative values make colors less saturated.
   ##
   ## fixAlpha will apply a fixed level of alpha transparency to resulting
   ## colors. Sometimes this function is useful to create a border color, and
   ## sometimes that color should be less transparent than the input color.
   ##
   ## This function attempts to be efficient for very large vectors, by
   ## performing calculations only on the unique input colors, typically
   ## a much smaller set of colors.
   ##
   if (!is.null(fixAlpha)) {
      fixAlpha <- rep(fixAlpha, length.out=length(hexColor));
   } else {
      fixAlpha <- col2alpha(hexColor);
   }

   ## Optimization step: convert only the unique colors...
   hexColorAll <- hexColor;
   darkFactorAll <- darkFactor;
   sFactorAll <- sFactor;
   hexColorAllNames <- gsub("_$", "",
      paste(rmNA(naValue="transparent", hexColor),
         darkFactor, sFactor, fixAlpha, sep="_"));
   fixAlphaAll <- fixAlpha;
   if (length(fixAlpha) == 0) {
      hexColorUniq <- unique(data.frame(stringsAsFactors=FALSE,
         check.names=FALSE,
         "hexColor"=rmNA(hexColor, naValue="transparent"),
         "darkFactor"=darkFactor,
         "sFactor"=sFactor,
         "fixAlpha"="",
         row.names=NULL));
   } else {
      hexColorUniq <- unique(data.frame(stringsAsFactors=FALSE,
         check.names=FALSE,
         "hexColor"=rmNA(hexColor, naValue="transparent"),
         "darkFactor"=darkFactor,
         "sFactor"=sFactor,
         "fixAlpha"=fixAlpha,
         row.names=NULL));
   }
   hexColnames <- c("hexColor","darkFactor","sFactor","fixAlpha");
   rownames(hexColorUniq) <- pasteByRow(hexColorUniq[,hexColnames,drop=FALSE],
      sep="_");
   hexMatrix <- grDevices::col2rgb(hexColorUniq[,"hexColor"], alpha=TRUE);
   if (verbose) {
      printDebug("hexColorUniq:");
      ch(hexColorUniq);
      printDebug("hexMatrix:");
      ch(hexMatrix);
   }

   darkFactors <- hexColorUniq[,"darkFactor"];
   sFactors <- hexColorUniq[,"sFactor"];

   ## Adjust factor logic here
   adjustFactor <- function
   (val, adjFactor)
   {
      ## Purpose is to tweak a number that is fixed between 0 and 1.
      ## If adjFactor is 1 or higher, or -1 or lower, it is used as-is.
      ## If adjFactor is 0.5, it is converted to -2.
      ## If adjFactor is -0.5, it is convert to 2.
      ##
      ## For positive adjustment, the value is scaled between itself and 1.
      ## For negative adjustment, the value is scaled between itself and 0.
      ##
      ## The adjustment equation, assuming the abs(adjFactor)>=1:
      ##
      ## 1 - (1 / 1) ==> no adjustment
      ## 1 - (1 / 2) ==> halfway adjustment
      ## 1 - (1 / 3) ==> 2/3rds adjustment
      ## 1 - (1 / 4) ==> 3/4ths adjustment
      ##
      ## Convert fractional adjFactors
      adjFraction <- (abs(adjFactor) > 0 & abs(adjFactor) < 1);
      if (any(adjFraction)) {
         adjFactor[adjFraction] <- -1/adjFactor[adjFraction];
      }
      ##
      valDiff <- ifelse(adjFactor >= 0, 1-val, -val);
      adj2 <- (1 - (1 / abs(adjFactor)));
      adj3 <- adj2 * valDiff + val;
      return(adj3);
   }

   adjustFactorTwoStep <- function
   (val, adjFactor, val2, ...)
   {
      ## Purpose is to implement scaling from 0 to 1, where
      ## there is a second step we apply to another value.
      ## E.g. scale brightness by adjusting value 0 to 1, but
      ## define another "step" from 1 to 2, where value stays
      ## 1, but saturation goes from 1 back to 0.
      adjFraction <- (abs(adjFactor) > 0 & abs(adjFactor) < 1);
      if (any(adjFraction)) {
         adjFactor[adjFraction] <- -1/adjFactor[adjFraction];
      }
      ##
      valDiff <- ifelse(adjFactor >= 0, 2-val, -val);
      adj2 <- (1 - (1 / abs(adjFactor)));
      adj3 <- adj2 * valDiff + val;
      #val2[adj3 > 1] <- (val2 * (2-adj3))[adj3 > 1];
      n <- 1.25;
      val2new <- val2 * (2- (adj3 + n - 1)/n);
      val2[adj3 > 1] <- val2new[adj3 > 1];
      return(data.frame(val=noiseFloor(adj3, ceiling=1), val2=val2));
   }

   ## TODO: implement brightening of fully-bright colors
   ## by reducing saturation.
   ## E.g. define the gradient not just from value 0 to 1, but
   ## value from 0 to 1 to 2
   ## saturation 0 to 1 to 0
   if (useMethod %in% 1) {
      #printDebug("New method.");
      j <- rbind(grDevices::rgb2hsv(r=hexMatrix["red",],
         g=hexMatrix["green",],
         b=hexMatrix["blue",]),
         hexMatrix["alpha",,drop=FALSE]/255);
      newVL <- adjustFactorTwoStep(j["v",], adjFactor=-darkFactor,
         val2=j["s",])
      newV <- newVL$val;
      newS1 <- newVL$val2;
      j["v",] <- newV;
      j["s",] <- newS1;
      newS <- noiseFloor(minimum=0,
         ceiling=1,
         adjustFactor(j["s",], adjFactor=sFactor));
      j["s",] <- newS;
      darkerColors <- hsv2col(j);
   } else {
      darkerColors <- sapply(1:ncol(hexMatrix), function(i1){
         i <- hexMatrix[,i1];
         if (verbose) {
            printDebug("i:", c("orange", "lightblue"));
            print(i);
         }
         j <- as.vector(grDevices::rgb2hsv(r=i[1], g=i[2], b=i[3]));
         sFactor <- sFactors[i1];
         darkFactor <- darkFactors[i1];
         ## We flip the sign because it is a darkFactor, so we
         ## should be making things darker than before...
         newV <- 1-adjustFactor(1-j[3], darkFactor);
         newS <- adjustFactor(j[2], sFactor);
         ## crude fix so grey doesn't become brown by mistake
         newS[j[2] == 0] <- 0;
         tryCatch({
            hsv1 <- hsv(h=j[1], s=newS, v=newV, alpha=i[4]/255);
            hsv1;
         }, error=function(e){
            printDebug("Error: ", cPaste(e), fgText=c("yellow", "red"));
            printDebug("h: ", format(digits=2, j[1]),
                         ", s: ", format(digits=2, newS),
                         ", v: ", format(digits=2, newV),
                         ", oldV: ", format(digits=2, j[3]),
                         ", darkFactor: ", format(digits=2, darkFactor),
                         ", alpha: ", format(digits=2, i[4]/255), c("orange", "lightblue") );
            hsv(h=j[1], s=newS, v=newV, alpha=i[4]/255);
         })
      });
   }

   ## Expand colors to the original vector length
   darkerColors <- darkerColors[match(hexColorAllNames, rownames(hexColorUniq))];
   if (!is.null(names(hexColor))) {
      names(darkerColors) <- names(hexColor);
   }
   if (!is.null(fixAlpha)) {
      darkerColors <- alpha2col(darkerColors, alpha=fixAlpha);
   }
   if (keepNA && any(is.na(hexColor))) {
      darkerColors[is.na(hexColor)] <- NA;
   }
   return(darkerColors);
}

#' get color ramp by name, color, or function
#'
#' get color ramp by name, color, or function
#'
#' This function accepts a color ramp name, a single color,
#' a vector of colors, or a function names, and returns a simple
#' vector of colors of the appropriate length, suitable as input
#' to a number of plotting functions.
#'
#' For example, it recognizes \code{\link{RColorBrewer}} color ramp
#' names, but can reverse those color ramps with a suffix "_r" at the
#' end, e.g. "RdBu_r" will supply a color ramp from blue to red, suitable
#' for heatmaps where red is typically associated with heat and high
#' numeric values.
#'
#' @param col accepts:
#'    \describe{
#'       \item{"character vector"}{one or more colors used to define a color
#'          gradient. Where one color is supplied, a gradient is created from
#'          defaultBaseColor to the supplied color.}
#'       \item{"character vector"}{one name matching a known color ramp either
#'          from \code{\link[RColorBrewer]{brewer.pal.info}}, or from the
#'          \code{\link[viridis]{viridis}} package.}
#'    }
#' @param trimRamp logical whether to trim off the extreme values of the
#'    color ramp, for example to remove the very lightest and very darkest
#'    colors. If trimRamp is an integer, then that many colors are removed.
#' @param verbose logical whether to print verbose output
#' @param defaultBaseColor character vector indicating a color from which to
#'    begin a color gradient, only used when col is a single color.
#' @param reverseRamp logical indicating whether to reverse the resulting
#'    color ramp. This value is ignored when a single value is supplied for
#'    col, and where "_r" or "_rev" is detected as a substring at the end
#'    of the character value.
#' @param alpha logical indicating whether to honor alpha transparency
#'    whenever \code{\link{colorRampPalette}} is called. If colors contain
#'    no alpha transparency, this setting has no effect, otherwise the
#'    alpha value appears to be applied using a linear gradient between
#'    colors.
#'
#' @examples
#' # get a gradient using red4
#' red4 <- getColorRamp("red4");
#' showColors(getColorRamp(red4));
#'
#' # make a custom gradient
#' BuOr <- getColorRamp(c("dodgerblue","grey10","orange"));
#' showColors(BuOr);
#' colorList <- list(red4=red4, BuOr=BuOr);
#'
#' # If RColorBrewer is available, use a brewer name
#' if (suppressPackageStartupMessages(require(RColorBrewer))) {
#'    RdBu <- getColorRamp("RdBu");
#'    RdBu_r <- getColorRamp("RdBu_r");
#'    colorList <- c(colorList, list(RdBu=RdBu, RdBu_r=RdBu_r));
#'    showColors(RdBu);
#' }
#'
#' if (suppressPackageStartupMessages(require(viridis))) {
#'    viridisV <- getColorRamp("viridis");
#'    colorList <- c(colorList, list(viridis=viridisV));
#' }
#'
#' # for fun, put a few color ramps onto one plot
#' showColors(colorList, cexCellnote=0.7);
#'
#' @export
getColorRamp <- function
(col,
 n=15,
 trimRamp=FALSE,
 verbose=FALSE,
 defaultBaseColor="grey90",
 reverseRamp=FALSE,
 alpha=TRUE,
 ...)
{
   ## Purpose is to wrapper the steps needed to take a colorRamp
   ## in the form of a recognized name, or a set of colors, and
   ## consistently return only the set of colors
   ##
   ## if "_r" is used as a suffix, the colorRamp is reversed
   ## if reverseRamp==TRUE, the colorRamp is reversed
   ##
   if (igrepHas("character", class(col)) && igrepHas("_r$", col)) {
      reverseRamp <- !reverseRamp;
      col <- gsub("_r$", "", col);
   }
   if (igrepHas("character", class(col)) && length(col) == 1) {
      if (col %in% c("viridis","inferno","plasma","magma")) {
         if (verbose) {
            printDebug("getColorRamp(): ", "viridis color function.");
         }
         if (!suppressPackageStartupMessages(require(viridis))) {
            stop(paste0("The viridis package is required for color ramps:",
               " viridis,inferno,magma,plasma."));
         }
         funcName <- gsub("_(rev|r).*$", "", col);
         if(verbose) {
            printDebug("funcName:", col);
         }
         colorFunc <- get(col, mode="function");
         if (is.null(n)) {
            cols <- colorFunc(15);
         } else {
            cols <- colorFunc(n + trimRamp*2);
         }
         if (reverseRamp) {
            cols <- rev(cols);
         }
         if (trimRamp) {
            if (verbose) {
               printDebug("trimRamp");
            }
            cols <- tail(head(cols, -1*abs(trimRamp)), -1*abs(trimRamp));
         }
         if (is.null(n)) {
            cols <- colorRampPalette(cols, alpha=alpha);
         }
      } else if (col %in% RColorBrewer:::namelist) {
         if (verbose) {
            printDebug("getColorRamp(): ", "RColorBrewer color function.");
         }
         if (!suppressPackageStartupMessages(require(RColorBrewer))) {
            stop(paste0("The RColorBrewer package is required for",
               "getColorRamp, when only one col is specified."));
         }
         if (is.null(n)) {
            cols <- brewer.pal(15, col);
         } else {
            cols <- brewer.pal(n, col);
         }
         if (reverseRamp) {
            cols <- rev(cols);
         }
         if (trimRamp) {
            if (verbose) {
               printDebug("trimRamp");
            }
            cols <- tail(head(cols, -1), -1);
         }
         if (is.null(n)) {
            cols <- colorRampPalette(cols, alpha=alpha);
         } else {
            cols <- colorRampPalette(cols, alpha=alpha)(n);
         }
      } else {
         ## If given one or more colors, use them to create a color ramp
         if (verbose) {
            printDebug("getColorRamp(): ", "checking character input.");
         }
         colset <- col[isColor(col)];
         if (length(colset) > 0) {
            if (verbose) {
               printDebug("getColorRamp(): ", "color input.");
            }
            if (length(colset) == 1) {
               ## If given one color, make a color ramp from white to this color
               colset <- c(defaultBaseColor, colset);
            }
            if (reverseRamp) {
               colset <- rev(colset);
            }
            if (is.null(n)) {
               if (trimRamp) {
                  cols <- colorRampPalette(
                     tail(head(
                        colorRampPalette(colset, alpha=alpha)(15), -1), -1),
                     alpha=alpha);
               } else {
                  cols <- colorRampPalette(colset, alpha=alpha);
               }
            } else {
               cols <- colorRampPalette(colset, alpha=alpha)(n+trimRamp*2);
               if (trimRamp) {
                  cols <- tail(head(cols, -1), -1);
               }
            }
         } else {
            ## Check if we are supplied a function name
            if (verbose) {
               printDebug("getColorRamp(): ",
                  "checking color function name input.");
            }
            colorFunc <- tryCatch({
               get(col, mode="function");
            }, error=function(e){
               printError(e, callName="get function");
               NULL;
            });
            ## If not a function, we stop here
            if (is.null(colorFunc)) {
               stop(paste0("The supplied color could not be used to create",
                  " a color ramp, col:", cPaste(col)));
            }
            if (!is.null(n)) {
               cols <- colorFunc(n + trimRamp*2);
               if (reverseRamp) {
                  cols <- rev(cols);
               }
               if (trimRamp) {
                  cols <- tail(head(cols, -1), -1);
               }
            } else {
               colorFactory <- function(n, tr1, rr1, cf1) {
                  function(n=15, ...) {
                     cols <- cf1(n + tr1*2);
                     if (rr1) {
                        cols <- rev(cols);
                     }
                     if (tr1) {
                        cols <- tail(head(cols, -1), -1);
                     }
                     return(cols);
                  }
               }
               cols <- colorFactory(tr1=trimRamp, rr1=reverseRamp,
                  cf1=colorFunc);
            }
         }
      }
   } else if (igrepHas("function", class(col))) {
      if (verbose) {
         printDebug("getColorRamp(): ", "color function input.");
      }
      colorFactory <- function(n, tr1, rr1, cf1) {
         function(n=15, ...) {
            cols <- cf1(n + tr1*2);
            if (rr1) {
               cols <- rev(cols);
            }
            if (tr1) {
               cols <- tail(head(cols, -1), -1);
            }
            return(cols);
         }
      }
      cols <- colorFactory(tr1=trimRamp, rr1=reverseRamp, cf1=col);
      if (!is.null(n)) {
         cols <- cols(n);
      }
   } else {
      if (verbose) {
         printDebug("getColorRamp(): ", "unrecognized color input.");
      }
      cols <- colorRampPalette(col, alpha=alpha)(n);
   }
   return(cols);
}

#' detect valid R color
#'
#' detect valid R color
#'
#' This function determines whether each element in a vector is a valid R
#' color, based upon the R color names, valid hex color format, and the
#' word "transparent" which is valid as an R color.
#'
#' @param x character vector of potential R colors
#' @param makeNamesFunc function used to make names for the resulting vector
#' @param ... additional parameters are ignored
#'
#' @export
isColor <- function
(x, makeNamesFunc=c, ...)
{
   ## Purpose is to check if a given text string is a valid R color
   allColors <- colors();
   grepString <- "^#[0-9A-F]{6}$|^#[0-9A-F]{8}$|^#[0-9A-F]{3}$|transparent";
   validSet <- c(igrep(grepString, x), which(x %in% allColors));
   validBoolean <- nameVector(seq_along(x) %in% validSet, x,
      makeNamesFunc=makeNamesFunc);
   return(validBoolean);
}

#' Make a color gradient
#'
#' Make a color gradient
#'
#' This function converts a single color into a color gradient by expanding
#' the initial color into lighter and darker colors around the central color.
#' The amount of gradient expansion is controlled by gradientWtFactor, which
#' is a weight factor scaled to the maximum available range of bright to
#' dark colors.
#'
#' As an extension, the function can take a vector of colors, and expand each
#' into its own color gradient, each with its own number of colors.
#' If a vector with supplied that contains repeated colors, these colors
#' are expanded in-place into a gradient, bypassing the value for \code{n}.
#'
#' If a list is supplied, a list is returned of the same length, where
#' each vector inside the list is a color gradient of length specified
#' by \code{n}. If the input list contains multiple values, only the first
#' color is used to define the color gradient.
#'
#' @param col vector of one or more colors, or list of color vectors, or
#'    vector of repeated colors. If the vector contains unique colors, each
#'    color is expanded into a gradient of length \code{n}, where \code{n} is
#'    recycled for each input color. If the vector contains repeated colors,
#'    they are expanded in place.
#' @param n integer vector of length one or more, which defines the number
#'    of colors to return for each gradient.
#' @param gradientWtFactor numeric fraction representing the amount to expand
#'    a color toward its maximum brightness and darkness.
#' @param reverseGradient logical whether to return light-to-dark gradient
#'    (TRUE) or dark-to-light gradient (FALSE).
#' @param verbose logical whether to print verbose output.
#' @param ... other parameters are ignored.
#'
#' @seealso jamba color manipulation functions: \code{\link{hsv2col}},
#'    \code{\link{col2hsv}},\code{\link{col2hcl}},
#'    \code{\link{rgb2col}},
#'    \code{\link{alpha2col}},\code{\link{col2alpha}},
#'    \code{\link{makeColorDarker}},
#'    \code{\link{color2gradient}},
#'    \code{\link{setTextContrastColor}},
#'    \code{\link{showColors}}
#'
#' @examples
#' # given a list, it returns a list
#' x <- color2gradient(list(Reds=c("red"), Blues=c("blue")), n=c(4,7));
#' showColors(x);
#'
#' # given a vector, it returns a vector
#' xv <- color2gradient(c(red="red", blue="blue"), n=c(4,7));
#' showColors(xv);
#'
#' # Expand colors in place
#' # This process is similar to color jittering
#' colors1 <- c("red","blue")[c(1,1,2,2,1,2,1,1)];
#' names(colors1) <- colors1;
#' colors2 <- color2gradient(colors1);
#' colors2;
#' showColors(list(`Input colors`=colors1, `Output colors`=colors2));
#'
#' # You can do the same using a list intermediate
#' colors1L <- split(colors1, colors1);
#' showColors(colors1L);
#' colors2L <- color2gradient(colors1L);
#' showColors(colors2L);
#'
#' @export
color2gradient <- function
(col, n=NULL, gradientWtFactor=2/3, reverseGradient=TRUE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to take a single color and create a light->dark gradient
   ##
   ## n can be a single value, or a vector of values to be applied to
   ## col in order
   ##
   ## if col is a vector of repeated colors, the colors will be split
   ## and converted to a gradient per color
   sMin <- 0.1;
   sMax <- 1;
   vMin <- 0.1;
   vMax <- 1;
   wtFactor <- gradientWtFactor;

   ## Expand n to the length of col
   if (!igrepHas("list", class(col))) {
      if (is.null(names(col))) {
         names(col) <- makeNames(col);
      }
      colOrig <- col;
      ## Note that using split() orders the data by the sort() of the names
      ## so we order by the original colors afterward to keep
      col <- split(col, col)[unique(col)];
      if (verbose) {
         printDebug("col:");
         print(head(col, 20));
      }
   } else {
      colOrig <- NULL;
   }
   if (is.null(names(col))) {
      names(col) <- makeNames(rep("col", length(col)));
   }
   ## If not given n, and if all entries are one color,
   ## we expand each gradient an equal amount
   doExpand <- FALSE;
   if (is.null(n) && all(lengths(col) == 1)) {
      n <- 3;
   }
   if (all(lengths(col) == 1)) {
      doExpand <- TRUE;
   }
   ## If not all entries are length=1, we set n to
   ## the length of each vector in the list. Intended
   ## for making a vector of colors visually distinct
   if (is.null(n)) {
      n <- lengths(col);
   }
   n <- rep(n, length.out=length(col));
   names(n) <- names(col);
   if (verbose) {
      printDebug("color2gradient() running.", c("orange", "lightblue"));
      printDebug("   col:", c("orange", "lightblue"));
      print(head(col, 10));
      printDebug("     n:", c("orange", "lightblue"));
      print(head(n, 10));
   }

   newColorSets <- lapply(nameVectorN(col), function(iName){
      i <- col[[iName]];
      if (verbose) {
         printDebug("i:", c("orange", i));
         print(head(i, 20));
      }
      if (length(unique(i)) > 1) {
         i <- head(i, 1);
      }
      hsvValues <- col2hsv(i);
      iLen <- n[iName];
      if (verbose) {
         printDebug("iLen:", iLen, c("orange", "lightblue"));
      }
      if (iLen == 1) {
         if (is.null(names(i))) {
            return(nameVector(head(i, 1), iName));
         } else {
            return(head(i, 1));
         }
      }
      sValue <- hsvValues["s",1];
      vValue <- hsvValues["v",1];
      sRange <- approx(x=unique(c(
            weighted.mean(c(sMax, sValue), w=c(wtFactor, 1)),
            sValue,
            weighted.mean(c(sMin, sValue), w=c(wtFactor, 1)))),
         n=iLen)$y;
      ## Keep grey as grey and not some random muddy color
      if (sValue == 0) {
         sRange <- sRange - sRange;
      }
      vRange <- approx(x=unique(c(
            weighted.mean(c(vMin, vValue), w=c(wtFactor, 1)),
            vValue,
            weighted.mean(c(vMax, vValue), w=c(wtFactor, 1)))),
         n=iLen)$y;
      hRange <- rep(hsvValues["h",1], iLen);
      alphaRange <- rep(hsvValues["alpha",1], iLen);
      newColors <- hsv(h=hRange,
         s=sRange,
         v=vRange,
         alpha=alphaRange);
      if (reverseGradient) {
         newColors <- rev(newColors);
      }
      if (is.null(names(i))) {
         names(newColors) <- makeNames(rep(iName,
            length.out=length(newColors)));
      } else {
         names(newColors) <- makeNames(rep(names(i),
            length.out=length(newColors)));
      }
      if (verbose) {
         printDebug("newColors:");
         print(head(newColors, 20));
      }
      newColors;
   });
   if (!is.null(colOrig)) {
      if (verbose) {
         printDebug("colOrig:", c("orange", "lightblue"));
         print(head(colOrig, 20));
         printDebug("newColorSets:", c("orange", "lightblue"));
         print(head(newColorSets, 20));
      }
      ## Remove list names before unlist() so the vector
      ## names are applied
      names(newColorSets) <- NULL;
      if (doExpand) {
         newColorSets <- unlist(newColorSets);
      } else {
         newColorSets <- unlist(newColorSets)[makeNames(names(colOrig))];
      }
   }
   return(newColorSets);
}

#' Extend kableExtra colorization of tables
#'
#' Extend kableExtra colorization of tables in Rmarkdown files
#'
#' This function extends the `kableExtra` package, and is only
#' available for use if the `kableExtra` package is installed. It is
#' intended to allow specific color assignment of elements in a
#' data.frame, but otherwise uses the `kableExtra` functions to
#' apply those colors.
#'
#' @param df `data.frame`
#' @param colorSub named vector of R colors, whose names match entries
#'    in the `data.frame` which are given these assigned colors.
#' @param background_as_tile boolean defining whether a cell background
#'    color will appear as a rounded tile (if TRUE), or a rectangle
#'    (if FALSE.)
#' @param color_cells boolean indicating whether to color individual cells
#' @param row_color_by optional vector of `colnames` found in `df`
#'    indicating how to colorize entire rows of a table. The typical
#'    colorization by cell will not fill the whole cell per row, so
#'    this colorization can be helpful to fill the remaining area.
#' @param sep character delimiter used to combine values in multiple
#'    columns when `row_color_by` is supplied and contains multiple
#'    `colnames`. The delimited character strings are compared to
#'    `colorSub` to assign colors.
#' @param return_type character string indicating the type of data
#'    to return. If `row_color_by` is supplied, then the only
#'    `"kable"` is allowed, otherwise `"data.frame"` is returned.
#'    Note that the `"kable"` option runs `%>% row_spec()` which
#'    returns an object only usable in downstream `kable` calls.
#' @param verbose boolean indicating whether to print verbose output.
#'
#' @return `data.frame` or `kable` data.frame dependent upon the
#'    `return_type` argument. Note that even the `data.frame` returned
#'    will have colors encoded into each cell, so it will likely
#'    not be possible to manipulate as normal.
#'
#' @examples
#' # Note the output will produce HTML code, which will
#' # open the Rstudio viewer when run inside Rstudio.
#' kable_coloring(
#'    df=data.frame(A=letters[1:5], B=LETTERS[1:5]),
#'    colorSub=nameVector(rainbow(10), c(letters[1:5], LETTERS[1:5])),
#'    row_color_by="B")
#'
#' @export
kable_coloring <- function
(df,
 colorSub=NULL,
 background_as_tile=TRUE,
 color_cells=TRUE,
 row_color_by=NULL,
 sep="_",
 return_type=c("kable","data.frame"),
 verbose=FALSE,
...)
{
   ## Purpose is to enhance kableExtra by automating some manual steps
   ## required to colorize text fields in a data.frame
   ##
   ## The input is expected to be a data.frame
   if (!suppressPackageStartupMessages(require(kableExtra))) {
      stop("kable_coloring requires the kableExtra package");
   }
   if (!suppressPackageStartupMessages(require(dplyr))) {
      stop("kable_coloring requires the dplyr package");
   }
   return_type <- match.arg(return_type);

   if (return_type %in% "kable" || length(row_color_by) > 0) {
      df1 <- df;
   }

   if (color_cells) {
      for (i in colnames(df)) {
         if (any(as.character(df[[i]]) %in% names(colorSub))) {
            if (verbose) {
               printDebug("kable_coloring(): ",
                  "colorizing column:",
                  i);
            }
            df[[i]] <- cell_spec(df[[i]],
               background_as_tile=background_as_tile,
               color=setTextContrastColor(
                  rmNA(colorSub[as.character(df[[i]])],
                     naValue="transparent")
               ),
               background=rmNA(colorSub[as.character(df[[i]])],
                  naValue="transparent"))
         } else {
            if (verbose) {
               printDebug("kable_coloring(): ",
                  "No values to colorize in column:",
                  i,
                  fgText=c("orange","red"));
            }
         }
      }
   }

   ## Convert to kable
   if (return_type %in% "kable" || length(row_color_by) > 0) {
      df <- df %>%
         kable(escape=FALSE,
            ...) %>%
         kable_styling();
   }

   ## Optionally color rows using values in a column
   if (length(row_color_by) > 0) {
      if (verbose) {
         printDebug("kable_coloring(): ",
            "row_color_by:",
            row_color_by);
      }
      row_color_by_value <- as.character(
         pasteByRow(df1[,row_color_by,drop=FALSE],
            sep=sep));
      row_colors <- colorSub[row_color_by_value];
      if (verbose) {
         printDebug("kable_coloring(): ",
            "row_color_by_value:",
            row_color_by_value);
         printDebug("kable_coloring(): ",
            "row_colors:",
            row_colors);
      }
      row_colors_valid <- unique(rmNA(row_colors));
      for (row_color in row_colors_valid) {
         which_rows <- which(row_colors %in% row_color);
         if (verbose) {
            printDebug("kable_coloring(): ",
               "colorizing rows:",
               which_rows,
               closestRcolor(row_color),
               fgText=c("orange","lightblue",
                  "orange",
                  row_color))
         }
         df <- df %>%
            row_spec(which_rows,
               background=row_color,
               color=setTextContrastColor(row_color));
      }
   }

   df;
}


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
#' typically using either white or black. The `useGrey` argument
#' defines the offset from pure white and pure black, to use a
#' contrasting grey shade.
#'
#' The `color` is expected to represent a background color, the
#' output is intended to be a color with enough contrast to read
#' text legibly.
#'
#' The brightness of the `color` is detected dependent upon
#' the `colorModel`: when `"hcl"` the luminance `L` is compared
#' to `hclCutoff`; when `"rgb"` the brightness is the sum of
#' the RGB channels which is compared to `rgbCutoff`. In most
#' cases the `"hcl"` and `L` will be more accurate.
#'
#' When `color` contains transparency, an optional argument `bg`
#' represents the figure background color, as if the `color` is
#' used to draw a color-filled rectangle. In this case, the `bg`
#' and `color` are combined to determine the resulting actual color.
#' This scenario is mostly useful when plotting text labels on
#' a dark background, such as black background with colored
#' text boxes.
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
#'    L value from the `col2hcl()` function, which scales colors from
#'    1 to 100.
#' @param useGrey numeric threshold used to define dark and bright text colors,
#'    using the R greyscale gradient from 0 to 100: `useGrey=10` implies
#'    `"grey10"` and `"grey90"` for the contrasting text colors;
#'    `useGrey=15` is useful if labels may also overlap white or black space,
#'    since the text will never be fully white or black.
#' @param keepAlpha logical indicates whether the input color alpha
#'    transparency should be maintained in the text color. By default, text
#'    alpha is not maintained, and instead is set to alpha=1, fully
#'    opaque.
#' @param alphaLens numeric value used to adjust the effect of alpha
#'    transparency, where positive values emphasize the background color,
#'    and negative values emphasize the foreground (transparent) color.
#' @param bg vector of R colors, used as a background when determining the
#'    brightness of a semi-transparent color. The corresponding brightness
#'    value from the `bg` is applied via weighted mean to the input
#'    `color` brightness, the result is compared the the relevant cutoff.
#'    By default `graphics::par("bg")` is used to determine the default
#'    plot background color, only when there is an open graphics device,
#'    otherwise calling `graphics::par("bg")` would open a graphics
#'    device, which is not desireable. When no graphics device is open,
#'    and when `bg=NULL`, the default is `bg="white"`.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' color <- c("red","yellow","lightblue","darkorchid1","blue4");
#' setTextContrastColor(color);
#'
#' # showColors() uses setTextContrastColor() for labels
#' showColors(color)
#' # printDebugI() uses setTextContrastColor() for foreground text
#' printDebugI(color)
#'
#' # demonstrate the effect of alpha transparency
#' colorL <- lapply(nameVector(c(1, 0.9, 0.8, 0.6, 0.3)), function(i){
#'    nameVector(alpha2col(color, alpha=i), color);
#' })
#' jamba::showColors(colorL,
#'    groupCellnotes=FALSE,
#'    srtCellnote=seq(from=15, to=-15, length.out=5));
#' graphics::title(ylab="alpha", line=1.5);
#'
#' # change background to dark blue
#' withr::with_par(list("bg"="navy", "col"="white", "col.axis"="white"), {
#' jamba::showColors(colorL,
#'    groupCellnotes=FALSE,
#'    srtCellnote=seq(from=15, to=-15, length.out=5))
#' graphics::title(ylab="alpha", line=1.5);
#' })
#'
#' # Example using transparency and drawLabels()
#' bg <- "blue4";
#' col <- fixYellow("palegoldenrod");
#' nullPlot(fill=bg, plotAreaTitle="", doMargins=FALSE);
#' for (alpha in c(0.1, 0.3, 0.5, 0.7, 0.9)) {
#'    labelCol <- setTextContrastColor(
#'       alpha2col("yellow", alpha),
#'       bg=bg);
#'    drawLabels(x=1 + alpha,
#'       y=2 - alpha,
#'       labelCex=1.5,
#'       txt="Plot Title",
#'       boxColor=alpha2col(col, alpha),
#'       boxBorderColor=labelCol,
#'       labelCol=labelCol);
#' }
#'
#' @family jam color functions
#'
#' @returns `character` vector of R colors.
#'
#' @export
setTextContrastColor <- function
(color,
 hclCutoff=60,
 rgbCutoff=127,
 colorModel=c("hcl", "rgb"),
 useGrey=0,
 keepAlpha=FALSE,
 alphaLens=0,
 bg=NULL,
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
   bwColors <- rgb2col(grDevices::col2rgb(paste0("grey", greyVals)));

   if (length(bg) == 0) {
      if (length(grDevices::dev.list()) > 0) {
         bg <- graphics::par("bg");
      } else {
         bg <- "white";
      }
   }

   if (colorModel %in% "rgb") {
      colRgbMean <- colMeans(grDevices::col2rgb(color));
      if (any(col2alpha(unique(color)) < 1)) {
         ## If any color is transparent, use weighted mean with the
         ## background color
         colWeight <- col2alpha(color);
         colRgbBg <- colMeans(grDevices::col2rgb(bg));
         colRgbMean <- (colRgbMean * colWeight + colRgbBg * (1 - colWeight));
      }
      iColor <- ifelse(colRgbMean > rgbCutoff,
         bwColors[1],
         bwColors[2]);
   } else {
      colL <- col2hcl(color)["L",];
      if (any(col2alpha(unique(color)) < 1)) {
         bgL <- col2hcl(bg)["L",];
         colWeight <- col2alpha(color);
         warpWeight <- warpAroundZero(1-colWeight, xCeiling=1, lens=alphaLens);
         colL <- ((colL) * (1-warpWeight) + (bgL) * warpWeight);
      }
      iColor <- ifelse(colL > hclCutoff,
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
#' When `model="hcl"` this function uses `farver::decode_colour()`
#' and bypasses `colorspace`. In future the `colorspace` dependency
#' will likely be removed in favor of using `farver`. In any event,
#' `model="hcl"` is equivalent to using `model="polarLUV"` and
#' `fixup=TRUE`, except that it should be much faster.
#'
#' @param x `character` R compatible color, either a color name, hex value, or
#'    a mixture of the two. Any value compatible with
#'    `grDevices::col2rgb()`.
#' @param maxColorValue `numeric` maximum value to return, useful
#'    when the downstream alpha range should be 255.
#'    By default maxValue=1 is returned.
#' @param model `character` color model to use
#'    * `"hcl"` to use `farver` HCL
#'    * `"polarLUV"` for the standard R conventional HCL,
#'    * `"polarLAB"` which uses the LAB-based HCL values.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' col2hcl("#FF000044")
#'
#' @family jam color functions
#'
#' @returns `numeric` matrix with H, C, L values.
#'
#' @export
col2hcl <- function
(x,
 maxColorValue=255,
 model=getOption("jam.model", c("hcl", "polarLUV", "polarLAB")),
 ...)
{
   ## Purpose is to convert R color to HCL
   ## R color can be a hex string or color name from grDevices::colors()
   model <- head(model, 1);
   if ("jam.model" %in% names(options())) {
      model <- getOption("jam.model");
   }
   if ("hcl" %in% model &&
         !requireNamespace("farver", quietly=TRUE)) {
      model <- "polarLUV";
      fixup <- TRUE;
   }

   if ("hcl" %in% model) {
      x3 <- t(farver::decode_colour(x,
         to="hcl",
         alpha=TRUE,
         ...));
      colnames(x3) <- names(x);
      rownames(x3)[1:3] <- toupper(rownames(x3)[1:3]);
      return(x3);
   }
   if (!requireNamespace("colorspace", quietly=TRUE)) {
      stop("The colorspace package is required.");
   }

   x1 <- grDevices::col2rgb(x);
   a1 <- col2alpha(x);
   x2 <- colorspace::sRGB(t(x1)[,1:3,drop=FALSE]/maxColorValue);
   ## Note: spatstat overrides coords() with a generic function but
   ## the colorspace function is not properly dispatched for class RGB.
   ## Currently a weakness in R, that generic functions can be overwritten
   ## and the only workaround is to prefix the specific package name,
   ## which of course requires only that exact package to provide the
   ## function. Over time, these workarounds will break, as functions
   ## may be migrated to new packages.
   x3 <- rbind(t(colorspace::coords(as(x2, model))), "alpha"=a1);
   if (length(names(x)) > 0) {
      colnames(x3) <- names(x);
   }
   x3[is.na(x3)] <- 0;
   x3;
}

#' convert HCL to R color
#'
#' Convert an HCL color matrix to vector of R hex colors
#'
#' This function takes an HCL matrix,and converts to an R color using
#' the colorspace package `colorspace::polarLUV()` and `colorspace::hex()`.
#'
#' When `model="hcl"` this function uses `farver::encode_colour()`
#' and bypasses `colorspace`. In future the `colorspace` dependency
#' will likely be removed in favor of using `farver`. In any event,
#' `model="hcl"` is equivalent to using `model="polarLUV"` and
#' `fixup=TRUE`, except that it should be much faster.
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
#' @param model `character` string indicating the color model to use:
#'    * hcl (default) uses `farver`
#'    * polarLUV uses `colorspace` polarLUV
#'    * polarLAB uses `colorspace polarLAB
#' @param verbose `logical` whether to print verbose output.
#' @param ... other arguments are ignored.
#'
#' @returns vector of R colors, or where the input was NA, then NA
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
#' @family jam color functions
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
 fixup=TRUE,
 model=getOption("jam.model", c("hcl", "polarLUV", "polarLAB")),
 verbose=FALSE,
 ...)
{
   ## Purpose is to convert HCL back to an R hex color string
   ## Note that this function uses the colorspace HCL, which differs from the
   ## used by the built-in R method hcl()
   if (!requireNamespace("colorspace", quietly=TRUE)) {
      stop("hcl2col() requires the colorspace package.");
   }
   if (!requireNamespace("matrixStats", quietly=TRUE)) {
      useMatrixStats <- TRUE;
   } else {
      useMatrixStats <- FALSE;
   }
   model <- head(model, 1);
   if (length(model) == 0) {
      model <- "hcl";
   }
   if ("hcl" %in% model &&
         !requireNamespace("farver", quietly=TRUE)) {
      model <- "polarLUV";
      fixup <- TRUE;
   }
   if (igrepHas("polarLUV|polarLAB", class(x))) {
      xnames <- colnames(x);
      x2 <- x;
      #x <- t(colorspace::coords(x));
   } else {
      if (igrepHas("matrix", class(x))) {
         x <- x[c("H","C","L","alpha"),,drop=FALSE];
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
      ## Convert to HCL using colorspace::polarLUV or colorspace::polarLAB
      if (igrepHas("polarLUV", model)) {
         x2 <- colorspace::polarLUV(
            H=x["H",],
            C=x["C",],
            L=x["L",]
         );
      } else if (igrepHas("polarLAB", model)) {
         x2 <- colorspace::polarLAB(
            H=x["H",],
            C=x["C",],
            L=x["L",]
         );
      } else {
         x2 <- x;
      }
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
      if (verbose) {
         printDebug("Error: ",
            e,
            fgText=c("orangered", "mediumslateblue"));
      }
      a1;
   });

   ## fixup is an optional boolean, which uses colorspace hex() to
   ## repair any colors outside of normal RGB ranges (the color gamut),
   ## otherwise they become NA. Note that the fixup=TRUE method is lossy,
   ## as colorspace apparently applies a non-linear conversion strategy.
   if ("hcl" %in% model) {
      if (verbose) {
         cat("hcl2col():\n");
         cat("   x2:\n");
         print(x2);
      }
      xCol <- farver::encode_colour(t(x2),
         alpha=a1,
         from="hcl");
      names(xCol) <- xnames;
   } else if (length(fixup) > 0) {
      xCol <- colorspace::hex(x2, fixup=fixup);
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
         x3colMax <- matrixStats::colMaxs(x3, na.rm=TRUE);
      } else {
         x3colMax <- apply(x3, 2, max, na.rm=TRUE);
      }
      if (any(x3colMax > 255)) {
         ## This method scales values in each column so the maximum
         ## value is 255, and therefore shrinks other values in those
         ## columns proportionally. This method is intended to maintain
         ## the relative ratios of color components to maintain the same
         ## combined color hue.
         x3[,x3colMax > 255] <- t(t(x3[,x3colMax > 255, drop=FALSE]) *
               (255 / x3colMax[x3colMax > 255]));
      }
      x3 <- rbind(x3, "alpha"=a1);
      xCol <- rgb2col(x3,
         maxColorValue=255);
      names(xCol) <- xnames;
   }

   xCol;
}


#' get R color alpha value
#'
#' Return the alpha transparency per R color
#'
#' @param x `character` R compatible color, either a color name, hex value, or
#'    a mixture of the two. Any value compatible with `grDevices::col2rgb()`.
#' @param maxValue `numeric` maximum value to return, useful when the downstream
#'    alpha range should be 255. By default maxValue=1 is returned.
#' @param ... Additional arguments are ignored.
#'
#' @returns `numeric` vector of alpha values
#'
#' @family jam color functions
#'
#' @examples
#' col2alpha(c("red", "#99004499", "beige", "transparent", "#FFFFFF00"))
#'
#' @export
col2alpha <- function
(x,
 maxValue=1,
 ...)
{
   ## Purpose is to extract the alpha value for a set of colors defined in hex space,
   ## for those R tools that use the inconsistent method of defining alpha separate from
   ## the RGB color, although most tools should really be using RGBA format instead...
   if (length(x) == 0) {
      return(x);
   }
   xNA <- is.na(x);
   alphaValues <- grDevices::col2rgb(x, alpha=TRUE)["alpha",]/255*maxValue;
   alphaValues[xNA] <- 0;
   return(alphaValues);
}

#' set R color alpha value
#'
#' Define the alpha transparency per R color
#'
#' @param x R compatible color, either a color name, or hex value, or
#'    a mixture of the two. Any value compatible with \code{\link[grDevices]{col2rgb}}.
#' @param alpha numeric alpha transparency to use per x color. alpha is
#'    recycled to length(x) as needed.
#' @param maxValue numeric maximum value to return, useful when the downstream
#'    alpha range should be 255. By default maxValue=1 is returned.
#' @param ... Additional arguments are ignored.
#'
#' @family jam color functions
#'
#' @returns `character` vector of R colors, with alpha values.
#'
#' @examples
#' withr::with_par(list("mfrow"=c(2,2)), {
#' for (alpha in c(1, 0.8, 0.5, 0.2)) {
#'    nullPlot(plotAreaTitle=paste0("alpha=", alpha),
#'       doMargins=FALSE);
#'    usrBox(fill=alpha2col("yellow",
#'       alpha=alpha));
#' }
#' })
#'
#' @export
alpha2col <- function
(x,
 alpha=1,
 maxValue=1,
 ...)
{
   ## Purpose is change the alpha of a vector of colors to the one given.
   ## Note that NA values are left as NA values
   if (length(x) == 0) {
      return(x);
   }
   xNA <- is.na(x);
   alpha <- rep(alpha, length.out=length(x));
   rgbx <- rgb2col(rbind(grDevices::col2rgb(x, alpha=FALSE), alpha=alpha*(255/maxValue)),
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
#' This function augments the `grDevices::hsv()` function in that it handles
#' output from `grDevices::rgb2hsv()` or `col2hsv()`, sufficient to
#' run a series of conversion functions, e.g. `hsv2col(col2hsv("red"))`.
#' This function also maintains alpha transparency, which is not maintained
#' by the `grDevices::hsv()` function.
#'
#' @param hsvValue `numeric` HSV matrix, with rownames c("h","s","v")
#'    in any order, and optionally "alpha" rowname for alpha transparency.
#' @param ... additional arguments are ignored.
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
#' @family jam color functions
#'
#' @returns `character` vector of R colors.
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
      do.call(grDevices::hsv, hsvValue, ...);
   } else {
      if (!"alpha" %in% rownames(hsvValue)) {
         hsvValue <- rbind(hsvValue,
            matrix(nrow=1, rep(1, ncol(hsvValue)),
               dimnames=c("alpha",list(colnames(hsvValue)))));
      }
      grDevices::hsv(h=hsvValue["h",],
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
#' @returns matrix of HSV colors
#'
#' @param x R color
#' @param ... additional parameters are ignored
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
#' @family jam color functions
#'
#' @export
col2hsv <- function
(x, ...)
{
   ## Purpose is to use col2rgb and rgb2hsv2 to convert colors to an hsv matrix
   rgbColors <- grDevices::col2rgb(x, alpha=TRUE);
   hsvValues <- grDevices::rgb2hsv(rgbColors[1:3,,drop=FALSE]);
   return(rbind(
      grDevices::rgb2hsv(rgbColors[1:3,,drop=FALSE]),
      rgbColors["alpha",,drop=FALSE]/255));
}

#' Convert RGB color matrix to R color
#'
#' Convert RGB color matrix to R color
#'
#' This function intends to augment the \code{\link[grDevices]{rgb}} function, which
#' does not handle output from \code{\link[grDevices]{col2rgb}}. The goal is to handle
#' multiple color conversions, e.g. \code{rgb2col(grDevices::col2rgb("red"))}. This
#' function also maintains alpha transparency when supplied.
#'
#' The output is named either by names(red), rownames(red), or if supplied,
#' the value of the parameter \code{names}.
#'
#' Note that `alpha` is used to define alpha transparency, but has
#' additional control over the output.
#'
#' * When `alpha` is `FALSE` then
#' output colors will not have the alpha transparency, in hex form that
#' means colors are in format `"#RRGGBB"` and not `"#RRGGBBAA"`.
#' * When `alpha` is `TRUE` the previous alpha transparency values are
#' used without change.
#' * When `alpha` is a numeric vector, numeric values are always
#' expected to be in range `[0,1]`, where `0` is completely transparent,
#' and `1` is completely not transparent. Supplied `alpha` values will
#' override those present in `red` when `red` is a matrix like that
#' produced from `grDevices::col2rgb(..., alpha=TRUE)`.
#' * When `alpha` is a numeric vector, use `-1` or any negative number
#' to indicate the alpha value should be removed.
#' * When `alpha` is a numeric vector, use `Inf` to indicate the alpha
#' transparency should be retained without change.
#'
#' Therefore, `alpha = c(-1, 0, 1, Inf)` will apply the following,
#' in order: remove alpha; set alpha to 0; set alpha to 1; set alpha
#' to the same as the input color.
#'
#' @param red `numeric` vector of red values; or RGB numeric matrix with
#'    rownames c("red","green","blue") in any order, with optional rowname
#'    "alpha"; or character strings with comma-separated rgb values, in
#'    format "100,20,10". The latter input is designed to handle web rgb
#'    values.
#' @param green `numeric` vector, or when red is a matrix or comma-delimited
#'    character string, this parameter is ignored.
#' @param blue `numeric` vector, or when red is a matrix or comma-delimited
#'    character string, this parameter is ignored.
#' @param alpha `numeric` vector, or when red is a matrix or comma-delimited
#'    character string, this parameter is ignored. Alpha values are always
#'    expected in range `[0,1]`, even when `maxColorValue` is higher
#'    than `1`. When `alpha` is `FALSE`, the alpha transparency is removed.
#'    When `alpha` is `TRUE` the original alpha transparency is retained
#'    without change. If supplying `alpha` as a numeric vector, use `Inf`
#'    to represent `TRUE` for alpha values to be kept without change, and
#'    use `-1` or any negative number to indicate alpha values to remove
#'    from the output.
#' @param names `character`, default NULL, with optional names to apply
#'    to output colors.
#' @param maxColorValue `numeric` maximum value for colors. If NULL then it
#'    defaults to 1 unless there are values above 1, in which case it defaults
#'    to 255.
#' @param keepNA `logical` whether to keep NA values, returning NA for any
#'    input where red, green, and/or blue are NA. If keepNA==FALSE then it
#'    substitutes 0 for any NA values.
#' @param verbose `logical` indicating whether to print verbose output
#' @param ... Additional arguments are ignored.
#'
#' @examples
#' # start with a color vector
#' # red and blue with partial transparency
#' colorV <- c("#FF000055", "#00339999");
#'
#' # Show the output of rgb2col
#' # make sure to include alpha=TRUE to maintain alpha transparency
#' grDevices::col2rgb(colorV, alpha=TRUE);
#'
#' # confirm we can convert from RGB back to the same colors
#' rgb2col(grDevices::col2rgb(colorV, alpha=TRUE));
#'
#' @family jam color functions
#'
#' @returns `character` vector of R colors.
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
 verbose=FALSE,
 ...)
{
   ## Purpose is to augment the function rgb() which does not handle output
   ## from grDevices::col2rgb().
   ## The goal is to be able to run rgb2col(grDevices::col2rgb()) and have it return
   ## the original colors.
   ##
   ## input here can be a matrix with columns c("red", "green", "blue") or
   ## comma-delimited text strings in the form "10,10,10" for red, green,
   ## and blue, respectively.
   ##
   ## maxColorValue is the highest color value, by default 1, but can be
   ## set to 255 to handle 8-bit colors.

   if (length(red) == 0 || all(is.na(red))) {
      return(red);
   }
   if (length(green) == 0 && length(blue) == 0) {
      if (igrepHas("character", class(red)) && igrepHas(",.+,", red)) {
         red <- rbindList(lapply(strsplit(red, ","), as.numeric));
         red[is.na(red)] <- 0;
         redCols <- 1:min(c(ncol(red),4));
         colnames(red)[redCols] <- c("red", "green", "blue", "alpha")[redCols];
      }
      if (is.matrix(red) || is.data.frame(red) || any(c("RGB") %in% class(red))) {
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
         rCol <- head(vigrep("^R$|red", colnames(red)), 1);
         gCol <- head(vigrep("^G$|green", colnames(red)), 1);
         bCol <- head(vigrep("^B$|blue", colnames(red)), 1);
         if (length(rCol) == 0) { rCol <- 1; }
         if (length(gCol) == 0) { gCol <- 2; }
         if (length(bCol) == 0) { bCol <- 3; }
         green <- red[,gCol];
         blue <- red[,bCol];
         if (length(maxColorValue) == 0) {
            if (max(c(red, green, blue), na.rm=TRUE) > 1) {
               maxColorValue <- 255;
            } else {
               maxColorValue <- 1;
            }
         }
         if (ncol(red) >= 4) {
            alphaCol <- head(vigrep("alpha", colnames(red)), 1);
            if (length(alphaCol) == 0) { alphaCol <- 4; }
            if (length(alpha) > 0) {
               ## Check if function argument defines alpha
               alpha <- rep(alpha, length.out=length(green));
               ## Entries to omit alpha have either FALSE, NA, or -1
               alphaBlank <- (isFALSEV(alpha) |
                  is.na(alpha) |
                  alpha < 0);
               ## Entries to keep the previous alpha have TRUE or Inf
               alphaAsis <- (isTRUEV(alpha) |
                  (is.infinite(alpha) & alpha > 0));
               if (any(alphaAsis)) {
                  alpha[alphaAsis] <- red[alphaAsis, alphaCol] / maxColorValue;
               }
               if (any(alphaBlank)) {
                  alpha[alphaBlank] <- rep(-1, sum(alphaBlank));
               }
               if (verbose) {
                  printDebug("rgb2col(): ",
                     "applying supplied alpha:",
                     alpha);
               }
            } else {
               ## If alpha is NULL from function arguments,
               ## use alpha as-is, without change from the input colors
               alpha <- red[, alphaCol] / maxColorValue;
            }
         }
         red <- red[,rCol];
      }
   }
   if (length(maxColorValue) == 0) {
      if (max(c(red, green, blue), na.rm=TRUE) > 1) {
         maxColorValue <- 255;
      } else {
         maxColorValue <- 1;
      }
   }

   if (length(alpha) == 0) {
      alpha <- rep(maxColorValue, length.out=length(green));
   } else if (any(isFALSEV(alpha) | alpha < 0 | is.na(alpha))) {
      alpha <- rep(alpha, length.out=length(green));
      alphaBlank <- (isFALSEV(alpha) | alpha < 0 | is.na(alpha));
      if (any(alphaBlank)) {
         alpha[alphaBlank] <- -1;
         alpha[!alphaBlank] <- alpha[!alphaBlank] * maxColorValue;
      } else {
         alpha <- alpha * maxColorValue;
      }
   } else {
      alpha <- rep(alpha, length.out=length(red)) * maxColorValue;
   }
   ## Make sure all alpha values are not higher than maxColorValue
   alpha <- noiseFloor(alpha, minimum=-1, ceiling=maxColorValue);

   ## Gracefully handle NA by substituting with zero
   anyNA <- (is.na(red) | is.na(green) | is.na(green));
   if (any(anyNA)) {
      red <- rmNA(red, naValue=0);
      green <- rmNA(green, naValue=0);
      blue <- rmNA(blue, naValue=0);
   }
   if (!any(alpha < 0)) {
      result <- grDevices::rgb(red=red,
         green=green,
         blue=blue,
         alpha=alpha,
         maxColorValue=maxColorValue,
         names=names);
   } else {
      whichNoalpha <- (alpha < 0);
      if (verbose) {
         printDebug("rgb2col(): ",
            "applying supplied alpha whichNoalpha:",
            whichNoalpha);
      }
      result1 <- grDevices::rgb(red=red[whichNoalpha],
         green=green[whichNoalpha],
         blue=blue[whichNoalpha],
         maxColorValue=maxColorValue,
         names=names[whichNoalpha]);
      result2 <- grDevices::rgb(red=red[!whichNoalpha],
         green=green[!whichNoalpha],
         blue=blue[!whichNoalpha],
         alpha=alpha[!whichNoalpha],
         maxColorValue=maxColorValue,
         names=names[!whichNoalpha]);
      result <- rep("", length.out=length(red));
      result[whichNoalpha] <- result1;
      result[!whichNoalpha] <- result2;
   }

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
#' graphics::points(x=c(1,2,1,2), y=c(1,2,2,1), pch=20, cex=4,
#'    col=colorVlite2);
#'
#' # Making a color lighter can make it easier to add labels
#' # The setTextContrastColor() function also helps.
#' graphics::text(x=c(1,2,1,2), y=c(1,2,2,1), 1:4,
#'    col=setTextContrastColor(colorVlite2));
#'
#' @family jam color functions
#'
#' @param hexColor `character` vector of colors to adjust
#' @param darkFactor `numeric` value to adjust darkness, values above 1
#'    make the color darker, values below 1 (or below 0) make the color
#'    brighter.
#' @param sFactor `numeric` value to adjust saturation, values above 1
#'    become more saturated.
#' @param fixAlpha `numeric`, default NULL, to assign a fixed alpha
#'    transparency value, where 0 is transparent and 1 is opaque.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param keepNA `logical`, default FALSE, whether to keep NA values
#'    as NA values in the output, otherwise NA values are considered grey
#'    input.
#' @param useMethod `integer` with two alternate methods, `1` is default.
#' @param ... Additional arguments are ignored.
#'
#' @returns `character` vector of R colors.
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
      j <- rbind(grDevices::rgb2hsv(r=hexMatrix["red",],
         g=hexMatrix["green",],
         b=hexMatrix["blue",]),
         hexMatrix["alpha",,drop=FALSE]/255);
      newVL <- adjustFactorTwoStep(j["v",],
         adjFactor=-darkFactors,
         val2=j["s",]);
      if (verbose) {
         printDebug("makeColorDarker(): ",
            "newVL:");
         print(head(newVL, 20));
         printDebug("makeColorDarker(): ",
            "j:");
         print(head(j, 20));
      }
      newV <- newVL$val;
      newS1 <- newVL$val2;
      j["v",] <- newV;
      j["s",] <- newS1;
      newS <- noiseFloor(minimum=0,
         ceiling=1,
         adjustFactor(j["s",],
            adjFactor=sFactors));
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
            hsv1 <- grDevices::hsv(h=j[1], s=newS, v=newV, alpha=i[4]/255);
            hsv1;
         }, error=function(e){
            if (verbose) {
               printDebug("Error: ", cPaste(e), fgText=c("yellow", "red"));
               printDebug("h: ", format(digits=2, j[1]),
                  ", s: ", format(digits=2, newS),
                  ", v: ", format(digits=2, newV),
                  ", oldV: ", format(digits=2, j[3]),
                  ", darkFactor: ", format(digits=2, darkFactor),
                  ", alpha: ", format(digits=2, i[4]/255),
                  c("orange", "lightblue") );
            }
            grDevices::hsv(h=j[1], s=newS, v=newV, alpha=i[4]/255);
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
#' When `n` is `NULL`, this function returns a color function,
#' wrapped by `grDevices::colorRampPalette()`. The colors used
#' are defined by `gradientN`, so the `grDevices::colorRampPalette()`
#' function actually uses a starting palette of `gradientN` number
#' of colors.
#'
#' When `n` is an integer greater than `0`, this function returns
#' a vector of colors with length `n`.
#'
#' When `col` is a single color value, a color gradient is created
#' by appending `defaultColorBase` to the output of
#' `color2gradient(..., n=3, gradientWtFactor=gradientWtFactor)`.
#' These 4 colors are used as the internal palette before
#' applying `grDevices::colorRampPalette()` as appropriate.
#' In this case, `gradientWtFactor` is used to adjust the
#' strength of the color gradient. The intended use is:
#' `getColorRamp("red", n=5)`. To remove the leading white
#' color, use `getColorRamp("red", n=5, trimRamp=c(1,0))`.
#'
#' When `col` contains multiple color values, they are used
#' to define a color ramp directly.
#'
#' When `col` is not a color value, it is compared to known color
#' palettes from `RColorBrewer::RColorBrewer` and `viridisLite`,
#' and will use the corresponding color function or color palette.
#'
#' When `col` refers to a color palette, the suffix `"_r"` may
#' be used to reverse the colors. For example,
#' `getColorRamp(col="RdBu_r", n=9)` will recognize the
#' `RColorBrewer` color palette `"RdBu"`, and will reverse the colors
#' to return blue to red, more suitable for heatmaps where
#' high values associated with heat are colored red,
#' and low values associated with cold are colored blue.
#'
#' The argument `reverseRamp=TRUE` may be used to reverse the
#' returned colors.
#'
#' Color functions from `viridisLite` are recognized:
#' `"viridis"`, `"cividis"`, `"inferno"`, `"magma"`, `"plasma"`.
#'
#' The argument `trimRamp` is used to trim colors from the beginning
#' and end of a color ramp, respectively. This mechanism is useful
#' to remove the first or last color when those colors may be too
#' extreme. Note that internally, colors are expanded to length
#' `gradientN`, then trimmed, then the corresponding `n` colors
#' are returned.
#'
#'  The `trimRamp` argument is also useful when returning a color
#'  function, which occurs when `n=NULL`. In this case, colors are
#'  expanded to length `gradientN`, then are trimmed using the
#'  values from `trimRamp`, then the returned function can be used
#'  to create a color ramp of arbitrary length.
#'
#' Note that when `reverseRamp=TRUE`, colors are reversed
#' before `trimRamp` is applied.
#'
#' By default, alpha transparency will be maintained if supplied in the
#' input color vector. Most color ramps have no transparency, in which
#' case transparency can be added after the fact using `alpha2col()`.
#'
#' @param col one of the following:
#'    * `character` vector of two or more R colors. A color gradient
#'    will be defined using these colors in order with `colorRampPalette()`.
#'    * `character` vector length=1 with one R color.
#'    A color gradient is defined from `defaultBaseColor` to `col`
#'    using `color2gradient()`. To adjust the range of light to dark
#'    luminance, use the `dex` argument, where higher values increase
#'    the range, and lower values decrease the range.
#'    * `character` vector length=1, with one recognized color ramp name:
#'    any color palette from RColorBrewer, for example
#'    `rownames(RColorBrewer::brewer.pal.info())`;
#'    any color palette function name from `viridisLite`.
#'    * `character` vector length=1, with one color function name,
#'    for example `col="rainbow_hcl"`. Input is equivalent to supplying
#'    one color `function`, see below.
#'    * `function` whose first argument expects `integer` number of colors
#'    to return, for example `col=viridisLite::viridis` defines the function
#'    itself as input.
#'    * `function` derived from `circlize::colorRamp2()`,  recognized
#'    by having attribute names `"breaks"` and `"colors"`. Note that
#'    only the colors are used for the individual color values, not the
#'    break points.
#' @param n `integer` number of output colors to return, or NULL if
#'    the output should be a color function in the form `function(n)`
#'    which returns `n` colors.
#' @param trimRamp `integer` vector, expanded to length=2 as needed,
#'    which defines the number of colors to trim from the beginning
#'    and end of the color vector, respectively. When `reverseRamp=TRUE`,
#'    the colors are reversed before the trimming is applied.
#'    If the two `trimRamp` values are not identical, symmetric divergent
#'    color scales will no longer be symmetric.
#' @param gradientN `integer` number of colors to expand gradient colors
#'    prior to trimming colors.
#' @param defaultBaseColor `character` vector indicating a color from which to
#'    begin a color gradient, only used when col is a single color.
#' @param reverseRamp `logical` indicating whether to reverse the resulting
#'    color ramp. This value is ignored when a single value is supplied for
#'    col, and where "_r" or "_rev" is detected as a substring at the end
#'    of the character value.
#' @param alpha `logical` indicating whether to honor alpha transparency
#'    whenever `colorRampPalette` is called. If colors contain
#'    no alpha transparency, this setting has no effect, otherwise the
#'    alpha value is applied by `grDevices::colorRampPalette()` using
#'    a linear gradient between each color.
#' @param gradientWtFactor `numeric` value used to expand single color
#'    input to a gradient, using `color2gradient()`, prior to making
#'    a full gradient to the `defaultBaseColor`.
#'    Note that `dex` is the preferred method for adjusting the range
#'    of light to dark for the given color `col`.
#' @param dex `numeric` darkness expansion factor, used only with input
#'    `col` is a single color, which is then split into a color gradient
#'    using `defaultBaseColor` by calling `color2gradient()`.
#'    The `dex` factor adjusts the range of dark to light colors,
#'    where higher values for `dex` increase the range,
#'    making the changes more dramatic.
#' @param lens,divergent arguments sent to `warpRamp()` to
#'    apply a warp effect to the color ramp, to compress or expand
#'    the color gradient: `lens` scales the warp effect, with
#'    positive values compressing colors toward baseline and
#'    negative values expanding colors near baseline; `divergent`
#'    is a logical indicating whether the middle color is considered
#'    the baseline.
#' @param verbose `logical` whether to print verbose output
#' @param ... additional arguments are ignored.
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
#' # From RColorBrewer use a brewer name
#' RdBu <- getColorRamp("RdBu");
#' RdBu_r <- getColorRamp("RdBu_r");
#' colorList <- c(colorList, list(RdBu=RdBu, RdBu_r=RdBu_r));
#' showColors(RdBu);
#'
#' if (requireNamespace("viridisLite", quietly=TRUE)) {
#'    viridisV <- getColorRamp("viridis");
#'    colorList <- c(colorList, list(viridis=viridisV));
#' }
#'
#' # for fun, put a few color ramps onto one plot
#' showColors(colorList, cexCellnote=0.7);
#'
#' showColors(list(`white background\ncolor='red'`=getColorRamp("red"),
#'    `black background\ncolor='red'`=getColorRamp("red", defaultBaseColor="black"),
#'    `white background\ncolor='gold'`=getColorRamp("gold"),
#'    `black background\ncolor='gold'`=getColorRamp("gold", defaultBaseColor="black")))
#'
#' @family jam color functions
#'
#' @returns `character` vector of R colors, or when N is NULL,
#'    `function` sufficient to create R colors.
#'
#' @export
getColorRamp <- function
(col,
 n=15,
 trimRamp=c(0, 0),
 gradientN=15,
 defaultBaseColor="grey99",
 reverseRamp=FALSE,
 alpha=TRUE,
 gradientWtFactor=NULL,
 dex=1,
 lens=0,
 divergent=NULL,
 verbose=FALSE,
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
   if (length(trimRamp) == 0) {
      trimRamp <- c(0, 0);
   } else {
      trimRamp <- abs(rep(trimRamp, length.out=2));
   }
   applyTrimRamp <- function(cols, trimRamp) {
      if (trimRamp[1] > 0) {
         cols <- tail(cols, -abs(trimRamp[1]));
      }
      if (trimRamp[2] > 0) {
         cols <- head(cols, -abs(trimRamp[2]));
      }
      return(cols);
   }

   if (igrepHas("character", class(col))) {
      viridis_colors <- c(
         "cividis",
         "viridis",
         "inferno",
         "magma",
         "mako",
         "plasma",
         "rocket",
         "turbo")
      if (length(col) == 1 &&
            col %in% viridis_colors) {
         #######################################
         ## Viridis package color handling
         if (!requireNamespace("viridisLite", quietly=TRUE)) {
            stop(paste0("The viridisLite package is required for color ramps: ",
               cPaste(viridis_colors, sep=", ")));
         }
         if (verbose) {
            printDebug("getColorRamp(): ",
               "viridisLite color function:",
               col);
         }
         colorFunc <- get(col,
            asNamespace("viridisLite"),
            mode="function")
      } else if (length(col) == 1 &&
            requireNamespace("RColorBrewer", quietly=TRUE) &&
            col %in% rownames(RColorBrewer::brewer.pal.info)) {
         #######################################
         ## Brewer Colors
         if (verbose) {
            printDebug("getColorRamp(): ",
               "RColorBrewer color palette:",
               col);
         }
         brewerN <- RColorBrewer::brewer.pal.info[col, "maxcolors"];
         if (lens != 0 && length(divergent) == 0) {
            if ("div" %in% RColorBrewer::brewer.pal.info[col, "category"]) {
               divergent <- TRUE;
            } else {
               divergent <- FALSE;
            }
         }
         colorFunc <- function(n){
            if (n <= brewerN) {
               RColorBrewer::brewer.pal(n, col);
            } else {
               grDevices::colorRampPalette(
                  RColorBrewer::brewer.pal(brewerN, col))(n);
            }
         }
      # } else if (length(col) == 1 &&
      #       requireNamespace("colorjam", quietly=TRUE) &&
      #       (col %in% names(colorjam::jam_linear) ||
      #       col %in% names(colorjam::jam_divergent)) ) {
      #    #######################################
      #    ## colorjam gradient
      #    if (verbose) {
      #       printDebug("getColorRamp(): ",
      #          "colorjam color gradient:",
      #          paste0(col, "."));
      #    }
      #    if (col %in% names(colorjam::jam_linear)) {
      #       colset <- colorjam::jam_linear[[col]];
      #    } else if (col %in% names(colorjam::jam_divergent)) {
      #       colset <- colorjam::jam_divergent[[col]];
      #    }
      #    colorFunc <- function(n){
      #       if (n == length(colset)) {
      #          colset
      #       } else {
      #          grDevices::colorRampPalette(colset)(n);
      #       }
      #    }
      } else {
         ## If given one or more colors, use them to create a color ramp
         if (verbose) {
            printDebug("getColorRamp(): ",
               "checking character color input.");
         }
         colset <- col[isColor(col)];
         if (length(colset) > 0) {
            if (length(colset) == 1) {
               ## If given one color, make a color ramp from white to this color
               mini_3set <- color2gradient(colset,
                  n=3,
                  dex=dex,
                  gradientWtFactor=gradientWtFactor);
               if (col2hcl(defaultBaseColor)["L",] < col2hcl(colset)["L",]) {
                  mini_3set <- rev(mini_3set);
               }
               colset <- c(defaultBaseColor,
                  mini_3set);
               if (verbose) {
                  printDebug("getColorRamp(): ",
                     "Using defaultBaseColor, color to make a gradient.");
               }
            }
            colorFunc <- grDevices::colorRampPalette(colset, alpha=alpha);
         } else {
            ## Check if we are supplied a function name
            if (verbose) {
               printDebug("getColorRamp(): ",
                  "checking color function name input.");
            }
            # retrieve based upon format
            if (igrepHas("::", col)) {
               # package::function prefix is evaluated
               colorFunc <- tryCatch({
                  eval(str2lang(col))
               }, error=function(e){
                  if (verbose) {
                     printDebug("Error:", e,
                        sep=" ", collapse=" ",
                        fgText=c("red", "orange"));
                     print(e);
                  }
                  NULL;
               });
            } else {
               # string is tested with get()
               colorFunc <- tryCatch({
                  get(col,
                     mode="function");
               }, error=function(e){
                  if (verbose) {
                     printDebug("Error:", e,
                        sep=" ", collapse=" ",
                        fgText=c("red", "orange"));
                  }
                  NULL;
               });
            }
            ## If not a function, we stop here
            if (length(colorFunc) == 0) {
               stop(paste0("The supplied color could not be used to create",
                  " a color ramp, col:", cPaste(col)));
            }
         }
      }
   } else if (is.function(col)) {
      if (all(c("colors", "breaks") %in% names(attributes(col)))) {
         # circlize::colorRamp2() color function
         # convert to colorRampPalette color function
         colorFunc <- grDevices::colorRampPalette(rgb2col(attr(col, "colors")))
      } else {
         # color function with N argument
         if (verbose) {
            printDebug("getColorRamp(): ",
               "color function input.");
         }
         colorFunc <- col;
      }
   } else {
      if (verbose) {
         printDebug("getColorRamp(): ",
            "unrecognized color input, using grDevices::colorRampPalette() anyway.");
      }
      colorFunc <- grDevices::colorRampPalette(col, alpha=alpha);
   }

   #############################################
   ## use colorFunc to define the color ramp
   if (lens != 0 && length(divergent) == 0) {
      divergent <- FALSE;
   }
   if (length(n) > 0) {
      if (length(gradientN) == 0) {
         gradientN <- n + sum(trimRamp);
      }
      cols <- colorFunc(gradientN);
      ## Optionally warp the color ramp before reversing and trimming colors
      if (lens != 0) {
         cols <- warpRamp(ramp=cols,
            lens=lens,
            divergent=divergent);
      }
      if (reverseRamp) {
         cols <- rev(cols);
      }
      if (sum(trimRamp) > 0) {
         cols <- applyTrimRamp(cols, trimRamp);
      }
      if (length(cols) != n) {
         cols <- grDevices::colorRampPalette(cols, alpha=alpha)(n);
      }
   } else {
      ## Get color function
      if (sum(trimRamp) > 0) {
         if (length(gradientN) == 0) {
            cols <- function(n){
               c1 <- colorFunc(n + sum(trimRamp));
               ## Optionally warp the color ramp before reversing and trimming colors
               if (lens != 0) {
                  c1 <- warpRamp(ramp=c1,
                     lens=lens,
                     divergent=divergent);
               }
               if (reverseRamp) {
                  c1 <- rev(c1);
               }
               if (trimRamp[1] > 0) {
                  c1 <- tail(c1, -trimRamp[1]);
               }
               if (trimRamp[2] > 0) {
                  c1 <- head(c1, -trimRamp[2]);
               }
               return(c1);
            }
         } else {
            cols <- colorFunc(gradientN);
            ## Optionally warp the color ramp before reversing and trimming colors
            if (lens != 0) {
               cols <- warpRamp(ramp=cols,
                  lens=lens,
                  divergent=divergent);
            }
            if (reverseRamp) {
               cols <- rev(cols);
            }
            if (sum(trimRamp) > 0) {
               cols <- applyTrimRamp(cols, trimRamp);
            }
            cols <- grDevices::colorRampPalette(cols, alpha=alpha);
         }
      } else {
         if (length(gradientN) > 0) {
            cols <- colorFunc(gradientN);
         } else {
            cols <- colorFunc(101);
         }
         ## Optionally warp the color ramp before reversing and trimming colors
         if (lens != 0) {
            cols <- warpRamp(ramp=cols,
               lens=lens,
               divergent=divergent);
         }
         if (reverseRamp) {
            if (verbose) {
               printDebug("reverseRamp:", reverseRamp)
            }
            cols <- rev(cols);
         }
         cols <- grDevices::colorRampPalette(cols, alpha=alpha);
      }
   }
   ###########
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
#' @family jam color functions
#'
#' @returns `logical` vector with length(x).
#'
#' @examples
#' isColor(c("red", "blue", "beige", "#99000099", "#aa00ff", "#AAE", "bleh"))
#'
#' @export
isColor <- function
(x,
 makeNamesFunc=c,
 ...)
{
   ## Purpose is to check if a given text string is a valid R color
   allColors <- grDevices::colors();
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
#' @param col some type of recognized R color input as:
#'    * `character` vector of one or more individual colors, each
#'    color is expanded into a gradient of length `n`, where `n` is
#'    recycled to the number of unique colors. The value `n` is applied
#'    in the order the colors appear in `col`.
#'    * `list` of color vectors where each vector contains one repeated color
#'    * `character` vector of repeated colors, where `n` is defined by
#'    the number of each color present.
#' @param n `integer` vector of length one or more, which defines the number
#'    of colors to return for each gradient. When `n=0` then only duplicated
#'    colors will be expanded into a gradient.
#' @param gradientWtFactor `numeric` fraction representing the amount to expand
#'    a color toward its maximum brightness and darkness.
#'    It is recommended to use `dex` and not this argument.
#'    * When `gradientWtFactor=NULL` this value is calculated based upon the
#'    number of colors requested, and the initial luminance in HCL
#'    space of the starting color.
#'    * When `gradientWtFactor` is defined, values are recycled to
#'    `length(col)`, and can be independently applied to each color.
#' @param dex `numeric` value to apply dramatic dark expansion, where:
#'    * `dex > 1` will make the gradient more dramatic, values
#'    * `dex < 1` will make the gradient less dramatic, and are considered
#'    fractions 1/x.
#'    * `dex < 0` will make the gradient less dramatic, and values are
#'    internally converted to fractions using `1/(2 + abs(dex))`
#' @param reverseGradient `logical` whether to return light-to-dark gradient
#'    (TRUE) or dark-to-light gradient (FALSE).
#' @param verbose `logical` whether to print verbose output.
#' @param ... other parameters are ignored.
#'
#' @returns `character` vector of R colors.
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
#' showColors(list(`Input colors`=colors1, `Output colors`=colors2));
#'
#' # You can do the same using a list intermediate
#' colors1L <- split(colors1, colors1);
#' showColors(colors1L);
#' colors2L <- color2gradient(colors1L);
#' showColors(colors2L);
#'
#' # comparison of fixed gradientWtFactor with dynamic gradientWtFactor
#' showColors(list(
#'    `dynamic\ngradientWtFactor\ndex=1`=color2gradient(
#'       c("yellow", "navy", "firebrick", "orange"),
#'       n=3,
#'       gradientWtFactor=NULL,
#'       dex=1),
#'    `dynamic\ngradientWtFactor\ndex=2`=color2gradient(
#'       c("yellow", "navy", "firebrick", "orange"),
#'       n=3,
#'       gradientWtFactor=NULL,
#'       dex=2),
#'    `fixed\ngradientWtFactor=2/3`=color2gradient(
#'       c("yellow", "navy", "firebrick", "orange"),
#'       n=3,
#'       gradientWtFactor=2/3,
#'       dex=1)
#' ))
#'
#' @family jam color functions
#'
#' @export
color2gradient <- function
(col,
 n=NULL,
 gradientWtFactor=NULL,
 dex=1,
 reverseGradient=TRUE,
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
   if (length(col) == 0) {
      return(col)
   }

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

   # 0.0.77.900: expand gradientWtFactor to length(col)
   if (length(gradientWtFactor) > 0) {
      gradientWtFactor <- rep(gradientWtFactor,
         length.out=length(col));
      names(gradientWtFactor) <- names(col);
   }
   if (length(dex) == 0) {
      dex <- 1;
   }
   dex <- rep(dex,
      length.out=length(col));
   dex[dex <= 0] <- 1/(2 + abs(dex[dex <= 0]));
   names(dex) <- names(col);

   # Determine n:
   # - when n=0 set to number of observations each color, no expansion
   # - when n=NULL and all colors are singlets, expand to n=3 by default
   # - when n=NULL otherwise use number of repeats for each color
   # - otherwise expand n to length of unique colors
   doExpand <- FALSE;
   if (length(n) == 1 && n == 0) {
      n <- lengths(col);
   } else if (length(n) == 0) {
      if (all(lengths(col) == 1)) {
         n <- rep(3, length(col));
      } else {
         n <- lengths(col);
      }
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
   n <- rep(n,
      length.out=length(col));
   names(n) <- names(col);
   if (verbose) {
      printDebug("color2gradient() running.");
      printDebug("   col:");
      print(head(col, 10));
      printDebug("     n:");
      print(head(n, 10));
   }

   newColorSets <- lapply(nameVectorN(col), function(iName){
      i <- col[[iName]];
      wtFactor <- head(gradientWtFactor[[iName]], 1);
      if (length(wtFactor) == 0 || wtFactor == 0) {
         # adjust for initial luminance, brighter colors need less wtFactor
         # dark colors benefit from more wtFactor
         i_L <- col2hcl(head(i, 1))["L",];
         wtFactor <- (n[[iName]] - 1) / (i_L / 14 + 2) * sqrt(dex[[iName]]);
         if (verbose) {
            jamba::printDebug("color2gradient(): ",
               "wtFactor: ",
               paste0("1 / ", round(1 / wtFactor, digits=1)));
         }
      }
      if (verbose > 1) {
         printDebug("i:", c("orange", i));
         print(head(i, 20));
      }
      if (length(unique(i)) > 1) {
         i <- head(i, 1);
      }
      hsvValues <- col2hsv(i);
      iLen <- n[iName];
      if (verbose > 1) {
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
      sRange <- stats::approx(x=unique(c(
            stats::weighted.mean(c(sMax, sValue), w=c(wtFactor, 1)),
            sValue,
            stats::weighted.mean(c(sMin, sValue), w=c(wtFactor, 1)))),
         n=iLen)$y;
      ## Keep grey as grey and not some random muddy color
      if (sValue == 0) {
         sRange <- sRange - sRange;
      }
      vRange <- stats::approx(x=unique(c(
            stats::weighted.mean(c(vMin, vValue), w=c(wtFactor, 1)),
            vValue,
            stats::weighted.mean(c(vMax, vValue), w=c(wtFactor, 1)))),
         n=iLen)$y;
      hRange <- rep(hsvValues["h",1], iLen);
      alphaRange <- rep(hsvValues["alpha",1], iLen);
      newColors <- grDevices::hsv(h=hRange,
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


#' Warp colors in a color ramp
#'
#' Warp colors in a color ramp
#'
#' This function takes a vector of colors in a color ramp (color gradient)
#' and warps the gradient using a lens factor. The effect causes the
#' color gradient to change faster or slower, dependent upon the lens
#' factor.
#'
#' The main intent is for heatmap color ramps, where the color gradient
#' changes are not consistent with meaningful numeric differences
#' being shown in the heatmap. In short, this function enhances
#' colors.
#'
#' @returns `character` vector of R colors, with the same length as the
#' input vector `ramp`.
#'
#' @param ramp character vector of R colors
#' @param lens numeric lens factor, centered at zero, where positive
#'    values cause colors to change more rapidly near zero, and
#'    negative values cause colors to change less rapidly near zero
#'    and more rapidly near the extreme.
#' @param divergent logical indicating whether the `ramp` represents
#'    divergent colors, which are assumed to be symmetric above and
#'    below zero. Otherwise, colors are assumed to begin at zero.
#' @param expandFactor numeric factor used to expand the color ramp
#'    prior to selecting the nearest warped numeric value as the
#'    result of `warpAroundZero()`. This value should not
#'    need to be changed unless the lens is extremely high (>100).
#' @param plot logical indicating whether to plot the input and
#'    output color ramps using `showColors()`.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional parameters are passed to `showColors()`.
#'
#' @family jam color functions
#'
#' @examples
#' BuRd <- rev(RColorBrewer::brewer.pal(11, "RdBu"));
#' BuRdPlus5 <- warpRamp(BuRd, lens=2, plot=TRUE);
#' BuRdMinus5 <- warpRamp(BuRd, lens=-2, plot=TRUE);
#'
#' Reds <- RColorBrewer::brewer.pal(9, "Reds");
#' RedsL <- lapply(nameVector(c(-10,-5,-2,0,2,5,10)), function(lens){
#'    warpRamp(Reds, lens=lens, divergent=FALSE)
#' });
#' showColors(RedsL);
#'
#' @export
warpRamp <- function
(ramp,
 lens=5,
 divergent=TRUE,
 expandFactor=10,
 plot=FALSE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to take a color ramp and warp the color spacing.
   ## When divergent=TRUE the colors are assumed to be symmetric
   ## around zero, and are warped symmetrically.

   ## Expand the color ramp by expandFactor
   newN <- round(length(ramp) * expandFactor - (expandFactor-1));
   rampExp <- grDevices::colorRampPalette(ramp)(newN);

   ## Define a numeric sequence to warp
   rampN <- seq_along(ramp);
   if (divergent) {
      if (verbose) {
         printDebug("warpRamp(): ",
            "divergent color ramp lens:",
            lens);
      }
      centerN <- (length(ramp)-1)/2 + 1;
      seqN <- rampN - centerN;
      warpN <- warpAroundZero(seqN, lens=-lens) + centerN;
      warpExpN <- round(warpN * expandFactor - (expandFactor-1));
      newRamp <- rampExp[warpExpN];
   } else {
      if (verbose) {
         printDebug("warpRamp(): ",
            "sequential color ramp lens:",
            lens);
      }
      rampN <- seq_along(ramp);
      seqN <- rampN - 1;
      warpN <- warpAroundZero(seqN, lens=-lens);
      warpExpN <- round(warpN * expandFactor) + 1;
      newRamp <- rampExp[warpExpN];
   }
   if (plot) {
      showColors(list(ramp=ramp,
         newRamp=nameVector(newRamp, seqN)),
         ...);
   }
   invisible(newRamp);
}

#' Remove alpha transparency from colors
#'
#' Remove alpha transparency from colors
#'
#' This function simply removes the alpha transparency from
#' R colors, returned in hex format, for example `"#FF0000FF"`
#' becomes `"#FF0000"`, or `"blue"` becomes `"#0000FF"`.
#'
#' It also silently converts R color names to hex format,
#' where applicable.
#'
#' @param x `character` vector of R colors
#' @param keepNA `logical` indicating whether `NA` values should be kept
#'   and therefore returned as `NA`.
#'   When `keepNA=FALSE` (default for backward compatibility) `NA`
#'   values are converted to `"#FFFFFF"` as done by `grDevices::col2rgb()`.
#' @param ... additional arguments are ignored.
#'
#' @returns character vector of R colors in hex format.
#'
#' @family jam color functions
#'
#' @examples
#' unalpha(c("#FFFF00DD", "red", NA, "#0000FF", "transparent"))
#'
#' unalpha(c("#FFFF00DD", "red", NA, "#0000FF", "transparent"), keepNA=TRUE)
#'
#' @export
unalpha <- function
(x,
 keepNA=FALSE,
 ...)
{
   ## Purpose is to remove alpha transparency from R colors.
   ## It also silently converts R color names to hex format.
   if (length(x) == 0) {
      return(x)
   }
   iV <- rgb2col(grDevices::col2rgb(x), alpha=FALSE);
   if (TRUE %in% keepNA && any(is.na(x))) {
      iV[is.na(x)] <- NA;
   }
   if (length(names(x)) > 0) {
      names(iV) <- names(x);
   }
   iV;
}

#' check lightMode for light background color
#'
#' check lightMode for light background color
#'
#' Check the lightMode status through function parameter, options, or
#' environment variable. If the function defines lightMode, it is used as-is.
#' If lightMode is NULL, then options("jam.lightMode") is used if defined.
#' Otherwise, it tries to detect whether the R session is running inside
#' Rstudio using the environmental variable "RSTUDIO", and if so it assumes
#' lightMode==TRUE.
#'
#' To set a default lightMode, add options("jam.lightMode"=TRUE) to .Rprofile, or
#' to the relevant R script.
#'
#' @returns logical or length=1, indicating whether lightMode is defined
#'
#' @family jam practical functions
#'
#' @param lightMode `logical` or NULL, indicating whether the lightMode
#'    parameter has been defined in the function call.
#' @param ... Additional arguments are ignored.
#'
#' @examples
#' checkLightMode(TRUE);
#' checkLightMode();
#'
#' @export
checkLightMode <- function
(lightMode=NULL,
 ...)
{
   ## Check lightMode, whether the background color is light or not
   if (length(lightMode) == 0) {
      jam_lightMode <- getOption("jam.lightMode");
      if (length(jam_lightMode) > 0) {
         lightMode <- (jam_lightMode %in% c(1, "TRUE"));
      } else if (Sys.getenv("RSTUDIO") == 1) {
         ## Use rstudioapi if available
         if (requireNamespace("rstudioapi", quietly=TRUE)) {
            if (rstudioapi::isAvailable() && rstudioapi::hasFun("getThemeInfo")) {
               theme <- rstudioapi::getThemeInfo();
               lightMode <- !(theme$dark);
            } else {
               lightMode <- TRUE;
            }
         } else {
            # Default for RStudio is white background
            lightMode <- TRUE;
         }
      } else {
         lightMode <- FALSE;
      }
   } else if (!is.logical(lightMode)) {
      as.logical(head(lightMode, 1))
   }
   return(head(lightMode, 1));
}


#' Get Chroma and Luminance ranges for the given lightMode
#'
#' Return Crange, Lrange, Cgrey, adjustRgb values for the given lightMode,
#' intended to provide ranges suitable for contrasting text displayed
#' on a light or dark background.
#'
#' This function is intended mainly for internal use by `jamba`
#' such as `printDebug()`, and `make_styles()`, which is also mainly
#' intended for console text or other printed text output.
#' The utility of this function is to store the logic of determining
#' sensible default ranges.
#'
#' Companion functions:
#'
#' * `applyCLranges()` is used to apply the ranges to a vector of R colors.
#' * `checkLightMode()` is used to detect whether console output is expected
#' to have a light or dark background.
#'
#' @family jam color functions
#'
#' @param lightMode boolean indicating whether the background color
#'    is light (TRUE is bright), or dark (FALSE is dark.) By default
#'    it calls `checkLightMode()` which queries `getOption("lightMode")`.
#' @param Crange numeric range of chroma values, ranging
#'    between 0 and 100. By default, `getOptions("Crange")` is used,
#'    otherwise defaults will be assigned based upon `lightMode`.
#' @param Lrange numeric range of luminance values, ranging
#'    between 0 and 100. By default, `getOptions("Crange")` is used,
#'    otherwise defaults will be assigned based upon `lightMode`.
#' @param Cgrey numeric chroma (C) value, which defines grey colors at or
#'    below this chroma. Any colors at or below the grey cutoff will have
#'    their C values unchanged. This mechanism prevents converting black
#'    to red, for example. To disable the effect, set `Cgrey=-1`.
#' @param adjustRgb numeric color adjustment factor, used during the
#'    conversion of RGB colors to the ANSI-compatible colors used
#'    by the `crayon` pacakge. The ANSI color range does not include
#'    a full RGB palette, and the conversion is somewhat lossy.
#'    By default, `getOptions("jam.adjustRgb")` is used to store a
#'    globally re-usable value.
#' @param setOptions character or logical whether to update `options()`
#'    `"jam.Crange"` and `"jam.Lrange"`, with the following behavior:
#'    * `"ifnull"` will update only `options()` which were previously `NULL`
#'    * `FALSE` or `"FALSE"` does not update `options()`
#'    * `TRUE` or `"TRUE"` will update `options()` with values determined
#'    by this function.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#'
#' @returns `list` with elements:
#'    \describe{
#'       \item{Crange}{Numeric vector of length 2, defining the
#'       HCL chroma (C) range.}
#'       \item{Lrange}{Numeric vector of length 2, defining the
#'       HCL luminance (L) range.}
#'       \item{adjustRgb}{Numeric vector of length 1, defining the
#'       adjustment to apply during RGB-to-ANSI color conversion.}
#'       \item{Cgrey}{Numeric vector of length 1, defining the
#'       HCL chroma (C) value below which colors are considered greyscale,
#'       and are converted to ANSI greyscale colors.
#'       HCL chroma ranges from 0 to 100. Set value `Cgrey=-1` or
#'       `Cgrey=FALSE` to disable this logic, causing colors to be
#'       matched using all available ANSI color values.}
#'    }
#' @examples
#' setCLranges(lightMode=FALSE)
#'
#' @export
setCLranges <- function
(lightMode=NULL,
 Crange=getOption("jam.Crange"),
 Lrange=getOption("jam.Lrange"),
 Cgrey=getOption("jam.Cgrey", 5),
 adjustRgb=getOption("jam.adjustRgb", 0),
 setOptions=c("FALSE", "ifnull", "TRUE"),
 verbose=FALSE,
 ...)
{
   ## Purpose is to set default values for chroma Crange and
   ## luminance Lrange values, dependent upon
   ## lightMode=TRUE (light background) or lightMode=FALSE (dark background)
   if (length(setOptions) == 0) {
      setOptions <- "ifnull";
   } else {
      setOptions <- as.character(setOptions);
   }

   ## Remove NA values and convert Lrange and Crange to range format
   if (length(rmNA(Lrange)) > 0) {
      Lrange <- range(rmNA(Lrange));
   } else {
      Lrange <- NULL;
   }
   if (length(rmNA(Crange)) > 0) {
      Crange <- range(rmNA(Crange));
   } else {
      Crange <- NULL;
   }

   ## First time through, these values are empty
   ## so we use checkLightMode() default values
   if (length(lightMode) == 0) {
      setLightMode <- checkLightMode();
   } else {
      setLightMode <- lightMode;
   }
   if (length(Lrange) == 0 && length(Crange) == 0) {
      lightMode <- setLightMode;
   }
   if (verbose) {
      printDebug("setCLranges(): ",
         "lightMode: ", rmNULL(nullValue="NULL", lightMode),
         ", setLightMode: ", setLightMode);
   }

   ## When lightMode is NULL use Lrange, Crange as supplied
   ## When lightMode is TRUE or FALSE, override existing values with defaults
   Lrange_default_lite <- c(5, 70);
   Crange_default_lite <- c(10, 190);
   Lrange_default_dark <- c(45, 100);
   Crange_default_dark <- c(30, 190);

   ## Define Crange and Lrange when lightMode is not NULL
   if (length(lightMode) > 0) {
      ## If lightMode is defined, its status overrides any exising values
      if (TRUE %in% lightMode) {
         Lrange <- Lrange_default_lite;
         Crange <- Crange_default_lite;
      } else {
         Lrange <- Lrange_default_dark;
         Crange <- Crange_default_dark;
      }
      if (verbose) {
         printDebug("setCLranges(): ",
            "Defined default values: ",
            "lightMode=", lightMode,
            "; Crange=c(", Crange,
            "); Lrange=c(", Lrange, ")");
      }
   } else {
      # define only ranges that are not yet defined
      if (length(Lrange) == 0) {
         if (TRUE %in% setLightMode) {
            Lrange <- Lrange_default_lite;
         } else {
            Lrange <- Lrange_default_dark;
         }
         if (verbose) {
            printDebug("setCLranges(): ",
               "Defined default values: ",
               "Lrange=c(", Lrange, ")");
         }
      } else {
         Lrange <- range(Lrange, na.rm=TRUE);
      }
      if (length(Crange) == 0) {
         if (TRUE %in% setLightMode) {
            Crange <- Crange_default_lite;
         } else {
            Crange <- Crange_default_dark;
         }
         if (verbose) {
            printDebug("setCLranges(): ",
               "Defined default values: ",
               "Crange=c(", Crange, ")");
         }
      } else {
         Crange <- range(Crange, na.rm=TRUE);
      }
   }

   # define adjustRgb=0 as needed
   if (length(rmNA(adjustRgb)) == 0) {
      adjustRgb <- 0;
   }

   # define Cgrey=5 as needed
   if (length(rmNA(Cgrey)) == 0 || !is.numeric(Cgrey)) {
      Cgrey <- 5;
   }

   ## Update options() as needed
   updatedOptions <- character(0);
   if ("ifnull" %in% setOptions) {
      if (length(getOption("jam.Crange")) == 0) {
         options("jam.Crange"=Crange);
         updatedOptions <- c(updatedOptions, "Crange");
      }
      if (length(getOption("jam.Lrange")) == 0) {
         options("jam.Lrange"=Lrange);
         updatedOptions <- c(updatedOptions, "Lrange");
      }
      if (length(getOption("jam.Cgrey")) == 0) {
         options("jam.Cgrey"=Cgrey);
         updatedOptions <- c(updatedOptions, "Cgrey");
      }
      if (length(getOption("jam.adjustRgb")) == 0) {
         options("jam.adjustRgb"=adjustRgb);
         updatedOptions <- c(updatedOptions, "adjustRgb");
      }
   } else if ("TRUE" %in% setOptions) {
      updatedOptions <- c(updatedOptions,
         "Crange",
         "Lrange",
         "Cgrey",
         "adjustRgb");
      options("jam.Crange"=Crange);
      options("jam.Lrange"=Lrange);
      options("jam.Cgrey"=Cgrey);
      options("jam.adjustRgb"=adjustRgb);
   }
   if (verbose) {
      if (length(updatedOptions) == 0) {
         printDebug("setCLranges(): ",
            "No options() were updated.");
      } else {
         printDebug("setCLranges(): ",
            "These options() were updated: ",
            updatedOptions);
      }
   }

   CLranges <- list(
      Crange=Crange,
      Lrange=Lrange,
      adjustRgb=adjustRgb,
      Cgrey=Cgrey);
   return(CLranges);
}

#' Apply CL color range
#'
#' Restrict chroma (C) and luminance (L) ranges for a vector of R colors
#'
#' This function is primarily intended to restrict the range of brightness
#' values so they contrast with a background color, particularly when the
#' background color may be bright or dark.
#'
#' Note that output is slightly different when supplying one color,
#' compared to supplying a vector of colors. One color is simply
#' restricted to the `Crange` and `Lrange`. However, a vector of colors
#' is scaled within the ranges so that relative `C` and `L` values
#' are maintained, for visual comparison.
#'
#' The C and L values are defined by `colorspace::polarLUV()`, where C is
#' typically restricted to `0..100` and L is typically `0..100`. For some
#' colors, values above 100 are allowed.
#'
#' Values are restricted to the given numeric range using one of three
#' methods, set via the `CLmethod` argument.
#'
#' As an example, consider what should be done when `Crange <- c(10,70)`
#' and the C values are `Cvalues <- c(50, 60, 70, 80)`.
#'
#' 1. "floor" uses `jamba::noiseFloor()` to apply fixed cutoffs at the
#' minimum and maximum range. This method has the effect of making all
#' values outside the range into an equal final value.
#' 2. "scale" will apply `jamba::normScale()` to rescale only values outside
#' the given range. For example, `c(Crange, Cvalues)` as the initial range,
#' it constrains values to `c(Crange)`.  This method has the effect of
#' maintaining the relative difference between values.
#' 3. "expand" will simply apply `jamba::normScale()` to fit the values
#' to the minimum and maximum range values. This method has the effect of
#' forcing colors to fit the full numeric range, even when the original
#' differences between values were small.
#'
#' In case (1) above, Cvalues will become `c(50, 60, 70, 70)`.
#' In case (2) above, Cvalues will become `c(44, 53, 61, 70)`
#' In case (3) above, Cvalues will become `c(10, 30, 50, 70)`
#'
#' Note that colors with C (chroma) values less than `Cgrey` will not have
#' the C value changed, in order to maintain colors at a greyscale, without
#' colorizing them. Particularly for pure `grey`, which has `C=0`, but
#' is still required to have a hue H, it is important not to increase
#' `C`.
#'
#' @returns vector of colors after applying the chroma (C) and luminance (L)
#'    ranges.
#'
#' @family jam color functions
#'
#' @param x vector of R colors
#' @param lightMode `NULL` or `logical`. When `lightMode=NULL` then
#'    `Crange` and `Lrange` values are used as-is; when `lightMode=TRUE`
#'    or `lightMode=FALSE` then default values are used for `Crange` and
#'    `Lrange` values, where `lightMode=TRUE` is intended for colors
#'    to have contrast against a light/bright/white background,
#'    and `lightMode=FALSE` is intended for colors to have contrast
#'    against a dark background.
#' @param Crange `NULL` or `numeric` range with minimum and maximum allowed
#'    values for the chroma (C) component.
#' @param Lrange `NUL`L or `numeric` range with minimum and maximum allowed
#'    values for the luminance (L) component.
#' @param Cgrey `numeric` chroma (C) value, which defines grey colors at or
#'    below this chroma. Any colors at or below the grey cutoff will have
#'    their C values unchanged. This mechanism prevents converting black
#'    to red, for example. To disable the effect, set `Cgrey=-1`.
#' @param fixYellow `logical` indicating whether to "fix" the darkening of
#'    yellow, which otherwise turns to green. Instead, since JAM can,
#'    JAM will make the yellow slightly more golden before darkening,
#'    which is achieved by calling `fixYellowHue()`.
#' @param CLmethod `character` string indicating how to alter values
#'    outside the respective `Crange` and `Lrange` ranges. "scale" will
#'    rescale values only if any are outside of range, and will rescale
#'    the full range of `c(Crange, Cvalues)` to `c(Crange)`. In this way,
#'    only values outside the range are rescaled. "floor" will apply a
#'    fixed cutoff, any values outside the range are set to equal the
#'    range boundary itself. "expand" will rescale all values so the
#'    range is equal to `Crange`.
#' @param fixup `logical` passed to `hcl2col()` and subsequently
#'    to `colorspace::hex()` when converting colors outside the color
#'    gamut (visible range.) When `fixup` is `NULL`, the `hcl2col()`
#'    method applies its own aggressive technique to restrict the color
#'    range.
#' @param ... additional argyments are passed to `fixYellowHue()` when
#'    `fixYellow` is `TRUE`.
#'
#' @examples
#' cl <- c("red", "blue", "navy", "yellow", "orange");
#' cl_lite <- applyCLrange(cl, lightMode=TRUE);
#' cl_dark <- applyCLrange(cl, lightMode=FALSE);
#'
#' # individual colors
#' cl_lite_ind <- sapply(cl, applyCLrange, lightMode=TRUE);
#' cl_dark_ind <- sapply(cl, applyCLrange, lightMode=FALSE);
#'
#' # display colors
#' showColors(list(`input colors`=cl,
#'    `lightMode=TRUE, vector`=cl_lite,
#'    `lightMode=TRUE, individual`=cl_lite_ind,
#'    `lightMode=FALSE, vector`=cl_dark,
#'    `lightMode=FALSE, individual`=cl_dark_ind))
#' printDebug(cl, lightMode=TRUE);
#'
#' @export
applyCLrange <- function
(x,
 lightMode=NULL,
 Crange=getOption("jam.Crange"),
 Lrange=getOption("jam.Lrange"),
 Cgrey=getOption("jam.Cgrey", 5),
 fixYellow=TRUE,
 CLmethod=c("scale", "floor", "expand"),
 fixup=TRUE,
 ...)
{
   ## Purpose is to restrict the chroma (C) or luminance (L) ranges
   CLranges <- setCLranges(
      lightMode=lightMode,
      Crange=Crange,
      Lrange=Lrange,
      Cgrey=Cgrey,
      # adjustRgb=adjustRgb,
      setOptions=FALSE);
   Crange <- CLranges$Crange;
   Lrange <- CLranges$Lrange;
   Cgrey <- CLranges$Cgrey;
   adjustRgb <- CLranges$adjustRgb;

   # ## for a vector of R colors
   # if (length(lightMode) > 0) {
   #    CLrange <- setCLranges(lightMode=lightMode,
   #       Crange=Crange,
   #       Lrange=Lrange,
   #       ...);
   #    Crange <- CLrange$Crange;
   #    Lrange <- CLrange$Lrange;
   # }

   CLmethod <- match.arg(CLmethod);
   if (length(x) == 0 ||
         all(is.na(x))) {
      return(x);
   }
   # if (length(x) == 0 ||
   #       all(is.na(x)) ||
   #       (length(Crange) == 0 &&
   #          length(Lrange) == 0 &&
   #          !any(fixYellow))) {
   #    return(x);
   # }
   if (is.null(names(x))) {
      names(x) <- makeNames(rep("col", length(x)));
   }
   fixYellow <- rep(fixYellow,
      length.out=length(x));
   styleHcl <- col2hcl(x);
   styleNA <- is.na(x);

   ## Apply L range
   if (length(Lrange) > 0) {
      if (CLmethod %in% "floor") {
         styleHcl["L",] <- jamba::noiseFloor(styleHcl["L",],
            minimum=min(Lrange),
            ceiling=max(Lrange));
      } else if (CLmethod %in% "scale") {
         styleHcl["L",] <- jamba::normScale(styleHcl["L",],
            from=min(Lrange),
            to=max(Lrange),
            low=min(c(Lrange, styleHcl["L",]), na.rm=TRUE),
            high=max(c(Lrange, styleHcl["L",]), na.rm=TRUE));
      } else {
         styleHcl["L",] <- jamba::normScale(styleHcl["L",],
            from=min(Lrange),
            to=max(Lrange));
      }
   }

   ## Apply C range
   if (length(Crange) > 0) {
      styleGrey <- (styleHcl["C",] <= Cgrey);
      if (any(styleGrey)) {
         styleGreyV <- styleHcl["C",styleGrey];
      }
      if (!all(styleGrey)) {
         if (CLmethod %in% "floor") {
            styleHcl["C",!styleGrey] <- jamba::noiseFloor(styleHcl["C",!styleGrey],
               minimum=min(Crange),
               ceiling=max(Crange));
         } else if (CLmethod %in% "scale") {
            styleHcl["C",!styleGrey] <- jamba::normScale(styleHcl["C",!styleGrey],
               from=min(Crange),
               to=max(Crange),
               low=min(c(Crange, styleHcl["C",!styleGrey]), na.rm=TRUE),
               high=max(c(Crange, styleHcl["C",!styleGrey]), na.rm=TRUE));
         } else {
            styleHcl["C",!styleGrey] <- jamba::normScale(styleHcl["C",!styleGrey],
               from=min(Crange),
               to=max(Crange));
         }
      }
      if (any(styleGrey)) {
         styleHcl["C",styleGrey] <- styleGreyV;
      }
   }

   ## Optionally "fix" yellows
   k <- (fixYellow & !styleNA);
   if (any(k)) {
      styleHcl[,k] <- fixYellowHue(styleHcl[, k, drop=FALSE], ...);
   }

   ## Convert back to hex color
   x <- hcl2col(styleHcl,
      fixup=fixup,
      ...);
   if (length(styleNA) > 0) {
      x[styleNA] <- NA;
   }
   x;
}


#' Fix yellow color hue
#'
#' Fix yellow color hue to be less green than default "yellow"
#'
#' This function "fixes" the color yellow, which by default appears green
#' especially when darkened. The effect of this function is to make yellows
#' appear more red, which appears more visibly yellow even when the color
#' is darkened.
#'
#' This function is intended to be tolerant to missing values. For example if
#' any of the values `HCL`, `Hrange`, or `Hshift` are length 0, the original
#' `HCL` is returned unchanged.
#'
#' @family jam color functions
#'
#' @param HCL numeric matrix with HCL color values, as returned by `col2hcl()`,
#'    but requiring only one rowname `"H"` representing the color hue on
#'    a scale of 0 to 360. If input data does not contain numeric values
#'    with rowname "H", `HCL` is return unchanged.
#' @param Hrange numeric vector whose range defines the region of hues
#'    to be adjusted. By default hues between 80 and 90 are adjusted. If
#'    NULL, `HCL` is return unchanged.
#' @param Hshift numeric value length one, used to adjust the hue of colors
#'    within the range `Hrange`. If NULL, `HCL` is return unchanged.
#' @param ... additional arguments are ignored.
#'
#' @returns returns the input `HCL` data where rowname `"H"` has hue values
#'    adjusted accordingly. In the event `HCL`, `Hrange`, or `Hshift` have
#'    length 0, the original `HCL` is returned. If input data does not
#'    meet the expected format, the input `HCL` is returned unchanged.
#'
#' @examples
#' yellows <- vigrep("yellow", grDevices::colors());
#' yellowsHCL <- col2hcl(yellows);
#' fixedYellowsHCL <- fixYellowHue(yellowsHCL);
#' fixedYellows <- hcl2col(fixedYellowsHCL);
#' showColors(list(yellows=yellows,
#'    fixedYellows=fixedYellows));
#'
#' @export
fixYellowHue <- function
(HCL,
 Hrange=c(80,90),
 Hshift=-15,
 ...)
{
   ## Purpose is to "fix" the color yellow, which when darkening the
   ## default R yellow becomes green. This function alters the actual
   ## hue to become more reddish, the visible effect is the color appears
   ## to retain "yellow" color even when darkened.
   if (length(HCL) == 0 ||
         length(dim(HCL)) < 2 ||
         !"H" %in% rownames(HCL) ||
         !is.numeric(HCL["H",])) {
      #warning("fixYellowHue() requires rowname 'H' with numeric values between 0 and 360.");
      return(HCL);
   }
   Hshift <- head(rmNA(Hshift), 1);
   if (length(rmNA(Hrange)) == 0 || length(Hshift) == 0) {
      #warning("fixYellowHue() requires Hrange and Hshift to have length > 0.")
      return(HCL);
   }
   Hrange <- range(Hrange, na.rm=TRUE);
   styleYellow <- (!is.na(HCL["H",]) &
         HCL["H",] >= Hrange[1] &
         HCL["H",] <= Hrange[2]);
   if (any(styleYellow)) {
      HCL["H",styleYellow] <- HCL["H",styleYellow] + Hshift;
   }
   return(HCL);
}

#' Fix yellow color
#'
#' Fix yellow color to be less green than default "yellow"
#'
#' This function "fixes" the color yellow, which by default appears green
#' especially when darkened. The effect of this function is to make yellows
#' appear more red, which appears more visibly yellow even when the color
#' is darkened.
#'
#' This function is intended to be tolerant to missing values. For example if
#' any of the values `col`, `Hrange`, or `Hshift` are length 0, the original
#' `col` is returned unchanged.
#'
#' @param col R color, either in hex color format or using values from
#'    `grDevices::colors()`.
#' @param Hrange numeric vector whose range defines the region of hues
#'    to be adjusted. By default hues between 80 and 90 are adjusted. If
#'    NULL, `HCL` is return unchanged.
#' @param Hshift numeric value length one, used to adjust the hue of colors
#'    within the range `Hrange`. If NULL, `HCL` is return unchanged.
#' @param fixup `logical`, default TRUE, whether to apply fixup to
#'    the resulting color, passed to `hcl2col()`
#' @param ... additional arguments are passed to `col2hcl()`, and
#'    `hcl2col()`.
#'
#' @returns returns a vector of R colors the same length as input `col`.
#'    In the event `col`, `Hrange`, or `Hshift` have length 0, or if any
#'    step in the conversion produces length 0, then the
#'    original `col` is returned.
#'
#' @family jam color functions
#'
#' @examples
#' yellows <- vigrep("yellow", grDevices::colors());
#' fixedYellows <- fixYellow(yellows);
#' showColors(list(yellows=yellows,
#'    fixedYellows=fixedYellows));
#'
#' @export
fixYellow <- function
(col,
 Hrange=c(70,100),
 Hshift=-20,
 fixup=TRUE,
 ...)
{
   ## Purpose is to provide a wrapper to fixColorHue() for other R colors
   if (length(rmNA(col)) == 0) {
      return(col);
   }
   HCL <- col2hcl(col, ...);
   if (length(HCL) == 0) {
      return(col);
   }
   HCL2 <- fixYellowHue(HCL,
      Hrange=Hrange,
      Hshift=Hshift,
      ...);
   if (length(HCL2) == 0) {
      return(col);
   }
   col2 <- hcl2col(HCL2,
      fixup=fixup,
      ...);
   return(col2);
}

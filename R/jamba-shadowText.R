
#' Get and set options for shadowText
#'
#' Get and set options for shadowText
#'
#' This function is intended to be a convenient method to get and set
#' options to be used with `jamba::shadowText()`.
#' This function stores the resulting values in `options()` for
#' use by `shadowText()`.
#'
#' @family jam plot functions
#'
#' @param r `numeric` radius used for outline or shadow
#' @param n `numeric` number of shadow steps to render around each text label
#' @param outline `logical` indicating whether to render shadowText
#'    as an outline (default), or when `outline=FALSE` it renders a
#'    drop shadow offset using `offset` which by default is slightly
#'    down and to the right of the text labels.
#' @param alphaOutline `numeric` value for alpha transparency used for
#'    label outlines when `outline=TRUE`, with values expected
#'    between 0 (fully transparent) and 1 (not transparent).
#' @param shadow `logical` indicating whether to render shadowText as a
#'    shadow, or not (default).
#' @param shadowColor `character` R color which defines the color used
#'    for the outline or shadow for each text label.
#' @param alphaShadow `numeric` value for alpha transparency used for
#'    label shadows when `shadow=TRUE`, with values expected
#'    between 0 (fully transparent) and 1 (not transparent).
#' @param r_ex `numeric` expansion factor used to adjust the radius `r`.
#'    The value for `r` is defined based upon the arguments provided,
#'    then is multiplied by the `r_ex` expansion factor.
#'    The result is stored in option "jam.shadow.r".
#' @param alpha_ex `numeric` expansion factor used to adjust the
#'    alpha transparency of both `alphaOutline` and `alphaShadow`.
#'    Values will be maintained no lower than 0 and no higher than 1.
#'    The values for `alphaOutline` and `alphaShadow` are defined
#'    based upon the arguments provided, then are multiplied by the
#'    `alpha_ex` expansion factor.
#'    The result is clipped to range 0,1 using `jamba::noiseFloor()`.
#'    The resulting values are stored in options
#'    "jam.alphaOutline" and "jam.alphaShadow", respectively.
#' @param preset `character` string which defines a preset with
#'    associated settings. Any value other than `"none"` will cause
#'    all other options to use the preset settings.
#'    * `"none"`: no preset settings are applied
#'    * `"default"`: reverts all options to the original default values,
#'    which produces an outline, and not a drop shadow.
#'    The color will use `shadowColor` which allows using all other settings
#'    from this preset, except with custom color.
#'    * `"bold"`: makes output produce visibly more distinct outline,
#'    with no drop shadow.
#'    The color will use `shadowColor` which allows using all other settings
#'    from this preset, except with custom color.
#'    * `"bold white"`: same as "bold" except default text color is white
#'    * `"bold black"`: same as "bold" except default text color is black
#'    * `"both"`: applies "default" and enables drop shadow
#'    * `"shadow"`: uses suggested default values to produce a drop shadow,
#'    and not an outline.
#'    The color will use `shadowColor` which allows using all other settings
#'    from this preset, except with custom color.
#'    * `"bold shadow"`: same as "shadow" except the shadow is more distinct.
#'    The color will use `shadowColor` which allows using all other settings
#'    from this preset, except with custom color.
#'    * `"bold white shadow"`: same as "bold shadow" with white shadow
#'    * `"bold black shadow"`: same as "bold shadow" with black shadow
#'    * `"bold both"`: same as "bold" except also enables bold shadow
#' @param verbose `logical` indicating whether to print verbose output
#' @param ... additional arguments are ignored.
#'
#' @examples
#' nullPlot(doBoxes=FALSE, xlim=c(-1, 4), ylim=c(-1, 4), asp=1);
#' usrBox(fill="grey")
#' cex <- 1.2
#' graphics::axis(1);graphics::axis(2, las=2)
#' shadowText_options(preset="default")
#' shadowText(x=0, y=3, "default", cex=cex)
#' shadowText_options(preset="bold")
#' shadowText(x=0, y=2, "bold", cex=cex)
#' shadowText_options(preset="bold white")
#' shadowText(x=0, y=1, col="black", "bold white", cex=cex)
#' shadowText_options(preset="bold black")
#' shadowText(x=0, y=0, col="white", "bold black", cex=cex)
#' shadowText_options(preset="shadow")
#' shadowText(x=3, y=3, "shadow", cex=cex)
#' shadowText_options(preset="bold shadow")
#' shadowText(x=3, y=2, "bold shadow", cex=cex)
#' shadowText_options(preset="bold white shadow")
#' shadowText(x=3, y=1, col="black", "bold white shadow", cex=cex)
#' shadowText_options(preset="bold black shadow")
#' shadowText(x=3, y=0, col="white", "bold black shadow", cex=cex)
#' shadowText_options(preset="both")
#' shadowText(x=1.5, y=3, col="white", "both", cex=cex)
#' shadowText(x=1.5, y=2.5, col="black", "both", cex=cex)
#' shadowText_options(preset="bold both")
#' shadowText(x=1.5, y=2, col="white", "bold both", cex=cex)
#' shadowText(x=1.5, y=1, col="black", "bold both", cex=cex)
#' shadowText(x=1.5, y=0.5, col="blue3", "bold both", cex=cex, font=2)
#' shadowText(x=1.5, y=0, col="indianred1", "bold both", cex=cex, font=2)
#' shadowText_options(preset="default")
#'
#' @returns `list` with the following options for `shadowText()`:
#'    * jam.shadow.r
#'    * jam.shadow.n
#'    * jam.outline
#'    * jam.alphaOutline
#'    * jam.shadow
#'    * jam.shadowColor
#'    * jam.alphaShadow
#'
#' @export
shadowText_options <- function
(r=getOption("jam.shadow.r", 0.15),
 n=getOption("jam.shadow.n", 8),
 outline=getOption("jam.outline", TRUE),
 alphaOutline=getOption("jam.alphaOutline", 0.4),
 shadow=getOption("jam.shadow", FALSE),
 shadowColor=getOption("jam.shadowColor", "black"),
 alphaShadow=getOption("jam.alphaShadow", 0.2),
 r_ex=1,
 alpha_ex=1,
 preset=c("none",
    "default",
    "bold",
    "bold white",
    "bold black",
    "both",
    "shadow",
    "bold shadow",
    "bold white shadow",
    "bold black shadow",
    "bold both"),
 verbose=FALSE,
 ...)
{
   #
   preset <- match.arg(preset);

   # define default values
   shadowText_default <- list(
      r=0.15,
      n=8,
      outline=TRUE,
      alphaOutline=0.4,
      shadow=FALSE,
      shadowColor="black",
      alphaShadow=0.2)

   # define list with values
   shadowText_values <- list(
      r=r,
      n=n,
      outline=outline,
      alphaOutline=alphaOutline,
      shadow=shadow,
      shadowColor=shadowColor)


   if (preset %in% c("default", "both")) {
      shadowText_values <- list(
         r=0.15,
         n=8,
         outline=TRUE,
         alphaOutline=0.4,
         shadow=FALSE,
         shadowColor="black",
         alphaShadow=0.2)
      if (preset %in% "both") {
         shadowText_values$shadow <- TRUE;
         shadowText_values$alphaOutline <- 0.2;
      }
   } else if (preset %in% c("bold",
      "bold white",
      "bold black",
      "bold both")) {
      shadowText_values <- list(
         r=0.2,
         n=16,
         outline=TRUE,
         alphaOutline=0.5,
         shadow=FALSE,
         shadowColor=shadowColor,
         alphaShadow=0.2)
      if (preset %in% "bold both") {
         shadowText_values$shadow <- TRUE;
         shadowText_values$alphaShadow <- 0.2;
      }
      if (preset %in% "bold white") {
         shadowText_values$shadowColor <- "#FFFFFF"
      } else if (preset %in% "bold black") {
         shadowText_values$shadowColor <- "#000000"
      }
   } else if ("shadow" %in% preset) {
      shadowText_values <- list(
         r=0.15,
         n=8,
         outline=FALSE,
         alphaOutline=0.3,
         shadow=TRUE,
         shadowColor=shadowColor,
         alphaShadow=0.2)
   } else if (preset %in% c("bold shadow",
      "bold white shadow",
      "bold black shadow")) {
      shadowText_values <- list(
         r=0.2,
         n=16,
         outline=FALSE,
         alphaOutline=0.4,
         shadow=TRUE,
         shadowColor=shadowColor,
         alphaShadow=0.2)
      if (preset %in% "bold white shadow") {
         shadowText_values$shadowColor <- "#FFFFFF"
      } else if (preset %in% "bold black shadow") {
         shadowText_values$shadowColor <- "#000000"
      }
   }
   if (verbose && !"none" %in% preset) {
      printDebug("shadowText_options(): ",
         "applied preset: '", preset, "'");
   }

   # optionally apply expansion factors
   if (length(r_ex) > 0 && !1 %in% head(r_ex, 1)) {
      shadowText_values$r <- shadowText_values$r * head(r_ex, 1);
      if (verbose) {
         printDebug("shadowText_options(): ",
            "applied r_ex: ", r_ex, ".");
      }
   }
   if (length(alpha_ex) > 0 && !1 %in% head(alpha_ex, 1)) {
      shadowText_values$alphaOutline <- noiseFloor(
         shadowText_values$alphaOutline * alpha_ex,
         minimum=0,
         ceiling=1)
      shadowText_values$alphaShadow <- noiseFloor(
         shadowText_values$alphaShadow * alpha_ex,
         minimum=0,
         ceiling=1)
      if (verbose) {
         printDebug("shadowText_options(): ",
            "applied alpha_ex: ", r_ex, ".");
      }
   }

   # now update options
   options("jam.shadow.r"=shadowText_values$r)
   options("jam.shadow.n"=shadowText_values$n)
   options("jam.outline"=shadowText_values$outline)
   options("jam.alphaOutline"=shadowText_values$alphaOutline)
   options("jam.shadow"=shadowText_values$shadow)
   options("jam.shadowColor"=shadowText_values$shadowColor)
   options("jam.alphaShadow"=shadowText_values$alphaShadow)
   if (verbose) {
      for (i in names(shadowText_values)) {
         printDebug("shadowText_options(): ",
            paste0(
               format(i,
                  justify="right",
                  width=max(nchar(names(shadowText_values)))),
               ": "),
            shadowText_values[[i]],
            fgText=c("darkorange1", "dodgerblue"));
      }
   }

   return(invisible(shadowText_values))
}

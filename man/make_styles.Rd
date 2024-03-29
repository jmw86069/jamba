% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba.r
\name{make_styles}
\alias{make_styles}
\title{vectorized make_styles for crayon output}
\usage{
make_styles(
  style = NULL,
  text,
  bg = FALSE,
  bg_style = NULL,
  grey = FALSE,
  colors = num_colors(),
  Cgrey = getOption("jam.Cgrey", 5),
  lightMode = NULL,
  Crange = getOption("jam.Crange"),
  Lrange = getOption("jam.Lrange"),
  adjustRgb = getOption("jam.adjustRgb"),
  adjustPower = 1.5,
  fixYellow = TRUE,
  colorTransparent = "grey45",
  alphaPower = 2,
  setOptions = c("ifnull", "FALSE", "TRUE"),
  verbose = FALSE,
  ...
)
}
\arguments{
\item{style}{\code{character} vector of one or more styles. When \code{NULL} or \code{NA},
no style is applied, except when \code{bg_style} is supplied
and is neither \code{NA} nor \code{NULL}, in which case entries with
a \code{bg_style} and no \code{style} will use \code{setTextContrastColor()}
to define a contrasting \code{style}.}

\item{text}{\code{character} vector (or coerced to \code{character}) of one or
more values,.}

\item{bg}{\code{logical} indicating whether the \code{style} should be
applied to the background instead of foreground. This argument
is ignored when \code{bg_style} is supplied.}

\item{bg_style}{\code{NULL} or a \code{character} vector of one or more background
styles. When this argument is not NULL, it applies both the foreground
\code{style} and background \code{bg_style} together, and therefore ignores
\code{Crange} and \code{Lrange} settings.}

\item{colors}{\code{integer} number of colors allowed for console output.}

\item{Cgrey}{\code{numeric} chroma (C) value, which defines grey colors at or
below this chroma. Any colors at or below the grey cutoff will have
use ANSI greyscale coloring. To disable, set \code{Cgrey=-1}.}

\item{lightMode}{\code{logical} indicating whether the background color
is light (TRUE is bright), or dark (FALSE is dark.) By default
it calls \code{checkLightMode()} which queries \code{getOption("lightMode")}.}

\item{Crange}{\code{numeric} range of chroma values, ranging
between 0 and 100. When NULL, default values will be
assigned to Crange. When supplied, range(Crange) is used.}

\item{Lrange}{\code{numeric} range of luminance values, ranging
between 0 and 100. When NULL, default values will be
assigned to Lrange. When supplied, range(Lrange) is used.}

\item{adjustRgb}{\code{numeric} value adjustment used during the conversion of
RGB colors to ANSI colors, which is inherently lossy. If not defined,
it uses the default returned by \code{setCLranges()} which itself uses
\code{getOption("jam.adjustRgb")} with default=0. In order to boost
color contrast, an alternate value of -0.1 is suggested.}

\item{adjustPower}{\code{numeric} adjustment power factor}

\item{fixYellow}{\code{logical} indicating whether to "fix" the darkening of
yellow, which otherwise turns to green. Instead, since JAM can,
JAM will make the yellow slightly more golden before darkening. This
change only affects color hues between 80 and 90. This argument is
passed to \code{applyCLrange()}.}

\item{colorTransparent}{\code{character} color used to substitute for
"transparent" which a valid R color, but not a valid color for
the crayon package.}

\item{alphaPower}{\code{numeric} value, used to adjust the RGB values for alpha
values less than 255, by raising the ratio to 1/alphaPower, which takes
the ratio of square roots.  alphaPower=100 for minimal adjustment.}

\item{setOptions}{\code{character} or \code{logical} whether to update
\code{Crange} and \code{Lrange} options during the subsequent call to
\code{setCLranges()}. By default,
\itemize{
\item \code{"ifnull"} will update only options which were previously \code{NULL};
\item \code{"FALSE"} prevents modifying the global options;
\item \code{"TRUE"} will update these options with the current values.
}}

\item{verbose}{\code{logical} indicating whether to print verbose output}

\item{...}{additional parameters are ignored}

\item{satCutoff}{\code{numeric} cutoff for color saturation, below which a color
is considered grey and the ANSI greyscale color set is used.}
}
\value{
\code{character} vector with the same length as \code{text} input vector, where
entries are surrounded by the relevant encoding consistent with
the \code{style} defined at input. In short, a character vector as input,
a colorized character vector as output.
}
\description{
vectorized make_styles for crayon output
}
\details{
This function is essentially a vectorized version of
\code{crayon::make_style()} in order to style a vector of
character strings with a vector of foreground and background styles.
}
\seealso{
Other jam practical functions: 
\code{\link{breakDensity}()},
\code{\link{checkLightMode}()},
\code{\link{check_pkg_installed}()},
\code{\link{colNum2excelName}()},
\code{\link{color_dither}()},
\code{\link{diff_functions}()},
\code{\link{exp2signed}()},
\code{\link{fileInfo}()},
\code{\link{fixYellow}()},
\code{\link{getAxisLabel}()},
\code{\link{handleArgsText}()},
\code{\link{heads}()},
\code{\link{isFALSEV}()},
\code{\link{isTRUEV}()},
\code{\link{jamba}},
\code{\link{jargs}()},
\code{\link{kable_coloring}()},
\code{\link{lldf}()},
\code{\link{log2signed}()},
\code{\link{make_html_styles}()},
\code{\link{match_unique}()},
\code{\link{mergeAllXY}()},
\code{\link{middle}()},
\code{\link{minorLogTicks}()},
\code{\link{newestFile}()},
\code{\link{printDebug}()},
\code{\link{renameColumn}()},
\code{\link{rmInfinite}()},
\code{\link{rmNAs}()},
\code{\link{rmNA}()},
\code{\link{rmNULL}()},
\code{\link{sclass}()},
\code{\link{sdim}()},
\code{\link{setPrompt}()}

Other jam color functions: 
\code{\link{alpha2col}()},
\code{\link{applyCLrange}()},
\code{\link{col2alpha}()},
\code{\link{col2hcl}()},
\code{\link{col2hsl}()},
\code{\link{col2hsv}()},
\code{\link{color2gradient}()},
\code{\link{fixYellowHue}()},
\code{\link{fixYellow}()},
\code{\link{getColorRamp}()},
\code{\link{hcl2col}()},
\code{\link{hsl2col}()},
\code{\link{hsv2col}()},
\code{\link{isColor}()},
\code{\link{kable_coloring}()},
\code{\link{makeColorDarker}()},
\code{\link{make_html_styles}()},
\code{\link{rgb2col}()},
\code{\link{setCLranges}()},
\code{\link{setTextContrastColor}()},
\code{\link{showColors}()},
\code{\link{unalpha}()},
\code{\link{warpRamp}()}
}
\concept{jam color functions}
\concept{jam practical functions}

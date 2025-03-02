#' vectorized make_styles for html span output
#'
#' vectorized make_styles for html span output
#'
#' Note this function is experimental.
#'
#' @family jam internal functions
#'
#' @inheritParams make_styles
#'
#' @returns `character` vector with the same length as `text` input vector,
#' where entries are surrounded by the relevant HTML consistent with
#' the `style` defined at input. In short, a character vector as input,
#' colorized HTML character vector as output.
#'
#' @examples
#' make_html_styles(style=c("red", "orange"), text=c("one ", "two"))
#'
#' @export
make_html_styles <- function
(style=NULL,
 text,
 bg=FALSE,
 bg_style=NULL,
 grey=FALSE,
 Cgrey=getOption("jam.Cgrey"),
 lightMode=NULL,
 Crange=getOption("jam.Crange"),
 Lrange=getOption("jam.Lrange"),
 adjustRgb=getOption("jam.adjustRgb"),
 adjustPower=1.5,
 fixYellow=TRUE,
 alphaPower=2,
 setOptions=FALSE,
 verbose=FALSE,
 ...)
{
   ## Purpose is to mimic make_styles() but to print HTML output
   ## <span style="color:blue">text</span>
   if (length(text) == 0) {
      return(text);
   }

   if (length(Cgrey) == 0) {
      Cgrey <- -1;
   }

   ## Determine Crange, Lrange, adjustRgb
   CLranges <- setCLranges(lightMode=lightMode,
      Crange=Crange,
      Lrange=Lrange,
      setOptions=setOptions);
   if (length(adjustRgb) == 0) {
      adjustRgb <- CLranges$adjustRgb;
   }
   Lrange <- CLranges$Lrange;
   Crange <- CLranges$Crange;

   if (length(fixYellow) == 0) {
      fixYellow <- FALSE;
   }
   fixYellow <- rep(fixYellow, length.out=length(text));

   ## Process style
   if (length(style) > 0 && igrepHas("matrix", class(style))) {
      if (verbose > 1) {
         print(paste0("make_html_styles(): ",
            "Handling argument style as matrix."))
      }
      if (!all(c("red", "green", "blue") %in% rownames(style))) {
         stop("When style is a matrix it must contain rownames 'red', 'green', and 'blue'.");
      }
      styleNA <- (is.na(style["red",]) |
            is.na(style["green",]) |
            is.na(style["blue",]));
      ## Convert to color vector to apply CL range, then back to rgb
      styleV <- rgb2col(style);
      styleV <- rep(styleV, length.out=length(text));
      styleNA <- rep(styleNA, length.out=length(text));
   } else {
      if (verbose > 1) {
         print(paste0("make_html_styles(): ",
            "Handling argument style as vector."))
      }
      if (length(style) == 0 || length(unlist(style)) == 0) {
         style <- NA;
      }
      style <- rep(style, length.out=length(text));
      styleNA <- is.na(style);
      styleV <- style;
   }

   ## Process bg_style
   if (length(bg_style) > 0 && igrepHas("matrix", class(bg_style))) {
      if (verbose > 1) {
         print(paste0("make_html_styles(): ",
            "Handling argument bg_style as matrix."))
      }
      if (!all(c("red", "green", "blue") %in% rownames(bg_style))) {
         stop("When bg_style is a matrix it must contain rownames 'red', 'green', and 'blue'.");
      }
      styleNA <- (is.na(bg_style["red",]) |
            is.na(bg_style["green",]) |
            is.na(bg_style["blue",]));
      ## Convert to color vector to apply CL range, then back to rgb
      bg_styleV <- rgb2col(bg_style);
      bg_styleV <- rep(bg_styleV, length.out=length(text));
      bg_styleNA <- rep(bg_styleNA, length.out=length(text));
   } else {
      if (verbose > 1) {
         print(paste0("make_html_styles(): ",
            "Handling argument bg_style as vector."))
      }
      if (length(bg_style) == 0 || length(unlist(bg_style)) == 0) {
         bg_style <- NA;
      }
      bg_style <- rep(bg_style, length.out=length(text));
      bg_styleNA <- is.na(bg_style);
      bg_styleV <- bg_style;
   }
   if (length(bg) == 0) {
      bg <- FALSE;
   }
   bg <- rmNA(naValue=FALSE,
      rep(bg, length.out=length(text)));

   ## Optionally apply fixYellow() to bg
   if (any(fixYellow & !bg_styleNA)) {
      ## fixYellow
      bg_style[!bg_styleNA & fixYellow] <- fixYellow(bg_style[!bg_styleNA & fixYellow],
         ...);
   }

   ## Apply Crange, Lrange only when bg_style is NA
   if (any(bg_styleNA & !styleNA)) {
      if (verbose > 1) {
         print(paste0("make_html_styles(): ",
            "applyCLrange()"));
         print(paste0("make_html_styles(): ",
            "bg_styleNA:",
            cPaste(bg_styleNA)));
         print(paste0("make_html_styles(): ",
            "Lrange:",
            cPaste(Lrange)));
         print(paste0("make_html_styles(): ",
            "Crange:",
            cPaste(Crange)));
      }
      styleV[bg_styleNA & !styleNA] <- applyCLrange(styleV[bg_styleNA & !styleNA],
         Lrange=Lrange,
         Crange=Crange,
         Cgrey=Cgrey,
         fixYellow=fixYellow[bg_styleNA & !styleNA],
         verbose=verbose,
         ...);
   }

   ## Optionally apply fixYellow() to styleV
   if (any(fixYellow & !styleNA)) {
      ## fixYellow
      styleV[!styleNA & fixYellow] <- fixYellow(styleV[!styleNA & fixYellow],
         ...);
   }

   if (1 == 2) {
      ## Convert each style to rgb color
      style <- grDevices::col2rgb(styleV, alpha=TRUE);
      if (any(styleNA)) {
         style[,styleNA] <- NA;
      }
      bg_style <- grDevices::col2rgb(bg_styleV, alpha=TRUE);
      if (any(bg_styleNA)) {
         bg_style[,bg_styleNA] <- NA;
      }
   }

   ## Apply each style to each text entry
   iVals <- sapply(seq_along(text), function(i){
      iText <- text[i];
      #iStyle <- style[,i,drop=FALSE];
      #ibgStyle <- bg_style[,i,drop=FALSE];
      iStyle <- styleV[i];
      ibgStyle <- bg_style[i];
      if (styleNA[i] && bg_styleNA[i]) {
         if (verbose > 1) {
            print(paste0("make_html_styles(): ",
               "No style applied to text:",
               iText));
         }
         iText;
      } else if (styleNA[i] && !bg_styleNA[i]) {
         ## Combine bg with contrasting fg color
         bg_contrast <- setTextContrastColor(ibgStyle,
            useGrey=5);
         if (verbose > 1) {
            print(paste0("make_html_styles(): ",
               "bg style and contrasting fg style applied to text:",
               iText));
            print(paste0("iText:", iText,
               ", bg_contrast:", bg_contrast,
               ", ibgStyle:", ibgStyle));
         }
         paste0('<span style="color:',
            bg_contrast,
            ';background-color:',
            ibgStyle,
            '">',
            iText,
            '</span>');
      } else if (!styleNA[i] && !bg_styleNA[i]) {
         ## Combine bg with fg color
         if (verbose > 1) {
            print(paste0("make_html_styles(): ",
               "bg style and fg style applied to text:",
               iText));
         }
         paste0('<span style="color:',
            iStyle,
            ';background-color:',
            ibgStyle,
            '">',
            iText,
            '</span>');
      } else {
         if (verbose > 1) {
            print(paste0("make_html_styles(): ",
               "fg style applied to text:",
               iText));
         }
         if (verbose > 1) {
            print(paste0("make_html_styles(): ",
               "iStyle:"));
            print(iStyle);
            print(paste0("make_html_styles(): ",
               "bg:", bg));
         }
         paste0('<span style="color:',
            iStyle,
            '">',
            iText,
            '</span>');
      }
   });
   if (verbose) {
      print(paste0("make_html_styles(): ",
         "iVals: ", cPaste(paste0("'", iVals, "'"), sep="; ")))
   }
   iVals

}

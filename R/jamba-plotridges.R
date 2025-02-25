
#' Plot ridges density plots for numeric matrix input
#'
#' Plot ridges density plots for numeric matrix input
#'
#' This function is a convenient wrapper for `ggridges::geom_density_ridges2()`,
#' intended to be analogous to `plotPolygonDensity()` which differs
#' by plotting each item in a separate plot panel using base graphics.
#' This function plots each item as a ridgeline plot in the same
#' plot window using `ggplot2::ggplot()`.
#'
#' @family jam plot functions
#'
#' @returns object with class `"gg", "ggplot"` with density plot
#'    in the form of ridges.
#'
#' @param x `matrix` with numeric values, or a `list` of `numeric`
#'    vectors. In either case the data is converted to long-tall
#'    format before plotting.
#' @param xScale `character` string indicating whether to transform
#'    the x-axis values:
#'    * `"none"`: no transformation
#'    * `"-log10"`: values are transformed with `log10(x)` and x-axis
#'    labels are adjusted accordingly.
#'    * `"log10"`: values are transformed with `log10(1 + x)` except
#'    that negative values are transformed with `-log10(1 - x)`. The
#'    x-axis labels are plotted to account for the `log10(1 + x)` offset.
#' @param xlab,ylab `character` strings optionally used as x-axis and y-axis
#'    labels.
#' @param title,subtitle,caption `character` string values optionally passed
#'    to the relevant downstream `ggplot2` functions.
#' @param xlim passed to `ggplot2::xlim()` to define the x-axis range.
#' @param color_sub `character` vector named by `colnames(x)`, or when
#'    `x` is a `list`, `names(color_sub)` should contain `names(x)`, used
#'    to define specific colors for each ridge plot.
#' @param rel_min_height `numeric` values passed to
#'    `ggridges::geom_density_ridges2()`
#' @param bandwidth `numeric` value used to define the bandwidth density
#'    when `share_bandwidth=TRUE` which is default. The bandwidth
#'    affects the level of detail presented in each ridgeline, and when
#'    shared across ridgelines `share_bandwidth=TRUE` then each ridgeline
#'    will use the same consistent level of detail. In this case, it
#'    is passed to `ggridges::geom_density_ridges2()`.
#'    Note when `bandwidth=NULL` a default value is derived from the
#'    range of data to be plotted.
#' @param adjust `numeric` used to adjust the default bandwidth only
#'    when `bandwidth=NULL`. It is intended as a convenient method to
#'    adjust the level of detail.
#' @param scale `numeric` passed directly to
#'    `ggridges::geom_density_ridges2()`.
#' @param share_bandwidth `logical` indicating whether to supply
#'    `ggridges::geom_density_ridges2()` a specific `bandwidth` to use
#'    for all ridgelines. When `share_bandwidth=FALSE` then each ridgeline
#'    is presented using the default bandwidth in
#'    `ggridges::geom_density_ridges2()`.
#' @param ... additional arguments are ignored.
#'
#' @examplesIf (requireNamespace("ggridges", quietly=TRUE))
#' # multiple columns
#' set.seed(123);
#' xm <- do.call(cbind, lapply(1:4, function(i){stats::rnorm(2000)}))
#' plotRidges(xm)
#'
#' set.seed(123);
#' x <- stats::rnorm(2000)
#' plotRidges(x)
#'
#' @export
plotRidges <- function
(x,
 xScale=c("none", "-log10", "log10"),
 xlab=NULL,
 ylab=NULL,
 title=ggplot2::waiver(),
 subtitle=ggplot2::waiver(),
 caption=ggplot2::waiver(),
 xlim=NULL,
 color_sub=NULL,
 rel_min_height=0.0,
 bandwidth=NULL,
 adjust=1,
 scale=1,
 share_bandwidth=TRUE,
 ...)
{
   if (!check_pkg_installed("ggplot2") || !check_pkg_installed("ggridges")) {
      stop("Note this function requires both packages: ggplot2 and ggridges");
   }
   xScale <- match.arg(xScale);

   if (!jamba::check_pkg_installed("ggridges")) {
      stop("The ggridges package is required.");
   }

   # convert list to tall ggplot2 format
   if (is.list(x)) {
      if (length(names(x)) == 0) {
         names(x) <- as.character(seq_along(x));
      }
      xtall <- lapply(names(x), function(i){
         xi <- x[[i]];
         if (!is.matrix(xi) && is.atomic(xi)) {
            xi <- matrix(ncol=1, xi);
            colnames(xi) <- i;
            rownames(xi) <- paste0("row",
               padInteger(seq_len(nrow(xi))));
         }
         data.frame(
            check.names=FALSE,
            stringsAsFactors=FALSE,
            row=rownames(xi),
            column=rep(i, nrow(xi)),
            value=xi[,1]
         );
      });
      x <- rbindList(xtall);
      if (length(rownames(x)) == 0) {
         rownames(x) <- seq_len(nrow(x));
      }
   } else {
      # convert vector to matrix
      if (!is.matrix(x) && is.atomic(x)) {
         xname <- deparse(substitute(x));
         x <- matrix(ncol=1, x);
         colnames(x) <- xname;
         rownames(x) <- paste0("row",
            padInteger(seq_len(nrow(x))));
      }

      if (length(rownames(x)) == 0) {
         rownames(x) <- seq_len(nrow(x));
      }
      if (length(colnames(x)) == 0) {
         colnames(x) <- seq_len(ncol(x));
      }
      # convert matrix to tall ggplot2 format
      if (is.matrix(x)) {
         x <- data.frame(
            check.names=FALSE,
            stringsAsFactors=FALSE,
            row=rep(rownames(x), ncol(x)),
            column=rep(colnames(x), each=nrow(x)),
            value=as.vector(x))
      }
   }
   if (!is.factor(x$column)) {
      x$column <- factor(x$column,
         levels=rev(unique(x$column)));
   }

   if (length(color_sub) < length(levels(x$column))) {
      n <- length(levels(x$column));
      # if (requireNamespace("colorjam", quietly=TRUE)) {
      #    color_sub <- nameVector(
      #       colorjam::rainbowJam(n=n),
      #       rev(levels(x$column)));
      # } else {
         color_sub <- nameVector(
            unalpha(rainbow2(n=n)),
            rev(levels(x$column)));
      # }
   } else if (length(names(color_sub)) == 0) {
      color_sub <- rep(color_sub,
         length.out=length(levels(x$column)));
      names(color_sub) <- rev(levels(x$column));
   }

   if ("log10" %in% xScale) {
      x$value <- log10(1 + x$value);
   } else if ("-log10" %in% xScale) {
      x$value <- -log10(x$value);
   }

   if (length(bandwidth) == 0) {
      bandwidth <- diff(range(x$value, na.rm=TRUE)) / 100 / 1.5 * adjust;
   }

   ###########################################
   # prepare ggplot output
   column <- NULL;
   value <- NULL;
   `..density..` <- NULL;

   gg <- ggplot2::ggplot(x,
      ggplot2::aes(x=value,
         y=column,
         color=column,
         fill=column)) +
      ggplot2::scale_color_manual(
         values=jamba::makeColorDarker(color_sub,
            darkFactor=1.5)) +
      ggplot2::scale_fill_manual(values=color_sub);
   # if (requireNamespace("colorjam", quietly=TRUE)) {
   #    gg <- gg +
   #       colorjam::theme_jam();
   # }
   if (share_bandwidth) {
      gg <- gg +
         ggridges::geom_density_ridges2(
            rel_min_height=rel_min_height,
            scale=scale,
            bandwidth=bandwidth,
            show.legend=FALSE
         )
   } else {
      `..density..` <- NULL;
      gg <- gg +
         ggridges::geom_density_ridges2(
            rel_min_height=rel_min_height,
            #bandwidth=bandwidth,
            stat="density",
            scale=scale,
            show.legend=FALSE,
            ggplot2::aes(height=..density..)
         )
   }

   if (length(xlab) == 1) {
      gg <- gg +
         ggplot2::xlab(label=xlab);
   }
   if (length(ylab) == 1) {
      gg <- gg +
         ggplot2::ylab(label=ylab);
   }

   gg <- gg +
      ggplot2::labs(
         title=title,
         subtitle=subtitle,
         caption=caption);

   if ("-log10" %in% xScale) {
      x_breaks <- pretty(range(x$value, na.rm=TRUE));
      x_labels <- sapply(x_breaks, function(i){
         if (i == 0) {
            1
         } else {
            eval(parse(text=paste0(
               "expression(10 ^ ", -i, ")")
            ))
         }
      })
      gg <- gg +
         ggplot2::scale_x_continuous(
            #name="padj",
            name=xlab,
            breaks=x_breaks,
            labels=x_labels)
   } else if ("log10" %in% xScale) {
      x_range <- range(x$value, na.rm=TRUE);
      x_breaks <- unique(round(
         pretty(x_range, n=20)));
      x_values <- sort(unique(c(0, 10^x_breaks)));
      x_minor_values <- sort(unique(unlist(
         lapply(x_values, function(k){
            k * c(1:9)
         }))));
      if (max(x_values) >= 10) {
         x_values <- setdiff(x_values, 1);
      }
      x_minor_values <- setdiff(x_minor_values,
         x_values);
      #x_values <- sort(unique(c(1:9, x_values)));
      x_minor_breaks_log10p <- log10(1 + x_minor_values);
      x_breaks_log10p <- log10(x_values + 1);
      x_labels <- sapply(x_values, function(i){
         if (i == 1) {
            "1"
         } else if (i == 0) {
            "0"
         } else {
            if (log10(i) >= 7) {
               j <- log10(i);
               eval(parse(text=paste0(
                  "expression(10 ^ ", j, ")")
               ))
            } else {
               jamba::formatInt(i)
            }
         }
      })
      gg <- gg +
         ggplot2::scale_x_continuous(
            name=xlab,
            limits=x_range,
            expand=c(0.01, 0.01),
            breaks=x_breaks_log10p,
            labels=x_labels,
            minor_breaks=x_minor_breaks_log10p)
   }

   if (length(xlim) > 0) {
      gg <- gg +
         ggplot2::coord_cartesian(xlim=xlim,
            expand=FALSE);
   }

   gg
}

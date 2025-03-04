
#' Display color raster image using a matrix of colors
#'
#' Display color raster image using a matrix of colors
#'
#' This function is similar to \code{\link[graphics]{image}} except that
#' it takes a matrix which already has colors defined for each cell.
#' This function calls \code{\link{imageDefault}} which enables updated
#' use of the \code{useRaster} functionality.
#'
#' Additionally, if \code{cellnote} is supplied, which contains a matrix
#' of labels for the image cells, those labels will also be displayed.
#' By default, labels are grouped, so that only one label is displayed
#' whenever two or more labels appear in consecutive cells. This behavior
#' can be disabled with groupCellnotes=FALSE.
#'
#' The `groupCellnotes` behavior uses `breaksByVector()` to
#' determine where to place consecutive labels, and it applies this logic
#' starting with rows, then columns. Note that labels are only grouped when
#' both the cell color and the cell label are identical for consecutive
#' cells.
#'
#' In general, if a large rectangular set of cells contains the same label,
#' and cell colors, the resulting label will be positioned in the
#' center. However, when the square is not symmetric, the label will be
#' grouped only where consecutive columns contain the same groups of
#' consecutive rows for a given label.
#'
#' It is helpful to rotate labels partially to prevent overlaps, e.g.
#' srtCellnote=10 or srtCellnote=80.
#'
#' To do:
#' \itemize{
#'    \item{Detect the size of the area being labeled and determine whether
#'       to rotate the label sideways.}
#'    \item{Detect the size of the label, compared to its bounding box,
#'       and resize the label to fit the available space.}
#'    \item{Optionally draw border around contiguous colored and labeled
#'       polygons. Whether to draw border based only upon color, or color
#'       and label, or just label... it may get confusing.}
#'    \item{Label proper contiguous polygons based upon color and label,
#'       especially when color and label are present on multiple rows and
#'       columns, but not always the same columns per row.}
#' }
#'
#' @family jam plot functions
#'
#' @returns `list` invisibly, with elements sufficient to create an
#'    image plot. This function is called for the byproduct of creating
#'    an image visualization.
#'
#' @param x `matrix` or `data.frame` containing colors
#' @param useRaster `logical` sent to \code{\link{imageDefault}} to enable
#'    raster rendering, as opposed to polygon rendering. This parameter is
#'    highly recommended when the matrix is large (>50 columns or rows).
#' @param fixRasterRatio `logical` sent to \code{\link{imageDefault}}.
#' @param maxRatioFix `numeric` sent to \code{\link{imageDefault}}.
#' @param xaxt,yaxt `character` values compatible with \code{\link[graphics]{par}} to
#'    determine whether x- and y-axes are plotted. Set both to "n" to
#'    suppress display of axes.
#' @param doPlot `logical` whether to create a plot, or simply return data which
#'    would have been used to create the plot.
#' @param cellnote `matrix` or `data.frame` of labels to be displayed on the
#'    image. If groupCellnotes==TRUE labels will be placed in the center
#'    of consecutive cells with the same label and identical color.
#'    Currently, cell text is colored using \code{\link{setTextContrastColor}}
#'    which uses either white or black depending upon the brightness of
#'    the background color.
#' @param cexCellnote,srtCellnote,fontCellnote `numeric` vectors, with values
#'    applied to cellnote text to be compatible with `graphics::par("cex")`,
#'    `graphics::par("srt")`,
#'    and `graphics::par("font")`, respectively.
#'    If supplied a matrix or data.frame with
#'    it is used as-is or expanded to equivalent dimensions of `x`.
#'    If the vector is named by colnames(x) then it is applied
#'    by column in order, otherwise it is applied by row, with values recycled
#'    to the number of columns or rows, respectively. Note `cexCellnote`
#'    can also be a list, with the list elements being applied to individual
#'    cells in order. If the list is named by colnames(x), each list element
#'    is applied to values in each column, in order. In future this parameter
#'    may also accept a matrix of cex values as input. Final note: values are
#'    applied to each cell, but when cell labels are combined with
#'    groupCellnotes==TRUE, the value for the first matching cell is used.
#'    Remember that values are placed by coordinate, bottom-to-top on the
#'    y-axis, and left-to-right on the x-axis.
#' @param groupCellnotes `logical` whether to group labels where consecutive
#'    cells contain the same label and identical cell colors, thus only
#'    displaying one label in the center of these groups.
#' @param groupBy `character` value indicating the direction to group
#'    cellnotes, when `groupCellnotes=TRUE`: `"row"` will group cellnote
#'    values by row; `"column"` will group cellnote values by column.
#'    By default, it will first group cellnotes by `"row"` then
#'    by `"column"`.
#' @param groupByColors `logical` indicating whether the cellnote grouping
#'    should also include the cell color. When `groupByColors=FALSE`,
#'    cellnote values will be grouped together regardless whether the
#'    underlying colors change, which may be preferred when applying
#'    text label to topographical data.
#' @param adjBy `character` value indicating how to apply adjustments for
#'    cexCellnote, srtCellnote, and fontCellnote, as described above.
#' @param adjustMargins `logical` indicating whether to adjust the axis
#'    label margins to ensure enough room to draw the text rownames
#'    and colnames.
#' @param interpolate `logical` whether to implement image interpolation,
#'    by default TRUE when useRaster=TRUE.
#' @param verbose `logical` whether to print verbose output.
#' @param xpd NULL or `logical` used for `graphics::par("xpd")`
#'    whether to crop displayed output to the plot area.
#'    * If xpd=NULL then `graphics::par("xpd")`
#'    will not be modified, otherwise `graphics::par("xpd"=xpd)`
#'    will be defined while adding any cell notes,
#'    then reverted to its previous value afterward.
#'    This parameter is mainly useful when cellnote labels may overhang the
#'    plot space, and would be cropped and not visible if
#'    `graphics::par("xpd"=TRUE)`.
#' @param bty `character` used to control box type, default
#'    `graphics::par("bty")`
#' @param flip `character` string, default "none", with optional axis flip:
#'    * none: perform no axis flip
#'    * x: flip x-axis orientation
#'    * y: flip y-axis orientation
#'    * xy: flip both x- and y-axis orientation
#' @param doTest `logical` whether to run a test showing basic features.
#' @param keepTextAlpha `logical` defaulit FALSE,
#'    passed to `setTextContrastColor()`, whether the text label color
#'    should inherit the alpha transparency from the background color.
#'    If TRUE then fully transparent background colors will not have
#'    a visible label.
#' @param add `logical`, default FALSE, whether to add to an existing
#'    device, otherwise it creates a new plot.
#' @param ... Additional arguments are ignored.
#'
#' @examples
#' a1 <- c("red4","blue")[c(1,1,2)];
#' b1 <- c("yellow","orange")[c(1,2,2)];
#' c1 <- c("purple","orange")[c(1,2,2)];
#' d1 <- c("purple","green4")[c(1,2,2)];
#' df1 <- data.frame(a=a1, b=b1, c=c1, d=d1);
#'
#' # default using polygons
#' imageByColors(df1, cellnote=df1);
#'
#' # using useRaster, edges are slightly blurred with small tables
#' imageByColors(df1, cellnote=df1, useRaster=TRUE);
#'
#' # some text features, rotation, font size, etc
#' imageByColors(df1, cellnote=df1, useRaster=TRUE, adjBy="column",
#'    cexCellnote=list(c(1.5,1.5,1), c(1,1.5), c(1.6,1.2), c(1.6,1.5)),
#'    srtCellnote=list(c(90,0,0), c(0,45), c(0,0,0), c(0,90,0)));
#' @export
imageByColors <- function
(x,
 useRaster=FALSE,
 fixRasterRatio=TRUE,
 maxRatioFix=100,
 xaxt="s",
 yaxt="s",
 doPlot=TRUE,
 cellnote=NULL,
 cexCellnote=1,
 srtCellnote=0,
 fontCellnote=1,
 groupCellnotes=TRUE,
 groupBy=c("column", "row"),
 groupByColors=TRUE,
 adjBy=c("column","row"),
 adjustMargins=FALSE,
 interpolate=getOption("interpolate", TRUE),
 verbose=FALSE,
 xpd=NULL,
 bty=graphics::par("bty"),
 flip=c("none","y","x","xy"),
 keepTextAlpha=FALSE,
 doTest=FALSE,
 add=FALSE,
 ...)
{
   ## Purpose is to take as input a matrix with color names
   ## and convert it for use by the image() function, which otherwise
   ## expects numerical values and an associated color ramp.
   ##
   ## if cellnote is provided, the text will be centered in each cell
   ##
   ## groupCellnotes=TRUE will label only one entry per group, if a
   ## series of cells have the same label.  Currently only supported for
   ## columns, not rows
   ##
   ## if xpd=NULL then graphics::par("xpd") will not be modified,
   ## otherwise graphics::par("xpd"=xpd) will be defined while
   ## adding any cell notes, then reverted to its previous value afterward.
   ## if xpd=FALSE then graphics::par("xpd") will be modified to graphics::par("xpd"=FALSE) while
   ## adding any cell notes, then reverted to its previous value afterward.
   ## The intent
   ##
   ## adjBy allows adjusting the cellnote using srtCellnote, cexCellnote, fontCellnote
   ## either by row or by column, helpful when using the colors beside a heatmap.
   adjBy <- match.arg(adjBy);
   flip <- match.arg(flip);

   ## Optionally run a test demonstrating imageByColors()
   if (doTest) {
      a1 <- c("red4","blue")[c(1,1,2)];
      b1 <- c("yellow","orange")[c(1,2,2)];
      c1 <- c("purple","orange")[c(1,2,2)];
      d1 <- c("purple","green4")[c(1,2,2)];
      df1 <- data.frame(a=a1, b=b1, c=c1, d=d1);
      rownames(df1) <- 1:3;
      imageByColors(df1, cellnote=df1, adjBy="column", doRaster=TRUE,
         srtCellnote=list(c(90,0,0), c(0,0), c(0,0), c(0,90)),
         cexCellnote=list(c(1.5,1.5,1), c(1,1.5), c(1.6,1.2), c(1.6,1.5)),
         fontCellnote=list(c(1,1,2), c(1,2), c(1,2), c(2,2)),
         maxRatioFix=maxRatioFix,
         verbose=verbose, ...);
      return(invisible(list(
         x=df1,
         cexCellnote=cexCellnote,
         srtCellnote=srtCellnote,
         fontCellnote=fontCellnote
      )));
   }

   ##
   cellnoteX <- NULL;
   srtCellnoteDF <- NULL;
   if ("data.frame" %in% class(x)) {
      x <- as.matrix(x);
   }
   xNcolSeq <- 1:ncol(x);
   xNrowSeq <- 1:nrow(x);

   ## Fix blank colors, sent in various forms, all of which we convert to
   ## "transparent"
   blankMatch <- c(NA, "NA", "...", "", "blank", "empty", "-");
   if (any(x %in% blankMatch)) {
      if (verbose) {
         printDebug("Blank values changed to transparent:",
            formatInt(sum(x %in% blankMatch)));
      }
      x[x %in% c(NA, "NA", "...", "", "blank", "empty", "-")] <- "transparent";
   }

   xFac <- as.factor(x);
   xFacM <- matrix(data=as.numeric(xFac),
      ncol=ncol(x),
      dimnames=dimnames(x));
   if (doPlot) {
      if (adjustMargins && (!xaxt %in% "n" || !yaxt %in% "n")) {
         if (!xaxt %in% "n") {
            withr::local_par(
               adjustAxisLabelMargins(x=colnames(x),
                  margin=1,
                  ...));
         }
         if (!yaxt %in% "n") {
            withr::local_par(
               adjustAxisLabelMargins(x=rownames(x),
                  margin=2,
                  ...));
         }
      }
      imageDefault(x=xNcolSeq,
         y=xNrowSeq,
         z=t(xFacM),
         col=levels(xFac),
         xaxt="n",
         yaxt="n",
         oldstyle=TRUE,
         useRaster=useRaster,
         xlab="",
         ylab="",
         axes=FALSE,
         flip=flip,
         fixRasterRatio=fixRasterRatio,
         maxRatioFix=maxRatioFix,
         bty=bty,
         interpolate=interpolate,
         verbose=verbose,
         ...);
   }
   ## Optionally add labels to the cells
   if (!is.null(cellnote)) {
      if ("data.frame" %in% class(cellnote)) {
         cellnote <- as.matrix(cellnote);
      } else if (is.atomic(cellnote)) {
         cellnote <- matrix(data=cellnote,
            ncol=ncol(x),
            nrow=nrow(x),
            dimnames=dimnames(x));
      }
      cellnoteY <- matrix(ncol=ncol(cellnote), nrow=nrow(cellnote),
         dimnames=dimnames(cellnote), rep(1:nrow(cellnote), ncol(cellnote)));
      cellnoteX <- matrix(ncol=ncol(cellnote), nrow=nrow(cellnote),
         dimnames=dimnames(cellnote), rep(1:ncol(cellnote), each=nrow(cellnote)));
      cellnote1 <- cellnote;
      xrow <- 1;
      xcol <- 1;
      xrowfac <- 1;
      xcolfac <- 1;
      if (verbose) {
         printDebug("dim(cellnote):", dim(cellnote));
      }
      ## apply cellnote grouping
      if (groupCellnotes) {
         if (nrow(x) > 1) {
            xrow <- rep(1:nrow(cellnote), each=2);
            xrowfac <- 2;
         }
         if (ncol(x) > 1) {
            xcol <- rep(1:ncol(cellnote), each=2);
            xcolfac <- 2;
         }
         if (verbose) {
            printDebug("   xrow:", xrow);
            printDebug("   xcol:", xcol);
         }
         cellnote <- cellnote[xrow,xcol,drop=FALSE];
         cellnote_fac <- cellnote;
         if (groupByColors) {
            cellnote_fac[] <- paste(cellnote, xFacM[xrow,xcol,drop=FALSE]);
         }
         cellnote_fac_n <- unique(as.vector(cellnote_fac));
         cellnote_fac_v <- nameVector(
            as.vector(cellnote)[match(cellnote_fac_n, cellnote_fac)],
            cellnote_fac_n);
         x1 <- x[xrow,xcol,drop=FALSE];
         if (nrow(cellnote) > 1) {
            cellnoteL <- apply(cellnote_fac, 2, function(i){
               i1 <- rmNA(i, naValue="")
               cellnoteXi <- 1:nrow(cellnote);
               if (!"column" %in% groupBy) {
                  i1seq <- seq_len(nrow(cellnote)/2) * 2 - 1;
                  i1 <- rep(i1seq, each=2);
                  bbv1 <- breaksByVector(i1, returnFractions=TRUE);
                  cellnoteXi[trunc(bbv1$labelPoints)] <- bbv1$labelPoints;
                  cellnoteVi <- rmNA(naValue="", i[as.numeric(bbv1$newLabels)]);
               } else if (length(unique(i1)) == 1) {
                  cellnoteXi[trunc(nrow(cellnote)/2+0.5)] <- nrow(cellnote)/2+0.5;
                  cellnoteVi <- rep("",nrow(cellnote));
                  cellnoteVi[trunc(nrow(cellnote)/2+0.5)] <- unique(i1);
               } else {
                  bbv1 <- breaksByVector(i1, returnFractions=TRUE);
                  cellnoteXi[trunc(bbv1$labelPoints)] <- bbv1$labelPoints;
                  cellnoteVi <- bbv1$newLabels;
               }
               list(cellnoteXi=cellnoteXi, cellnoteVi=cellnoteVi);
            });
            cellnoteY <- do.call(cbind, lapply(cellnoteL, function(i){
               i$cellnoteXi;
            }));
            cellnote_fac <- do.call(cbind, lapply(cellnoteL, function(i){
               i$cellnoteVi;
            }));
         } else {
            cellnoteY <- matrix(ncol=ncol(cellnote), nrow=nrow(cellnote),
               dimnames=dimnames(cellnote), rep(1:nrow(cellnote), ncol(cellnote)));
         }
         if (ncol(cellnote) > 1) {
            cellnoteL <- apply(cellnote_fac, 1, function(i){
               i1 <- rmNA(i, naValue="")
               cellnoteXi <- 1:ncol(cellnote);
               if (!"row" %in% groupBy) {
                  i1seq <- seq_len(ncol(cellnote)/2) * 2 - 1;
                  i1 <- rep(i1seq, each=2);
                  bbv1 <- breaksByVector(i1, returnFractions=TRUE);
                  cellnoteXi[trunc(bbv1$labelPoints)] <- bbv1$labelPoints;
                  cellnoteVi <- rmNA(naValue="", i[as.numeric(bbv1$newLabels)]);
               } else if (length(unique(i1)) == 1) {
                  cellnoteXi[trunc(ncol(cellnote)/2+0.5)] <- ncol(cellnote)/2+0.5;
                  cellnoteVi <- rep("",ncol(cellnote));
                  cellnoteVi[trunc(ncol(cellnote)/2+0.5)] <- unique(i1);
               } else {
                  bbv1 <- breaksByVector(i1, returnFractions=TRUE);
                  cellnoteXi[trunc(bbv1$labelPoints)] <- bbv1$labelPoints;
                  cellnoteVi <- bbv1$newLabels;
               }
               list(cellnoteXi=cellnoteXi, cellnoteVi=cellnoteVi);
            });
            cellnoteX <- rbindList(lapply(cellnoteL, function(i){
               i$cellnoteXi;
            }));
            cellnote_fac <- rbindList(lapply(cellnoteL, function(i){
               i$cellnoteVi;
            }));
         } else {
            cellnoteX <- matrix(ncol=ncol(cellnote), nrow=nrow(cellnote),
               dimnames=dimnames(cellnote),
               rep(1:ncol(cellnote), each=nrow(cellnote)));
            if (verbose) {
               printDebug("cellnoteX:");
               print(head(cellnoteX));
            }
         }
         cellnote[] <- cellnote_fac_v[as.vector(cellnote_fac)];
      } else {
         x1 <- x;
      }
      cellWhich <- which(!cellnote %in% c(NA, ""));
      if (xcolfac > 1) {
         cellX <- (as.vector(cellnoteX)[cellWhich]+0.5)/xcolfac;
      } else {
         cellX <- as.vector(cellnoteX)[cellWhich];
      }
      if (verbose) {
         printDebug("xcolfac:", xcolfac,
            ",\ncellX:", cellX,
            ",\nas.vector(cellnoteX):", as.vector(cellnoteX),
            ",\ncellWhich:", cellWhich,
            ",\nclass(cellnote):", class(cellnote));
         print(cellnote);
      }
      if (xrowfac > 1) {
         cellY <- (as.vector(cellnoteY)[cellWhich]+0.5)/xrowfac;
      } else {
         cellY <- as.vector(cellnoteY)[cellWhich];
      }
      celltext <- as.vector(cellnote)[cellWhich];
      cellColor <- as.vector(x1)[cellWhich];

      prepMatrixParam <- function(x, param, ...) {
         ## Purpose is to take a vector or list as input, and
         ## return a vector sufficient to cover the matrix x.
         ##
         ## If given a vector, with length=ncol(x)*nrow(x),
         ## its values are returned as-is, to be applied to the matrix
         ## by column.
         ## Otherwise if given a vector, its values are extended to ncol(x)
         ## then repeated within each column.
         ## If given a list, the list is repeated to ncol(x), then
         ## each list element is repeated to nrow(x) as needed.
         ##
         if (igrepHas("matrix|data.frame", class(param))) {
            paramX <- rep(seq_len(ncol(param)), length.out=ncol(x));
            paramY <- rep(seq_len(nrow(param)), length.out=nrow(x));
            param <- param[paramY,paramX,drop=FALSE];
            dimnames(param) <- dimnames(x);
            return(param);
         }
         if (length(param) != prod(ncol(x), nrow(x))) {
            param <- rep(param, length.out=ncol(x));
            if (!igrepHas("list", class(param))) {
               param <- as.list(param);
            }
            if (!all(lengths(param) == nrow(x))) {
               param <- lapply(param, function(i){
                  rep(i, length.out=nrow(x));
               });
            }
         }
         param <- matrix(ncol=ncol(x), nrow=nrow(x), unlist(param),
            dimnames=dimnames(x));
         return(param);
      }
      if (!is.null(names(cexCellnote)) &&
            all(names(cexCellnote) %in% colnames(x))) {
         adjBy <- "column";
      } else if (!is.null(names(cexCellnote)) &&
            all(names(cexCellnote) %in% rownames(x))) {
         adjBy <- "row";
      }

      if (adjBy %in% "column") {
         cexCellnote <- prepMatrixParam(x, cexCellnote);
         fontCellnote <- prepMatrixParam(x, fontCellnote);
         srtCellnote <- prepMatrixParam(x, srtCellnote);
      } else {
         cexCellnote <- t(prepMatrixParam(t(x), cexCellnote));
         fontCellnote <- t(prepMatrixParam(t(x), fontCellnote));
         srtCellnote <- t(prepMatrixParam(t(x), srtCellnote));
      }
      if (verbose) {
         printDebug("head(cexCellnote):");
         print(head(cexCellnote, 10));
      }

      ## paramCell is the cell for which the parameter is applied, by column,
      ## then by row.
      ## Note that for labels spanning two rows or columns, it simply takes
      ## the first parameter.
      paramCell <- floor(cellX)*nrow(x)-(nrow(x)) + floor(cellY);
      if (verbose) {
         printDebug("head(cellX):");print(head(cellX));
         printDebug("head(paramCell):");print(head(paramCell));
      }
      srtCellnoteDF <- data.frame(cellX=cellX,
         cellY=cellY,
         celltext=celltext,
         cexCellnote=cexCellnote[paramCell],
         srtCellnote=srtCellnote[paramCell],
         fontCellnote=fontCellnote[paramCell],
         paramCell=paramCell,
         cellColor=cellColor);
      if (verbose) {
         printDebug("srtCellnoteDF:");
         print(head(srtCellnoteDF));
      }
      if (!is.null(xpd)) {
         withr::local_par("xpd"=xpd);
      }
      ## srt can be set only once per graphics::text() call, so we must loop through
      ## each srtCellnote value
      ## Text can only be customized once per graphics::text() call, so for every combination of
      ## srt, font, and cex, we must run this function again
      srtCellnoteDF[,"textKey"] <- pasteByRow(srtCellnoteDF[,c("cexCellnote",
         "srtCellnote", "fontCellnote")], sep="_");
      if (doPlot) {
         text1 <- tapply(seq_len(nrow(srtCellnoteDF)),
            srtCellnoteDF[,"textKey"], function(iRow){
               if (verbose) {
                  printDebug("textKey:", head(srtCellnoteDF[iRow,"textKey"], 1),
                     ", iRow:", head(iRow));
               }
               graphics::text(x=srtCellnoteDF[iRow,"cellX"],
                  y=srtCellnoteDF[iRow,"cellY"],
                  labels=srtCellnoteDF[iRow,"celltext"],
                  pos=NULL,
                  cex=head(srtCellnoteDF[iRow,"cexCellnote"],1),
                  srt=head(srtCellnoteDF[iRow,"srtCellnote"],1),
                  font=head(srtCellnoteDF[iRow,"fontCellnote"],1),
                  col=setTextContrastColor(srtCellnoteDF[iRow,"cellColor"],
                     keepAlpha=keepTextAlpha,
                     useGrey=18,
                     ...),
                  adj=c(0.5,0.5));
            });
      }
   }
   ## Print column and row labels if defined
   if (doPlot) {
      if (!xaxt %in% "n" && !is.null(colnames(x))) {
         graphics::axis(1,
            las=2,
            at=1:ncol(x),
            labels=colnames(x),
            ...);
      }
      if (!yaxt %in% "n" && !is.null(rownames(x))) {
         graphics::axis(2,
            las=2,
            at=1:nrow(x),
            labels=rownames(x),
            ...);
      }
      graphics::box(bty=bty,
         ...);
   }
   invisible(list(x=xNcolSeq,
      y=xNrowSeq,
      z=t(xFacM),
      col=levels(xFac),
      cellnoteX=cellnoteX,
      srtCellnoteDF=srtCellnoteDF,
      cexCellnote=cexCellnote,
      mar=graphics::par("mar")));
}


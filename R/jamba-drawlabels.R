#' Draw text labels on a base R plot
#'
#' Draw text labels on a base R plot
#'
#' This function takes a vector of coordinates and text labels,
#' and draws the labels with colored rectangles around each label
#' on the plot. Each label can have unique font, cex, and color,
#' and are drawn using vectorized operations.
#'
#' To enable shadow text include argument: `text_fn=jamba::shadowText`
#'
#' TODO: In future allow rotated text labels. Not that useful within
#' a plot panel, but sometimes useful when draw outside a plot, for
#' example axis labels.
#'
#' @returns invisible data.frame containing label coordinates used
#' to draw labels. This data.frame can be manipulated and provided
#' as input to `drawLabels()` for subsequent customized label
#' positioning.
#'
#' @family jam plot functions
#'
#' @param newCoords `data.frame` optional, typically as a result of
#'    a previous call to `drawLabels()`. In general, it should contain
#'    colnames equivalent to the function parameters of `drawLabels()`.
#' @param x,y `numeric` vector of x- and y- coordinates.
#' @param txt `character` vector of labels, length equal to `x` and `y`.
#' @param lx,ly `numeric` optional vector of segment endpoint coordinates, used
#'    to draw a line from x,y coordinates to the segment lx,ly coordinate.
#' @param segmentLwd,segmentCol `numeric` vector of segment line widths,
#'    and `character` colors, respectively.
#'    Each vector will be recycled to `length(txt)` as needed.
#' @param drawSegments `logical` whether to draw segments, where applicable.
#' @param boxBorderColor `character` vector of colors used for the
#'    box border around each label.
#' @param boxColor `character` vector of colors used for the box background
#'    behind each label.
#' @param boxLwd `numeric` vector of box line widths, sent to
#'    `graphics::rect()`, this vector will be recycled to `length(txt)`.
#' @param drawBox `logical` whether to draw boxes behind each text label.
#' @param drawLabels `logical` whether to draw each text label.
#' @param font `integer` vector of font values as described in
#'    `graphics::par()`, where 1=normal, 2=bold, 3=italics, 4=bold-italics.
#' @param labelCex `numeric` vector of cex values used for text labels,
#'    recycled to `length(txt)` as needed.
#' @param boxCexAdjust `numeric` vector length=2, used to expand the x-width
#'    and y-height of the box around around text labels.
#' @param labelCol `character` vector of label colors, by default it calls
#'    `jamba::setTextContrastColor()` to generate a color to contrast
#'    the background box color.
#' @param doPlot `logical` whether to perform any plot operations. Set
#'    `FALSE` to calculate coordinates and return a `data.frame` of
#'    label coordinates, which can then be manipulated before calling
#'    `drawLabels()` again.
#' @param xpd `logical` value compatible with `graphics::par("xpd")`, where NA allows labels
#'    anywhere in the device region, TRUE retricts labels within the figure
#'    region, and FALSE restricts labels within the plot region.
#' @param preset `character` vector passed to `coordPresets()`
#'    used to position text labels relative
#'    to the x,y coordinate, where "topleft" will position the label so the
#'    entire label box is top-left of the point, therefore the point will be
#'    at the bottom-right corner of the label box. When `preset` is anything
#'    by `"none"` the `adjX` and `adjY` values are ignored.
#' @param preset_type,adjPreset `character` passed to `coordPresets()` to
#'    define orientation of each label relative to the `x`,`y` coordinate.
#' @param adjX,adjY `numeric` the text adjustment of labels relative to the x,y
#'    coordinate. The values are recycled to `length(txt)`.
#' @param panelWidth `character` string or vector, recycled to the number
#'    of labels to be displayed. The argument indicates whether to size
#'    each label box relative to the plot panel width, intended when
#'    the label `preset` and `adjPreset` are set for the label to be inside
#'    the plot panel, e.g. `preset="top", adjPreset="top"`, or
#'    `preset="topleft", adjPreset="topright"`. Either both are centered,
#'    or one is "right" and the other is "left". In these cases, the label
#'    box is expanded to the full plot panel width, thus filling the full
#'    visible x-axis range for the plot panel. Allowed values for `panelWidth`:
#'    * `"default"` size label boxes by text dimensions
#'    * `"force"` size label to full plot panel width
#'    * `"minimum"` size label at least the plot panel width, or larger if
#'    necessary to fit the text label
#'    * `"maximum"` size label to the text label width, but no larger than
#'    the plot panel width
#' @param trimReturns `logical` whether to trim leading and trailing return
#'    (newline) characters from labels.
#' @param text_fn `function` used to render text, by default it checks
#'    `getOption("jam.text_fn", graphics::text)` which then defaults
#'    to `graphics::text`.
#'    * This argument is specifically to enable `jamba::shadowText()`,
#'    for example `text_fn=jamba::shadowText`.
#'    * Previous to version 0.0.107.900, one could assign
#'    `text <- jamba::shadowText` however that option was removed
#'    to make jamba more compliant with recommended R code, and
#'    ready for CRAN.
#' @param verbose `logical` whether to print verbose output.
#' @param ... additional arguments are passed to `graphics::segments()` when
#'    segments are drawn, to `graphics::rect()` when label boxes are drawn,
#'    and to `graphics::text()` when text labels are drawn.
#'
#' @examples
#' nullPlot(plotAreaTitle="");
#' dl_topleft <- drawLabels(x=graphics::par("usr")[1],
#'    y=graphics::par("usr")[4],
#'    txt="Top-left\nof plot",
#'    preset="topleft",
#'    boxColor="blue4");
#'
#' drawLabels(x=graphics::par("usr")[2],
#'    y=graphics::par("usr")[3],
#'    txt="Bottom-right\nof plot",
#'    preset="bottomright",
#'    boxColor="green4");
#'
#' drawLabels(x=mean(graphics::par("usr")[1:2]),
#'    y=mean(graphics::par("usr")[3:4]),
#'    txt="Center\nof plot",
#'    preset="center",
#'    boxColor="purple3");
#'
#' graphics::points(x=c(graphics::par("usr")[1], graphics::par("usr")[2],
#'       mean(graphics::par("usr")[1:2])),
#'    y=c(graphics::par("usr")[4], graphics::par("usr")[3],
#'       mean(graphics::par("usr")[3:4])),
#'    pch=20,
#'    col="red",
#'    xpd=NA);
#'
#' nullPlot(plotAreaTitle="");
#' graphics::title(main="place label across the full top plot panel", line=2.5)
#' dl_top <- drawLabels(
#'    txt=c("preset='topright', adjPreset='topright', \npanelWidth='force'",
#'       "preset='topright',\nadjPreset='bottomleft'",
#'       "preset='bottomleft', adjPreset='topright',\npanelWidth='force'"),
#'    preset=c("topright", "topright", "bottomleft"),
#'    adjPreset=c("topleft", "bottomleft", "topright"),
#'    panelWidth=c("force", "none", "force"),
#'    boxColor=c("red4",
#'       "blue4",
#'       "purple3"));
#' graphics::box(lwd=2);
#'
#' withr::with_par(list("mfrow"=c(1, 3), "xpd"=TRUE), {
#'
#' isub <- c(force="Always full panel width",
#'    minimum="At least full panel width or larger",
#'    maximum="No larger than panel width");
#' for (i in c("force", "minimum", "maximum")) {
#' nullPlot(plotAreaTitle="", doMargins=FALSE);
#' graphics::title(main=paste0("panelWidth='", i, "'\n",
#'    isub[i]));
#' drawLabels(labelCex=1.2,
#'    txt=c("Super-wide title across the top\npanelWidth='force'",
#'    "bottom label"),
#'    preset=c("top", "bottom"),
#'    panelWidth=i,
#'    boxColor="red4")
#' }
#' })
#'
#' @export
drawLabels <- function
(txt=NULL,
 newCoords=NULL,
 x=NULL,
 y=NULL,
 lx=NULL,
 ly=NULL,
 segmentLwd=1,
 segmentCol="#00000088",
 drawSegments=TRUE,
 boxBorderColor="#000000AA",
 boxColor="#FFEECC",
 boxLwd=1,
 drawBox=TRUE,
 drawLabels=TRUE,
 font=1,
 labelCex=0.8,
 boxCexAdjust=1.9,
 labelCol=alpha2col(alpha=0.8, setTextContrastColor(boxColor)),
 doPlot=TRUE,
 xpd=NA,
 preset="default",
 adjPreset="default",
 preset_type="plot",
 adjX=0.5,
 adjY=0.5,
 panelWidth="default",
 trimReturns=TRUE,
 text_fn=getOption("jam.text_fn", graphics::text),
 verbose=FALSE,
 ...)
{
   ## Purpose is to wrapper only the last portion of addNonOverlappingLabels()
   ## which draws the labels, boxes, and segments after positions are determined
   ## by addNonOverlappingLabels().
   if (length(boxCexAdjust) == 0) {
      boxCexAdjust <- 1;
   }
   boxCexAdjust <- rep(boxCexAdjust,
      length.out=2);
   presetValid <- c("default",
      "top",
      "topright",
      "right",
      "bottomright",
      "topleft",
      "left",
      "bottomleft",
      "bottom",
      "center");
   if (!all(preset %in% presetValid)) {
      stop(paste0("preset must be one of ",
         jamba::cPaste(presetValid)));
   }
   if (!all(adjPreset %in% presetValid)) {
      stop(paste0("adjPreset must be one of ",
         jamba::cPaste(presetValid)));
   }

   if (length(newCoords) == 0) {
      ## Create a new coordinate data.frame
      if (verbose) {
         jamba::printDebug("drawLabels(): ",
            "Creating new coordinates data.frame");
      }
      if (jamba::igrepHas("top|bottom|left|right|center",
         c(preset, adjPreset))) {
         if (verbose) {
            jamba::printDebug("drawLabels(): ",
               "Processing non-default preset and adjPreset values.");
         }
         presetL <- coordPresets(
            preset=preset,
            x=x,
            y=y,
            adjPreset=adjPreset,
            preset_type=preset_type,
            adjX=adjX,
            adjY=adjY,
            verbose=verbose);
         x <- presetL$x;
         y <- presetL$y;
         adjX <- presetL$adjX;
         adjY <- presetL$adjY;
         preset <- presetL$preset;
         adjPreset <- presetL$adjPreset;
      }
      newCoords <- data.frame(
         stringsAsFactors=FALSE,
         x=x,
         y=y,
         txt=txt,
         w=graphics::strwidth(txt,
            font=font,
            cex=labelCex),
         h=graphics::strheight(txt,
            font=font,
            cex=labelCex),
         hNudge=0,
         labelCex=labelCex,
         adjX=adjX,
         adjY=adjY,
         preset=preset,
         adjPreset=adjPreset);
      if (length(lx) > 0 && length(ly) > 0) {
         newCoords$lx <- rep(lx, length.out=nrow(newCoords));
         newCoords$ly <- rep(ly, length.out=nrow(newCoords));
      } else {
         drawSegments <- FALSE;
      }

      ## Make additional adjustments based upon leading or trailing "\n"
      if (trimReturns) {
         ## First determine the height of the buffer between two lines of text
         labelCexU <- unique(newCoords$labelCex);
         labelCexUh <- sapply(labelCexU, function(iCex){
            bufferH <- (graphics::strheight("|\n|", cex=iCex) -
                  2*(graphics::strheight("|", cex=iCex)));
         });
         #labelCexUh[match(newCoords$labelCex, labelCexU)]
         if (jamba::igrepHas("\n$", newCoords$txt)) {
            if (verbose) {
               jamba::printDebug("drawLabels(): ",
                  "adjusting adjY for trailing return characters.");
               print(newCoords);
            }
            while(jamba::igrepHas("\n$", newCoords$txt)) {
               iAdj <- jamba::igrep("\n$", newCoords$txt);
               adjY[iAdj] <- adjY[iAdj] - newCoords$adjY[iAdj];
               newCoords$txt[iAdj] <- sub("\n$", "", newCoords$txt[iAdj]);
            }
         }
         if (jamba::igrepHas("^\n", newCoords$txt)) {
            if (verbose) {
               jamba::printDebug("drawLabels(): ",
                  "adjusting adjY for leading return characters.");
            }
            while(jamba::igrepHas("^\n", newCoords$txt)) {
               iAdj <- jamba::igrep("^\n", newCoords$txt);
               adjY[iAdj] <- adjY[iAdj] + newCoords$adjY[iAdj];
               newCoords$txt[iAdj] <- sub("^\n", "", newCoords$txt[iAdj]);
            }
         }
         newCoords$adjY <- adjY;
         newCoords$h <- graphics::strheight(newCoords$txt, cex=labelCex);
      } else {
         newCoords$txt <- gsub("\n", "\n|", newCoords$txt);
      }

      ## Now widen the label box using boxCexAdjust
      if (verbose) {
         jamba::printDebug("drawLabels(): ",
            "adjusting w,h using boxCexAdjust");
         print(newCoords);
      }
      ## New strategy intended to keep the bottom-left edge fixed
      ##
      ## Use single-line height as a basis for adjustments
      numLines <- lengths(strsplit(as.character(newCoords$txt), "\n"));
      ## Assuming the buffer between lines is 1/5 the line height
      ## calculate the per-line height without the buffer
      perLineH <- 5*newCoords$h / (6*numLines-1);

      ## Change 07feb2019 to use height as scaling indicator
      newCoords$h <- newCoords$h + perLineH * (boxCexAdjust[2]-1);
      newCoords$w <- newCoords$w + perLineH/jamba::getPlotAspect() * (boxCexAdjust[1]-1) / 1;
      ##
      newCoords$x <- newCoords$x - adjX * newCoords$w;
      newCoords$y <- newCoords$y - adjY * newCoords$h;
   } else {
      ## Re-use an existing coordinates data.frame
      if (verbose) {
         jamba::printDebug("drawLabels(): ",
            "Re-using coordinates data.frame");
      }
      # update x and y if preset or adjPreset changed
      if (igrepHas("top|bottom|left|right|center", c(preset, adjPreset))) {
         if (verbose) {
            printDebug("drawLabels(): ",
               "Processing custom preset and adjPreset values.");
         }
         if (length(preset) > 0 && !"default" %in% preset) {
            newCoords$preset <- preset;
         }
         if (length(adjPreset) > 0 && !"default" %in% adjPreset) {
            newCoords$adjPreset <- adjPreset;
         }

         presetL <- coordPresets(
            x=newCoords$x,
            y=newCoords$y,
            preset=newCoords$preset,
            adjPreset=newCoords$adjPreset,
            adjX=newCoords$adjX,
            adjY=newCoords$adjY);
         if (presetL$adjX != newCoords$adjX) {
            newCoords$x <- newCoords$x - (presetL$adjX - newCoords$adjX)  * newCoords$w;
            newCoords$adjX <- presetL$adjX;
         }
         if (presetL$adjY != newCoords$adjY) {
            newCoords$y <- newCoords$y - (presetL$adjY - newCoords$adjY) * newCoords$h;
            newCoords$adjY <- presetL$adjY;
         }
         newCoords$preset <- presetL$preset;
         newCoords$adjPreset <- presetL$adjPreset;
      }
   }

   ## Add height and width if not supplied
   if (!"h" %in% names(newCoords)) {
      newCoords$h <- graphics::strheight(txt, cex=labelCex*1.1);
   }
   if (!"w" %in% names(newCoords)) {
      newCoords$w <- graphics::strwidth(txt, cex=labelCex*1.1);
   }

   ## Optional panelWidth adjustment
   if (length(panelWidth) > 0) {
      newCoords$panelWidth <- rep(panelWidth,
         length.out=nrow(newCoords));
   } else if (!"panelWidth" %in% newCoords) {
      newCoords$panelWidth <- "default";
   }
   if (any(!newCoords$panelWidth %in% "default")) {
      panel_w <- diff(graphics::par("usr")[1:2]);
      w <- newCoords$w;
      # mid_x <- newCoords$x + w * newCoords$adjX;
      newCoords$w <- ifelse(newCoords$panelWidth %in% "force",
         panel_w,
         ifelse(newCoords$panelWidth %in% "minimum",
            pmax(w, panel_w),
            ifelse(newCoords$panelWidth %in% "maximum",
               pmin(w, panel_w),
               newCoords$w)));
      new_x <- newCoords$x - newCoords$adjX * (newCoords$w - w);
      newCoords$x <- new_x;
   }

   x <- newCoords$ptX;
   y <- newCoords$ptY;
   if (!"labelCex" %in% names(newCoords)) {
      if (verbose) {
         jamba::printDebug("drawLabels(): ",
            "Defining all fixedLabels=FALSE");
      }
      newCoords[,"fixedLabels"] <- FALSE;
   }
   if (!"fixedLabels" %in% names(newCoords)) {
      newCoords[,"fixedLabels"] <- TRUE;
   }
   whichLabels <- which(!newCoords$fixedLabels > 1);
   if (verbose) {
      jamba::printDebug("drawLabels(): ",
         "head(whichLabels):",
         head(whichLabels));
   }

   segmentCol <- rep(segmentCol, length.out=nrow(newCoords));
   segmentLwd <- rep(segmentLwd, length.out=nrow(newCoords));

   if (!"labelCex" %in% names(newCoords)) {
      newCoords$labelCex <- rep(labelCex,
         length.out=nrow(newCoords));
   }
   if (!"labelCol" %in% names(newCoords)) {
      newCoords$labelCol <- rep(labelCol,
         length.out=nrow(newCoords));
   }

   if (!"boxColor" %in% names(newCoords)) {
      newCoords$boxColor <- rep(boxColor,
         length.out=nrow(newCoords));
   }
   boxColor <- newCoords$boxColor;
   if (!"boxBorderColor" %in% names(newCoords)) {
      newCoords$boxBorderColor <- rep(boxBorderColor,
         length.out=nrow(newCoords));
   }
   boxBorderColor <- newCoords$boxBorderColor;

   ## Draw segments first so the box and labels will be printed on top of them
   if (doPlot &&
         drawSegments &&
         any(whichLabels) &&
         all(c("lx","ly") %in% names(newCoords))) {
      ## Determine closest corner or flat side for each segment
      x0l <- newCoords$x[whichLabels];
      x0m <- newCoords$x[whichLabels] + newCoords$w[whichLabels]/2;
      x0r <- newCoords$x[whichLabels] + newCoords$w[whichLabels];
      y0b <- newCoords$y[whichLabels];
      y0m <- newCoords$y[whichLabels] + newCoords$h[whichLabels]/2;
      y0t <- newCoords$y[whichLabels] + newCoords$h[whichLabels];
      x1 <- newCoords$lx[whichLabels];
      y1 <- newCoords$ly[whichLabels];
      sx0 <- ifelse(x0r < x1, x0r,
         ifelse(x0l > x1, x0l,
            x1));
      sy0 <- ifelse(y0t < y1, y0t,
         ifelse(y0b > y1, y0b,
            y1));
      if (verbose) {
         headN <- 10;
         jamba::printDebug("drawLabels(): ",
            "Drawing line segments.");
         jamba::printDebug("sx0: ",
            head(sx0, headN));
         jamba::printDebug("x1: ",
            head(x1, headN));
         jamba::printDebug("sy0: ",
            head(sy0, headN));
         jamba::printDebug("y1: ",
            head(y1, headN));
         jamba::printDebug("segmentCol[whichLabels]: ",
            head(c(segmentCol[whichLabels]), headN), c("orange", "lightblue"));
         jamba::printDebug("segmentLwd[whichLabels]: ",
            head(c(segmentLwd[whichLabels]), headN), c("orange", "lightblue"));
      }
      graphics::segments(x0=sx0,
         y0=sy0,
         x1=newCoords$lx[whichLabels],
         y1=newCoords$ly[whichLabels],
         col=segmentCol[whichLabels],
         lwd=segmentLwd[whichLabels],
         xpd=xpd,
         ...);
   } else {
      if (verbose) {
         jamba::printDebug("drawLabels(): ",
            "Drawing no segments.");
      }
   }

   ## The code below calculates the text to be the exact center of each box,
   ## then forces the graphics::text() method below to use adj=c(0.5,0.5) which centers
   ## the text exactly at this coordinate. Looks much nicer than other
   ## options.
   boxX1 <- newCoords$x;
   boxX2 <- boxX1 + newCoords$w;
   boxY1 <- newCoords$y - newCoords$hNudge;
   boxY2 <- boxY1 + newCoords$h;
   textX <- (boxX1 + boxX2) / 2;
   textY <- (boxY1 + boxY2) / 2;

   ## Draw boxes before labels so the labels will be printed on top
   if (drawBox && doPlot && any(whichLabels)) {
      if (verbose) {
         jamba::printDebug("drawLabels(): ",
            "Drawing rectangles.");
         jamba::printDebug("drawLabels(): ",
            "First rectangle:",
            c(boxX1[whichLabels][1], boxX2[whichLabels][1],
               boxY1[whichLabels][1], boxY2[whichLabels][1]));
      }
      ## Only draw boxes where there are characters to be printed
      graphics::rect(xleft=boxX1[whichLabels],
         ybottom=boxY1[whichLabels],
         xright=boxX2[whichLabels],
         ytop=boxY2[whichLabels],
         col=boxColor[whichLabels],
         border=boxBorderColor[whichLabels],
         lwd=boxLwd,
         xpd=xpd,
         ...);
   }
   if (drawLabels && doPlot && any(whichLabels)) {
      if (verbose) {
         jamba::printDebug("drawLabels(): ",
            "Printing labels head(txt): ",
            paste(head(txt), collapse=", "));
      }
      text_fn(x=textX[whichLabels],
         y=textY[whichLabels],
         font=font,
         labels=newCoords$txt[whichLabels],
         col=newCoords$labelCol[whichLabels],
         cex=newCoords$labelCex[whichLabels],
         adj=c(0.5,0.5),
         xpd=xpd,
         ...);
   }
   invisible(newCoords);
}


#' Process coordinate adjustment presets
#'
#' Process coordinate adjustment presets
#'
#' This function is intended to be a convenient way to define
#' coordinates using preset terms like "topleft", "bottom", "center".
#'
#' Similarly, it is intended to help define corresponding text
#' adjustments, using `adj` compatible with `graphics::text()`,
#' using preset terms like "bottomright", "center".
#'
#' When `preset` is `"default"`, the original `x,y` coordinates
#' are used. Otherwise the `x,y` coordinates are defined using the
#' plot region coordinates, where `"left"` uses `graphics::par("usr")[1]`,
#' and `"top"` uses `graphics::par("usr")[4]`.
#'
#' When `adjPreset` is `"default"` it will use the `preset` to
#' define a reciprocal text placement. For example when `preset="topright"`
#' the text placement will be equivalent to `adjPreset="bottomleft"`.
#' The `adjPreset` terms `"top"`, `"bottom"`, `"right"`, `"left"`,
#' and `"center"` refer to the text label placement relative to
#' `x,y` coordinate.
#'
#' If both `preset="default"` and `adjPreset="default"` the original
#' `adjX,adjY` values are returned.
#'
#' The function is vectorized, and uses the longest input argument,
#' so one can supply a vector of `preset` and it will return coordinates
#' and adjustments of length equal to the input `preset` vector.
#' The `preset` value takes priority over the supplied `x,y` coordinates.
#'
#' @returns `data.frame` after adjustment, where the number of rows
#'    is determined by the longest input argument, with colnames:
#'    * x
#'    * y
#'    * adjX
#'    * adjY
#'    * preset
#'    * adjPreset
#'
#' @family jam plot functions
#'
#' @param preset `character` vector of coordinate positions, or the default
#'    "default" to use the `x,y` coordinates.
#'    * Recognized terms: center, bottom, top, left, right, topleft,
#'    topright, bottomleft, bottomright.
#' @param x,y `numeric` vectors indicating the default coordinates `x,y`.
#' @param adjPreset `character` vector of text label positions, or
#'    the default "default" to use `preset`, or when `preset="default"` the
#'    `adjX,adjY` values are used.
#'    * Recognized terms: center, bottom, top, left, right, topleft,
#'    topright, bottomleft, bottomright.
#' @param adjX,adjY numeric vectors indicating default text adjustment
#'    values, as described for `adj` in `graphics::text()`.
#' @param adjOffsetX,adjOffsetY `numeric` vector used to apply an offset
#'    value to the `adjX,adjY` values, where positive values would
#'    place a label farther away from center. Note these units are
#'    relative to the text label size, when used with `graphics::text()`,
#'    larger labels will be adjusted more than smaller labels.
#' @param preset_type `character` string indicating the reference point
#'    for the preset boundaries:
#'    * `"plot"` uses the plot border.
#'    * `"margin"` uses the margin border. Note that the margin used
#'    is the inner margin around the plot figure, not the outer margin
#'    which may be applied around multi-panel plot figures.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' # determine coordinates
#' presetV <- c("top",
#'    "bottom",
#'    "left",
#'    "right",
#'    "topleft");
#' cp1 <- coordPresets(preset=presetV);
#' cp1;
#'
#' # make sure to prepare the plot region first
#' jamba::nullPlot(plotAreaTitle="");
#' graphics::points(cp1$x, cp1$y, pch=20, cex=2, col="red");
#'
#' # unfortunately graphics::text() does not have vectorized adj
#' # so it must iterate each row
#' graphics::title(main="graphics::text() is not vectorized, text is adjacent to edges")
#' for (i in seq_along(presetV)) {
#'    graphics::text(cp1$x[i], cp1$y[i],
#'       labels=presetV[i],
#'       adj=c(cp1$adjX[i], cp1$adjY[i]));
#' }
#'
#' # drawLabels() will be vectorized for unique adj subsets
#' # and adds a small buffer around text
#' jamba::nullPlot(plotAreaTitle="");
#' graphics::title(main="drawLabels() is vectorized, includes small buffer")
#' drawLabels(txt=presetV,
#'    preset=presetV)
#'
#' jamba::nullPlot(plotAreaTitle="");
#' graphics::title(main="drawLabels() can place labels outside plot edges")
#' drawLabels(txt=presetV,
#'    preset=presetV,
#'    adjPreset=presetV)
#'
#' # drawLabels() is vectorized for example
#' jamba::nullPlot(plotAreaTitle="");
#' graphics::title(main="Use adjPreset to position labels at a center point")
#' presetV2 <- c("topleft",
#'    "topright",
#'    "bottomleft",
#'    "bottomright");
#' cp2 <- coordPresets(preset="center",
#'    adjPreset=presetV2,
#'    adjOffsetX=0.1,
#'    adjOffsetY=0.4);
#' graphics::points(cp2$x,
#'    cp2$y,
#'    pch=20,
#'    cex=2,
#'    col="red");
#' drawLabels(x=cp2$x,
#'    y=cp2$y,
#'    adjX=cp2$adjX,
#'    adjY=cp2$adjY,
#'    txt=presetV2,
#'    boxCexAdjust=c(1.15,1.6),
#'    labelCex=1.3,
#'    lx=rep(1.5, 4),
#'    ly=rep(1.5, 4))
#'
#' # demonstrate margin coordinates
#' withr::with_par(list("oma"=c(1, 1, 1, 1)), {
#' nullPlot(xlim=c(0, 1), ylim=c(1, 5));
#' cpxy <- coordPresets(rep(c("top", "bottom", "left", "right"), each=2),
#'    preset_type=rep(c("plot", "figure"), 4));
#' drawLabels(preset=c("top", "top"),
#'    txt=c("top label relative to figure",
#'       "top label relative to plot"),
#'    preset_type=c("figure", "plot"))
#' graphics::points(cpxy$x, cpxy$y, cex=2,
#'    col="red4", bg="red1", xpd=NA,
#'    pch=rep(c(21, 23), 4))
#' })
#'
#' @export
coordPresets <- function
(preset="default",
 x=0,
 y=0,
 adjPreset="default",
 adjX=0.5,
 adjY=0.5,
 adjOffsetX=0,
 adjOffsetY=0,
 preset_type=c("plot"),
 verbose=FALSE,
 ...)
{
   ## Takes vector of preset values, and x,y
   ## and re-defines x,y for anything with
   ## top, left, right, bottom, center
   ## Takes vector of adjPreset values, and adjX, adjY
   ## and re-defines adjX,adjY.
   ## When adjPreset="default" it uses opposite orientation
   ## of "preset", otherwise position is defined by adjPreset
   ## for top, bottom, left, right, center
   if (length(preset_type) == 0) {
      preset_type <- NA;
   }
   preset_type[!preset_type %in% c("plot", "figure")] <- NA;

   n <- max(lengths(list(
      x,
      y,
      adjX,
      adjY,
      preset,
      adjPreset
   )));
   parUsr <- graphics::par("usr");
   if (length(x) == 0) {
      x <- mean(parUsr[1:2]);
   }
   if (length(y) == 0) {
      y <- mean(parUsr[3:4]);
   }
   x <- rep(x, length.out=n);
   y <- rep(y, length.out=n);
   preset_type <- rep(preset_type, length.out=n)

   ## Verify preset is valid
   presetValid <- c("default",
      "top",
      "topright",
      "right",
      "bottomright",
      "topleft",
      "left",
      "bottomleft",
      "bottom",
      "center");
   if (length(preset) == 0) {
      preset <- "default";
   }
   if (length(adjPreset) == 0) {
      adjPreset <- "default";
   }
   if (!all(preset %in% presetValid)) {
      stop(paste0("preset must be one of ",
         jamba::cPaste(presetValid)));
   }
   if (!all(adjPreset %in% presetValid)) {
      stop(paste0("adjPreset must be one of ",
         jamba::cPaste(presetValid)));
   }
   preset <- rep(preset, length.out=n);
   adjPreset <- rep(adjPreset, length.out=n);

   if (length(adjX) == 0) {
      adjX <- 0.5;
   }
   if (length(adjY) == 0) {
      adjY <- 0.5;
   }
   adjX <- rep(adjX, length.out=n);
   adjY <- rep(adjY, length.out=n);
   if (verbose) {
      jamba::printDebug("coordPresets(): ",
         "n:",
         n);
   }

   ## Process the preset logic
   x <- ifelse(grepl("right", preset), parUsr[2],
      ifelse(grepl("left", preset), parUsr[1],
         ifelse(grepl("center|top|bottom", preset), mean(parUsr[1:2]),
            x)));
   if (verbose) {
      jamba::printDebug("coordPresets(): ",
         "x:",
         x);
   }
   adjX <- ifelse(grepl("default", adjPreset),
      ifelse(grepl("left", preset), 0-adjOffsetX,
         ifelse(grepl("right", preset), 1+adjOffsetX,
            ifelse(grepl("center|top|bottom", preset), 0.5,
               adjX))),
      ifelse(grepl("right", adjPreset), 0-adjOffsetX,
         ifelse(grepl("left", adjPreset), 1+adjOffsetX,
            ifelse(grepl("center|top|bottom", adjPreset), 0.5,
               adjX))));
   y <- ifelse(grepl("top", preset), parUsr[4],
      ifelse(grepl("bottom", preset), parUsr[3],
         ifelse(grepl("center|left|right", preset), mean(parUsr[3:4]),
            y)));
   adjY <- ifelse(grepl("default", adjPreset),
      ifelse(grepl("top", preset), 1+adjOffsetY,
         ifelse(grepl("bottom", preset), 0-adjOffsetY,
            ifelse(grepl("center|left|right", preset), 0.5,
               adjY))),
      ifelse(grepl("top", adjPreset), 0-adjOffsetY,
         ifelse(grepl("bottom", adjPreset), 1+adjOffsetY,
            ifelse(grepl("center|left|right", adjPreset), 0.5,
               adjY))));

   # optionally adjust to figure margin
   # when preset_type="figure" and preset is directional for the same entry
   if (any("figure" %in% preset_type &
         any(c("left", "right", "top", "bottom") %in% preset))) {
      # plot range in in ches
      parpin <- graphics::par("pin")
      # plot margin in inches
      parmai <- graphics::par("mai");
      # x coord range per inch
      xcoord_inch <- diff(parUsr[1:2]) / parpin[1];
      # y coordinate range
      ycoord_inch <- diff(parUsr[3:4]) / parpin[2];
      # adjust x for "left" or "right"
      x <- ifelse(preset_type %in% "figure" & preset %in% "left",
         x - xcoord_inch * parmai[2],
         ifelse(preset_type %in% "figure" & preset %in% "right",
            x + xcoord_inch * parmai[4], x))
      # adjust y for "top" or "bottom"
      y <- ifelse(preset_type %in% "figure" & preset %in% "bottom",
         y - ycoord_inch * parmai[1],
         ifelse(preset_type %in% "figure" & preset %in% "top",
            y + ycoord_inch * parmai[3], y))
   }

   return(data.frame(x=x,
      y=y,
      adjX=adjX,
      adjY=adjY,
      preset=preset,
      adjPreset=adjPreset));
}

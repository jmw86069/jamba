#' Export a data.frame to Excel xlsx format
#'
#' Export a data.frame to Excel xlsx format
#'
#' This function is a minor but useful customization of the
#' `openxlsx::saveWorkbook()` and associated functions, intended
#' to provide some pre-configured formatting of known column
#' types, typically relevant to statistical values, and
#' in some cases, gene or transcript expression values.
#'
#' There are numerous configurable options when saving an Excel
#' worksheet, most of the defaults in this function are intended
#' not to require changes, but are listed as formal function
#' arguments to make each option visibly obvious.
#'
#' If `colorSub` is supplied as a named vector of colors, then
#' by default text values will be colorized accordingly, which
#' can be especially helpful when including data with categorical
#' text values.
#'
#' This function pre-configures formatting options for the following
#' column data types, each of which has conditional
#' color-formatting, defined numeric ranges, and color scales.
#'
#' \describe{
#'    \item{int}{integer values, where numeric values are formatted
#'    without visible decimal places, and the `big.mark=","` standard
#'    is used to help visually distinguish large integers. The color
#'    scale is by default c(0, 100, 10000).}
#'    \item{num}{numeric values, with fixed number of visible decimal
#'    places, which helps visibly align values along each row.}
#'    \item{hit}{numeric type, a subset of "int" intended when data
#'    is flagged with something like a "+1" or "-1" to indicate a
#'    statistical increase or decrease.}
#'    \item{pvalue}{P-value, where numeric values range from 1 down
#'    near zero, and values are formatted consistently with scientific
#'    notation.}
#'    \item{fc}{numeric fold change, whose values are expected to range
#'    from 1 and higher, and -1 and lower. Decimal places are by default
#'    configured to show one decimal place, to simplify the Excel visual
#'    summary.}
#'    \item{lfc}{numeric log fold change, whose values are expected to be
#'    centered at zero. Decimal places are by default
#'    configured to show one decimal place, to simplify the Excel visual
#'    summary.}
#'    \item{highlight}{character and undefined columns to be highlighted
#'    with a brighter background color, and bold text.}
#' }
#'
#' For each column data type, a color scale and default numeric range
#' is defined, which allows conditional formatting of cells based upon
#' expected ranges of values.
#'
#' A screenshot of the file produced by the example is shown below.
#'
#' \if{html}{\figure{writeopenxlsx.png}{xlsx screenshot}}
#'
#' \if{latex}{\figure{writeopenxlsx.png}{options: width=0.5in}}
#'
#' @family jam export functions
#'
#' @param x data.frame to be saved to an Excel xlsx file.
#' @param file a valid path to save an Excel xlsx file. If the file exists,
#'    and `append=TRUE` the new data will be added to the existing file with
#'    the defined `sheetName`.
#' @param sheetName character value less with a valid
#'    Excel xlsx worksheet name. At this time (version 0.0.29.900) the
#'    sheetName is restricted to 31 characters, with no puntuation except
#'    "-" and "_".
#' @param headerColors,columnColors,highlightHeaderColors,highlightColors,borderColor,borderPosition
#'    default values for the Excel worksheet background and border
#'    colors. As of version 0.0.29.900, colors must use Excel-valid
#'    color names.
#' @param highlightColumns,numColumns,fcColumns,lfcColumns,hitColumns,intColumns,pvalueColumns
#'    integer vector referring the column number in the input data.frame `x`
#'    to define as each column type, as relevant.
#' @param numFormat,fcFormat,lfcFormat,hitFormat,intFormat,pvalueFormat
#'    character string with valid Excel cell formatting, for example
#'    "#,##0.00" defines a column to use comma-delimited numbers above
#'    one thousand, and display two decimal places in all numeric cells.
#' @param numRule,fcRule,lfcRule,hitRule,intRule,pvalueRule numeric vector
#'    `length=3` indicating the breakpoints for Excel to apply conditional
#'    color formatting, using the corresponding style.
#' @param numStyle,fcStyle,lfcStyle,intStyle,hitStyle,pvalueStyle character
#'    vector `length=3` containing three valid R colors. Note that alpha
#'    transparency will be removed prior to use in Excel, as required.
#' @param doConditional logical indicating whether to apply conditional
#'    formatting of cells, with this function only the background cell
#'    color (and contrasting text color) is affected.
#' @param doCategorical logical indicating whether to apply categorical
#'    color formatting, of only the background cell colors and contrasting
#'    text color. This argument requires `colorSub` be defined.
#' @param colorSub character vector of R colors, whose names refer to
#'    cell values in the input `x` data.frame.
#' @param freezePaneColumn,freezePaneRow integer value of the row or
#'    column before which the Excel "freeze panes" is applied.
#' @param doFilter logical indicating whether to enable column
#'    filtering by default.
#' @param fontName,fontSize default font configuration, containing
#'    a valid Excel font name, and a font size in Excel point units,
#'    respectively.
#' @param minWidth,maxWidth,autoWidth numeric minimum, maximum size
#'    for each Excel cell, in character units as defined by Excel,
#'    used when `autoWidth=TRUE` to restrict cell widths to this range,
#'    dependent upon the data content. When `autoWidth=FALSE` Excel
#'    naturally auto-sizes cells to the width of the largest value
#'    in each column.
#' @param wrapHeaders logical indicating whether to enable word wrap
#'    for column headers, which is helpful when `autoWidth=TRUE` since
#'    it fixed the cell width while allowing the column header to be seen.
#' @param headerRowMultiplier integer value, the row height of the first
#'    header row in Excel, as a multiple of subsequent rows. This argument
#'    is helpful when `wrapHeaders=TRUE` and `autoWidth=TRUE`.
#' @param keepRownames logical indicating whether to include
#'    `rownames(x)` in its own column in Excel.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional arguments are passed to `applyXlsxConditionalFormat()`
#'    and `applyXlsxCategoricalFormat()`.
#'
#' @examples
#' # set up a test data.frame
#' set.seed(123);
#' df1 <- data.frame(
#'    Gene=paste0("Gene", LETTERS[2:11]),
#'    log2fc=rnorm(10)*5,
#'    `P-value`=1/sample(((10:50)/10)^3, 10));
#' df1$hit <- sign(df1[,2]) * (abs(df1[,2]) >= 1 & df1[,3] < 0.03);
#' # apply sort
#' df1 <- mixedSortDF(df1, byCols=c(4,2,3));
#' # set of categorical colors for genes
#' colorSubGene <- nameVector(
#'    rep(c("purple4","orange","red4"),
#'       c(2,6,2)),
#'    df1$Gene);
#' df1;
#' writeOpenxlsx(file="/Users/wardjm/test.xlsx",
#'    sheetName="sheet1",
#'    append=FALSE,
#'    x=df1,
#'    colorSub=colorSubGene,
#'    highlightColumns=1,
#'    pvalueColumns=3,
#'    lfcColumns=2,
#'    hitColumns=4,
#'    freezePaneRow=2,
#'    freezePaneColumn=2);
#'
#' @export
writeOpenxlsx <- function
(x,
file,
sheetName="Sheet1",
append=FALSE,
headerColors=c("lightskyblue1", "lightskyblue2"),
columnColors=c("aliceblue", "azure2"),
highlightHeaderColors=c("tan1", "tan2"),
highlightColors=c("moccasin", "navajowhite"),
borderColor="gray75",
borderPosition="BottomRight",

highlightColumns=NULL,
numColumns=NULL,
fcColumns=NULL,
lfcColumns=NULL,
hitColumns=NULL,
intColumns=NULL,
pvalueColumns=NULL,

numFormat="#,##0.00",
fcFormat="#,##0.0",
lfcFormat="#,##0.0",
hitFormat="#,##0.0",
intFormat="#,##0",
pvalueFormat="0.00E+00",

numRule=c(1,10,20),
fcRule=c(-6,0,6),
lfcRule=c(-3,0,3),
hitRule=c(-2,0,2),
intRule=c(0,100,10000),
pvalueRule=c(1e-5,0.01,1),

numStyle=c("#EEECE1", "#678ADF", "#5673AA"),
fcStyle= c("#4F81BD", "#EEECE1", "#C0504D"),
lfcStyle=c("#4F81BD", "#EEECE1", "#C0504D"),
hitStyle=c("#4F81BD", "#EEECE1", "#C0504D"),
intStyle=c("#EEECE1", "#DF8A67", "#AA6346"),
pvalueStyle=c("#AA6346", "#DF8A67", "#EEECE1"),

doConditional=TRUE,
doCategorical=TRUE,
colorSub=NULL,

freezePaneColumn=1,
freezePaneRow=2,
doFilter=TRUE,

fontName="Arial",
fontSize=12,

minWidth=8,
maxWidth=40,
autoWidth=TRUE,

wrapHeaders=TRUE,
headerRowMultiplier=5,
keepRownames=FALSE,

verbose=FALSE,
...)
{
   ## Purpose is to use openxlsx::writeDataTable() to write .xlsx files
   ##
   if (!suppressPackageStartupMessages(require(openxlsx))) {
      stop("The openxlsx package is required for writeOpenxlsx().");
   }
   if (append && file.exists(file)) {
      ## Load the requested file as a workbook
      if (verbose) printDebug("writeOpenxlsx():",
         "loadWorkbook");
      wb <- openxlsx::loadWorkbook(file);
   } else {
      if (verbose) printDebug("writeOpenxlsx():",
         "createWorkbook");
      wb <- openxlsx::createWorkbook();
   }
   ## Set column widths
   if (verbose) printDebug("writeOpenxlsx():",
      "setColWidths");
   options("openxlsx.minWidth"=minWidth,
      "openxlsx.maxWidth"=maxWidth);

   ## Create a sheet for this data.frame
   if (verbose) printDebug("writeOpenxlsx():",
      "addWorksheet");
   openxlsx::addWorksheet(wb, sheetName);

   ## Write data.frame into the sheet for this workbook
   if (verbose) printDebug("writeOpenxlsx():",
      "writeDataTable");
   openxlsx::writeDataTable(wb,
      x=x,
      sheet=sheetName,
      rowNames=keepRownames,
      bandedRows=FALSE,
      bandedCols=TRUE);

   ## Set base font
   if (verbose) printDebug("writeOpenxlsx():",
      "modifyBaseFont");
   openxlsx::modifyBaseFont(wb,
      fontSize=fontSize,
      fontColour="grey10",
      fontName=fontName);

   ## Style all borders
   borderStyle <- openxlsx::createStyle(borderColour=borderColor,
      border=borderPosition);
   if (verbose) printDebug("writeOpenxlsx():",
      "borderStyle");
   openxlsx::addStyle(wb,
      sheet=sheetName,
      style=borderStyle,
      cols=seq_len(ncol(x) + keepRownames),
      rows=seq_len(nrow(x)+1),
      stack=TRUE,
      gridExpand=TRUE);

   ## Wrap text and set text alignment for header row
   wrapTextStyle <- openxlsx::createStyle(wrapText=TRUE,
      valign="top");
   if (wrapHeaders) {
      if (verbose) printDebug("writeOpenxlsx():",
         "wrapHeaders");
      openxlsx::addStyle(wb,
         sheet=sheetName,
         style=wrapTextStyle,
         cols=seq_len(ncol(x) + keepRownames),
         rows=1,
         stack=TRUE,
         gridExpand=TRUE);
   }

   ## Set column widths
   if (autoWidth) {
      if (verbose) {
         printDebug("writeOpenxlsx():",
            "autoWidth=TRUE");
      }
      openxlsx::setColWidths(wb,
         sheetName,
         cols=seq_len(ncol(x) + keepRownames),
         widths=rep(c("auto"), length.out=ncol(x) + keepRownames));
   } else {
      if (verbose) {
         printDebug("writeOpenxlsx():",
            "autoWidth=FALSE");
      }
      openxlsx::setColWidths(wb,
         sheetName,
         cols=seq_len(ncol(x) + keepRownames),
         widths=rep(minWidth, length.out=ncol(x) + keepRownames));
   }

   ## Make sure colors are in hex format
   headerColors <- gsub("(#......)..", "\\1",
      rgb2col(col2rgb(headerColors)));
   columnColors <- gsub("(#......)..", "\\1",
      rgb2col(col2rgb(columnColors)));
   highlightHeaderColors <- gsub("(#......)..", "\\1",
      rgb2col(col2rgb(highlightHeaderColors)));
   highlightColors <- gsub("(#......)..", "\\1",
      rgb2col(col2rgb(highlightColors)));
   if (verbose) {
      printDebug("writeOpenxlsx(): ",
         "headerColors:", headerColors,
         fgText=c("orange", headerColors));
      printDebug("writeOpenxlsx(): ",
         "highlightHeaderColors:", highlightHeaderColors,
         fgText=c("orange", highlightHeaderColors));
      printDebug("writeOpenxlsx(): ",
         "highlightColors:", highlightColors,
         fgText=c("orange", highlightColors));
   }

   ## Style basic non-highlightColumns A and B
   basicColumns <- setdiff(seq_len(ncol(x) + keepRownames),
      highlightColumns + keepRownames);
   if (length(basicColumns) > 0) {
      basicAHStyle <- openxlsx::createStyle(#bgFill=head(headerColors, 1),
         fgFill=head(headerColors, 1),
         fontColour="black");
      basicBHStyle <- openxlsx::createStyle(#bgFill=tail(headerColors, 1),
         fgFill=tail(headerColors, 1),
         fontColour="black");
      basicACStyle <- openxlsx::createStyle(#bgFill=head(columnColors, 1),
         fgFill=head(columnColors, 1));
      basicBCStyle <- openxlsx::createStyle(#bgFill=tail(columnColors, 1),
         fgFill=tail(columnColors, 1));
      basicColumnsA <- basicColumns[basicColumns %% 2 == 1];
      basicColumnsB <- basicColumns[basicColumns %% 2 == 0];
      if (verbose) printDebug("writeOpenxlsx():",
         "basicStyles");
      if (length(basicColumnsA) > 0) {
         openxlsx::addStyle(wb,
            sheet=sheetName,
            style=basicAHStyle,
            cols=basicColumnsA,
            rows=1,
            stack=TRUE,
            gridExpand=TRUE);
         openxlsx::addStyle(wb,
            sheet=sheetName,
            style=basicACStyle,
            cols=basicColumnsA,
            rows=seq_len(nrow(x))+1,
            stack=TRUE,
            gridExpand=TRUE);
      }
      if (length(basicColumnsB) > 0) {
         openxlsx::addStyle(wb,
            sheet=sheetName,
            style=basicBHStyle,
            cols=basicColumnsB,
            rows=1,
            stack=TRUE,
            gridExpand=TRUE);
         openxlsx::addStyle(wb,
            sheet=sheetName,
            style=basicBCStyle,
            cols=basicColumnsB,
            rows=seq_len(nrow(x))+1,
            stack=TRUE,
            gridExpand=TRUE);
      }
   }


   ## Style highlightColumns A and B
   if (length(highlightColumns) > 0) {
      if (verbose) printDebug("writeOpenxlsx():",
         "Applying highlightStyles");
      highAHStyle <- openxlsx::createStyle(#bgFill=head(highlightHeaderColors, 1),
         fgFill=head(highlightHeaderColors, 1),
         textDecoration="bold", fontColour="navy");
      highBHStyle <- openxlsx::createStyle(#bgFill=tail(highlightHeaderColors, 1),
         fgFill=tail(highlightHeaderColors, 1),
         textDecoration="bold", fontColour="navy");
      highACStyle <- openxlsx::createStyle(#bgFill=head(highlightColors, 1),
         fgFill=head(highlightColors, 1),
         textDecoration="bold", fontColour="navy");
      highBCStyle <- openxlsx::createStyle(#bgFill=tail(highlightColors, 1),
         fgFill=tail(highlightColors, 1),
         textDecoration="bold", fontColour="navy");
      highlightColumnsA <- highlightColumns[highlightColumns %% 2 == 1];
      highlightColumnsB <- highlightColumns[highlightColumns %% 2 == 0];
      if (length(highlightColumnsA) > 0) {
         openxlsx::addStyle(wb,
            sheet=sheetName,
            style=highAHStyle,
            cols=highlightColumnsA + keepRownames,
            rows=1,
            stack=TRUE,
            gridExpand=TRUE);
         openxlsx::addStyle(wb,
            sheet=sheetName,
            style=highACStyle,
            cols=highlightColumnsA + keepRownames,
            rows=seq_len(nrow(x))+1,
            stack=TRUE,
            gridExpand=TRUE);
      }
      if (length(highlightColumnsB) > 0) {
         openxlsx::addStyle(wb,
            sheet=sheetName,
            style=highBHStyle,
            cols=highlightColumnsB + keepRownames,
            rows=1,
            stack=TRUE,
            gridExpand=TRUE);
         openxlsx::addStyle(wb,
            sheet=sheetName,
            style=highBCStyle,
            cols=highlightColumnsB + keepRownames,
            rows=seq_len(nrow(x))+1,
            stack=TRUE,
            gridExpand=TRUE);
      }
   }

   ## Style integer fields
   if (length(intColumns) > 0) {
      if (verbose) printDebug("writeOpenxlsx():",
         "Applying intStyles");
      intStyle1 <- openxlsx::createStyle(numFmt=intFormat, halign="right");
      openxlsx::addStyle(wb,
         sheet=sheetName,
         style=intStyle1,
         cols=intColumns,
         rows=seq_len(nrow(x))+1,
         stack=TRUE,
         gridExpand=TRUE);
   }

   ## Style numeric fields
   if (length(numColumns) > 0) {
      if (verbose) printDebug("writeOpenxlsx():",
         "Applying numStyles");
      numStyle1 <- openxlsx::createStyle(numFmt=numFormat, halign="right");
      openxlsx::addStyle(wb,
         sheet=sheetName,
         style=numStyle1,
         cols=numColumns,
         rows=seq_len(nrow(x))+1,
         stack=TRUE,
         gridExpand=TRUE);
   }

   ## Style FC fields
   if (length(fcColumns) > 0) {
      if (verbose) printDebug("writeOpenxlsx():",
         "Applying fcStyles");
      fcStyle1 <- openxlsx::createStyle(numFmt=fcFormat, halign="right");
      openxlsx::addStyle(wb,
         sheet=sheetName,
         style=fcStyle1,
         cols=fcColumns + keepRownames,
         rows=seq_len(nrow(x))+1,
         stack=TRUE,
         gridExpand=TRUE);
   }

   ## Style LFC fields
   if (length(lfcColumns) > 0) {
      if (verbose) printDebug("writeOpenxlsx():",
         "Applying lfcStyles");
      lfcStyle1 <- openxlsx::createStyle(numFmt=lfcFormat, halign="right");
      openxlsx::addStyle(wb,
         sheet=sheetName,
         style=lfcStyle1,
         cols=lfcColumns + keepRownames,
         rows=seq_len(nrow(x))+1,
         stack=TRUE,
         gridExpand=TRUE);
   }

   ## Style hit fields
   if (length(hitColumns) > 0) {
      if (verbose) printDebug("writeOpenxlsx():",
         "Applying hitStyles");
      hitStyle1 <- openxlsx::createStyle(numFmt=hitFormat, halign="right");
      openxlsx::addStyle(wb,
         sheet=sheetName,
         style=hitStyle1,
         cols=hitColumns + keepRownames,
         rows=seq_len(nrow(x))+1,
         stack=TRUE,
         gridExpand=TRUE);
   }

   ## Style P-value fields
   if (length(pvalueColumns) > 0) {
      if (verbose) printDebug("writeOpenxlsx():",
         "Applying pvalueStyles with pvalueFormat:",
         pvalueFormat);
      pvalueStyle1 <- openxlsx::createStyle(numFmt=pvalueFormat, halign="right");
      openxlsx::addStyle(wb,
         sheet=sheetName,
         style=pvalueStyle1,
         cols=pvalueColumns + keepRownames,
         rows=seq_len(nrow(x))+1,
         stack=TRUE,
         gridExpand=TRUE);
   }

   ## Apply freeze panes
   if (freezePaneRow > 1 || freezePaneColumn > 1) {
      if (verbose) {
         printDebug("writeOpenxlsx():",
            "Applying freezePaneRow:", freezePaneRow,
            ", freezePaneColumn:", freezePaneColumn);
      }
      openxlsx::freezePane(wb,
         sheetName,
         firstActiveRow=freezePaneRow,
         firstActiveCol=freezePaneColumn + keepRownames);
   }

   ## Add header filter
   if (doFilter && 1 == 2) {
      if (verbose) printDebug("writeOpenxlsx():",
         "Applying addFilter");
      openxlsx::addFilter(wb,
         sheetName,
         rows=1,
         cols=seq_len(ncol(x) + keepRownames));
   }

   ## Adjust header row height
   if (headerRowMultiplier > 1) {
      if (verbose) printDebug("writeOpenxlsx():",
         "Applying headerRowMultiplier");
      openxlsx::setRowHeights(wb,
         sheetName,
         rows=1,
         heights=15*headerRowMultiplier);
   }

   ## Write the .xlsx file here
   if (verbose) {
      printDebug("writeOpenxlsx(): ",
         "saveWorkbook");
   }
   openxlsx::saveWorkbook(wb,
      file,
      overwrite=TRUE);

   ## Optionally apply conditional column formatting
   if (doConditional &&
         length(c(numColumns,
            fcColumns,
            lfcColumns,
            intColumns,
            hitColumns)) > 0) {
      #
      if (verbose) printDebug("writeOpenxlsx():",
         "calling applyXlsxConditionalFormat");
      applyXlsxConditionalFormat(xlsxFile=file,
         sheet=sheetName,
         fcColumns=fcColumns + keepRownames,
         lfcColumns=lfcColumns + keepRownames,
         numColumns=numColumns + keepRownames,
         intColumns=intColumns + keepRownames,
         hitColumns=hitColumns + keepRownames,
         pvalueColumns=pvalueColumns + keepRownames,
         fcStyle=fcStyle, fcRule=fcRule,
         lfcStyle=lfcStyle, lfcRule=lfcRule,
         hitStyle=hitStyle, hitRule=hitRule,
         intStyle=intStyle, intRule=intRule,
         numStyle=numStyle, numRule=numRule,
         pvalueStyle=pvalueStyle, pvalueRule=pvalueRule,
         verbose=verbose,
         ...);
   }

   if (doCategorical && !is.null(colorSub)) {
      textColumns <- setdiff(seq_len(ncol(x) + keepRownames),
         c(numColumns,
            fcColumns,
            lfcColumns,
            intColumns,
            hitColumns,
            pvalueColumns) + keepRownames);
      ## Apply categorical colors to text columns
      if (length(textColumns) > 0) {
         #
         if (verbose) {
            printDebug("writeOpenxlsx():",
               "applyXlsxCategoricalFormat: ",
               textColumns);
         }
         applyXlsxCategoricalFormat(xlsxFile=file,
            sheet=sheetName,
            rowRange=seq_len(nrow(x))+0,
            colRange=textColumns,
            colorSub=colorSub,
            ...);
      }
      ## Apply categorical colors to column headers
      if (any(colnames(x) %in% names(colorSub))) {
         colRange <- which(colnames(x) %in% names(colorSub));
         if (verbose) printDebug("writeOpenxlsx(): ",
            "applyXlsxCategoricalFormat to colRange:",
            colRange);
         applyXlsxCategoricalFormat(xlsxFile=file,
            sheet=sheetName,
            rowRange=c(0),
            colRange=colRange + keepRownames,
            colorSub=colorSub,
            verbose=verbose,
            ...);
      }
   }

}


#' Xlsx Conditional formatting
#'
#' Xlsx Conditional formatting
#'
#' This function is a convenient wrapper for applying conditional formatting
#' to Excel xlsx worksheets, with reasonable settings for commonly used
#' data types.
#'
#' A description of column types follows:
#'    \describe{
#'       \item{"fc"}{Fold change, typically positive and negative values,
#'          which are formatted to show one decimal place, and use commas
#'          to separate thousands places, e.g. 1,020.1. Colors are applied
#'          with a neutral midpoint, coloring values which are above and
#'          below zero.}
#'       \item{"lfc"}{log fold change, typically positive and negative values,
#'          which are formatted to show one decimal place, and use commas
#'          to separate thousands places, e.g. 12.1. Colors are applied
#'          with a neutral midpoint, coloring values which are above and
#'          below zero. Log fold changes have slightly different color
#'          thresholds than fold changes.}
#'       \item{"hit"}{Hit columns, often just values like \code{c(-1,0,1)},
#'          but which could be fold changes for statistical hits for example.
#'          They are formatted to show one decimal place, and use commas
#'          to separate thousands places, e.g. 1.5. Colors are applied
#'          with a neutral midpoint, coloring values which are above and
#'          below zero, typically with a fairly low threshold.}
#'       \item{"int"}{Integer columns, which are formatted to hide decimal
#'          place values even if present, which can help clean up visible
#'          tabular data. They are formatted to use commas
#'          to separate thousands places, e.g. 1,020. Colors are applied
#'          with a baseline of zero, intended for highlighting two thresholds
#'          of values above zero.}
#'       \item{"num"}{Numeric columns, which are formatted to display 2 decimal
#'          places, and to use commas to separate thousands places,
#'          e.g. 1,020.1. Colors are applied
#'          with a baseline of zero, intended for highlighting two thresholds
#'          of values above zero.}
#'       \item{"pvalue"}{P-value columns, which are formatted to display
#'          scientific notation always, for consistency, with two decimal
#'          places, e.g. 1.02e-02. Colors are applied starting at white for
#'          P-value of 1 (non-significant) and becoming more red as the
#'          P-value approaches 0.01, then 0.0001.}
#'    }
#'
#' For each column type, one can describe the column using integer indices,
#' or colnames, or optionally use the Grep parameters. The Grep parameters
#' are intended for pattern matching, and may contain a vector of grep patterns
#' which are used by \code{\link{provigrep}} to match to colnames. The Grep
#' method is particularly useful when applying conditional formatting for
#' multiple worksheets in the same .xlsx file, where the colnames are not
#' identical in each worksheet.
#'
#' Each column type has an associated 3-threshold rule, and three associated
#' colors. In order to apply different thresholds, one would need to call
#' this function multiple times, specifying different subsets of columns
#' corresponding to each set of thresholds. The same process is required
#' in order to apply different color gradients to different columns. Note
#' that styles are by default "stacked", which
#' maintains font and cell border styles without removing them. However, it
#' this "stacking" means that applying two rules to the same cell will not
#' work, since only the first rule will be applied by Microsoft Excel.
#' Interestingly, if multiple conditional rules are applied to the same
#' cell, they will be visible in order inside the Microsoft Excel
#' application.
#'
#' @family jam export functions
#'
#' @param xlsxFile character filename to a file with ".xlsx" extension.
#' @param sheet integer or character, either the worksheet number, in order
#'    or character worksheet name. This vector can contain multiple values,
#'    which will cause conditional formatting to be applied to each
#'    worksheet in the order given.
#' @param fcColumns,lfcColumns,hitColumns,intColumns,numColumns,pvalueColumns
#'    integer column indices, or character colnames indicating which columns
#'    are to be treated as each of the various column types.
#' @param fcGrep,lfcGrep,hitGrep,intGrep,numGrep,pvalueGrep
#'    optional character vector which is used by \code{\link{provigrep}} to
#'    colnames(x). This process may be more convenient to apply formatting
#'    to known colname character patterns, rather than supplying exact column
#'    indices or colnames.
#' @param fcStyle,lfcStyle,hitStyle,intStyle,numStyle,pvalueStyle
#'    color vector of length=3, corresponding to the numeric thresholds
#'    defined by the corresponding Rules.
#' @param fcRule,lfcRule,hitRule,intRule,numRule,pvalueRule
#'    numeric vector of length=3, used to define three numeric thresholds
#'    for color gradients to be applied.
#' @param fcType,lfcType,hitType,intType,numType,pvalueType
#'    character string indicating the type of conditional rule to apply,
#'    which in most cases should be "colourScale" which allows three numeric
#'    thresholds, and three corresponding colors. For other allowed values,
#'    see \code{\link{openxlsx::conditionalFormatting}}.
#' @param verbose logical indicating whether to print verbose output.
#' @param startRow integer indicating which row to begin applying conditional
#'    formatting. In most cases startRow=2, which allows one row for column
#'    headers. However, if there are multiple header rows, startRow should be
#'    1 more than the number of header rows.
#' @param overwrite logical indicating whether the original Excel files will
#'    be replaced with the new one, or whether a new file will be created.
#' @param ... additional parameters are ignored.
#'
#' @export
applyXlsxConditionalFormat <- function
(xlsxFile, sheet=1,
## fold change
fcColumns=NULL, fcGrep=NULL,
## blue-white-red color scale
fcStyle=c("#4F81BD", "#EEECE1", "#C0504D"),
fcRule=c(-6,0,6),  fcType="colourScale",
## log fold change
lfcColumns=NULL, lfcGrep=NULL,
## blue-white-red color scale
lfcStyle=c("#4F81BD", "#EEECE1", "#C0504D"),
lfcRule=c(-3,0,3), lfcType="colourScale",
## Hit numeric, usually integer
hitColumns=NULL, hitGrep=NULL,
## blue-white-red color scale
hitStyle=c("#4F81BD", "#EEECE1", "#C0504D"),
hitRule=c(-2,0,2), hitType="colourScale",
## Integer numbers
intColumns=NULL, intGrep=NULL,
## white-orange-red color scale
intStyle=c("#EEECE1", "#DF8A67", "#AA6346"),
intRule=c(0,100,10000), intType="colourScale",
## Decimal numbers
numColumns=NULL, numGrep=NULL,
## white-purple-navy color scale
numStyle=c("#EEECE1", "#678ADF", "#5673AA"),
numRule=c(1,10,20),
numType="colourScale",
## P-values
pvalueColumns=NULL, pvalueGrep=NULL,
## red-orange-white color scale
pvalueStyle=c("#AA6346", "#DF8A67", "#EEECE1"),
pvalueRule=c(1e-4,0.01,1), pvalueType="colourScale",
##
verbose=FALSE,
startRow=2,
overwrite=TRUE,
...)
{
   ## Purpose is to take an xlsx file, and apply conditional formatting
   ## only to the columns as requested
   ##
   ## overwrite=TRUE will overwrite the original file
   ## overwrite=FALSE will create a new file
   ##
   if (!suppressPackageStartupMessages(require(openxlsx))) {
      stop("The openxlsx package is required for applyXlsxConditionalFormat().");
   }

   ## Load the requested file as a workbook
   wb <- openxlsx::loadWorkbook(xlsxFile);


   ## Wrap in a large loop to handle multiple worksheets
   sheets <- sheet;
   for (sheet in sheets) {

      ## Load the data.frame, because it is the only reliable way to count rows
      df <- openxlsx::read.xlsx(xlsxFile, sheet=sheet);
      nrowDf <- nrow(df) + 1;

      ## If grep patterns are supplied, use those to define the colnames
      if (!is.null(hitGrep)) {
         hitColumns <- provigrep(c(hitGrep), colnames(df));
      }
      if (!is.null(intGrep)) {
         intColumns <- provigrep(c(intGrep), colnames(df));
      }
      if (!is.null(numGrep)) {
         numColumns <- provigrep(c(numGrep), colnames(df));
      }
      if (!is.null(fcGrep)) {
         fcColumns <- provigrep(c(fcGrep), colnames(df));
      }
      if (!is.null(lfcGrep)) {
         lfcColumns <- provigrep(c(lfcGrep), colnames(df));
      }
      if (!is.null(pvalueGrep)) {
         pvalueColumns <- provigrep(c(pvalueGrep), colnames(df));
      }
      ## Convert to integer
      hitColumns1 <- NULL;
      intColumns1 <- NULL;
      numColumns1 <- NULL;
      fcColumns1 <- NULL;
      lfcColumns1 <- NULL;
      pvalueColumns1 <- NULL;
      if (!is.null(hitColumns)) {
         if (igrepHas("character", class(hitColumns))) {
            hitColumns1 <- which(colnames(df) %in% hitColumns);
         } else {
            hitColumns1 <- hitColumns;
         }
      }
      if (!is.null(intColumns)) {
         if (igrepHas("character", class(intColumns))) {
            intColumns1 <- which(colnames(df) %in% intColumns);
         } else {
            intColumns1 <- intColumns;
         }
      }
      if (!is.null(numColumns)) {
         if (igrepHas("character", class(numColumns))) {
            numColumns1 <- which(colnames(df) %in% numColumns);
         } else {
            numColumns1 <- numColumns;
         }
      }
      if (!is.null(fcColumns)) {
         if (igrepHas("character", class(fcColumns))) {
            fcColumns1 <- which(colnames(df) %in% fcColumns);
         } else {
            fcColumns1 <- fcColumns;
         }
      }
      if (!is.null(lfcColumns)) {
         if (igrepHas("character", class(lfcColumns))) {
            lfcColumns1 <- which(colnames(df) %in% lfcColumns);
         } else {
            lfcColumns1 <- lfcColumns;
         }
      }
      if (length(pvalueColumns) > 0) {
         if (igrepHas("character", class(pvalueColumns))) {
            pvalueColumns1 <- which(colnames(df) %in% pvalueColumns);
         } else {
            pvalueColumns1 <- pvalueColumns;
         }
      }

      ###############################################
      ## Apply fold change conditional formatting
      if (!is.null(fcColumns1)) {
         if (verbose) {
            printDebug("conditionalFormatting fcColumns.");
            printDebug("fcColumns:", fcColumns1, c("orange", "lightblue"));
         }
         for (fcColumn in fcColumns1) {
            openxlsx::conditionalFormatting(wb,
               sheet=sheet,
               cols=fcColumn,
               rows=startRow:nrowDf,
               style=fcStyle,
               rule=fcRule,
               type=fcType);
         }
      }

      ## Apply log fold change conditional formatting
      if (!is.null(lfcColumns1)) {
         if (verbose) {
            printDebug("conditionalFormatting lfcColumns.");
            printDebug("lfcColumns:", lfcColumns1, c("orange", "lightblue"));
         }
         for (lfcColumn in lfcColumns1) {
            openxlsx::conditionalFormatting(wb,
               sheet=sheet,
               cols=lfcColumn,
               rows=startRow:nrowDf,
               style=lfcStyle,
               rule=lfcRule,
               type=lfcType);
         }
      }

      ## Apply signed hit conditional formatting
      if (!is.null(hitColumns1)) {
         if (verbose) {
            printDebug("conditionalFormatting hitColumns.");
            printDebug("hitColumns:", hitColumns1, c("orange", "lightblue"));
         }
         for (hitColumn in hitColumns1) {
            openxlsx::conditionalFormatting(wb,
               sheet=sheet,
               cols=hitColumn,
               rows=startRow:nrowDf,
               style=hitStyle,
               rule=hitRule,
               type=hitType);
         }
      }

      ## Apply integer value conditional formatting
      if (!is.null(intColumns1)) {
         if (verbose) {
            printDebug("conditionalFormatting intColumns.");
            printDebug("intColumns:", intColumns1, c("orange", "lightblue"));
         }
         for (intColumn in intColumns1) {
            openxlsx::conditionalFormatting(wb,
               sheet=sheet,
               cols=intColumn,
               rows=startRow:nrowDf,
               style=intStyle,
               rule=intRule,
               type=intType);
         }
      }

      ## Apply numeric value conditional formatting
      if (!is.null(numColumns1)) {
         if (verbose) {
            printDebug("conditionalFormatting numColumns.");
            printDebug("numColumns:", numColumns1, c("orange", "lightblue"));
         }
         for (numColumn in numColumns1) {
            openxlsx::conditionalFormatting(wb,
               sheet=sheet,
               cols=numColumn,
               rows=startRow:nrowDf,
               style=numStyle,
               rule=numRule,
               type=numType);
         }
      }

      ## Apply P-value conditional formatting
      if (length(pvalueColumns1) > 0) {
         if (verbose) {
            printDebug("conditionalFormatting pvalueColumns.");
            printDebug("pvalueColumns:", pvalueColumns1, c("orange", "lightblue"));
         }
         for (pvalueColumn in pvalueColumns1) {
            openxlsx::conditionalFormatting(wb,
               sheet=sheet,
               cols=pvalueColumn,
               rows=startRow:nrowDf,
               style=pvalueStyle,
               rule=pvalueRule,
               type=pvalueType);
         }
      }

   }

   ## Save the updated workbook
   if (!overwrite) {
      xlsxFile <- gsub("([.]xls[x]*)$", paste0("_", getDate(), "\\1"), xlsxFile);
   }
   openxlsx::saveWorkbook(wb, xlsxFile, overwrite=TRUE);
   invisible(xlsxFile);
}

#' Add categorical colors to Excel xlsx worksheets
#'
#' Add categorical colors to Excel xlsx worksheets
#'
#' This function is a convenient wrapper for applying categorical
#' color formatting to cell background colors, and applies a contrasting
#' color to the text in cells using `jamba::setTextContrastColor()`.
#' It uses a named character vector of colors supplied as `colorSub`
#' to define cell background colors, and optionally `colorSubText`
#' to define a specific color for the cell text.
#'
#' @family jam export functions
#'
#' @param xlsxFile filename pointing to an existing Excel xlsx file.
#' @param sheet integer index of the worksheet or worksheets.
#' @param rowRange,colRange integer vectors of rows and columns
#'    to apply categorical colors in the Excel xlsx worksheet.
#' @param colorSub named character vector of valid R colors, whose
#'    names correspond to values in the worksheet cells.
#' @param colorSubText optional character vector of colors, whose
#'    names correspond to values in the worksheet cells. In
#'    absence of a specific text color, `jamba::setTextContrastColor()`
#'    is used to define a contrasting text color to be visible on
#'    the colored background.
#' @param trimCatNames logical whether to trim whitespace and punctuation
#'    from `colorSub` and from Excel cell fields before matching colors
#'    to Excel values.
#' @param overwrite logical indicating whether new cell color styles
#'    should be forced overwrite of previous cell styles.
#' @param stack logical indicating whether new color rules should be
#'    applied above existing styles, many of whose styles may not affect
#'    the specific cell color, for example the font size and font name.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#'
#' @export
applyXlsxCategoricalFormat <- function
(xlsxFile,
sheet=1,
rowRange=NULL,
colRange=NULL,
colorSub=NULL,
colorSubText=setTextContrastColor(colorSub),
trimCatNames=TRUE,
overwrite=TRUE,
stack=TRUE,
verbose=FALSE,
...)
{
   ## Purpose is to take an xlsx file, and apply categorical formatting
   ## similar to conditional formatting, but mainly intended to help
   ## color-code non-numeric columns, or headers.
   ##
   ## sheet is a 1-based integer indicating one or more worksheets
   ## to process
   ##
   ## overwrite=TRUE will overwrite the original file
   ## overwrite=FALSE will create a new file
   ##
   ## rowRange, colRange indicates the range of rows and columns to
   ## perform the operation.  It will colorize categorical entries
   ## independent of any outside area.
   ##
   ## colorSub is a named vector of colors, whenever the name is found as
   ## and perfect, complete match, the cell color will be changed to that color.
   ##
   ## trimCatNames will trim any whitespace and punctuation characters from
   ## cell fields before matching names of colorSub
   if (!suppressPackageStartupMessages(require(openxlsx))) {
      stop("The openxlsx package is required for applyXlsxCategoricalFormat().");
   }
   retVals <- list();

   ## Load the requested file as a workbook
   wb <- openxlsx::loadWorkbook(xlsxFile);

   ## Wrap in a large loop to handle multiple worksheets
   sheets <- sheet;
   for (sheet in sheets) {

      ## Load the data.frame, because it is the only reliable way to count rows
      df <- openxlsx::read.xlsx(xlsxFile,
         sheet=sheet);
      nrowDf <- nrow(df) + 1;
      if (is.null(rowRange)) {
         rowRange <- seq_len(nrow(df));
      }
      if (is.null(colRange)) {
         colRange <- seq_len(ncol(df));
      }
      if (verbose) {
         printDebug("head(rowRange):", head(rowRange));
         if (length(rowRange) > 10) printDebug("tail(rowRange):", tail(rowRange));
         printDebug("head(colRange):", head(colRange));
         if (length(colRange) > 10) printDebug("tail(colRange):", tail(colRange));
      }
      rangeM <- rbind(colnames(df)[colRange],
         as.matrix(df[,colRange,drop=FALSE]))[rowRange+1,,drop=FALSE];
      colnames(rangeM) <- colRange;
      rownames(rangeM) <- rowRange+1;
      ## Allow colorizing NA entries
      if (any(is.na(rangeM))) {
         rangeM[is.na(rangeM)] <- "NA";
      }

      ## Optionally clean up some cell fields before matching
      if (trimCatNames) {
         rangeM[!is.na(rangeM)] <- gsub("^[-_() ]+|[-_() ]+$", "", rangeM[!is.na(rangeM)]);
      }

      ## Identify which colorSub entries are found
      #colorSubText <- setTextContrastColor(colorSub);
      if (verbose) {
         printDebug("head(unique(as.vector(rangeM))):",
            head(unique(as.vector(rangeM))));
         print(head(unique(as.vector(rangeM))));
      }
      colorSubFound <- which(gsub(" ", ".", names(colorSub)) %in% rangeM |
            names(colorSub) %in% rangeM);
      if (verbose) {
         printDebug("found:",
            names(colorSub)[colorSubFound])
      };
      colorSubStyles <- lapply(nameVector(colorSubFound), function(i) {
         ## Create style for each color
         if (verbose) {
            printDebug("   colorSubFound i:",
               names(colorSub)[i]);
         }
         bgColor <- rgb2col(col2rgb(colorSub[i]));
         fgColor <- rgb2col(col2rgb(colorSubText[i]));

         ## Remove all traces of alpha transparency, lest openxlsx choke
         bgColor <- gsub("([#][0-9A-Za-z]{6})..$", "\\1", bgColor);
         fgColor <- gsub("([#][0-9A-Za-z]{6})..$", "\\1", fgColor);
         #bgColor <- closestRcolor(bgColor);
         #fgColor <- closestRcolor(colorSubText[i]);
         if (verbose) {
            printDebug(paste0("     fontColour:", fgColor),
               paste0(", bgColor:", bgColor),
               fgText=c(fgColor, fgColor),
               bgText=c(bgColor, bgColor));
         }
         openxlsx::createStyle(fontColour=fgColor,
            bgFill=bgColor,
            fgFill=bgColor);
      });
      if (verbose) {
         printDebug("   completed colorSubFound");
      }
      rangeMatch <- rangeM;
      rangeMatch[1:nrow(rangeMatch),1:ncol(rangeMatch)] <- "";
      for (i in colorSubFound) {
         isMatched <- (rangeM %in% names(colorSub)[i] |
               rangeM %in% gsub(" ", ".", names(colorSub)[i]));
         rangeMatch[isMatched] <- names(colorSub)[i];
      }
      retVals$rangeMatch <- rangeMatch;

      ## Apply colorSub formatting
      if (verbose) printDebug("   Determining colorSub replacements");
      catRes <- lapply(nameVector(colorSubFound, names(colorSub)[colorSubFound]), function(i){
         if (verbose) printDebug("      i:", i);
         catColRes <- rbindList(rmNULL(lapply(1:ncol(rangeMatch), function(iCol){
            iWhich <- which(rangeMatch[,iCol] %in% names(colorSub)[i] |
                  rangeMatch[,iCol] %in% gsub(" ", ".", names(colorSub)[i]));
            catRowRes <- rbindList(lapply(iWhich, function(iRow){
               doRow <- as.numeric(rownames(rangeMatch)[iRow]);
               doCol <- as.numeric(colnames(rangeMatch)[iCol]);
               c(row=doRow, col=doCol);
            }));
         })));
      });
      retVals$catRes <- catRes;
      if (verbose) {
         printDebug("   Applying colorSub formatting with addStyle()");
      }
      addRes <- lapply(nameVector(seq_along(catRes), names(catRes)), function(i) {
         iStyle <- colorSubStyles[[i]];
         if (verbose) {
            printDebug("iStyle:");print(iStyle);
         }
         doRow <- catRes[[i]][,"row"];
         doCol <- catRes[[i]][,"col"];
         if (verbose) {
            printDebug("Updating colorSub:",
               names(colorSub)[i],
               ".",
               fgText=c("orange", colorSub[i], "orange"));
            printDebug("   length(doCol):", length(doCol));
            printDebug("   length(doRow):", length(doRow));
            printDebug("   head(doCol):", head(doCol));
            if (length(doCol) > 10) {
               printDebug("      tail(doCol):", tail(doCol));
            }
            printDebug("   head(doRow):", head(doRow));
            if (length(doRow) > 10) {
               printDebug("      tail(doRow):", tail(doRow));
            }
         }
         ## Note: code below iterates each cell, instead of using the
         ## apparently vectorized approach... unsure if vectorized method
         ## caused visual bugs in Excel
         #openxlsx::addStyle(wb, sheet=sheet, style=iStyle, rows=doRow, cols=doCol, stack=stack);
         if (verbose) {
            printDebug("     openxlsx::addStyle in vectorized mode.");
         }
         openxlsx::addStyle(wb,
            sheet=sheet,
            style=iStyle,
            rows=doRow,
            cols=doCol,
            stack=stack,
            gridExpand=FALSE);
         if (1 == 2) {
            for (k in seq_along(doRow)) {
               if (verbose) cat(paste0(doRow, ","));
               openxlsx::addStyle(wb, sheet=sheet, style=iStyle, rows=doRow[k], cols=doCol[k], stack=stack);
            }
            if (verbose) cat("\n");
         }
         list(iStyle=iStyle,
            doRow=doRow,
            doCol=doCol);
      });
      retVals$addRes <- addRes;
   }

   ## Save the updated workbook
   if (!overwrite) {
      xlsxFile <- gsub("([.]xls[x]*)$", paste0("_", getDate(), "\\1"), xlsxFile);
   }
   openxlsx::saveWorkbook(wb, xlsxFile, overwrite=TRUE);
   retVals$xlsxFile <- xlsxFile;
   invisible(retVals);
}

#' Export a data.frame to 'Excel' 'xlsx' format
#'
#' Export a data.frame to 'Excel' 'xlsx' format
#'
#' This function is a minor but useful customization of the
#' `openxlsx::saveWorkbook()` and associated functions, intended
#' to provide some pre-configured formatting of known column
#' types, typically relevant to statistical values, and
#' in some cases, gene or transcript expression values.
#'
#' There are numerous configurable options when saving an 'Excel'
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
#'    configured to show one decimal place, to simplify the 'Excel' visual
#'    summary.}
#'    \item{lfc}{numeric log fold change, whose values are expected to be
#'    centered at zero. Decimal places are by default
#'    configured to show one decimal place, to simplify the 'Excel' visual
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
#' @returns `Workbook` object as defined by the `openxlsx` package
#'    is returned invisibly with `invisible()`. This `Workbook`
#'    can be used in argument `wb` to provide a speed boost when
#'    saving multiple sheets to the same file.
#'
#' @param x `data.frame` to be saved to an 'Excel' 'xlsx' file.
#' @param file `character` valid path to save an 'Excel' 'xlsx' file. If the file
#'    exists, and `append=TRUE` the new data will be added to the existing
#'    file withthe defined `sheetName`.
#'    * Note when `file=NULL` the output is not saved to a file,
#'    instead the `Workbook` object is returned by this function.
#'    The `Workbook` object can be passed as argument `wb` in order
#'    to add multiple sheets to the same Workbook prior to saving
#'    them together. This operation is intended to provide a
#'    substantial improvement in speed.
#' @param wb `Workbook` object as defined in R package `openxlsx`.
#'    When this argument is defined, data is not imported from `file`,
#'    and instead the workbook data is used from `wb`. This option is
#'    intended to improve speed of writing several sheets to the same
#'    output file, by preventing the slow read/write steps each time
#'    a new sheet is added.
#' @param sheetName `character` value less with a valid
#'    'Excel' 'xlsx' worksheet name. At this time (version 0.0.29.900) the
#'    sheetName is restricted to 31 characters, with no puntuation except
#'    "-" and "_".
#' @param startRow,startCol `integer` indicating the row and column number
#'    to start with the top,left cell written to the worksheet,
#'    default are 1.
#' @param append `logical` default FALSE, whether to append to file (TRUE),
#'    or to write over an existing file. The `append=TRUE` is useful when
#'    adding a worksheet to an existing file.
#' @param wrapCells `logical` default FALSE, indicating whether to
#'    enable word-wrap within cells.
#' @param headerColors,columnColors,highlightHeaderColors,highlightColors,borderColor,borderPosition
#'    default values for the 'Excel' worksheet background and border
#'    colors. As of version 0.0.29.900, colors must use valid 'Excel'
#'    color names.
#' @param highlightColumns,numColumns,fcColumns,lfcColumns,hitColumns,intColumns,pvalueColumns
#'    `integer` vector referring the column number in the input `data.frame`
#'    `x` to define as each column type, as relevant.
#' @param numFormat,fcFormat,lfcFormat,hitFormat,intFormat,pvalueFormat
#'    `character` string with valid 'Excel' cell formatting, for example
#'    `"#,##0.00"` defines a column to use comma-delimited numbers above
#'    one thousand, and display two decimal places in all numeric cells.
#'    See `[https://support.microsoft.com]` topic
#'    `"Excel Create and apply a custom number format."` or
#'    `"Excel Number format codes"` for more details. Some examples below:
#'    * `"#,##0"` : display only integer values, using comma as delimiter
#'    for every thousands place.
#'    The number `2142.12` would be represented: `"2,142"`
#'    * `"###0.0"` : display numeric values rounded to the `0.1` place,
#'    using no comma delimiter for values above one thousand.
#'    The number `2142.12` would be represented: `"2142.1"`
#'    * `"[>0.01]0.00#;0.00E+00"` : this rule is a conditional format,
#'    values above `0.01` are represented as numbers rounded to the
#'    thousandths position `0.001`; values below `0.01` are represented
#'    with scientific notation with three digits.
#'    The number `0.1256` would be represented: `"0.126"`
#'    The number `0.001256` would be represented: `"1.26E-03"`
#'    * `"[Red]#,###.00_);[Blue](#,###.00);[Black]0.00_)"` : this format applies
#'    to positive values, negative values, and zero, in order delimited
#'    by semicolons. Positive values are colored red.
#'    The string `"_)"` adds whitespace (defined by `"_"`)
#'    equale to the width of the character `")"` to the end
#'    of positive values.
#'    Negative values are surrounded by parentheses `"()"`
#'    and are colored blue.
#'    Values equal to zero are represented with two trailing digits,
#'    and whitespace (`"_"`) equal to width `")"`.
#'    The whitespace at the end of positive values and zero are used
#'    to align all values at the same decimal position.
#' @param numRule,fcRule,lfcRule,hitRule,intRule,pvalueRule `numeric` vector
#'    `length=3` indicating the breakpoints for 'Excel' to apply conditional
#'    color formatting, using the corresponding style.
#'    Note that all conditional formatting applied by this function uses
#'    the `"3-Color Scale"`, therefore there should be three values,
#'    and three corresponding colors in the corresponding Style arguments.
#' @param numStyle,fcStyle,lfcStyle,intStyle,hitStyle,pvalueStyle `character`
#'    vector `length=3` containing three valid R colors. Note that alpha
#'    transparency will be removed prior to use in 'Excel', as required.
#'    Note that all conditional formatting applied by this function uses
#'    the `"3-Color Scale"`, therefore there should be three colors,
#'    which match three values in the corresponding Rule arguments.
#' @param doConditional `logical` indicating whether to apply conditional
#'    formatting of cells, with this function only the background cell
#'    color (and contrasting text color) is affected.
#' @param doCategorical `logical` indicating whether to apply categorical
#'    color formatting, of only the background cell colors and contrasting
#'    text color. This argument requires `colorSub` be defined.
#' @param colorSub `character` vector of R colors, whose names refer to
#'    cell values in the input `x` data.frame.
#' @param freezePaneColumn,freezePaneRow `integer` value of the row or
#'    column before which the 'Excel' "freeze panes" is applied.
#'    Note that these values are adjusted relative by `startRow` and
#'    `startCol` in the 'Excel' worksheet, so that the values are applied
#'    relative to the `data.frame` argument `x`.
#' @param doFilter `logical` indicating whether to enable column
#'    filtering by default.
#' @param fontName `character` default font configuration, containing
#'    a valid 'Excel' font name.
#' @param fontSize `numeric` default font size in 'Excel' point units.
#' @param minWidth,maxWidth,autoWidth `numeric` minimum, maximum size
#'    for each 'Excel' cell, in character units as defined by 'Excel',
#'    used when `autoWidth=TRUE` to restrict cell widths to this range.
#'    Note that the argument `colWidths` is generally preferred, if the
#'    numeric widths can be reasonable calculated or anticipated upfront.
#'    When `autoWidth=FALSE` 'Excel' typically auto-sizes cells to the width
#'    of the largest value in each column, which may not be ideal when
#'    values are extremely large.
#' @param wrapHeaders `logical` indicating whether to enable word wrap
#'    for column headers, which is helpful when `autoWidth=TRUE` since
#'    it fixed the cell width while allowing the column header to be seen.
#' @param headerRowMultiplier `numeric` value to define the row height of
#'    the first header row in 'Excel'. This value is defined as a multiple
#'    of subsequent rows, and should usually represent the maximum number
#'    of lines after word-wrapping, as relevant. This argument
#'    is helpful when `wrapHeaders=TRUE` and `autoWidth=TRUE`.
#' @param colWidths `numeric` width of each column in `x`, recycled
#'    to the total number of columns required. Note that when
#'    `keepRownames=TRUE`, the first column will contain `rownames(x)`,
#'    therefore the length of `colWidths` in that case will be
#'    `ncol(x) + 1`.
#' @param keepRownames `logical` indicating whether to include
#'    `rownames(x)` in its own column in 'Excel'.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are passed to `applyXlsxConditionalFormat()`
#'    and `applyXlsxCategoricalFormat()` as relevant.
#'
#' @examples
#' # set up a test data.frame
#' set.seed(123);
#' lfc <- -3:3 + stats::rnorm(7)/3;
#' colorSub <- nameVector(
#'    rainbow2(7),
#'    LETTERS[1:7])
#' df <- data.frame(name=LETTERS[1:7],
#'    int=round(4^(1:7)),
#'    num=(1:7)*4-2 + stats::rnorm(7),
#'    fold=2^abs(lfc)*sign(lfc),
#'    lfc=lfc,
#'    pvalue=10^(-1:-7 + stats::rnorm(7)),
#'    hit=sample(c(-1,0,0,1,1), replace=TRUE, size=7));
#' df;
#' # write to tempfile for examples
#' if (check_pkg_installed("openxlsx")) {
#'    out_xlsx <- tempfile(pattern="writeOpenxlsx_", fileext=".xlsx")
#'    writeOpenxlsx(x=df,
#'       file=out_xlsx,
#'       sheetName="jamba_test",
#'       colorSub=colorSub,
#'       intColumns=2,
#'       numColumns=3,
#'       fcColumns=4,
#'       lfcColumns=5,
#'       pvalueColumns=6,
#'       hitColumn=7,
#'       freezePaneRow=2,
#'       freezePaneColumn=2,
#'       append=FALSE);
#'    # now read it back
#'    df_list <- readOpenxlsx(xlsx=out_xlsx);
#'    sdim(df_list)
#' }
#'
#' @export
writeOpenxlsx <- function
(x,
 file=NULL,
 wb=NULL,
 sheetName="Sheet1",
 startRow=1,
 startCol=1,
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
 pvalueFormat="[>0.01]0.00#;0.00E+00",

 numRule=c(1, 10, 20),
 fcRule=c(-6, 0, 6),
 lfcRule=c(-3, 0, 3),
 hitRule=c(-1.5, 0, 1.5),
 intRule=c(0, 100, 10000),
 pvalueRule=c(0, 0.01, 0.05),

 numStyle=c("#F2F0F7", "#B4B1D4", "#938EC2"),
 fcStyle= c("#4F81BD", "#EEECE1", "#C0504D"),
 lfcStyle=c("#4F81BD", "#EEECE1", "#C0504D"),
 hitStyle=c("#4F81BD", "#EEECE1", "#C0504D"),
 intStyle=c("#EEECE1", "#FDA560", "#F77F30"),
 pvalueStyle=c("#F77F30", "#FDC99B", "#EEECE1"),

 doConditional=TRUE,
 doCategorical=TRUE,
 colorSub=NULL,

 freezePaneColumn=0,
 freezePaneRow=2,
 doFilter=TRUE,

 fontName="Arial",
 fontSize=12,

 minWidth=getOption("openxlsx.minWidth", 8),
 maxWidth=getOption("openxlsx.maxWidth", 40),
 autoWidth=TRUE,
 colWidths=NULL,

 wrapCells=FALSE,
 wrapHeaders=TRUE,
 headerRowMultiplier=5,
 keepRownames=FALSE,

 verbose=FALSE,
 ...)
{
   ## Purpose is to use openxlsx::writeDataTable() to write .xlsx files
   ##
   if (!requireNamespace("openxlsx", quietly=TRUE)) {
      stop("The openxlsx package is required for writeOpenxlsx().");
   }
   if (length(wb) > 0) {
      if (!"Workbook" %in% class(wb)) {
         stop("wb must be openxlsx class 'Workbook'");
      }
      if (verbose) printDebug("writeOpenxlsx():", "re-use Workbook wb");
   }
   if (length(file) == 0) {
      if (length(wb) == 0) {
         if (verbose) printDebug("writeOpenxlsx():", "createWorkbook");
         wb <- openxlsx::createWorkbook();
      }
   }
   if (length(file) > 0 && length(wb) == 0) {
      if (append && file.exists(file)) {
         ## Load the requested file as a workbook
         if (verbose) printDebug("writeOpenxlsx():", "loadWorkbook");
         wb <- openxlsx::loadWorkbook(file);
      } else {
         if (verbose) printDebug("writeOpenxlsx():", "createWorkbook");
         wb <- openxlsx::createWorkbook();
      }
   }
   ## Set column widths
   if (verbose) printDebug("writeOpenxlsx():",
      "setting options: ",
      c("openxlsx.minWidth", "openxlsx.maxWidth"));
   withr::local_options(list("openxlsx.minWidth"=minWidth,
      "openxlsx.maxWidth"=maxWidth))

   ## Create a sheet for this data.frame
   if (verbose) printDebug("writeOpenxlsx():", "addWorksheet");
   openxlsx::addWorksheet(wb, sheetName);

   ## Write data.frame into the sheet for this workbook
   if (verbose) printDebug("writeOpenxlsx():", "writeDataTable");
   openxlsx::writeDataTable(wb,
      x=x,
      sheet=sheetName,
      startRow=startRow,
      startCol=startCol,
      rowNames=keepRownames,
      withFilter=doFilter,
      bandedRows=FALSE,
      bandedCols=FALSE);

   ## Set base font
   if (verbose) printDebug("writeOpenxlsx():", "modifyBaseFont");
   openxlsx::modifyBaseFont(wb,
      fontSize=fontSize,
      fontColour="grey10",
      fontName=fontName);

   ## Style all borders
   borderStyle <- openxlsx::createStyle(borderColour=borderColor,
      border=borderPosition);
   if (verbose) printDebug("writeOpenxlsx():", "borderStyle");
   openxlsx::addStyle(wb,
      sheet=sheetName,
      style=borderStyle,
      cols=seq_len(ncol(x) + keepRownames) + (startCol - 1),
      rows=seq_len(nrow(x) + 1) + (startRow - 1),
      stack=TRUE,
      gridExpand=TRUE);

   ## Wrap text and set text alignment for header row
   wrapTextStyle <- openxlsx::createStyle(
      wrapText=TRUE,
      valign="top");
   if (wrapHeaders) {
      if (verbose) printDebug("writeOpenxlsx():", "wrapHeaders");
      openxlsx::addStyle(wb,
         sheet=sheetName,
         style=wrapTextStyle,
         cols=seq_len(ncol(x) + keepRownames) + (startCol - 1),
         rows=startRow,
         stack=TRUE,
         gridExpand=TRUE);
   }

   ## Make sure colors are in hex format
   headerColors <- gsub("(#......)..", "\\1",
      rgb2col(grDevices::col2rgb(headerColors)));
   columnColors <- gsub("(#......)..", "\\1",
      rgb2col(grDevices::col2rgb(columnColors)));
   highlightHeaderColors <- gsub("(#......)..", "\\1",
      rgb2col(grDevices::col2rgb(highlightHeaderColors)));
   highlightColors <- gsub("(#......)..", "\\1",
      rgb2col(grDevices::col2rgb(highlightColors)));
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
   basicColumns <- setdiff(
      seq_len(ncol(x) + keepRownames) + (startCol - 1),
      highlightColumns + keepRownames + (startCol - 1));
   if (length(basicColumns) > 0) {
      ## Header styles
      basicAHStyle <- openxlsx::createStyle(#bgFill=head(headerColors, 1),
         fgFill=head(headerColors, 1),
         wrapText=wrapHeaders,
         fontColour="black");
      basicBHStyle <- openxlsx::createStyle(#bgFill=tail(headerColors, 1),
         fgFill=tail(headerColors, 1),
         wrapText=wrapHeaders,
         fontColour="black");
      ## Cell styles
      basicACStyle <- openxlsx::createStyle(#bgFill=head(columnColors, 1),
         valign="top",
         wrapText=wrapCells,
         fgFill=head(columnColors, 1));
      basicBCStyle <- openxlsx::createStyle(#bgFill=tail(columnColors, 1),
         valign="top",
         wrapText=wrapCells,
         fgFill=tail(columnColors, 1));
      basicColumnsA <- basicColumns[basicColumns %% 2 == 1];
      basicColumnsB <- basicColumns[basicColumns %% 2 == 0];
      if (verbose) printDebug("writeOpenxlsx():", "basicStyles");
      if (verbose) printDebug("writeOpenxlsx():", "basicColumnsA:", basicColumnsA);
      if (verbose) printDebug("writeOpenxlsx():", "basicColumnsB:", basicColumnsB);
      if (length(basicColumnsA) > 0) {
         openxlsx::addStyle(wb,
            sheet=sheetName,
            style=basicAHStyle,
            cols=basicColumnsA,
            rows=startRow,
            stack=TRUE,
            gridExpand=TRUE);
         openxlsx::addStyle(wb,
            sheet=sheetName,
            style=basicACStyle,
            cols=basicColumnsA,
            rows=seq_len(nrow(x)) + startRow,
            stack=TRUE,
            gridExpand=TRUE);
      }
      if (length(basicColumnsB) > 0) {
         openxlsx::addStyle(wb,
            sheet=sheetName,
            style=basicBHStyle,
            cols=basicColumnsB,
            rows=startRow,
            stack=TRUE,
            gridExpand=TRUE);
         openxlsx::addStyle(wb,
            sheet=sheetName,
            style=basicBCStyle,
            cols=basicColumnsB,
            rows=seq_len(nrow(x)) + startRow,
            stack=TRUE,
            gridExpand=TRUE);
      }
   }


   ## Style highlightColumns A and B
   if (length(highlightColumns) > 0) {
      if (verbose) printDebug("writeOpenxlsx():", "Applying highlightStyles");
      highAHStyle <- openxlsx::createStyle(#bgFill=head(highlightHeaderColors, 1),
         valign="top",
         wrapText=wrapHeaders,
         fgFill=head(highlightHeaderColors, 1),
         textDecoration="bold",
         fontColour="navy");
      highBHStyle <- openxlsx::createStyle(#bgFill=tail(highlightHeaderColors, 1),
         valign="top",
         wrapText=wrapHeaders,
         fgFill=tail(highlightHeaderColors, 1),
         textDecoration="bold",
         fontColour="navy");
      highACStyle <- openxlsx::createStyle(#bgFill=head(highlightColors, 1),
         valign="top",
         wrapText=wrapCells,
         fgFill=head(highlightColors, 1),
         textDecoration="bold",
         fontColour="navy");
      highBCStyle <- openxlsx::createStyle(#bgFill=tail(highlightColors, 1),
         valign="top",
         wrapText=wrapCells,
         fgFill=tail(highlightColors, 1),
         textDecoration="bold",
         fontColour="navy");
      highlightColumnsA <- highlightColumns[highlightColumns %% 2 == 1];
      highlightColumnsB <- highlightColumns[highlightColumns %% 2 == 0];
      if (length(highlightColumnsA) > 0) {
         openxlsx::addStyle(wb,
            sheet=sheetName,
            style=highAHStyle,
            cols=highlightColumnsA + keepRownames + (startCol - 1),
            rows=startRow,
            stack=TRUE,
            gridExpand=TRUE);
         openxlsx::addStyle(wb,
            sheet=sheetName,
            style=highACStyle,
            cols=highlightColumnsA + keepRownames + (startCol - 1),
            rows=seq_len(nrow(x)) + startRow,
            stack=TRUE,
            gridExpand=TRUE);
      }
      if (length(highlightColumnsB) > 0) {
         openxlsx::addStyle(wb,
            sheet=sheetName,
            style=highBHStyle,
            cols=highlightColumnsB + keepRownames + (startCol - 1),
            rows=startRow,
            stack=TRUE,
            gridExpand=TRUE);
         openxlsx::addStyle(wb,
            sheet=sheetName,
            style=highBCStyle,
            cols=highlightColumnsB + keepRownames + (startCol - 1),
            rows=seq_len(nrow(x)) + startRow,
            stack=TRUE,
            gridExpand=TRUE);
      }
   }

   ## Style integer fields
   if (length(intColumns) > 0) {
      if (verbose) printDebug("writeOpenxlsx():", "Applying intStyles");
      intStyle1 <- openxlsx::createStyle(numFmt=intFormat, halign="right");
      openxlsx::addStyle(wb,
         sheet=sheetName,
         style=intStyle1,
         cols=intColumns + keepRownames + (startCol - 1),
         rows=seq_len(nrow(x)) + startRow,
         stack=TRUE,
         gridExpand=TRUE);
   }

   ## Style numeric fields
   if (length(numColumns) > 0) {
      if (verbose) printDebug("writeOpenxlsx():", "Applying numStyles");
      numStyle1 <- openxlsx::createStyle(numFmt=numFormat, halign="right");
      openxlsx::addStyle(wb,
         sheet=sheetName,
         style=numStyle1,
         cols=numColumns + keepRownames + (startCol - 1),
         rows=seq_len(nrow(x)) + startRow,
         stack=TRUE,
         gridExpand=TRUE);
   }

   ## Style FC fields
   if (length(fcColumns) > 0) {
      if (verbose) printDebug("writeOpenxlsx():", "Applying fcStyles");
      fcStyle1 <- openxlsx::createStyle(numFmt=fcFormat, halign="right");
      openxlsx::addStyle(wb,
         sheet=sheetName,
         style=fcStyle1,
         cols=fcColumns + keepRownames + (startCol - 1),
         rows=seq_len(nrow(x)) + startRow,
         stack=TRUE,
         gridExpand=TRUE);
   }

   ## Style LFC fields
   if (length(lfcColumns) > 0) {
      if (verbose) printDebug("writeOpenxlsx():", "Applying lfcStyles");
      lfcStyle1 <- openxlsx::createStyle(numFmt=lfcFormat, halign="right");
      openxlsx::addStyle(wb,
         sheet=sheetName,
         style=lfcStyle1,
         cols=lfcColumns + keepRownames + (startCol - 1),
         rows=seq_len(nrow(x)) + startRow,
         stack=TRUE,
         gridExpand=TRUE);
   }

   ## Style hit fields
   if (length(hitColumns) > 0) {
      if (verbose) printDebug("writeOpenxlsx():", "Applying hitStyles");
      hitStyle1 <- openxlsx::createStyle(numFmt=hitFormat, halign="right");
      openxlsx::addStyle(wb,
         sheet=sheetName,
         style=hitStyle1,
         cols=hitColumns + keepRownames + (startCol - 1),
         rows=seq_len(nrow(x)) + startRow,
         stack=TRUE,
         gridExpand=TRUE);
   }

   ## Style P-value fields
   if (length(pvalueColumns) > 0) {
      if (verbose) {
         printDebug("writeOpenxlsx():",
            "Applying pvalueStyles with pvalueFormat:",
            pvalueFormat);
      }
      pvalueStyle1 <- openxlsx::createStyle(numFmt=pvalueFormat, halign="right");
      openxlsx::addStyle(wb,
         sheet=sheetName,
         style=pvalueStyle1,
         cols=pvalueColumns + keepRownames + (startCol - 1),
         rows=seq_len(nrow(x)) + startRow,
         stack=TRUE,
         gridExpand=TRUE);
   }

   ## Set column widths
   if (TRUE %in% autoWidth && length(colWidths) == 0) {
      if (verbose) {
         printDebug("writeOpenxlsx():",
            "autoWidth=TRUE");
      }
      openxlsx::setColWidths(wb,
         sheetName,
         cols=seq_len(ncol(x) + keepRownames) + (startCol - 1),
         widths=rep(c("auto"), length.out=ncol(x) + keepRownames));
   } else if (length(colWidths) > 0) {
      colWidths <- rep(colWidths,
         length.out=ncol(x) + keepRownames);
      if (verbose) {
         printDebug("writeOpenxlsx():",
            "set_xlsx_colwidths():",
            head(colWidths, 50));
      }
      wb <- set_xlsx_colwidths(wb,
         sheet=sheetName,
         cols=seq_len(ncol(x) + keepRownames) + (startCol - 1),
         widths=colWidths)
   } else {
      # without autoWidth=TRUE, and no colWidths,
      # set minWidth and nothing else
      if (verbose) {
         printDebug("writeOpenxlsx():",
            "autoWidth=", "FALSE", ", using minWidth=",
            minWidth);
      }
      openxlsx::setColWidths(wb,
         sheetName,
         cols=seq_len(ncol(x) + keepRownames) + (startCol - 1),
         widths=rep(minWidth, length.out=ncol(x) + keepRownames));
   }

   ## Apply freeze panes
   freezePaneRow <- head(freezePaneRow, 1);
   freezePaneColumn <- head(freezePaneColumn, 1);
   if (freezePaneRow >= 1 || freezePaneColumn >= 1) {
      if (freezePaneRow == 0) {
         firstActiveRow <- NULL;
      } else {
         firstActiveRow <- freezePaneRow + (startRow - 1)
      }
      if (freezePaneColumn == 0) {
         firstActiveCol <- NULL;
      } else {
         firstActiveCol <- freezePaneColumn + keepRownames + (startCol - 1)
      }
      if (verbose) {
         desc_row <- "";
         if (length(firstActiveRow) > 0 &&
               !any(firstActiveRow == freezePaneRow)) {
            desc_row <- c("(applied to:", firstActiveRow, ")");
         }
         desc_col <- "";
         if (length(firstActiveCol) > 0 &&
            !any(firstActiveCol == freezePaneColumn)) {
            desc_col <- c("(applied to:", firstActiveCol, ")");
         }
         printDebug("writeOpenxlsx():",
            "Applying freezePaneRow:", freezePaneRow,
            desc_row,
            ";", " freezePaneColumn:", freezePaneColumn,
            desc_col,
            sep="")
      }
      openxlsx::freezePane(wb,
         sheetName,
         firstActiveRow=firstActiveRow,
         firstActiveCol=firstActiveCol);
   }

   ## Add header filter
   # if (doFilter && 1 == 2) {
   #    if (verbose) printDebug("writeOpenxlsx():", "Applying addFilter");
   #    openxlsx::addFilter(wb,
   #       sheetName,
   #       rows=startRow,
   #       cols=seq_len(ncol(x) + keepRownames) + (startCol - 1));
   # }

   ## Adjust header row height
   if (headerRowMultiplier > 1) {
      if (verbose) {
         printDebug("writeOpenxlsx():",
            "Applying headerRowMultiplier");
      }
      openxlsx::setRowHeights(wb,
         sheetName,
         rows=startRow,
         heights=15 * headerRowMultiplier);
   }

   ## Optionally apply conditional column formatting
   if (doConditional &&
         length(c(numColumns,
            fcColumns,
            lfcColumns,
            intColumns,
            hitColumns)) > 0) {
      #
      if (verbose) {
         printDebug("writeOpenxlsx():",
            "calling applyXlsxConditionalFormat, startRow:",
            startRow+1);
      }
      wb <- applyXlsxConditionalFormat(xlsxFile=wb,
         sheet=sheetName,
         startRow=startRow + 1,
         fcColumns=fcColumns + keepRownames + (startCol - 1),
         lfcColumns=lfcColumns + keepRownames + (startCol - 1),
         numColumns=numColumns + keepRownames + (startCol - 1),
         intColumns=intColumns + keepRownames + (startCol - 1),
         hitColumns=hitColumns + keepRownames + (startCol - 1),
         pvalueColumns=pvalueColumns + keepRownames + (startCol - 1),
         fcStyle=fcStyle, fcRule=fcRule,
         lfcStyle=lfcStyle, lfcRule=lfcRule,
         hitStyle=hitStyle, hitRule=hitRule,
         intStyle=intStyle, intRule=intRule,
         numStyle=numStyle, numRule=numRule,
         pvalueStyle=pvalueStyle, pvalueRule=pvalueRule,
         verbose=verbose,
         ...);
   }

   ## Optionally apply categorical column formatting
   if (doCategorical && !is.null(colorSub)) {
      if (is.atomic(colorSub)) {
         # apply only to text/character columns
         applyColumns <- setdiff(
            seq_len(ncol(x) + keepRownames) + (startCol - 1),
            c(numColumns,
               fcColumns,
               lfcColumns,
               intColumns,
               hitColumns,
               pvalueColumns) + keepRownames + (startCol - 1));
      } else if (is.list(colorSub)) {
         applyColumns <- seq_len(ncol(x) + keepRownames) + (startCol - 1)
      }
      ## Apply categorical colors to text columns
      if (length(applyColumns) > 0) {
         #
         if (verbose) {
            printDebug("writeOpenxlsx():",
               "applyXlsxCategoricalFormat applyColumns:",
               applyColumns,
               ", startRow:", startRow,
               ", rowRange:", range(seq_len(nrow(x))));
         }
         wb <- applyXlsxCategoricalFormat(xlsxFile=wb,
            sheet=sheetName,
            rowRange=seq_len(nrow(x) + 1) + (startRow - 1),
            # startRow=startRow + 1,
            # rowRange=startRow,
            colRange=applyColumns,
            colorSub=colorSub,
            wrapText=wrapCells,
            ...);
      }
      ## Apply categorical colors to column headers
      colorSub_headers <- NULL;
      if (is.atomic(colorSub) && any(colnames(x) %in% names(colorSub))) {
         colorSub_headers <- colorSub;
      } else if (is.list(colorSub) &&
            "colnames" %in% names(colorSub) &&
            any(colnames(x) %in% names(colorSub[["colnames"]]))) {
         colorSub_headers <- colorSub[["colnames"]];
      }
      if (length(colorSub_headers) > 0) {
         colRange <- which(colnames(x) %in% names(colorSub_headers));
         if (verbose) {
            printDebug("writeOpenxlsx(): ",
               "applyXlsxCategoricalFormat to colRange:",
               colRange + keepRownames + (startCol - 1));
         }
         wb <- applyXlsxCategoricalFormat(xlsxFile=wb,
            sheet=sheetName,
            rowRange=startRow,
            colRange=colRange + keepRownames + (startCol - 1),
            colorSub=colorSub_headers,
            wrapText=wrapCells,
            verbose=verbose,
            ...);
      }
   }

   ## Write the .xlsx file here
   if (length(file) > 0) {
      if (verbose) {
         printDebug("writeOpenxlsx(): ",
            "saveWorkbook");
      }
      openxlsx::saveWorkbook(wb,
         file,
         overwrite=TRUE);
   }

   return(invisible(wb));
}


#' Xlsx Conditional formatting
#'
#' Xlsx Conditional formatting
#'
#' This function is a convenient wrapper for applying conditional formatting
#' to 'Excel' 'xlsx' worksheets, with reasonable settings for commonly used
#' data types.
#'
#' Note that this function does not apply cell formatting, such as numeric
#' formatting as displayed in 'Excel'.
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
#' which are used by `provigrep()` to match to colnames. The Grep
#' method is particularly useful when applying conditional formatting for
#' multiple worksheets in the same 'xlsx' file, where the colnames are not
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
#' work, since only the first rule will be applied by 'Microsoft Excel'.
#' Interestingly, if multiple conditional rules are applied to the same
#' cell, they will be visible in order inside the 'Microsoft Excel'
#' application.
#'
#' @family jam export functions
#'
#' @returns `Workbook` object as defined by the `openxlsx` package
#'    is returned invisibly with `invisible()`. This `Workbook`
#'    can be used in argument `wb` to provide a speed boost when
#'    saving multiple sheets to the same file.
#'
#' @param xlsxFile `character` filename to a file with ".xlsx" extension,
#'    or `Workbook` object defined in the `openxlsx` package. When
#'    `xlsxFile` is a `Workbook` the output is not saved to a file.
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
#'    see `openxlsx::conditionalFormatting()`.
#' @param verbose logical indicating whether to print verbose output.
#' @param startRow integer indicating which row to begin applying conditional
#'    formatting. In most cases startRow=2, which allows one row for column
#'    headers. However, if there are multiple header rows, startRow should be
#'    1 more than the number of header rows.
#' @param overwrite logical indicating whether the original 'Excel' files will
#'    be replaced with the new one, or whether a new file will be created.
#' @param ... additional parameters are ignored.
#'
#' @examples
#' # write to tempfile for examples
#' if (check_pkg_installed("openxlsx")) {
#'    out_xlsx <- tempfile(pattern="writeOpenxlsx_", fileext=".xlsx")
#'    df <- data.frame(a=LETTERS[1:5], b=1:5);
#'    writeOpenxlsx(x=df,
#'       file=out_xlsx,
#'       sheetName="jamba_test");
#'
#'    applyXlsxConditionalFormat(out_xlsx,
#'       sheet="jamba_test",
#'       intColumns=2,
#'       intRule=c(0,3,5),
#'       intStyle=c("#FFFFFF", "#1E90FF", "#9932CC")
#'    )
#' }
#' @export
applyXlsxConditionalFormat <- function
(xlsxFile,
 sheet=1,
 ## fold change
 fcColumns=NULL,
 fcGrep=NULL,
 ## blue-white-red color scale
 fcStyle=c("#4F81BD", "#EEECE1", "#C0504D"),
 fcRule=c(-6,0,6),
 fcType="colourScale",
 ## log fold change
 lfcColumns=NULL,
 lfcGrep=NULL,
 ## blue-white-red color scale
 lfcStyle=c("#4F81BD", "#EEECE1", "#C0504D"),
 lfcRule=c(-3, 0, 3),
 lfcType="colourScale",
 ## Hit numeric, usually integer
 hitColumns=NULL,
 hitGrep=NULL,
 ## blue-white-red color scale
 hitStyle=c("#4F81BD", "#EEECE1", "#C0504D"),
 hitRule=c(-1.5, 0, 1.5),
 hitType="colourScale",
 ## Integer numbers
 intColumns=NULL,
 intGrep=NULL,
 ## white-orange-red color scale
 intStyle=c("#EEECE1", "#FDC99B", "#F77F30"),
 intRule=c(0, 100, 10000),
 intType="colourScale",
 ## Decimal numbers
 numColumns=NULL,
 numGrep=NULL,
 ## white-purple-navy color scale
 numStyle=c("#F2F0F7","#B4B1D4","#938EC2"),
 numRule=c(1, 10, 20),
 numType="colourScale",
 ## P-values
 pvalueColumns=NULL,
 pvalueGrep=NULL,
 ## red-orange-white color scale
 pvalueStyle=c("#F77F30","#FDC99B","#EEECE1"),
 pvalueRule=c(0, 0.01, 0.05),
 pvalueType="colourScale",
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
   if (!requireNamespace("openxlsx", quietly=TRUE)) {
      stop("The openxlsx package is required for applyXlsxConditionalFormat().");
   }

   ## Load the requested file as a workbook
   if ("Workbook" %in% class(xlsxFile)) {
      wb <- xlsxFile;
   } else {
      wb <- openxlsx::loadWorkbook(xlsxFile);
   }


   ## Wrap in a large loop to handle multiple worksheets
   sheets <- sheet;
   for (sheet in sheets) {

      ## Load the data.frame
      colNames <- TRUE;
      startRowLoad <- startRow - 1;
      if (startRow == 1) {
         startRowLoad <- 1;
         colNames <- FALSE;
      }
      df <- openxlsx::readWorkbook(wb,
         sheet=sheet,
         startRow=startRowLoad,
         colNames=colNames,
         ...);
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

      # define sequence of rows to apply conditional formatting
      rows_seq <- seq_len(nrowDf) + (startRow - 1);

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
               rows=rows_seq,
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
               rows=rows_seq,
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
               rows=rows_seq,
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
               rows=rows_seq,
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
               rows=rows_seq,
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
               rows=rows_seq,
               style=pvalueStyle,
               rule=pvalueRule,
               type=pvalueType);
         }
      }

   }

   ## Save the updated workbook
   if (!"Workbook" %in% class(xlsxFile)) {
      if (!overwrite) {
         xlsxFile <- gsub("([.]xls[x]*)$", paste0("_", getDate(), "\\1"), xlsxFile);
      }
      openxlsx::saveWorkbook(wb, xlsxFile, overwrite=TRUE);
   }
   invisible(wb);
}

#' Add categorical colors to 'Excel' 'xlsx' worksheets
#'
#' Add categorical colors to 'Excel' 'xlsx' worksheets
#'
#' This function is a convenient wrapper for applying categorical
#' color formatting to cell background colors, and applies a contrasting
#' color to the text in cells using `setTextContrastColor()`.
#' It uses a named character vector of colors supplied as `colorSub`
#' to define cell background colors, and optionally `colorSubText`
#' to define a specific color for the cell text.
#'
#' @family jam export functions
#'
#' @returns `Workbook` object as defined by the `openxlsx` package
#'    is returned invisibly with `invisible()`. This `Workbook`
#'    can be used in argument `wb` to provide a speed boost when
#'    saving multiple sheets to the same file.
#'
#' @param xlsxFile `character` filename to a file with ".xlsx" extension,
#'    or `Workbook` object defined in the `openxlsx` package. When
#'    `xlsxFile` is a `Workbook` the output is not saved to a file.
#' @param sheet `integer` index of the worksheet or worksheets.
#' @param rowRange,colRange `integer` vectors of rows and columns
#'    to apply categorical colors in the 'Excel' 'xlsx' worksheet,
#'    passed as `openxlsx::readWorkbook(..., rows=rowRange, cols=colRange)`.
#'    This step defines which columns are read from each workbook,
#'    however when `colorSub` is provided as a `list` whose names
#'    are intended to match `colnames()`, only matching colnames
#'    are processed.
#' @param colorSub one of the following types of input:
#'    * Named `character` vector of valid R colors, whose
#'    names correspond to values in worksheet cells.
#'    * Named `list` whose names correspond to colnames one or more
#'    workbooks in `sheet`. Each list element should be a `character`
#'    vector named by column values, or color `function` that takes
#'    column values and returns a `character` vector of colors for
#'    each value.
#' @param colorSubText optional `character` vector of colors, whose
#'    names correspond to values in the worksheet cells. In
#'    absence of a specific text color, `setTextContrastColor()`
#'    is used to define a contrasting text color to be visible on
#'    the colored background.
#' @param trimCatNames `logical` whether to trim whitespace and punctuation
#'    from `colorSub` and from 'Excel' cell fields before matching colors
#'    to 'Excel' values.
#' @param overwrite `logical` indicating whether new cell color styles
#'    should be forced overwrite of previous cell styles.
#' @param wrapText `logical` indicating whether to wrap text.
#' @param stack `logical` indicating whether new color rules should be
#'    applied above existing styles, many of whose styles may not affect
#'    the specific cell color, for example the font size and font name.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' # write to tempfile for examples
#' if (check_pkg_installed("openxlsx")) {
#'    out_xlsx <- tempfile(pattern="writeOpenxlsx_", fileext=".xlsx")
#'    df <- data.frame(a=LETTERS[1:5], b=1:5);
#'    writeOpenxlsx(x=df,
#'       file=out_xlsx,
#'       sheetName="jamba_test");
#'
#'    colorSub <- nameVector(
#'       rainbow2(5, s=c(0.8, 1), v=c(0.8, 1)),
#'       LETTERS[1:5]);
#'    applyXlsxCategoricalFormat(out_xlsx,
#'       sheet="jamba_test",
#'       colorSub=colorSub
#'    )
#' }
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
 wrapText=FALSE,
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
   if (!check_pkg_installed("openxlsx")) {
      stop("The openxlsx package is required for applyXlsxCategoricalFormat().");
   }
   retVals <- list();

   ## Load the requested file as a workbook
   if ("Workbook" %in% class(xlsxFile)) {
      wb <- xlsxFile;
   } else {
      wb <- openxlsx::loadWorkbook(xlsxFile);
   }

   # prepare colorSub with Excel-friendly names
   prep_xlsx_color_vector <- function
   (x,
    changefrom="[-_() ]+",
    changeto=".")
   {
      if (length(changefrom) != 1 || nchar(changefrom[1]) == 0) {
         return(x)
      }
      has_changefrom <- grep(changefrom, names(x));
      if (any(has_changefrom)) {
         x2 <- x[has_changefrom];
         names(x2) <- gsub(changefrom,
            changeto,
            names(x2));
         x <- c(x, x2);
      }
      if (any(duplicated(names(x)))) {
         x <- x[!duplicated(names(x))]
      }
      return(x)
   }
   # same as above but operates on values and does not make unique
   prep_xlsx_match_value <- function
   (x,
    changefrom="[-_() ]+",
    changeto=".")
   {
      if (length(changefrom) != 1 || nchar(changefrom[1]) == 0) {
         return(x)
      }
      has_changefrom <- grep(changefrom, x);
      if (any(has_changefrom)) {
         x[has_changefrom] <- gsub(changefrom,
            changeto,
            x[has_changefrom]);
      }
      return(x)
   }

   # fix colorSub names
   if (is.atomic(colorSub) && length(names(colorSub)) > 0) {
      colorSub <- prep_xlsx_color_vector(colorSub)
   }

   ## Internal custom wrapper function to create xlsx styles
   ## returns `list` named by the primary fill color of each cell,
   ##    containing a `openxlsx::style` object suitable for use
   ##    by `openxlsx::addStyle()`.
   create_matching_styles <- function
   (colorSub,
    colorSubText=NULL,
    colorSubFound,
    verbose=FALSE,
    wrapText=FALSE,
    ...)
   {
      if (length(colorSub) == 0) {
         return(NULL)
      }
      colorSubValues <- unalpha(colorSub[colorSubFound])
      if (length(colorSubValues) == 0) {
         return(NULL)
      }
      if (length(names(colorSubValues)) == 0) {
         names(colorSubValues) <- makeNames(colorSubValues);
      }
      if (length(colorSubText) == 0 || length(names(colorSubText)) == 0) {
         colorSubTextValues <- setTextContrastColor(colorSubValues,
            ...)
         names(colorSubTextValues) <- names(colorSubValues)
      } else {
         colorSubTextValues <- rmNA(colorSubText[names(colorSubValues)],
            naValue="#000000")
         names(colorSubTextValues) <- names(colorSubValues)
      }
      colorSubStyles <- lapply(nameVector(unique(colorSubValues)), function(ic) {
         ## convert to hex while also removing all alpha transparency
         bgColor <- unalpha(ic);
         ict <- colorSubTextValues[match(ic, colorSubValues)]
         fgColor <- unalpha(ict);
         # optional output
         if (verbose) {
            printDebug("Adding style: ",
               paste0(" fontColour:", fgColor),
               fgText=c("darkorange2", fgColor),
               bgText=c(NA, bgColor));
         }
         openxlsx::createStyle(fontColour=fgColor,
            bgFill=bgColor,
            fgFill=bgColor,
            wrapText=wrapText);
      });
      colorSubStyles
   }
   ## Internal function to get row/cell coordinates for each unique color
   ## returns `list` named by style, by default the hex color with no alpha
   ##    where each element is a `matrix` with colnames `row` and `col`
   ##    indicating individual row/column coordinate positions.
   define_matching_style_cells <- function
   (rangeMatch)
   {
      # unique colors
      # assumes character matrix with colors or "" absent of color
      # colors should already be unalpha but is repeated to confirm
      column_seq <- seq_len(ncol(rangeMatch));
      unique_colors <- unalpha(setdiff(rangeMatch, c("", NA)))

      unique_colors_set <- nameVector(unique_colors)
      catRes <- lapply(unique_colors_set, function(icolor){
         # iterate each column and identify matching rows, columns
         catColRes <- rbindList(rmNULL(lapply(column_seq, function(icolnum){
            irow <- which(rangeMatch[, icolnum] %in% icolor)
            data.frame(row=irow,
               col=rep(icolnum, length.out=length(irow)))
            # style_name=rep(icolor, length.out=length(irow)))
         })));
      })
      return(catRes)
   }
   ## Internal function to apply styles to cell coordinates
   ## param style_coord_list `list` named by unique hex R colors,
   ##    containing `matrix` objects with colnames `c("row", "col")`
   ##    with individual cell positions to apply the corresponding style
   ## param styles_list `list` named by hex R color, containing
   ##    `openxlsx::style` objects using the corresponding color as
   ##    the background fill color.
   ## param wb,sheet,stack arguments passed to `openxlsx::addStyle()`
   ## param verbose `logical` indicating whether to print verbose output.
   ##
   ## returns `NULL`, this function is called for the by-product
   ##    of adjusting
   apply_matching_styles <- function
   (style_coord_list,
    styles_list,
    wb,
    colRange,
    sheet,
    stack=TRUE,
    verbose=FALSE,
    ...)
   {
      #
      # addRes <- lapply(nameVector(seq_along(style_coord_list), names(style_coord_list)), function(i) {
      addRes <- lapply(nameVectorN(style_coord_list), function(i) {
         iStyle <- styles_list[[i]];
         if (verbose > 1) {
            printDebug("iStyle:");print(iStyle);
         }
         style_coords <- style_coord_list[[i]];
         if (length(style_coords) == 0 || nrow(style_coords) == 0) {
            return(NULL)
         }
         doRow <- style_coords[,"row"];
         doCol <- style_coords[,"col"];
         doCol <- colRange[style_coords[,"col"]];
         if (verbose) {
            printDebug("     openxlsx::addStyle()",
               " in vectorized mode for style: ",
               i,
               bgText=c(NA, NA, i),
               fgText=c("darkorange2",
                  "deepskyblue2",
                  setTextContrastColor(i)));
         }
         openxlsx::addStyle(wb,
            sheet=sheet,
            style=iStyle,
            rows=doRow,
            cols=doCol,
            stack=stack,
            gridExpand=FALSE);
      });
      return(invisible(addRes))
   }



   ## Wrap in a large loop to handle multiple worksheets
   sheets <- sheet;
   wb_changed <- FALSE;
   if (verbose) {
      printDebug("applyXlsxCategoricalFormat(): ",
         "range(rowRange):", range(rowRange),
         ", colRange:", colRange);
   }
   for (sheet in sheets) {

      ## Load the data.frame
      ## Load the data.frame
      colNames <- FALSE;
      df <- openxlsx::readWorkbook(wb,
         sheet=sheet,
         cols=colRange,
         rows=rowRange,
         skipEmptyCols=TRUE,
         skipEmptyRows=FALSE,
         colNames=FALSE,
         ...);
      if (verbose) {
         printDebug("applyXlsxCategoricalFormat(): ",
            "head(df, 50):");
         print(head(df, 50));
      }
      nrowDf <- nrow(df) + 1;
      if (is.null(rowRange)) {
         rowRange <- seq_len(nrow(df));
      }
      # 0.0.99.900 - data is only loaded for colRange
      # so we should always use seq_len(ncol(df))
      if (length(colRange) == 0) {
         colRange <- seq_len(ncol(df));
      }
      if (verbose) {
         printDebug("head(rowRange):", head(rowRange));
         if (length(rowRange) > 10) printDebug("tail(rowRange):", tail(rowRange));
         printDebug("head(colRange):", head(colRange));
         if (length(colRange) > 10) printDebug("tail(colRange):", tail(colRange));
      }

      ############################################################
      # Processing list colorSub
      if (is.list(colorSub) && length(names(colorSub)) > 0) {
         # edit names to Excel-friendly format
         df_names <- prep_xlsx_match_value(unname(unlist(df[1, ])))
         # edit names(colorSub) to Excel-friendly format
         # Note that duplicated names(colorSub) are not supported
         names(colorSub) <- prep_xlsx_match_value(names(colorSub))
         df_match <- which(df_names %in% names(colorSub));
         # # if any colnames match, process color assignments
         if (any(df_match)) {
            wb_changed <- TRUE;
            dfcol_colors_list <- lapply(df_match, function(dfcol) {
               df_name <- df_names[dfcol];
               icolors <- colorSub[[df_name]];
               if (is.function(icolors)) {
                  df_values <- c(NA,
                     utils::read.table(text=df[[dfcol]][-1],
                        stringsAsFactors=FALSE)[[1]])
                  dfcol_colors <- rmNA(unalpha(icolors(df_values), keepNA=TRUE),
                     naValue="")
               } else {
                  dfcol_values <- prep_xlsx_match_value(df[[dfcol]])
                  dfcol_colors <- ifelse(dfcol_values %in% names(icolors),
                     unalpha(icolors[dfcol_values],
                        keepNA=TRUE),
                     "")
               }
               dfcol_colors
            })
            dfcol_colors <- do.call(cbind, dfcol_colors_list);
            rangeMatch <- matrix(ncol=ncol(df), nrow=nrow(df), data="");
            rangeMatch[, df_match] <- dfcol_colors
            retVals$rangeMatch <- rangeMatch;

            # call method to create styles for matching colors
            # Note: potential issue when document contains zillion colors,
            # it will create zillion styles. Unclear how common or how
            # big an issue it might be when it happens.
            # Could detect length(unique_colors) and react accordingly.
            unique_colors <- unalpha(setdiff(rangeMatch, c("", NA)))
            styles_list <- create_matching_styles(colorSub=unique_colors,
               colorSubText=NULL,
               colorSubFound=TRUE,
               verbose=verbose,
               wrapText=wrapText)

            # call method to determine cell coordinates for each style
            style_coord_list <- define_matching_style_cells(rangeMatch)
            retVals$style_coord_list <- style_coord_list;

            # call method to update colors in each relevant cell
            addRes <- apply_matching_styles(style_coord_list=style_coord_list,
               styles_list=styles_list,
               wb=wb,
               colRange=colRange,
               sheet=sheet,
               stack=stack,
               verbose=verbose)


         }
      }
      ############################################################
      # Processing atomic colorSub
      if (is.atomic(colorSub)) {
         rangeM <- as.matrix(df);
         colnames(rangeM) <- colRange;
         rownames(rangeM) <- rowRange;
         ## Allow colorizing NA entries
         if (any(is.na(rangeM))) {
            rangeM[is.na(rangeM)] <- "NA";
         }

         ## Optionally clean up some cell fields before matching
         if (trimCatNames) {
            rangeM[] <- prep_xlsx_match_value(rangeM)
         }

         ## Identify which colorSub entries are found
         #colorSubText <- setTextContrastColor(colorSub);
         if (verbose) {
            printDebug("head(unique(as.vector(rangeM))):",
               head(unique(as.vector(rangeM))));
            print(head(unique(as.vector(rangeM))));
         }
         colorSubFound <- which(
            names(colorSub) %in% rangeM);
         if (length(colorSubFound) > 0) {
            wb_changed <- TRUE;
            if (verbose) {
               printDebug("found:",
                  names(colorSub)[colorSubFound])
            };
            # 0.0.97.900: create unique styles, one per unique color
            colorSubStyles <- create_matching_styles(colorSub=colorSub,
               colorSubText=colorSubText,
               colorSubFound=colorSubFound,
               verbose=verbose,
               wrapText=wrapText)
            if (verbose) {
               printDebug("   completed adding color styles");
            }
            rangeMatch <- rangeM;
            rangeMatch[] <- "";
            # 0.0.96.900: updated logic for color assignment to cells
            rangeMatch[] <- ifelse(rangeM %in% names(colorSub),
               rmNA(unalpha(colorSub[rangeM]),
                  naValue=""),
               "")
            retVals$rangeMatch <- rangeMatch;

            # Apply colorSub formatting
            # Determine each row,column cell coordinate with a color substitution
            if (verbose) {
               printDebug("   Determining color cell coordinates.");
            }
            # 0.0.96.900: simplify method to obtain coordinates for each style
            style_coord_list <- define_matching_style_cells(rangeMatch)
            retVals$style_coord_list <- style_coord_list;
            if (verbose) {
               printDebug("   Applying colorSub formatting with addStyle()");
               printDebug("   length(style_coord_list): ",
                  formatInt(length(style_coord_list)));
            }
            # 0.0.96.900: simplify method to apply styles to cell coordinates
            addRes <- apply_matching_styles(style_coord_list=style_coord_list,
               styles_list=colorSubStyles,
               wb=wb,
               colRange=colRange,
               sheet=sheet,
               stack=stack,
               verbose=verbose)
         }
      }
   }

   ## Save the updated workbook
   if (!"Workbook" %in% class(xlsxFile)) {
      if (wb_changed) {
         if (!overwrite) {
            xlsxFile <- gsub("([.]xls[x]*)$", paste0("_", getDate(), "\\1"), xlsxFile);
         }
         if (verbose) {
            printDebug("Calling openxlsx::saveWorkbook().");
         }
         openxlsx::saveWorkbook(wb, xlsxFile, overwrite=TRUE);
      }
   }
   invisible(wb);
}

#' Set column widths in Xlsx files
#'
#' Set column widths in Xlsx files
#'
#' This function is a light wrapper to perform these steps
#' from the very useful `openxlsx` R package:
#'
#' * `openxlsx::loadWorkbook()`
#' * `openxlsx::setColWidths()`
#' * `openxlsx::saveWorkbook()`
#'
#' @family jam export functions
#'
#' @returns `Workbook` object as defined by the `openxlsx` package
#'    is returned invisibly with `invisible()`. This `Workbook`
#'    can be used in argument `wb` to provide a speed boost when
#'    saving multiple sheets to the same file.
#'
#' @param xlsxFile `character` filename to a file with ".xlsx" extension,
#'    or `Workbook` object defined in the `openxlsx` package. When
#'    `xlsxFile` is a `Workbook` the output is not saved to a file.
#' @param sheet `integer` sheet number or `character` sheet name,
#'    passed to `openxlsx::setColWidths()` indicating
#'    the worksheet to affect.
#' @param cols `integer vector` indicating the column numbers to affect.
#' @param widths `numeric vector` indicating the width of each column
#'    defined by `cols`.
#' @param ... additional arguments are passed to `openxlsx::setColWidths()`.
#'
#' @examples
#' # write to tempfile for examples
#' if (check_pkg_installed("openxlsx")) {
#'    out_xlsx <- tempfile(pattern="writeOpenxlsx_", fileext=".xlsx")
#'    df <- data.frame(a=LETTERS[1:5], b=1:5);
#'    writeOpenxlsx(x=df,
#'       file=out_xlsx,
#'       sheetName="jamba_test");
#'
#'    ## By default, cols starts at column 1 and continues to length(widths)
#'    set_xlsx_colwidths(out_xlsx,
#'       sheet="jamba_test",
#'       widths=rep(20, ncol(df))
#'    )
#' }
#'
#' @export
set_xlsx_colwidths <- function
(xlsxFile,
 sheet=1,
 cols=seq_along(widths),
 widths=11,
 ...)
{
   ## Load the requested file as a workbook
   if ("Workbook" %in% class(xlsxFile)) {
      wb <- xlsxFile;
   } else {
      wb <- openxlsx::loadWorkbook(xlsxFile);
   }

   openxlsx::setColWidths(wb,
      sheet=sheet,
      cols=cols,
      widths=widths,
      ...);

   ## Save workbook
   if (!"Workbook" %in% class(xlsxFile)) {
      openxlsx::saveWorkbook(wb,
         xlsxFile,
         overwrite=TRUE);
   }
   invisible(wb);
}

#' Set row heights in Xlsx files
#'
#' This function is a light wrapper to perform these steps
#' from the very useful `openxlsx` R package:
#'
#' * `openxlsx::loadWorkbook()`
#' * `openxlsx::setRowHeights()`
#' * `openxlsx::saveWorkbook()`
#'
#' Note that when only the argument `heights` is defined,
#' the argument `rows` will point to row 2 and lower, thus
#' skipping the first (header) row. Define `rows` specifically
#' in order to affect the header row as well.
#'
#' @family jam export functions
#'
#' @returns `Workbook` object as defined by the `openxlsx` package
#'    is returned invisibly with `invisible()`. This `Workbook`
#'    can be used in argument `wb` to provide a speed boost when
#'    saving multiple sheets to the same file.
#'
#' @param xlsxFile `character` filename to a file with ".xlsx" extension,
#'    or `Workbook` object defined in the `openxlsx` package. When
#'    `xlsxFile` is a `Workbook` the output is not saved to a file.
#' @param sheet `integer` sheet number or `character` sheet name,
#'    passed to `openxlsx::setRowHeights()` indicating
#'    the worksheet to affect.
#' @param rows `integer vector` indicating the row numbers to affect.
#' @param heights `numeric vector` indicating the height of each column
#'    defined by `rows`.
#' @param ... additional arguments are passed to `openxlsx::setRowHeights()`.
#'
#' @examples
#' # write to tempfile for examples
#' if (check_pkg_installed("openxlsx")) {
#'    out_xlsx <- tempfile(pattern="writeOpenxlsx_", fileext=".xlsx")
#'    df <- data.frame(a=LETTERS[1:5], b=1:5);
#'    writeOpenxlsx(x=df,
#'       file=out_xlsx,
#'       sheetName="jamba_test");
#'
#'    ## by default, rows will start at row 2, skipping the header
#'    set_xlsx_rowheights(out_xlsx,
#'       sheet="jamba_test",
#'       heights=rep(17, nrow(df))
#'    )
#'
#'    ## to include the header row
#'    set_xlsx_rowheights(out_xlsx,
#'       sheet="jamba_test",
#'       rows=seq_len(nrow(df)+1),
#'       heights=rep(17, nrow(df)+1)
#'    )
#' }
#'
#' @export
set_xlsx_rowheights <- function
(xlsxFile,
 sheet=1,
 rows=seq_along(heights)+1,
 heights=17,
 ...)
{
   ## Load the requested file as a workbook
   if ("Workbook" %in% class(xlsxFile)) {
      wb <- xlsxFile;
   } else {
      wb <- openxlsx::loadWorkbook(xlsxFile);
   }

   openxlsx::setRowHeights(wb,
      sheet=sheet,
      rows=rows,
      heights=heights);

   ## Save workbook
   if (!"Workbook" %in% class(xlsxFile)) {
      openxlsx::saveWorkbook(wb,
         xlsxFile,
         overwrite=TRUE);
   }
   invisible(wb);
}


#' Import one or more data.frame from 'Excel' 'xlsx' format
#'
#' Import one or more data.frame from 'Excel' 'xlsx' format
#'
#' This function is equivalent to `openxlsx::read.xlsx()`
#' with a few minor additions:
#'
#' 1. It returns a `list` of `data.frame` objects, one per `sheet`.
#' 2. It properly reads the `colnames` with `check.names=FALSE`.
#'
#' By default this function returns every `sheet` for a given
#' `xlsx` file.
#'
#' Some useful details:
#'
#' * Empty columns are not skipped during loading, which means a worksheet
#' whose data starts at column 3 will be returned with two empty columns,
#' followed by data from that worksheet. Similarly, any empty columns
#' in the middle of the data in that worksheet will be included in the
#' output.
#' * When both `startRow` and `rows` are applied, `rows` takes priority
#' and will be used instead of `startRows`. In fact `startRows` will be
#' defined `startRows <- min(rows)` for each relevant worksheet. However,
#' for each worksheet either argument can be `NULL`.
#'
#' @family jam export functions
#'
#' @returns `list` of `data.frame` objects, one per sheet in `xlsx`.
#'
#' @param xlsx `character` path to an 'Excel' file in `xlsx` format,
#'    compatible with `openxlsx::read.xlsx()`.
#' @param sheet one of `NULL`, `character`, or `integer` vector,
#'    where: `sheet=NULL` will import every sheet; `character` is
#'    a vector of sheet names; and `integer` is a vector of sheet
#'    index values. The sheet names are determined with
#'    `openxlsx::getSheetNames()`.
#' @param startRow `integer` indicating the row number to start
#'    importing each `sheet`.
#'    * Note `startRow` can be a vector
#'    with length `length(sheet)`, to specify the `startRow` for
#'    each `sheet`.
#'    * Note `startRow` is ignored when `rows` is defined for the same sheet,
#'    to minimize confusion about using both togetheer.
#' @param rows `integer` vector indicating specific rows to import
#'    for each `sheet`.
#'    * To specify different `rows` for each `sheet`,
#'    supply `rows` as a `list` of `integer` vectors.
#'    * Note that when `rows` is defined for a sheet, it will be used
#'    and `startRow` will be ignored for that same sheet.
#' @param startCol `integer` indicating the first column number to retain
#'    after importing each `sheet`.
#'    * Note `startCol` can be a vector with length `length(sheet)`,
#'    to specify the `startCol` for each `sheet`.
#'    * Note `startCol` is ignored when `cols` is defined for the same sheet,
#'    to minimize confusion about using both togetheer.
#' @param cols `integer` vector indicating specific column numbers to import
#'    for each `sheet`.
#'    * To specify different `cols` for each `sheet`, supply `cols`
#'    as a `list` of `integer` vectors.
#'    * Note that when `cols` is defined for a sheet, it will be used
#'    and `startCol` will be ignored for that same sheet.
#' @param check.names `logical` indicating whether to call `make.names()`
#'    on the `colnames` of each `data.frame`.
#'    * Note that `openxlsx::read.xlsx()` does not honor `check.names=FALSE`,
#'    so a workaround is applied which loads a single line
#'    without column headers, in order to obtain the same data without
#'    mangling column headers. If this process fails, another workaround
#'    is to use `startRow=2` (one higher than previous) and `colNames=FALSE`.
#' @param check_header `logical` indicating whether to test for presence
#'    of header rows, which may be multi-line column headers.
#'    When `check_header=TRUE`, this
#'    method simply tests for the presence of rows that have `ncol`
#'    different than the remaining rows of data in the given sheet.
#'    When header rows are detected, the values are assigned to column
#'    `dimnames` of the `data.frame`.
#' @param check_header_n `integer` number of rows to test for header rows,
#'    only used when `check_header=TRUE`. This step is intended when
#'    the top row(s) contain fewer columns with headers, above actual
#'    column headers, for example the first row `c("Sample", "", "", "Lane", "")`,
#'    and the second row `c("Name", "Type", "Label", "Name", "Type")`.
#'    In this case the desired output is
#'    `"Sample_Name","Sample_Type","Sample_Label","Lane_Name","Lane_Type")`.
#'    This option default is `FALSE` due to the number of exceptions
#'    seen in real data.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are passed to `openxlsx::read.xlsx()`.
#'
#' @examples
#' # set up a test data.frame
#' set.seed(123);
#' lfc <- -3:3 + stats::rnorm(7)/3;
#' colorSub <- nameVector(
#'    rainbow2(7),
#'    LETTERS[1:7])
#' df <- data.frame(name=LETTERS[1:7],
#'    int=round(4^(1:7)),
#'    num=(1:7)*4-2 + stats::rnorm(7),
#'    fold=2^abs(lfc)*sign(lfc),
#'    lfc=lfc,
#'    pvalue=10^(-1:-7 + stats::rnorm(7)),
#'    hit=sample(c(-1,0,0,1,1), replace=TRUE, size=7));
#' df;
#' # write to tempfile for examples
#' if (check_pkg_installed("openxlsx")) {
#'    out_xlsx <- tempfile(pattern="writeOpenxlsx_", fileext=".xlsx")
#'    writeOpenxlsx(x=df,
#'       file=out_xlsx,
#'       sheetName="jamba_test",
#'       append=FALSE);
#'    # now read it back
#'    df_list <- readOpenxlsx(xlsx=out_xlsx);
#'    df_list[[1]]
#' }
#'
#' @export
readOpenxlsx <- function
(xlsx,
 sheet=NULL,
 startRow=1,
 startCol=1,
 rows=NULL,
 cols=NULL,
 check.names=FALSE,
 check_header=FALSE,
 check_header_n=10,
 verbose=FALSE,
 ...)
{
   if (!requireNamespace("openxlsx", quietly=TRUE)) {
      stop("The openxlsx package is required for readOpenxlsx().");
   }
   if (length(xlsx) == 0 || !file.exists(xlsx)) {
      stop("xlsx file not found.");
   }
   if (length(sheet) == 0) {
      sheet <- nameVector(openxlsx::getSheetNames(xlsx));
   } else if (length(names(sheet)) == 0) {
      if (is.numeric(sheet)) {
         sheets <- openxlsx::getSheetNames(xlsx);
         names(sheet) <- sheets[sheet];
      } else {
         names(sheet) <- sheet;
      }
   }

   # recycle startRow to length(sheet)
   if (length(startRow) < length(sheet)) {
      startRow <- rep(startRow, length.out=length(sheet));
   }
   # recycle startCol to length(sheet)
   if (length(startCol) < length(sheet)) {
      startCol <- rep(startCol, length.out=length(sheet));
   }
   # recycle rows to length(sheet)
   if (length(rows) == 0) {
      rows <- list(NULL);
   }
   if (!is.list(rows)) {
      rows <- list(rows);
   }
   if (length(rows) < length(sheet)) {
      rows <- rep(rows, length.out=length(sheet));
   }
   # recycle cols to length(sheet)
   if (length(cols) == 0) {
      cols <- list(NULL);
   }
   if (!is.list(cols)) {
      cols <- list(cols);
   }
   if (length(cols) < length(sheet)) {
      cols <- rep(cols, length.out=length(sheet));
   }

   sheet_idx <- nameVector(seq_along(sheet),
      names(sheet));
   dfs <- lapply(sheet_idx, function(j){
      i <- sheet[[j]];
      if (verbose) {
         printDebug("readOpenxlsx(): ",
            "reading sheet:",
            i);
      }
      # rows
      irows <- rmNA(rows[[j]]);
      istartRow <- rmNA(naValue=1, head(startRow[[j]], 1));
      if (length(istartRow) == 0 || any(istartRow < 1)) {
         istartRow <- 1;
      }
      # Adjustment when both startRow and rows are defined
      if (length(irows) > 0 && is.numeric(irows)) {
         istartRow <- min(irows);
      }
      # cols
      icols <- rmNA(cols[[j]]);
      istartCol <- rmNA(naValue=1, head(startCol[[j]], 1));
      if (length(istartCol) == 0 || any(istartCol < 1)) {
         istartCol <- 1;
      }
      # Adjustment when both startCol and cols are defined
      if (length(icols) > 0 && is.numeric(icols)) {
         istartCol <- min(icols);
      }

      if (verbose) {
         printDebug(#"readOpenxlsx(): ",
            indent=3,
            "rows:", irows,
            ", startRow:", istartRow,
            ", cols:", icols,
            ", startCol:", istartCol)
      }

      # optionally check for header rows
      header_v <- NULL;
      header_df <- NULL;
      if (length(check_header) > 0 && TRUE %in% check_header) {
         test_ncol <- integer(0);
         test_classes <- list();
         test_data_rows <- list();
         if (length(irows) > 0) {
            test_rows <- head(irows, check_header_n);
         } else {
            test_rows <- seq(from=istartRow, length.out=check_header_n);
         }
         for (irow in test_rows) {
            if (verbose) {
               printDebug(indent=3,
                  "check_header rows:", irow, ", cols:", cols);
            }
            df <- openxlsx::read.xlsx(xlsxFile=xlsx,
               sheet=i,
               skipEmptyCols=FALSE,
               rows=irow,
               cols=icols,
               colNames=FALSE);

            if (length(df) > 0 && istartCol > 1) {
               if (ncol(df) < istartCol) {
                  df <- NULL;
               } else {
                  col_seq <- seq(from=istartCol, to=ncol(df))
                  df <- df[, col_seq, drop=FALSE];
               }
            }
            if (length(df) > 0) {
               test_ncol[irow] <- ncol(df)
               test_data_rows[[irow]] <- df;
               test_classes[[irow]] <- sclass(df)
            } else {
               test_ncol[irow] <- NA;
               test_data_rows[[irow]] <- NULL;
               test_classes[[irow]] <- NULL;
            }
            if (length(df) == 0) {
               test_ncol[irow] <- NA;
               test_data_rows[[irow]] <- NULL;
               test_classes[[irow]] <- NULL;
            } else {
               test_ncol[irow] <- ncol(df)
               test_data_rows[[irow]] <- df;
               test_classes[[irow]] <- sclass(df)
            }
         }
         #data_ncol <- tail(test_ncol, 1);
         #data_ncol <- as.integer(names(head(tcount(tail(test_ncol, -2)), 1)));
         data_ncol <- max(tail(test_ncol, -1));
         if (verbose > 1) {
            printDebug("test_ncol: ", test_ncol);
            printDebug("data_ncol: ", data_ncol);
         }

         if (length(unique(test_ncol[!is.na(test_ncol)])) == 1 ||
               all(head(test_ncol, 1) == data_ncol)) {
            # no header row
         } else {
            # bottom-up: keep rows where ncol matches data_ncol
            istartRow <- length(test_ncol);
            test_seq <- rev(seq_along(test_ncol))
            for (irow in test_seq) {
               if (is.na(test_ncol[irow]) || test_ncol[irow] == data_ncol) {
                  istartRow <- irow;
               } else {
                  break;
               }
            }
            data_seq <- seq(from=istartRow,
               to=length(test_ncol))
            headerRows <- setdiff(test_rows, data_seq)
            if (verbose > 1) {
               printDebug("istartRow: ", istartRow);
               printDebug("data_seq: ", data_seq);
               printDebug("headerRows: ", headerRows);
            }
            if (length(headerRows) > 1) {
               warning(paste0("readOpenxlsx() found ",
                  length(headerRows),
                  " header rows in sheet '",
                  i,
                  "'. Please verify data to ensure there were no problems."));
            }
            header_df <- rbindList(test_data_rows[headerRows])
            if (verbose > 1) {
               print(header_df);
            }
            header_v <- pasteByRow(header_df[1, , drop=FALSE], sep="; ")
            if (verbose) {
               printDebug("readOpenxlsx(): ",
                  c("detected header: ", header_v),
                  sep="");
            }
         }
      }

      # load into data.frame
      idf <- call_fn_ellipsis(openxlsx::read.xlsx,
         xlsxFile=xlsx,
         sheet=i,
         skipEmptyCols=FALSE,
         startRow=istartRow,
         rows=irows,
         cols=icols,
         ...);
      # apply optional startCol only if icols is not already being used
      if (istartCol > 1 && length(icols) == 0) {
         if (ncol(idf) < istartCol) {
            idf <- NULL;
         } else {
            col_seq <- seq(from=istartCol, to=ncol(idf))
            idf <- idf[, col_seq, drop=FALSE];
         }
      }

      # When check.names=FALSE, manually load the header ourselves.
      # Note that openxlsx always applies check.names=TRUE, causing
      # column headers to become mangled.
      if (length(check.names) > 0 && !check.names) {
         if (length(irows) > 0) {
            k <- head(irows, 1);
         } else if (length(istartRow) > 0) {
            k <- istartRow;
         } else {
            k <- 1;
         }
         if (verbose > 1) {
            printDebug("readOpenxlsx(): ",
               c("column header row: ", k),
               sep="");
         }
         iheader <- openxlsx::read.xlsx(xlsxFile=xlsx,
            sheet=i,
            skipEmptyCols=FALSE,
            startRow=k,
            rows=k,
            colNames=FALSE);
         # apply optional startCol
         if (istartCol > 1) {
            if (ncol(iheader) < istartCol) {
               iheader <- NULL;
            } else {
               col_seq <- seq(from=istartCol, to=ncol(iheader))
               iheader <- iheader[, col_seq, drop=FALSE];
            }
         }
         # apply the header to the loaded data.frame
         colnames(idf) <- unname(unlist(iheader[1,]));
      }
      if (length(header_v) > 0) {
         names(dimnames(idf)) <- c("rows", header_v);
      }
      if (length(header_df) > 0) {
         attr(idf, "header") <- header_df;
      }
      idf;
   });
   return(dfs);
}

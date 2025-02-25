
#' convert numeric value or R object to human-readable size
#'
#' convert numeric value or R object to human-readable size
#'
#' This function returns human-readable size based upon `numeric` input.
#' Alternatively, when input is any other R object, it calls
#' `utils::object.size()` to produce a single `numeric` value which is then
#' used to produce human-readable size.
#'
#' The default behavior is to report computer size in bytes, where
#' 1024 is considered "kilo", however argument `kiloSize` can be
#' used to produce values where `kiloSize=1000` which is suitable
#' for monetary and other scientific values.
#'
#' @returns `character` vector representing human-friendly size,
#'    based upon the `kiloSize` argument to determine whether to
#'    report byte (1024) or scientific (1000) units.
#'
#' @family jam string functions
#'
#' @param x `numeric` vector, class `object_size` which is converted
#'    to `numeric`, any other R object is converted to a single `numeric`
#'    value using `utils::object.size()`.
#' @param digits `integer` number of digits used by `base::format()` when
#'    formatting the number to create a character string
#' @param abbreviateUnits `logical`, default TRUE,  whether to print
#'    abbreviated units, for example using k, M, G, T, P instead of
#'    kilo, mega, Giga, Tera, Peta, respectively.
#' @param unitType `character` string indicating the base unit of measure,
#'    by default "bytes". Note that trailing "s" is removed when the number
#'    is singular.
#' @param unitAbbrev `character` string indicating an abbreviated base unit,
#'    by default it uses the first character from `unitType.`
#' @param kiloSize `numeric`, default 1024,  number of base units when
#'    converting from to one "kilo" base unit. For computer-based size
#'    such as file size and object size, this value is 1024.
#'    For other purposes, such as scientific or monetary numbers, this
#'    value should usually be 1000.
#' @param sep `delimiter` used between the numeric value and the unit,
#'    default " ".
#' @param ... other parameters passed to `base::format()`.
#'
#' @examples
#' asSize(c(1, 10,2010,22000,52200))
#' #> "1 byte"   "10 bytes" "2 kb"     "21 kb"    "51 kb"
#'
#' # demonstration of straight numeric units
#' asSize(c(1, 100, 1000, 10000), unitType="", kiloSize=100)
#'
#' @export
asSize <- function
(x,
 digits=3,
 abbreviateUnits=TRUE,
 unitType="bytes",
 unitAbbrev=gsub("^(.).*$", "\\1", unitType),
 kiloSize=1024,
 sep=" ",
 ...)
{
   # validate input
   if (length(x) == 0) {
      return(character(0))
   }
   # non-numeric input is converted to numeric using utils::object.size()
   if ("object_size" %in% class(x)) {
      x <- as.numeric(x);
   }
   if (!is.numeric(x)) {
      x <- utils::object.size(x)
   }

   ## unitType is typically "byte" but can be any relevant unit, e.g.
   ## unitType="" and kiloSize=1000 will convert to user-friendly
   ## numerical values, so 4500 becomes 4.5k
   if (is.null(unitType)) {
      unitType <- "";
   }
   if (is.null(unitAbbrev)) {
      unitAbbrev <- gsub("^(.).*$", "\\1", unitType);
   }

   if (abbreviateUnits) {
      sizeUnits <- c(unitType, paste0(c("k", "M", "G", "T", "P"),
         unitAbbrev));
      sizeUnitsX <- nameVector(c(0, 1, 2, 3, 4, 5),
         sizeUnits);
   } else {
      sizeUnits <- paste0(c("", "kilo", "Mega", "Giga", "Tera", "Peta"),
         unitType);
      sizeUnitsX <- nameVector(c(0, 1, 2, 3, 4, 5),
         sizeUnits);
   }
   xUnits <- rep("", length(x));
   xValues <- x;
   ## Iterate through large to small values, progressively dividing out
   ## orders of magnitude until the result fits within the range available
   for (i in names(rev(sizeUnitsX))) {
      sizeUnitsXi <- sizeUnitsX[names(sizeUnitsX) %in% i];
      whichX <- (!is.na(x) &
            xUnits %in% "" &
            x >= kiloSize^sizeUnitsXi);
      xUnits[whichX] <- i;
      xValues[whichX] <- x[whichX] / kiloSize^sizeUnitsXi;
   }
   ## If we have zeros, and we have a unit defined for zero, we use that
   ## to describe the zeros, e.g. "0 bytes"
   if (any(sizeUnitsX == 0) && any(!is.na(xValues) & xValues == 0)) {
      xUnits[!is.na(xValues) & xValues == 0] <- names(which(sizeUnitsX == 0));
   }
   ## If we have values==1, remove trailing 's', a common pet peeve of mine
   xOnes <- which(!is.na(xValues) & xValues == 1);
   if (length(xOnes) > 0) {
      xUnits[xOnes] <- gsub("([^s])s$", "\\1", xUnits[xOnes]);
   }

   ## Style 2: decimals are independent per value
   xValuesV <- sapply(xValues, function(i){
      format(trim=TRUE,
         digits=digits,
         i);
   });

   ## Create one label
   newX <- gsub("[ ]+$", "",
      paste(xValuesV,
         xUnits,
         sep=sep));
   newX[is.na(x)] <- NA;

   return(newX);
}


#' convert size to numeric value
#'
#' convert size to numeric value
#'
#' This function is intended to provide the inverse of `asSize()`
#' by converting an abbreviated size into a full numeric value.
#'
#' It makes one simplifying assumption, that the first character in
#' the unit is enough to determine the unit. This assumption also means
#' the units are currently case-sensitive, for example `Mega` requires
#' upper-case `"M"`, because `"milli"` which is not supported,
#' requires `"m"`.
#'
#' Unit abbreviations recognized:
#' * `k` - kilo - size is defined by `kiloSize`
#' * `M` - Mega - size is defined by `kiloSize ^ 2`
#' * `G` - Giga - size is defined by `kiloSize ^ 3`
#' * `T` - Tera - size is defined by `kiloSize ^ 4`
#' * `P` - Peta - size is defined by `kiloSize ^ 5`
#'
#' Everything else is considered to have no abbreviated units, thus
#' the numeric value is returned as-is.
#'
#' Note that the round trip `asSize()` followed by `sizeAsNum()` will
#' not produce identical values, because the intermediate value is
#' rounded by `digits` in `asSize()`.
#'
#' @returns `numeric` vector representing the numeric value represented
#'    by an abbreviated size.
#'
#' @family jam string functions
#'
#' @param x `character` vector. When `x` is numeric, it is returned as-is;
#'    otherwise x is coerced to `character` with `as.character()` and
#'    will throw an error if it fails.
#' @param kiloSize `numeric` number of base units when converting from one
#'    base unit, to one "kilo" base unit. For file sizes, this value is 1024,
#'    but for other purposes this value may be 1000, like one thousand units
#'    is `"1k units"`.
#' @param verbose `logical` indicating whether to print verbose output.
#'    The output includes a `data.frame` summarizing the input,
#'    and the unit matched, and the final value. If `verbose==2` it
#'    will return this `data.frame` for review.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' x <- asSize(c(1, 10,2010,22000,52200), unitType="")
#' x
#' #> "1"   "10" "2k"     "21k"    "51k"
#' sizeAsNum(x)
#'
#' sizeAsNum(x, kiloSize=1000)
#'
#' @export
sizeAsNum <- function
(x,
 kiloSize=1024,
 verbose=FALSE,
 ...)
{
   ## This function essentially reverses asSize()
   ## and prints a numeric value based upon an abbreviated size
   if ("object_size" %in% class(x)) {
      x <- as.numeric(x);
   }
   if (is.numeric(x)) {
      return(x);
   }

   sizeUnits <- c("k", "M", "G", "T", "P");
   sizeUnitsX <- nameVector(c(1, 2, 3, 4, 5),
      sizeUnits);

   # verify input is characteror coerce to character
   if (!"character" %in% class(x)) {
      x <- tryCatch({
         as.character(x);
      }, error=function(e){
         stop("Input x must be coercible to class 'character'.")
      });
   }

   # trim leading and trailing whitespace
   if (igrepHas("^[ \t]+|[ \t]+$|[ \t]+", x)) {
      x <- gsub("^[ \t]+|[ \t]+$|[ \t]+",
         "",
         x);
   }

   # valid num:
   # 1000
   # 1,000
   # 1000.02
   # 100.1
   # 100.1e3
   # 100.1e-2
   # 100.1e+2
   # -100.1e+5
   valid_num_grep <- "^([-+]{0,1}[0-9,]*(|[.][0-9]*)(|e[-+]{0,1}[0-9]+))";
   # valid unit:
   # ""
   # k
   # kilo
   # M
   # Mega
   valid_unit_grep <- "(([a-zA-Z])[a-zA-Z]*$|$)";

   # detect valid numeric size strings
   # and allow NA values
   has_valid_num <- grepl(valid_num_grep, x) | is.na(x);
   has_valid_unit <- grepl(paste0(valid_num_grep, valid_unit_grep), x)  | is.na(x);
   if (!all(has_valid_num)) {
      stop("Not all input x represent valid numeric values.");
   }
   if (!all(has_valid_unit)) {
      stop("Not all input x contain valid numeric and unit definitions.");
   }

   # detect units
   valid_num_gsub <- paste0(valid_num_grep, ".*$");
   valid_unit_gsub <- paste0(valid_num_grep, valid_unit_grep);
   # convert character string of a number to numeric value
   x_num <- as.numeric(gsub(",", "",
      gsub(valid_num_gsub, "\\1", x)));
   # extract the first character of the unit
   x_unit <- gsub(valid_unit_gsub,
      "\\5",
      x);
   x_value <- x_num;

   # adjust by units
   has_unit <- (x_unit %in% sizeUnits);
   if (any(has_unit)) {
      x_value[has_unit] <- x_num[has_unit] * (kiloSize ^ sizeUnitsX[ x_unit[has_unit] ]);
   }

   if (verbose) {
      printDebug("sizeAsNum(): ");
      x_df <- data.frame(x, x_num, x_unit, has_unit, x_value);
      print(x_df);
      if (verbose > 1) {
         return(x_df);
      }
   }
   return(x_value);

}

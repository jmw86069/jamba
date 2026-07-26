# Search for objects in the environment

Search for objects in the environment

## Usage

``` r
grepls(
  x,
  where = "all",
  ignore.case = TRUE,
  searchNames = TRUE,
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  `character` string used as a grep pattern

- where:

  `character` string compatible with
  [`base::ls()`](https://rdrr.io/r/base/ls.html) or if installed,
  [`AnnotationDbi::ls()`](https://rdrr.io/pkg/AnnotationDbi/man/Bimap-envirAPI.html).
  A special value `"all"` will search all environments on the search
  path [`base::search()`](https://rdrr.io/r/base/search.html) in order.

- ignore.case:

  `logical` indicating whether the pattern match is case-insensitive.

- searchNames:

  `logical` indicating whether names should also be searched, which is
  only relevant for `AnnDb` objects, for example `org.Mm.egSYMBOL2EG`
  from the `org.Mm.eg.db` Bioconductor package.

- verbose:

  `logical` indicating whether to print verbose output.

- ...:

  additional parameters are ignored.

## Value

`character` vector of matching object names, or if `where="all"` it
returns a named list whose names indicate the search environment name,
and whose entries are matching object names within each environment.

## Details

This function searches the active R environment for an object name using
[`vigrep()`](https://jmw86069.github.io/jamba/reference/vigrep.md)
(value, case-insensitive grep). It is helpful when trying to find an
object using a substring, for example `grepls("statshits")`.

## See also

Other jam grep functions:
[`igrep()`](https://jmw86069.github.io/jamba/reference/igrep.md),
[`igrepHas()`](https://jmw86069.github.io/jamba/reference/igrepHas.md),
[`igrepl()`](https://jmw86069.github.io/jamba/reference/igrepl.md),
[`provigrep()`](https://jmw86069.github.io/jamba/reference/provigrep.md),
[`unigrep()`](https://jmw86069.github.io/jamba/reference/unigrep.md),
[`unvigrep()`](https://jmw86069.github.io/jamba/reference/unvigrep.md),
[`vgrep()`](https://jmw86069.github.io/jamba/reference/vgrep.md),
[`vigrep()`](https://jmw86069.github.io/jamba/reference/vigrep.md)

## Examples

``` r
# Find all objects named "grep", which should find
# base grep() and jamba::vigrep() among other results.
grepls("grep");
#> $`package:jamba`
#>  [1] "grepls"    "igrep"     "igrepHas"  "igrepl"    "proigrep"  "provigrep"
#>  [7] "unigrep"   "unvigrep"  "vgrep"     "vigrep"   
#> 
#> $`package:base`
#> [1] "agrep"   "agrepl"  "grep"    "grepRaw" "grepl"   "grepv"  
#> 

# Find objects in the local environment
allStatsHits <- c(1:12);
someStatsHits <- c(1:3);
grepls("statshits");
#> named list()
# shortcut way to search only the .GlobalEnv, the active local environment
grepls("statshits", 1);
#> character(0)

# return objects with "raw" in the name
grepls("raw");
#> $`package:jamba`
#> [1] "drawLabels"
#> 
#> $`package:datasets`
#> [1] "penguins_raw"
#> 
#> $`package:base`
#>  [1] "all.equal.raw"      "as.data.frame.raw"  "as.raw"            
#>  [4] "charToRaw"          "grepRaw"            "is.raw"            
#>  [7] "raw"                "rawConnection"      "rawConnectionValue"
#> [10] "rawShift"           "rawToBits"          "rawToChar"         
#> 

# Require "Raw" to be case-sensitive
grepls("Raw", ignore.case=FALSE)
#> $`package:jamba`
#> character(0)
#> 
#> $`package:datasets`
#> character(0)
#> 
#> $`package:base`
#> [1] "charToRaw" "grepRaw"  
#> 
```

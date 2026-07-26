# Reload 'Rmarkdown' cache

Reload 'Rmarkdown' cache in the order files were created, into an R
environment

## Usage

``` r
reload_rmarkdown_cache(
  dir = ".",
  maxnum = 1000,
  max_cache_name = NULL,
  envir = new.env(),
  file_sort = c("globals", "objects", "ctime", "mtime"),
  preferred_load_types = c("lazyLoad", "load"),
  dryrun = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- dir:

  `character` path to the directory that contains 'Rmarkdown' cache
  files. Each file is recognized by the file extension `".rdx"`.

- maxnum:

  `integer` indicating the maximum number of cache files to re-load, in
  order.

- max_cache_name:

  `character` optional string indicating the name of an 'Rmarkdown'
  cache chunk where this function will stop loading cache data. All
  cache files after this point will not be loaded. This option is
  intended to help recreate the data available to a particular
  'Rmarkdown' chunk during processing.

- envir:

  `environment` in which data is populated, default
  [`new.env()`](https://rdrr.io/r/base/environment.html) creates a new
  `environment` which is returned invisibly.

- file_sort:

  `character` string indicating how to sort cache files, default uses
  best available, in order: "globals" (global index file), "objects"
  (object index file), "ctime" (creation time), "mtime" (modification
  time). The global index is preferred, and other options are intended
  for rare scenarios when the global index is not available. The methods
  using `"mtime"` or `"ctime"` **is less accurate**, yet may be
  sufficient for simple output.

  - `"globals"` uses the `"__globals"` file in the cache directory.

  - `"objects"` uses the `"__objects"` file in the cache directory.

  - `ctime` sorts by file creation time

  - `mtime` sorts by file modification time

- preferred_load_types:

  `character` string indicating the preferred load mechanism, default
  uses `"lazyLoad"` if '.rdx'/'.rdb' files are available, otherwise
  `"load"` to load '.RData'/'.rda' files. The 'lazyLoad' '.rdx'/.'rdb'
  files are created when 'Rmarkdown' options include
  `cache=TRUE, cache.lazy=TRUE`. The load option is used when
  `cache=TRUE, cache.lazy=FALSE` which is preferred for some analyses
  involving large data objects.

- dryrun:

  `logical` default FALSE, whether to perform a dry-run, which prints
  messages and does not load the data.

- verbose:

  `logical` default TRUE, whether to print verbose output. This argument
  is not passed to other functions.

- ...:

  additional arguments are passed to
  [`lazyLoad()`](https://rdrr.io/r/base/lazyload.html) or
  [`load()`](https://rdrr.io/r/base/load.html) as relevant to the method
  used to re-load the cache object data.

## Value

`envir` is returned invisibly, with data objects populated into that
`environment`.

## Details

This function is intended to help re-load 'Rmarkdown' cache files
created during the processing/rendering of an 'Rmarkdown' file.

By default, all cached R objects are loaded into the environment defined
by `envir`, However, **it is recommended that `envir` is used to define
a new environment** into which the cached session is loaded.

    cache_env <- new.env()
    reload_rmarkdown_cache(cachedir, envir=cache_env)

From then on, the cached data objects can be seen with `ls(cache_env)`
and retrieved with `get("objectname", envir=cache_env)`.

If supplied with `maxnum` or `max_cache_name` then the cache will be
loaded only up to this point, and not beyond. The recommended method to
determine the cache is to use `dryrun=TRUE` to view all sections, then
to choose the `integer` number, or `character` name to define the
maximum chunk to load.

## See also

Other jam practical functions:
[`breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md),
[`call_fn_ellipsis()`](https://jmw86069.github.io/jamba/reference/call_fn_ellipsis.md),
[`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md),
[`check_pkg_installed()`](https://jmw86069.github.io/jamba/reference/check_pkg_installed.md),
[`colNum2excelName()`](https://jmw86069.github.io/jamba/reference/colNum2excelName.md),
[`color_dither()`](https://jmw86069.github.io/jamba/reference/color_dither.md),
[`exp2signed()`](https://jmw86069.github.io/jamba/reference/exp2signed.md),
[`getAxisLabel()`](https://jmw86069.github.io/jamba/reference/getAxisLabel.md),
[`isFALSEV()`](https://jmw86069.github.io/jamba/reference/isFALSEV.md),
[`isTRUEV()`](https://jmw86069.github.io/jamba/reference/isTRUEV.md),
[`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md),
[`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md),
[`lldf()`](https://jmw86069.github.io/jamba/reference/lldf.md),
[`log2signed()`](https://jmw86069.github.io/jamba/reference/log2signed.md),
[`middle()`](https://jmw86069.github.io/jamba/reference/middle.md),
[`minorLogTicks()`](https://jmw86069.github.io/jamba/reference/minorLogTicks.md),
[`newestFile()`](https://jmw86069.github.io/jamba/reference/newestFile.md),
[`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md),
[`reload_qmd_cache()`](https://jmw86069.github.io/jamba/reference/reload_qmd_cache.md),
[`renameColumn()`](https://jmw86069.github.io/jamba/reference/renameColumn.md),
[`rmInfinite()`](https://jmw86069.github.io/jamba/reference/rmInfinite.md),
[`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md),
[`rmNAs()`](https://jmw86069.github.io/jamba/reference/rmNAs.md),
[`rmNULL()`](https://jmw86069.github.io/jamba/reference/rmNULL.md),
[`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)

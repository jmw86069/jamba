# Reload Quarto 'qmd' cache

Reload Quarto 'qmd' cache by chunk order, into an R environment

## Usage

``` r
reload_qmd_cache(
  dir = ".",
  qmd_file = NULL,
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

- qmd_file:

  `character` path to the source '.qmd' file which provides the order of
  chunks used. The chunks are expected to have lines that begin 'r
  chunk_name'. In theory, one could create a file with only such lines,
  and it would be sufficient to supply the order of chunks to be used.

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

  `character` with permitted cache load types, in order of preference.

  - 'lazyLoad': Reloads the R objects as result of each chunk using
    corresponding 'rdx' files. For Quarto documents, this option is the
    only reliable option to access the stored R objects.

  - 'load': Reloads the 'RData' files associated with each chunk,
    although in the case of Quarto documents, the 'RData' contains a
    representation of the R code used to generate the R objects, and not
    the R objects.

  - **Important:** The 'load' operation typically used with Rmarkdown to
    reload via 'RData' files is not supported for reloading the R data
    environment. Use 'lazyLoad'.

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

This function is intended to help re-load Quarto 'qmd' cache created
during the processing/rendering of a 'qmd' file. It is intended to be
roughly equivalent to
[`reload_rmarkdown_cache()`](https://jmw86069.github.io/jamba/reference/reload_rmarkdown_cache.md)
with specific changes to handle Quarto rendering, with the following
requirements:

1.  It requires the Quarto `'.qmd'` or `'.Qmd'` file used to produce the
    document.

    - When using default file paths, the 'dir' argument is used to
      detect the corresponding 'qmd_file'. For example
      `dir='document.name_cache/html/'` implies
      `qmd_file='document.name.qmd'`.

    - The '.qmd' file is necessary to establish the correct order of
      each output chunk. The Quarto output no longer includes an index
      file with cache files in order.

    - The order of R chunk names is parsed, therefore must follow basic
      formatting 'r chunkname, ...' or 'r, chunkname, chunk.options,
      ...' for example.

    - Chunks display a modified time stamp, to help recognize when the
      caching may have been refreshed out of order compared to the
      Quarto document.

2.  All chunks must be cached.

    - Any chunk with no R objects defined will indicate '(no-file)'.

    - The '(no-file)' indicates either (1) the chunk option
      'cache=FALSE' or (2) the chunk made no changes to the R
      environment.

3.  It will only reload cache chunks that produce R objects.

    - Some chunks do not produce visible output R objects, and therefore
      are not associated with a cache `.rdx` or `.RData` cache file.

4.  Context steps, including
    [`options()`](https://rdrr.io/r/base/options.html) and
    [`par()`](https://rdrr.io/r/graphics/par.html) are not applied.

    - The cache files only appear to represent R objects produced or
      modified in each chunk.

When `dir` is provided, it can often be used to infer the source '.qmd'
file. For example:

- `dir='some_long_filename_cache/html/'` can be used to infer the
  original file:

- `qmd_file='some_long_filename.qmd'`

This function is experimental because Quarto is not intended to provide
the final R environment. Instead, Quarto prioritizes running the steps
in a new, clean R environment, and does not then return the resulting
environment to the user session.

An official mechanism to reload the exact R environment would be to run
[`knitr::purl()`](https://rdrr.io/pkg/knitr/man/knit.html) which
extracts the R code from a Quarto or Rmarkdown document and evaluates
the code completely. In *most scenarios*, running `reload_qmd_cache()`
should be sufficient to reload the resulting cached R objects suitable
enough for continued exploration or review of the data.

By default, all cached R objects are loaded into the environment defined
by `envir`, However, **it is recommended that `envir` is used to define
a new environment** into which the cached session is loaded.

    cache_env <- new.env()
    reload_qmd_cache(cachedir, envir=cache_env)

From then on, the cached data objects can be seen with `ls(cache_env)`
and retrieved with `get("objectname", envir=cache_env)`.

If supplied with `maxnum` or `max_cache_name` then the cache will be
loaded only up to this point, and not beyond. The recommended method to
determine the cache is to use `dryrun=TRUE` to view all sections, then
to choose the `integer` number, or `character` name to define the
maximum chunk to load.

### Todo

- Evaluate how best to handle 'RData' file input via the 'load' cache
  type. Currently it returns each environment, each chunk defines one
  hidden R object with the R code to be evaluated by each chunk.

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
[`reload_rmarkdown_cache()`](https://jmw86069.github.io/jamba/reference/reload_rmarkdown_cache.md),
[`renameColumn()`](https://jmw86069.github.io/jamba/reference/renameColumn.md),
[`rmInfinite()`](https://jmw86069.github.io/jamba/reference/rmInfinite.md),
[`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md),
[`rmNAs()`](https://jmw86069.github.io/jamba/reference/rmNAs.md),
[`rmNULL()`](https://jmw86069.github.io/jamba/reference/rmNULL.md),
[`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)

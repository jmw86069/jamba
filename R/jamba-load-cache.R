
#' Reload Rmarkdown cache
#'
#' Reload Rmarkdown cache in the order files were created
#'
#' This function is intended to help re-load Rmarkdown cache files
#' created during the processing/rendering of an Rmarkdown file.
#'
#' By default, all cached R objects are loaded into the
#' global environment `globalenv()`.
#'
#' It can be given an argumen `envir` to store R objects inside
#' a specific `environment`.
#'
#' If supplied with `max_cache_name` then this function will only
#' load the cache chunks in order, until it recognizes that
#' chunk name. This option is intended to help restore the R
#' data available for a particular Rmarkdown chunk.
#'
#' @return this function does not return data, but instead is called
#'    for the by-product of loading data into the given `envir`
#'    environment.
#'
#' @family jam utility functions
#'
#' @param dir `character` path to the directory that contains Rmarkdown
#'    cache files. Each file is recognized by the file extension `".rdx"`.
#' @param maxnum `integer` indicating the maximum number of cache files
#'    to re-load, in order.
#' @param max_cache_name `character` optional string indicating the
#'    name of an Rmarkdown cache chunk where this function will stop
#'    loading cache data. All cache files after this point will
#'    not be loaded. This option is intended to help recreate the
#'    data available to a particular Rmarkdown chunk during
#'    processing.
#' @param envir `environment` where cache data will be loaded.
#' @param file_sort `character` string indicating how to sort cache files
#'    to place them in proper order for re-loading. The default is to use
#'    one RMarkdown cache index file, which should accurately reflect only
#'    RMarkdown chunks present in the .Rmd file during rendering, and
#'    in the order they appear in that file.
#'    Using `"mtime"` or `"ctime"` below will sort files by the modification
#'    or creation time, respectively, and is less accurate, but often
#'    sufficient for most purposes. It would only be advised if for
#'    some reason the `"__globals"` or `"__objects"` files are not present.
#'    * `"globals"` uses the `"__globals"` file in the cache directory.
#'    * `"objects"` uses the `"__objects"` file in the cache directory.
#'    * `ctime` sorts by file creation time, default
#'    * `mtime` sorts by file modification time
#' @param preferred_load_types `character` string indicating the preferred
#'    load mechanism to use. By default it will use `lazyLoad()` if `.rdx/.rdb`
#'    files are present, otherwise it falls back to using `load()` for `.RData`
#'    files.
#'    Remove `"lazyLoad"` to prevent lazy-loading of cached objects.
#'    * `"lazyLoad"` will try to use `lazyLoad()` to load `.rdx/.rdb` files
#'    * `"load"` will try to use `load()` to load `.RData` files
#' @param dryrun `logical` indicating whether to perform a dry-run,
#'    which prints messages but does not process the data.
#' @param verbose `logical` indicating whether to print verbose output.
#'    Note that this variable is not passed along to `load()`, since it
#'    is inconsistent with `lazyLoad()`.
#' @param ... additional arguments are passed to `lazyLoad()` or `load()`
#'    as relevant to the method used to re-load the cache object data.
#'
#' @export
reload_rmarkdown_cache <- function
(dir=".",
 maxnum=1000,
 max_cache_name=NULL,
 envir=globalenv(),
 file_sort=c(
    "globals",
    "objects",
    "ctime",
    "mtime"),
 preferred_load_types=c("lazyLoad",
    "load"),
 dryrun=FALSE,
 verbose=TRUE,
 ...)
{
   file_sort <- match.arg(file_sort);
   preferred_load_types <- match.arg(preferred_load_types,
      several.ok=TRUE);

   # load .rdx files
   rdx_files <- list.files(path=dir,
      pattern="[.]rdx$",
      full.names=TRUE)
   rd_type <- "rdx";
   if ("load" %in% head(preferred_load_types, 1) ||
         (length(rdx_files) == 0 &&
          "load" %in% preferred_load_types)) {
      # attempt to load RData files
      rdx_files <- list.files(path=dir,
         pattern="[.]rdata$",
         ignore.case=TRUE,
         full.names=TRUE)
      rd_type <- "rdata";
   }
   if (length(rdx_files) == 0) {
      stop(paste0("No .", rd_type, " files found in directory."));
   }

   # order by file create time by default
   if (any(c("ctime", "mtime") %in% file_sort)) {
      rdx_df <- file.info(rdx_files);
      rdx_df <- rdx_df[order(rdx_df[[file_sort]]), , drop=FALSE];
   } else {
      if ("globals" %in% file_sort) {
         globals_file <- file.path(dir, "__globals");
         if (!file.exists(globals_file)) {
            stop(paste0("File does not exist: '", globals_file, "'"))
         }
         globals_lines <- readLines(globals_file)
         globals_names <- sapply(strsplit(globals_lines, "\t"), head, 1)
         globals_pattern <- paste0(globals_names,
            "_[a-z0-9A-Z]+[.](RData|rdx)$")
         rdx_files <- provigrep(globals_pattern, rdx_files)
      } else if ("objects" %in% file_sort) {
         objects_file <- file.path(dir, "__objects");
         if (!file.exists(objects_file)) {
            stop(paste0("File does not exist: '", objects_file, "'"))
         }
         objects_lines <- readLines(objects_file)
         objects_names <- sapply(strsplit(objects_lines, "\t"), head, 1)
         objects_pattern <- paste0(objects_names,
            "_[a-z0-9A-Z]+[.](RData|rdx)$")
         rdx_files <- provigrep(objects_pattern, rdx_files)
      } else {
         stop(paste0("Un-implemented file_sort: '", file_sort, "'"))
      }
      rdx_df <- file.info(rdx_files);
   }

   # prepare rdx_names in order
   rdx_names <- gsub("[.](RData|rdx)$",
      "",
      ignore.case=TRUE,
      rownames(rdx_df));
   rdx_df$rdx_names <- rdx_names;

   # create user-friendly names
   rdx_labels <- basename(rdx_names);
   rdx_labels <- gsub("_[a-z0-9]{24,}$",
      "",
      rdx_labels);
   names(rdx_labels) <- rdx_names;

   # optionally subset the list to process
   if (length(maxnum) == 1 && !is.infinite(maxnum)) {
      rdx_df <- head(rdx_df,
         maxnum);
      rdx_names <- head(rdx_names,
         maxnum);
   }
   if (length(max_cache_name) > 0) {
      if (!any(max_cache_name %in% rdx_labels)) {
         jamba::printDebug("reload_rmarkdown_cache(): ",
            c("'",
               "max_cache_name",
               "' was not found in cache files. Re-run with ",
               "max_cache_name=NULL, dryrun=TRUE",
               " to see the list of cache names."),
            fgText=c("darkorange1", "red4"),
            sep="");
         return(invisible(NULL));
      }
      maxnum <- max(
         jamba::rmNA(match(max_cache_name,
            rdx_labels)),
         na.rm=TRUE);
      rdx_df <- head(rdx_df,
         maxnum);
      rdx_names <- head(rdx_names,
         maxnum);
   }

   cache_num <- 0;
   ll_objects <- list();
   for (rdx_name in rdx_names) {
      cache_num <- cache_num + 1;
      if (verbose) {
         fgText <- c("darkorange1", "dodgerblue");
         extra <- ": ";
         if (dryrun) {
            fgText <- c("darkorange1", "red4");
            extra <- "(dryrun): ";
         }
         jamba::printDebug(c("reload_rmarkdown_cache()",
            extra),
            c("re-loading entry ",
               cache_num,
            ": '",
               rdx_labels[rdx_name],
               "'"),
            fgText=fgText,
            sep="");
      }
      if (!dryrun) {
         if ("rdx" %in% rd_type) {
            ll <- lazyLoad(filebase=rdx_name,
               envir=envir,
               ...);
         } else if ("rdata" %in% rd_type) {
            rd_file <- rownames(subset(rdx_df, rdx_names %in% rdx_name))
            ll <- load(file=rd_file,
               envir=envir,
               ...);
         }
         ll_objects[[rdx_name]] <- ll;
      }
   }
   return(invisible(ll_objects))
}


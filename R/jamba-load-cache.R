
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
#'    to place them in proper order for re-loading. Note that
#'    file modification time `mtime` may incorrectly sort files
#'    that were modified after the initial processing of the
#'    Rmarkdown file.
#'    * `ctime` sorts by file creation time, default
#'    * `mtime` sorts by file modification time
#' @param dryrun `logical` indicating whether to perform a dry-run,
#'    which prints messages but does not process the data.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are passed to `lazyLoad()`.
#'
#' @export
reload_rmarkdown_cache <- function
(dir=".",
 maxnum=1000,
 max_cache_name=NULL,
 envir=globalenv(),
 file_sort=c("ctime", "mtime"),
 dryrun=FALSE,
 verbose=TRUE,
 ...)
{
   file_sort <- match.arg(file_sort);

   # load .rdx files
   rdx_files <- list.files(path=dir,
      pattern="[.]rdx$",
      full.names=TRUE)
   if (length(rdx_files) == 0) {
      stop("No .rdx files found in directory.");
   }

   # order by file create time by default
   rdx_df <- file.info(rdx_files);
   rdx_df <- rdx_df[order(rdx_df[[file_sort]]), , drop=FALSE];

   # prepare rdx_names in order
   rdx_names <- gsub("[.]rdx$", "", rownames(rdx_df));

   # create user-friendly names
   rdx_labels <- basename(rdx_names);
   rdx_labels <- gsub("_[a-z0-9]{24,}",
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
         ll <- lazyLoad(filebase=rdx_name,
            envir=envir,
            ...);
      }
   }
}


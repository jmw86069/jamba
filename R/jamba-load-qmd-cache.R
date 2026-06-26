#' Reload Quarto 'qmd' cache
#'
#' Reload Quarto 'qmd' cache by chunk order, into an
#' R environment
#' 
#' This function is intended to help re-load Quarto 'qmd' cache
#' created during the processing/rendering of a 'qmd' file.
#' It is intended to be roughly equivalent to
#' `reload_rmarkdown_cache()` with specific changes to handle
#' Quarto rendering, with the following requirements:
#' 
#' 1. It requires the Quarto `'.qmd'` or `'.Qmd'` file used
#' to produce the document.
#' 
#'    * When using default file paths, the 'dir' argument 
#'    is used to detect the corresponding 'qmd_file'.
#'    For example `dir='document.name_cache/html/'` implies
#'    `qmd_file='document.name.qmd'`.
#'    * The '.qmd' file is necessary to establish the
#'    correct order of each output chunk. The Quarto output no
#'    longer includes an index file with cache files in order.
#'    * The order of R chunk names is parsed, therefore must
#'    follow basic formatting '{r chunkname, ...}'
#'    or '{r, chunkname, chunk.options, ...}' for example.
#'    * Chunks display a modified time stamp, to help recognize
#'    when the caching may have been refreshed out of order
#'    compared to the Quarto document.
#' 
#' 2. All chunks must be cached.
#' 
#'    * Any chunk with no R objects defined will indicate '(no-file)'.
#'    * The '(no-file)' indicates either (1) the chunk option
#'    'cache=FALSE' or (2) the chunk made no changes to the R
#'    environment.
#' 
#' 3. It will only reload cache chunks that produce R objects.
#' 
#'    * Some chunks do not produce visible output R objects,
#'    and therefore are not associated with a cache `.rdx` or `.RData`
#'    cache file.
#' 
#' 4. Context steps, including `options()` and `par()` are not applied.
#' 
#'    * The cache files only appear to represent R objects produced
#'    or modified in each chunk.
#' 
#' When `dir` is provided, it can often be used to infer the source
#' '.qmd' file. For example:
#' 
#'    * `dir='some_long_filename_cache/html/'`
#'    can be used to infer the original file:
#'    * `qmd_file='some_long_filename.qmd'`
#' 
#' This function is experimental because Quarto is not intended
#' to provide the final R environment. Instead, Quarto prioritizes
#' running the steps in a new, clean R environment, and does
#' not then return the resulting environment to the user
#' session.
#' 
#' An official mechanism to reload
#' the exact R environment would be to run `knitr::purl()` which
#' extracts the R code from a Quarto or Rmarkdown document and
#' evaluates the code completely. In *most scenarios*, running
#' `reload_qmd_cache()` should be sufficient to reload the resulting
#' cached R objects suitable enough for continued exploration
#' or review of the data.
#'
#' By default, all cached R objects are loaded into the
#' environment defined by `envir`, However,
#' **it is recommended that `envir` is used to define a new environment**
#' into which the cached session is loaded.
#' 
#' ```
#' cache_env <- new.env()
#' reload_qmd_cache(cachedir, envir=cache_env)
#' ```
#'
#' From then on, the cached data objects can be seen with `ls(cache_env)`
#' and retrieved with `get("objectname", envir=cache_env)`.
#'
#' If supplied with `maxnum` or `max_cache_name` then the cache
#' will be loaded only up to this point, and not beyond.
#' The recommended method to determine the cache is to use `dryrun=TRUE`
#' to view all sections, then to choose the `integer` number, or
#' `character` name to define the maximum chunk to load.
#' 
#' ## Todo
#' 
#' * Evaluate how best to handle 'RData' file input via the 'load'
#' cache type. Currently it returns each environment, each chunk
#' defines one hidden R object with the R code to be evaluated
#' by each chunk.
#' 
#' @returns `envir` is returned invisibly, with data objects populated
#'    into that `environment`.
#'
#' @family jam practical functions
#' 
#' @inheritParams reload_rmarkdown_cache
#' @param qmd_file `character` path to the source '.qmd' file which
#'    provides the order of chunks used. The chunks are expected to
#'    have lines that begin 'r chunk_name'.  
#'    In theory, one could
#'    create a file with only such lines, and it would be sufficient
#'    to supply the order of chunks to be used.
#' @param preferred_load_types `character` with permitted cache load
#'    types, in order of preference.  
#'    * 'lazyLoad': Reloads the R objects as result of each chunk
#'    using corresponding 'rdx' files. For Quarto documents, this option
#'    is the only reliable option to access the stored R objects.
#'    * 'load': Reloads the 'RData' files associated with each chunk,
#'    although in the case of Quarto documents, the 'RData' contains
#'    a representation of the R code used to generate the R objects,
#'    and not the R objects.
#'    * **Important:** The 'load' operation typically used with Rmarkdown
#'    to reload via 'RData' files is not supported for reloading the
#'    R data environment. Use 'lazyLoad'.
#' 
#' @export
reload_qmd_cache <- function
(dir=".",
 qmd_file=NULL,
 maxnum=1000,
 max_cache_name=NULL,
 envir=new.env(),
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
   #
   preferred_load_types <- match.arg(preferred_load_types,
      several.ok=TRUE);

   # list .rdx files
   rdx_files <- list.files(path=dir,
      pattern="[.]rdx$",
      full.names=TRUE)
   rd_type <- "rdx";
   if ("load" %in% head(preferred_load_types, 1) ||
         (length(rdx_files) == 0 &&
         "load" %in% preferred_load_types)) {
      # attempt to load RData files
      rdx_files1 <- list.files(path=dir,
         pattern="[.]rdata$",
         ignore.case=TRUE,
         full.names=TRUE)
      if (length(rdx_files1) == 0 &&
         "lazyLoad" %in% preferred_load_types) {
         # keep rdx_files as above
      } else {
         rd_type <- "rdata";
         rdx_files <- rdx_files1;
      }
   }

   if (length(rdx_files) == 0) {
      stop(paste0("No .", rd_type, " files found in directory."));
   }
   if (verbose > 1) {
      printDebug("reload_qmd_cache(): ",
         "rdx_files: ", rdx_files);
   }

   # file.info - color by timestamp order
   rdx_files_info <- file.info(rdx_files);
   rdx_mtimes <- format(rdx_files_info$mtime);
   rdx_mtimes_rank <- rank(rdx_files_info$mtime)
   rdx_mtimes_ranknum <- as.numeric(factor(rank(rdx_files_info$mtime)))
   rdx_mtimes_ramp <- colorRampPalette(c("gold3", "orangered",
      "firebrick", "darkorchid", "royalblue1", "aquamarine"))(max(rdx_mtimes_ranknum))
   rdx_mtimes_color <- rdx_mtimes_ramp[rdx_mtimes_ranknum];
   names(rdx_mtimes_color) <- rdx_files;
   names(rdx_mtimes) <- rdx_files;

   # try to infer qmd_file
   if (length(qmd_file) == 0) {
      if (!any(grepl("_cache(/|$)", dir))) {
         stop(paste0("qmd_file must be provided, or 'dir' must",
            "contain '_cache/' or '_cache' to infer the .qmd file."));
      }
      qmd_file <- gsub("_cache(/.*|$)", ".qmd", dir)
      qmd_file_alt <- gsub("[.]qmd", "[.]Qmd", qmd_file);
      if (!file.exists(qmd_file) && !file.exists(qmd_file_alt)) {
         stop(paste0("qmd_file could not be inferred, and '",
            qmd_file, "' does not exist."));
      }
      if (!file.exists(qmd_file) && file.exists(qmd_file_alt)) {
         qmd_file <- qmd_file_alt;
      }
      if (verbose > 1) {
         printDebug("reload_qmd_cache(): ",
            "qmd_file: ", qmd_file);
      }
   }

   # get R chunk names in order
   chunk_lines <- jamba::vigrep("^```[{]r[, \\t].*[}]",
      readLines(qmd_file))
   rdx_labels <- gsub("^```[{]r[, \\t]+([^, ]+)[, \\t].*[}]",
      "\\1",
      chunk_lines);
   names(rdx_labels) <- rdx_labels;
   chunk_rdxs <- lapply(rdx_labels, function(ichunk){
      ipattern <- paste0("^", ichunk, "_[a-zA-Z0-9]+[.](RData|rdx)$")
      rdx_files[jamba::igrep(ipattern, basename(rdx_files))]
   })
   names(chunk_rdxs) <- rdx_labels;
   rdx_names <- rdx_labels;

   # optionally subset the list to process
   if (length(maxnum) == 1 && !is.infinite(maxnum)) {
      rdx_labels <- head(rdx_labels,
         maxnum);
   }
   if (length(max_cache_name) > 0) {
      if (!any(max_cache_name %in% rdx_labels)) {
         if (verbose) {
            jamba::printDebug("reload_rmarkdown_cache(): ",
               c("'",
                  "max_cache_name",
                  "' was not found in cache files. Re-run with ",
                  "max_cache_name=NULL, dryrun=TRUE",
                  " to see the list of cache names."),
               fgText=c("darkorange1", "red4"),
               sep="");
         }
         return(invisible(NULL));
      }
      maxnum <- max(
         jamba::rmNA(match(max_cache_name,
            rdx_labels)),
         na.rm=TRUE);
      rdx_labels <- head(rdx_labels,
         maxnum);
   }

   cache_num <- 0;
   ll_objects <- list();
   for (rdx_name in rdx_labels) {
      rdx_name_ws <- gsub("^[^ ]*([ ]*)$", "\\1",
         head(format(c(rdx_name, rdx_labels)), 1))
      cache_num <- cache_num + 1;
      use_rdx <- chunk_rdxs[[rdx_name]];
      use_rdx_mtime <- "";

      if (verbose) {
         fgText <- c("darkorange1", "dodgerblue");
         extra <- "";
         if (length(use_rdx) == 0) {
            fgText <- c("darkorange1", "grey");
            extra <- paste0(extra, "(no-file)");
         } else {
            # use_rdx_info <- file.info(use_rdx);
            use_rdx_info <- rdx_mtimes[use_rdx];
            use_rdx_mtime <- c("  ", rdx_name_ws, "(",
               use_rdx_info, ")")
            fgText <- c(fgText,
               rdx_mtimes_color[use_rdx]);
         }
         if (dryrun) {
            fgText[2] <- c("red4");
            extra <- paste0(extra, "(dryrun)");
         }
         extra <- paste0(extra, ": ");
         printDebug(c("reload_rmarkdown_cache()",
            extra),
            c("re-loading entry ",
               cache_num,
               ": '",
               rdx_labels[rdx_name],
               "'"),
            use_rdx_mtime,
            fgText=fgText,
            sep="");
      }
      if (length(use_rdx) == 0) {
         if (verbose > 1) {
            printDebug("reload_qmd_cache(): ",
               "No action taken.")
         }
         next;
      }
      if (!dryrun) {
         if ("rdx" %in% rd_type) {
            use_rdx_filebase <- gsub("[.]rdx$", "", use_rdx);
            if (verbose > 1) {
               printDebug("reload_qmd_cache(): ",
                  "lazyLoad(filebase='",
                  use_rdx_filebase,
                  "', envir=envir)");
            }
            ll <- lazyLoad(filebase=use_rdx_filebase,
               envir=envir,
               ...);
         } else if ("rdata" %in% rd_type) {
            if (verbose) {
               printDebug("reload_qmd_cache(): ",
                  "load('", use_rdx, "', envir=envir)");
            }
            ll <- load(file=use_rdx,
               envir=envir,
               ...);
         }
         ll_objects[[rdx_name]] <- ll;
      }
   }
   return(invisible(envir))
}

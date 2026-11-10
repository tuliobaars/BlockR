# Data pipeline: ingestion -> pre-processing -> cache.

#' Load the analysis-ready BlockR datasets
#'
#' The single entry point to the data. Returns the analysis-ready datasets,
#' using (in order) the first cache that is still valid:
#' 1. session memory (kept inside the package for the R session; makes
#'    repeated app launches instant),
#' 2. disk (`blockr_adam.rds` in `cache_dir`; survives R restarts),
#' 3. a fresh build: [ingest_adam()] then [preprocess_adam()], which is cached.
#'
#' Caches are invalidated automatically when the data-producing code changes
#' (package version or the ingestion/pre-processing functions) or when the data
#' source changes (source type, synthetic settings, 'random.cdisc.data'
#' version, or any parquet file's name, size or modification time).
#'
#' @inheritParams ingest_adam
#' @param use_cache Use (and maintain) the memory and disk caches?
#' @param refresh Ignore existing caches and rebuild from the source?
#' @param cache_global Also copy every dataset (`ADSL`, `ADAE`, ...) into
#'   `global_env` for easy console access?
#' @param cache_dir Folder of the disk cache.
#' @param global_env Environment receiving the dataset copies when
#'   `cache_global = TRUE`.
#' @param memory_env Environment holding the session memory cache.
#' @return A list with `datasets` (named list of data.frames), `meta`,
#'   `source` (the resolved source settings), `signature` and `created`.
#' @seealso [blockr_options], [blockr_clear_cache()]
#' @examples
#' \donttest{
#' bk <- load_blockr_data(cache_dir = tempdir())
#' names(bk$datasets)
#' # second call: served from the session memory cache
#' bk <- load_blockr_data(cache_dir = tempdir())
#' }
#' @export
load_blockr_data <- function(source = getOption("blockr.data_source", "bundled"),
                                synthetic = getOption("blockr.synthetic", list()),
                                parquet_dir = getOption("blockr.parquet_dir", "cache"),
                                use_cache = getOption("blockr.use_cache", TRUE),
                                refresh = getOption("blockr.refresh_cache", FALSE),
                                cache_global = getOption("blockr.cache_global", FALSE),
                                cache_dir = blockr_cache_dir_default(),
                                global_env = globalenv(),
                                memory_env = the) {
  t0 <- Sys.time()
  config <- blockr_source_config(source, synthetic, parquet_dir)
  signature <- blockr_data_signature(config)
  cache_file <- file.path(cache_dir, "blockr_adam.rds")
  result <- NULL

  if (isTRUE(use_cache) && !isTRUE(refresh)) {
    mem <- get0("data_cache", envir = memory_env, inherits = FALSE)
    if (is.list(mem) && identical(mem$signature, signature)) {
      result <- mem
      from <- "session memory cache"
    } else if (file.exists(cache_file)) {
      disk <- tryCatch(readRDS(cache_file), error = function(e) NULL)
      if (is.list(disk) && identical(disk$signature, signature)) {
        result <- disk
        from <- paste("disk cache", sQuote(cache_file, FALSE))
      }
    }
  }

  if (is.null(result)) {
    raw <- ingest_adam(config$source, config$synthetic, config$parquet_dir)
    result <- preprocess_adam(raw)
    result$source <- config
    result$signature <- signature
    result$created <- Sys.time()
    from <- sprintf("%s source (fresh ingestion + pre-processing)", config$source)
    if (isTRUE(use_cache)) {
      tryCatch(
        {
          dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
          saveRDS(result, cache_file)
        },
        error = function(e) warning("[BlockR] Could not write disk cache: ", conditionMessage(e), call. = FALSE)
      )
    }
  }

  if (isTRUE(use_cache)) {
    assign("data_cache", result, envir = memory_env)
  }

  if (isTRUE(cache_global)) {
    list2env(result$datasets, envir = global_env)
    message("[BlockR] Datasets available in the Global Environment: ", toString(names(result$datasets)))
  }

  message(sprintf(
    "[BlockR] %d datasets ready from %s in %.1fs",
    length(result$datasets), from, as.numeric(difftime(Sys.time(), t0, units = "secs"))
  ))
  result
}

#' Remove the BlockR data caches
#'
#' Clears the session memory caches (data and the app built from it) and
#' deletes the disk cache file; optionally also removes the dataset copies
#' made with `cache_global = TRUE`.
#'
#' @inheritParams load_blockr_data
#' @param remove_global_datasets Also remove `ADSL`, `ADAE`, ... from
#'   `global_env`?
#' @return Invisibly `TRUE`.
#' @examples
#' \donttest{
#' blockr_clear_cache(cache_dir = tempdir())
#' }
#' @export
blockr_clear_cache <- function(cache_dir = blockr_cache_dir_default(),
                                  remove_global_datasets = FALSE,
                                  global_env = globalenv(),
                                  memory_env = the) {
  mem <- get0("data_cache", envir = memory_env, inherits = FALSE)
  if (isTRUE(remove_global_datasets) && is.list(mem)) {
    suppressWarnings(rm(list = names(mem$datasets), envir = global_env))
  }
  if (!is.null(mem)) rm("data_cache", envir = memory_env)
  if (exists("app_cache", envir = memory_env, inherits = FALSE)) rm("app_cache", envir = memory_env)
  unlink(file.path(cache_dir, "blockr_adam.rds"))
  invisible(TRUE)
}

#' Cache signature: changes whenever the data-producing code or the source changes
#' @noRd
blockr_data_signature <- function(config = blockr_source_config(),
                                     code = blockr_code_fingerprint(),
                                     cache_version = 3L) {
  paste(c(cache_version, code, blockr_source_fingerprint(config)), collapse = "|")
}

#' Hash of the package version and of every function that produces data
#' @noRd
blockr_code_fingerprint <- function() {
  ns <- environment(blockr_code_fingerprint)
  pattern <- "^(ingest_|preprocess_|derive_|decode_ae_|ae_|decode_cm_|cm_dictionary$|adsl_character_vars$|recode_column$|synthetic_settings$)"
  fns <- sort(grep(pattern, ls(ns, all.names = TRUE), value = TRUE))
  rlang::hash(list(
    version = as.character(utils::packageVersion("BlockR")),
    code = lapply(mget(fns, envir = ns), deparse)
  ))
}

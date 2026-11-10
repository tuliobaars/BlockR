# The teal_data object: datasets + reproducible code + join keys.

#' Build the app's teal_data object
#'
#' Wraps [load_blockr_data()] in a `teal_data` object with join keys.
#' `blockr_join_keys()` returns the standard CDISC keys for the standard
#' datasets plus explicit keys for the derived dataset `ADLBPCA`, which is
#' not part of the CDISC defaults.
#'
#' Steps:
#' 1. Run the data pipeline in plain R, so any ingestion or pre-processing
#'    error is raised with a normal traceback (and the cache is filled).
#' 2. Record the reproducible code shown under "Show R code". It calls
#'    `BlockR::load_blockr_data()` with the *resolved* source settings written
#'    out (e.g. the synthetic seed), so it reproduces the same data in a new
#'    session. In the app session it is served from the memory cache.
#' 3. If the recorded code fails, stop with its real error message (teal
#'    returns code errors as a `qenv.error` object instead of raising them).
#'
#' @inheritParams load_blockr_data
#' @return `build_blockr_data()`: a `teal_data` object with 12 datasets
#'   and join keys. `blockr_join_keys()`: a `join_keys` object.
#' @examples
#' \donttest{
#' data <- build_blockr_data(cache_global = FALSE)
#' names(data)
#' teal.data::join_keys(data)
#' }
#' @export
build_blockr_data <- function(source = getOption("blockr.data_source", "bundled"),
                                 synthetic = getOption("blockr.synthetic", list()),
                                 parquet_dir = getOption("blockr.parquet_dir", "cache"),
                                 cache_global = getOption("blockr.cache_global", FALSE)) {
  # 1. Plain-R pipeline run (errors surface normally); resolves the settings
  config <- blockr_source_config(source, synthetic, parquet_dir)
  load_blockr_data(config$source, config$synthetic, config$parquet_dir, cache_global = cache_global)

  # 2. Reproducible teal code
  data <- teal.data::teal_data()
  data <- teal.code::eval_code(data, blockr_library_code())
  data <- within(
    data,
    {
      .blockr <- BlockR::load_blockr_data(
        source = BLOCKR_SOURCE,
        synthetic = BLOCKR_SYNTHETIC,
        parquet_dir = BLOCKR_PARQUET_DIR,
        cache_global = FALSE
      )
      ADSL <- .blockr$datasets$ADSL
      ADRS <- .blockr$datasets$ADRS
      ADLB <- .blockr$datasets$ADLB
      ADLBPCA <- .blockr$datasets$ADLBPCA
      ADTTE <- .blockr$datasets$ADTTE
      ADQS <- .blockr$datasets$ADQS
      ADAE <- .blockr$datasets$ADAE
      ADAETTE <- .blockr$datasets$ADAETTE
      ADEX <- .blockr$datasets$ADEX
      ADEG <- .blockr$datasets$ADEG
      ADVS <- .blockr$datasets$ADVS
      ADCM <- .blockr$datasets$ADCM
    },
    BLOCKR_SOURCE = config$source,
    BLOCKR_SYNTHETIC = config$synthetic,
    BLOCKR_PARQUET_DIR = config$parquet_dir
  )

  # 3. Surface teal-code errors instead of passing a qenv.error along
  if (inherits(data, "qenv.error")) {
    stop("[BlockR] The teal data code failed:\n", conditionMessage(data), call. = FALSE)
  }

  teal.data::join_keys(data) <- blockr_join_keys()
  data
}

#' @rdname build_blockr_data
#' @export
blockr_join_keys <- function() {
  c(
    teal.data::default_cdisc_join_keys[c(
      "ADSL", "ADRS", "ADLB", "ADTTE", "ADQS",
      "ADAE", "ADAETTE", "ADEX", "ADEG", "ADVS", "ADCM"
    )],
    teal.data::join_keys(
      # wide lab data for the PCA module: one row per subject
      teal.data::join_key("ADLBPCA", keys = c("STUDYID", "USUBJID")),
      teal.data::join_key("ADSL", "ADLBPCA", keys = c("STUDYID", "USUBJID"))
    )
  )
}

#' Packages used by the code the analysis modules generate (attached in the
#' teal code so "Show R code" is runnable on its own).
#' NOTE: MASS must be attached *before* dplyr, otherwise MASS::select() masks
#' dplyr::select().
#' @noRd
blockr_library_code <- function() {
  pkgs <- c(
    "MASS", "dplyr", "tidyr", "ggExtra", "ggpmisc", "ggpp", "goftest", "gridExtra",
    "htmlwidgets", "jsonlite", "lattice", "rlang", "rtables", "nestcolor", "broom",
    "colourpicker", "sparkline"
  )
  paste0("library(", pkgs, ")", collapse = "\n")
}

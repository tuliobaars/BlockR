#' BlockR: Drag-and-Drop Clinical Trial Analysis Built on 'teal'
#'
#' BlockR puts a drag-and-drop canvas on top of 'teal' so that people who do
#' not write R can query clinical trial data and build tables, listings and
#' graphs. Any output of the TLG Catalog is a starting point: its R code
#' becomes blocks (data, filters, derivations, table layout, statistics,
#' plot layers) stacked into pipelines and wired together, every block can
#' be changed, bypassed, removed or added from a palette, and the result is
#' recomputed as you go, with the R code of the whole canvas always
#' available. The TLG Catalog, exploratory and safety modules remain, on a
#' cached data pipeline that reads bundled or freshly generated synthetic
#' ADaM data, or ADaM parquet files.
#'
#' Main entry points:
#' * [run_app()] / [blockr_app()]: build and run the app.
#' * [load_blockr_data()]: the analysis-ready datasets (cached).
#' * [ingest_adam()], [preprocess_adam()]: the two pipeline steps.
#' * [adam_contract()]: what the data must provide.
#' * [blockr_ggplot2_args()]: the plot defaults.
#' * [tlg_example_data()]: example datasets used by some TLG Catalog outputs.
#' * [blockr_options]: all settings.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import teal
#' @import teal.modules.clinical
#' @import teal.modules.general
#' @import teal.picks
#' @importFrom rlang .data
## usethis namespace: end
NULL

# Session-level memory cache used by load_blockr_data()
the <- new.env(parent = emptyenv())

# Column names used with dplyr's non-standard evaluation, and the placeholders
# substituted into the reproducible teal code (see build_blockr_data()).
utils::globalVariables(c(
  "ABLFL", "ABLFL2", "AEREL", "AESER", "AETOXGR", "AGE", "ANRIND", "ARM", "ARMCD",
  "AVAL", "AVISIT", "AVISITN", "BNRIND", "CMSEQ", "CNSR", "CQ01NAM", "PARAMCD",
  "PARCAT1", "SEX", "SMQ01NAM", "SMQ02NAM", "STUDYID", "USUBJID", "is_event",
  "BLOCKR_SOURCE", "BLOCKR_SYNTHETIC", "BLOCKR_PARQUET_DIR", ".blockr"
))

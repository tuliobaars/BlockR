# Module-specific filters: each module starts with the population of its
# source template.
#   all modules      : ADSL SEX, ADSL AGE
#   Safety modules   : + SAFFL == "Y", ADLB/ADEX/ADEG AVAL
# The canvas and the TLG Catalog start from all subjects: each template
# chooses its own population (e.g. filter(SAFFL == "Y")), as a block.

#' Filter set for the app
#' @param modules The app's modules (see blockr_modules()).
#' @noRd
blockr_filters <- function(modules) {
  group <- function(label) {
    Filter(function(x) identical(x$label, label), modules$children)[[1]]
  }
  global_filter_ids <- c("ADSL SEX", "ADSL AGE")
  safety_filter_ids <- c("ADSL SAFFL", "ADLB AVAL", "ADEX AVAL", "ADEG AVAL")

  teal::teal_slices(
    count_type = "all",
    # shared by every module
    teal.slice::teal_slice(dataname = "ADSL", varname = "SEX", id = "ADSL SEX"),
    teal.slice::teal_slice(dataname = "ADSL", varname = "AGE", id = "ADSL AGE"),
    # safety-evaluable population + default safety filters
    teal.slice::teal_slice(dataname = "ADSL", varname = "SAFFL", selected = "Y", id = "ADSL SAFFL"),
    teal.slice::teal_slice(dataname = "ADLB", varname = "AVAL", id = "ADLB AVAL"),
    teal.slice::teal_slice(dataname = "ADEX", varname = "AVAL", id = "ADEX AVAL"),
    teal.slice::teal_slice(dataname = "ADEG", varname = "AVAL", id = "ADEG AVAL"),
    module_specific = TRUE,
    mapping = c(
      list(global_filters = global_filter_ids),
      sapply(leaf_labels(group("Safety Analysis")), function(x) safety_filter_ids, simplify = FALSE)
    )
  )
}

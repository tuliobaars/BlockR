# Pre-processing: turns the raw ADaM datasets from ingest_adam() into the
# analysis-ready datasets used by the app. The logic is taken unchanged from
# the teal.gallery templates (inst/templates/; ADQS and ADSL's age group from
# the efficacy template, whose module group BlockR does not include, because
# the TLG Catalog code uses them); only the packaging changed (one function
# per dataset, fully namespaced calls).

#' Pre-process raw ADaM datasets
#'
#' Turns the raw datasets from [ingest_adam()] into the analysis-ready
#' datasets used by the app, following the teal.gallery templates:
#' * `ADSL`: adds `AGEGR1` (age group) and converts character columns to factors.
#' * `ADLBPCA`: wide lab data (one column per lab x visit) for the PCA module.
#' * `ADQS`: the three post-baseline analysis visits.
#' * `ADAE`: readable adverse event names ([decode_ae_terms()]) and analysis flags.
#' * `ADCM`: real medication names and ATC classes ([decode_cm_terms()]).
#' * `ADAETTE`, `ADEX`, `ADVS`, `ADCM`: safety template derivations.
#'
#' @param raw Named list of raw datasets, e.g. from [ingest_adam()].
#' @return A list with `datasets` (named list of the 12 analysis-ready
#'   data.frames) and `meta` (extra information for the app configuration).
#' @examples
#' out <- preprocess_adam(ingest_adam("bundled"))
#' names(out$datasets)
#' levels(out$datasets$ADSL$AGEGR1)
#' @export
preprocess_adam <- function(raw) {
  list(
    datasets = list(
      # shared
      ADSL     = preprocess_adsl(raw$ADSL),
      ADLB     = raw$ADLB,
      # exploratory
      ADRS     = raw$ADRS,
      ADLBPCA  = derive_adlbpca(raw$ADLB),
      # time to event, questionnaires (TLG Catalog)
      ADTTE    = raw$ADTTE,
      ADQS     = preprocess_adqs(raw$ADQS),
      # safety
      ADAE     = preprocess_adae(raw$ADAE),
      ADAETTE  = preprocess_adaette(raw$ADAETTE),
      ADEX     = preprocess_adex(raw$ADEX),
      ADEG     = raw$ADEG,
      ADVS     = preprocess_advs(raw$ADVS),
      ADCM     = preprocess_adcm(raw$ADCM)
    ),
    meta = list(
      # ADSL columns that were character before being converted to factors
      adsl_char_vars = adsl_character_vars(raw$ADSL)
    )
  )
}

# ---- ADSL (shared) ---------------------------------------------------------------

#' @noRd
adsl_character_vars <- function(adsl) {
  names(Filter(isTRUE, sapply(adsl, is.character)))
}

#' @noRd
preprocess_adsl <- function(adsl) {
  labels <- teal.data::col_labels(adsl, fill = FALSE)
  char_vars <- adsl_character_vars(adsl)
  labels <- c(labels, AGEGR1 = "Age Group")

  adsl <- adsl |>
    dplyr::mutate(
      AGEGR1 = factor(dplyr::case_when(
        AGE < 45 ~ "<45",
        AGE >= 45 ~ ">=45"
      ))
    ) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(char_vars), factor))

  teal.data::col_labels(adsl) <- labels
  adsl
}

# ---- Exploratory -------------------------------------------------------------

# Wide lab data (one column per PARAMCD x AVISIT) for the PCA module
#' @noRd
derive_adlbpca <- function(adlb) {
  adlb |>
    dplyr::select(USUBJID, STUDYID, SEX, ARMCD, AVAL, AVISIT, PARAMCD) |>
    tidyr::pivot_wider(
      values_from = "AVAL",
      names_from = c("PARAMCD", "AVISIT"),
      names_sep = " - "
    )
}

# ---- Questionnaires (TLG Catalog: ANCOVA, MMRM) --------------------------------

#' @noRd
preprocess_adqs <- function(adqs) {
  labels <- teal.data::col_labels(adqs, fill = FALSE)
  adqs <- adqs |>
    dplyr::filter(ABLFL != "Y" & ABLFL2 != "Y") |>
    dplyr::filter(AVISIT %in% c("WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22")) |>
    dplyr::mutate(
      AVISIT = as.factor(AVISIT),
      AVISITN = rank(AVISITN) |>
        as.factor() |>
        as.numeric() |>
        as.factor()
    )
  teal.data::col_labels(adqs) <- labels
  adqs
}

# ---- Safety ------------------------------------------------------------------

#' @noRd
preprocess_adaette <- function(adaette) {
  adaette <- adaette |>
    dplyr::mutate(is_event = dplyr::case_when(
      grepl("TOT", .data$PARAMCD, fixed = TRUE) ~ TRUE,
      TRUE ~ CNSR == 0
    )) |>
    dplyr::mutate(n_events = dplyr::case_when(
      grepl("TOT", .data$PARAMCD, fixed = TRUE) ~ as.integer(.data$AVAL),
      TRUE ~ as.integer(is_event)
    )) |>
    teal.data::col_relabel(is_event = "Is an Event") |>
    teal.data::col_relabel(n_events = "Number of Events")

  adaette_ae <- dplyr::filter(adaette, grepl("TOT", .data$PARAMCD, fixed = TRUE)) |>
    dplyr::select(-"AVAL")
  adaette_oth <- dplyr::filter(adaette, !(grepl("TOT", .data$PARAMCD, fixed = TRUE)))

  adaette_tte <- adaette |>
    dplyr::filter(PARAMCD == "AEREPTTE") |>
    dplyr::select(USUBJID, ARM, ARMCD, AVAL)

  adaette_ae <- dplyr::full_join(adaette_ae, adaette_tte, by = c("USUBJID", "ARM", "ARMCD"))
  rbind(adaette_ae, adaette_oth)
}

#' @noRd
preprocess_adex <- function(adex) {
  labels <- teal.data::col_labels(adex, fill = FALSE)
  # Simulate the TDURD parameter, which is not in the ADEX data from
  # `random.cdisc.data` (fixed seed => identical values on every run)
  set.seed(1, kind = "Mersenne-Twister")
  adex <- adex |>
    dplyr::distinct(USUBJID, .keep_all = TRUE) |>
    dplyr::mutate(
      PARAMCD = "TDURD",
      PARAM = "Overall duration (days)",
      AVAL = sample(
        x = seq(1, 200),
        size = dplyr::n(),
        replace = TRUE
      ),
      AVALU = "Days",
      PARCAT1 = "OVERALL"
    ) |>
    dplyr::bind_rows(adex)
  adex <- adex |>
    dplyr::filter(PARCAT1 == "OVERALL" & PARAMCD %in% c("TDOSE", "TNDOSE", "TDURD"))
  teal.data::col_labels(adex) <- labels
  adex
}

# For real data, ADVS needs some preprocessing like grouping different ANRIND
# and BNRIND into abnormal
#' @noRd
preprocess_advs <- function(advs) {
  advs |>
    dplyr::mutate(ONTRTFL = ifelse(AVISIT %in% c("SCREENING", "BASELINE"), "", "Y")) |>
    teal.data::col_relabel(ONTRTFL = "On Treatment Record Flag") |>
    dplyr::mutate(ANRIND = as.character(ANRIND), BNRIND = as.character(BNRIND)) |>
    dplyr::mutate(
      ANRIND = dplyr::case_when(
        ANRIND == "HIGH HIGH" ~ "HIGH",
        ANRIND == "LOW LOW" ~ "LOW",
        TRUE ~ ANRIND
      ),
      BNRIND = dplyr::case_when(
        BNRIND == "HIGH HIGH" ~ "HIGH",
        BNRIND == "LOW LOW" ~ "LOW",
        TRUE ~ BNRIND
      )
    )
}

#' @noRd
preprocess_adcm <- function(adcm) {
  # real medication names and ATC classes instead of placeholder codes
  adcm <- decode_cm_terms(adcm)
  dplyr::mutate(adcm, CMSEQ = as.integer(CMSEQ))
}

# Study-specific analysis subgroups and baskets from ADAE
#' @noRd
preprocess_adae <- function(adae) {
  # Readable AE names first, so the AESI basket labels below use them too
  adae <- decode_ae_terms(adae)
  adae |>
    dplyr::mutate(
      TMPFL_SER = AESER == "Y",
      TMPFL_REL = AEREL == "Y",
      TMPFL_GR5 = AETOXGR == "5",
      TMP_SMQ01 = !is.na(SMQ01NAM),
      TMP_SMQ02 = !is.na(SMQ02NAM),
      TMP_CQ01 = !is.na(CQ01NAM)
    ) |>
    teal.data::col_relabel(
      TMPFL_SER = "Serious AE",
      TMPFL_REL = "Related AE",
      TMPFL_GR5 = "Grade 5 AE",
      TMP_SMQ01 = tern::aesi_label(adae$SMQ01NAM, adae$SMQ01SC),
      TMP_SMQ02 = tern::aesi_label(adae$SMQ02NAM, adae$SMQ02SC),
      TMP_CQ01 = tern::aesi_label(adae$CQ01NAM)
    )
}

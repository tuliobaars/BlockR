# Properties the pre-processing must satisfy for *any* valid input data.
# Each block runs once per data scenario (see helper-blockr.R).

label_of <- function(x) attr(x, "label", exact = TRUE)

for (sc in blockr_scenarios()) {
  test_that(sprintf("[%s] output has exactly the datasets the app expects", sc), {
    out <- scenario(sc)$processed
    expect_setequal(
      names(out$datasets),
      c("ADSL", "ADRS", "ADLB", "ADLBPCA", "ADTTE", "ADQS", "ADAE", "ADAETTE", "ADEX", "ADEG", "ADVS", "ADCM")
    )
    for (ds in names(out$datasets)) expect_s3_class(out$datasets[[ds]], "data.frame")
    expect_type(out$meta$adsl_char_vars, "character")
  })

  test_that(sprintf("[%s] pre-processing raised no warnings", sc), {
    expect_identical(scenario(sc)$warnings[grepl("AE codes", scenario(sc)$warnings)], character(0))
  })

  test_that(sprintf("[%s] ADSL: one row per subject, AGEGR1 consistent with AGE, no character columns", sc), {
    raw <- scenario(sc)$raw$ADSL
    adsl <- scenario(sc)$processed$datasets$ADSL
    expect_identical(nrow(adsl), nrow(raw))
    expect_false(anyDuplicated(adsl$USUBJID) > 0)
    expect_s3_class(adsl$AGEGR1, "factor")
    expect_true(all(levels(adsl$AGEGR1) %in% c("<45", ">=45")))
    has_age <- !is.na(adsl$AGE)
    expect_identical(as.character(adsl$AGEGR1[has_age]), ifelse(adsl$AGE[has_age] < 45, "<45", ">=45"))
    expect_false(any(vapply(adsl, is.character, logical(1))))
    expect_identical(label_of(adsl$AGEGR1), "Age Group")
    expect_identical(scenario(sc)$processed$meta$adsl_char_vars, names(raw)[vapply(raw, is.character, logical(1))])
  })

  test_that(sprintf("[%s] ADSL: column labels survive pre-processing", sc), {
    raw <- scenario(sc)$raw$ADSL
    adsl <- scenario(sc)$processed$datasets$ADSL
    for (col in names(raw)) {
      if (!is.null(label_of(raw[[col]]))) expect_identical(label_of(adsl[[col]]), label_of(raw[[col]]), info = col)
    }
  })

  test_that(sprintf("[%s] ADRS unchanged; no efficacy subset any more", sc), {
    raw <- scenario(sc)$raw$ADRS
    ds <- scenario(sc)$processed$datasets
    expect_identical(ds$ADRS, raw)
    expect_null(ds$ADRS_EFF)
  })

  test_that(sprintf("[%s] ADLBPCA: one row per subject, one column per lab x visit", sc), {
    adlb <- scenario(sc)$raw$ADLB
    pca <- scenario(sc)$processed$datasets$ADLBPCA
    expect_identical(nrow(pca), nrow(unique(adlb[c("USUBJID", "STUDYID", "SEX", "ARMCD")])))
    combos <- unique(paste(adlb$PARAMCD, adlb$AVISIT, sep = " - "))
    expect_setequal(setdiff(names(pca), c("USUBJID", "STUDYID", "SEX", "ARMCD")), combos)
    # columns selected by default in the PCA module
    expect_true(all(c("ALT - WEEK 5 DAY 36", "CRP - WEEK 5 DAY 36", "IGA - WEEK 5 DAY 36") %in% names(pca)))
  })

  test_that(sprintf("[%s] ADQS: only the 3 post-baseline analysis visits", sc), {
    adqs <- scenario(sc)$processed$datasets$ADQS
    expect_gt(nrow(adqs), 0)
    expect_true(all(as.character(adqs$AVISIT) %in% c("WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22")))
    expect_true(all(adqs$ABLFL != "Y" & adqs$ABLFL2 != "Y"))
    expect_s3_class(adqs$AVISIT, "factor")
    expect_s3_class(adqs$AVISITN, "factor")
  })

  test_that(sprintf("[%s] ADAETTE: event flags and counts follow the template rules", sc), {
    raw <- scenario(sc)$raw$ADAETTE
    x <- scenario(sc)$processed$datasets$ADAETTE
    expect_type(x$is_event, "logical")
    expect_type(x$n_events, "integer")
    tot <- grepl("TOT", x$PARAMCD, fixed = TRUE)
    expect_true(all(x$is_event[tot]))
    expect_identical(as.vector(x$is_event[!tot]), as.vector(x$CNSR[!tot] == 0))
    expect_identical(as.vector(x$n_events[!tot]), as.integer(x$is_event[!tot]))
    # non-"TOT" records pass through unchanged in number (rows the full_join may
    # add for subjects without "TOT" records have PARAMCD = NA)
    expect_identical(sum(!tot & !is.na(x$PARAMCD)), sum(!grepl("TOT", raw$PARAMCD, fixed = TRUE)))
    expect_identical(label_of(x$is_event), "Is an Event")
    expect_identical(label_of(x$n_events), "Number of Events")
  })

  test_that(sprintf("[%s] ADEX: overall parameters only, simulated TDURD is valid and reproducible", sc), {
    raw <- scenario(sc)$raw$ADEX
    x <- scenario(sc)$processed$datasets$ADEX
    expect_true(all(x$PARCAT1 == "OVERALL"))
    expect_true(all(x$PARAMCD %in% c("TDOSE", "TNDOSE", "TDURD")))
    tdurd <- x[x$PARAMCD == "TDURD", ]
    expect_identical(nrow(tdurd), length(unique(raw$USUBJID)))
    expect_false(anyDuplicated(tdurd$USUBJID) > 0)
    expect_true(all(tdurd$AVAL >= 1 & tdurd$AVAL <= 200 & tdurd$AVAL == round(tdurd$AVAL)))
    expect_identical(preprocess_adex(raw), x)
  })

  test_that(sprintf("[%s] ADVS: abnormality grades collapsed, on-treatment flag derived", sc), {
    x <- scenario(sc)$processed$datasets$ADVS
    expect_type(x$ANRIND, "character")
    expect_false(any(c("HIGH HIGH", "LOW LOW") %in% c(x$ANRIND, x$BNRIND)))
    expect_identical(as.vector(x$ONTRTFL == ""), as.character(x$AVISIT) %in% c("SCREENING", "BASELINE"))
  })

  test_that(sprintf("[%s] ADCM: CMSEQ is an integer sequence", sc), {
    raw <- scenario(sc)$raw$ADCM
    x <- scenario(sc)$processed$datasets$ADCM
    expect_type(x$CMSEQ, "integer")
    expect_identical(x$CMSEQ, as.integer(raw$CMSEQ))
  })

  test_that(sprintf("[%s] ADAE: flags match their source columns", sc), {
    x <- scenario(sc)$processed$datasets$ADAE
    # as.vector(): the flags carry a label attribute, compare values only
    expect_identical(as.vector(x$TMPFL_SER), as.vector(x$AESER == "Y"))
    expect_identical(as.vector(x$TMPFL_REL), as.vector(x$AEREL == "Y"))
    expect_identical(as.vector(x$TMPFL_GR5), as.vector(x$AETOXGR == "5"))
    expect_identical(as.vector(x$TMP_SMQ01), !is.na(x$SMQ01NAM))
    expect_identical(as.vector(x$TMP_SMQ02), !is.na(x$SMQ02NAM))
    expect_identical(as.vector(x$TMP_CQ01), !is.na(x$CQ01NAM))
    expect_identical(label_of(x$TMPFL_SER), "Serious AE")
  })

  test_that(sprintf("[%s] ADAE: names replace codes without merging or losing records", sc), {
    raw <- scenario(sc)$raw$ADAE
    x <- scenario(sc)$processed$datasets$ADAE
    expect_identical(nrow(x), nrow(raw))
    for (col in c("AETERM", "AEDECOD", "AEBODSYS", "AESOC")) {
      # same number of distinct terms => counts per term are unchanged
      expect_identical(length(unique(x[[col]])), length(unique(raw[[col]])), info = col)
      expect_identical(is.na(x[[col]]), is.na(raw[[col]]), info = col)
      expect_false(any(grepl(ae_code_pattern, as.character(x[[col]]))), info = col)
    }
  })

  test_that(sprintf("[%s] all analysis datasets only contain ADSL subjects", sc), {
    ds <- scenario(sc)$processed$datasets
    for (nm in setdiff(names(ds), "ADSL")) {
      expect_true(all(as.character(ds[[nm]]$USUBJID) %in% as.character(ds$ADSL$USUBJID)), info = nm)
    }
  })

  test_that(sprintf("[%s] pre-processing is deterministic", sc), {
    raw <- scenario(sc)$raw
    expect_identical(suppressWarnings(preprocess_adam(raw)), scenario(sc)$processed)
  })
}

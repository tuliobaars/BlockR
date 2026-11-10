# ADAE: readable adverse event names.
#
# {random.cdisc.data} uses placeholder codes for the MedDRA-style hierarchy:
#   AESOC "cl A"  > AEBODSYS "cl A.1" > AEHLGT "hlgt A.1.1" > AEHLT "hlt A.1.1.1"
#   > AEDECOD "dcd A.1.1.1.1" (AELLT "llt ...", AETERM "trm ..." share the code)
# The depth of the code identifies the level, so one dictionary covers all
# levels. Names follow MedDRA spelling (e.g. "Diarrhoea"). Every code maps to a
# distinct name within its level, so no AE groups are merged and counts are
# unchanged. NOTE: the grades/seriousness stay as simulated by
# random.cdisc.data; the names are illustrative, not clinically matched.

#' Adverse event dictionary and decoding
#'
#' 'random.cdisc.data' uses placeholder codes for the MedDRA-style hierarchy:
#' `AESOC "cl A"` > `AEBODSYS "cl A.1"` > `AEHLGT "hlgt A.1.1"` >
#' `AEHLT "hlt A.1.1.1"` > `AEDECOD "dcd A.1.1.1.1"` (`AELLT "llt ..."` and
#' `AETERM "trm ..."` share the code). `ae_dictionary()` maps each code to an
#' illustrative MedDRA-style name (the code depth gives the level);
#' `decode_ae_terms()` applies it to the AE term columns and to the AESI basket
#' names (`SMQ01NAM`, `SMQ02NAM`, `CQ01NAM`).
#'
#' Every code maps to a distinct name within its level, so no AE groups are
#' merged and counts are unchanged. Values that are not codes (e.g. real
#' MedDRA terms) are left alone; codes missing from the dictionary are kept and
#' reported in a warning.
#'
#' @param adae An ADAE data.frame.
#' @return `ae_dictionary()`: named character vector (code -> name).
#'   `decode_ae_terms()`: `adae` with decoded values (same rows, labels kept).
#' @examples
#' head(ae_dictionary())
#' decode_ae_terms(data.frame(AEDECOD = c("dcd A.1.1.1.1", "dcd D.1.1.4.2")))
#' @export
ae_dictionary <- function() {
  c(
    # AESOC (depth 1)
    "A" = "Gastrointestinal disorders",
    "B" = "Cardiovascular disorders",
    "C" = "Infections and immune system disorders",
    "D" = "Nervous system and general disorders",
    # AEBODSYS (depth 2)
    "A.1" = "Gastrointestinal disorders",
    "B.1" = "Cardiac disorders",
    "B.2" = "Vascular disorders",
    "C.1" = "Infections and infestations",
    "C.2" = "Immune system disorders",
    "D.1" = "Nervous system disorders",
    "D.2" = "General disorders and administration site conditions",
    # AEHLGT (depth 3)
    "A.1.1" = "Gastrointestinal signs and symptoms",
    "B.1.1" = "Coronary artery disorders",
    "B.2.1" = "Vascular hypertensive disorders",
    "B.2.2" = "Decreased and nonspecific blood pressure disorders and shock",
    "C.1.1" = "Infections - pathogen unspecified",
    "C.2.1" = "Allergic conditions",
    "D.1.1" = "Central nervous system disorders",
    "D.2.1" = "General system disorders NEC",
    # AEHLT (depth 4)
    "A.1.1.1" = "Gastrointestinal signs and symptoms NEC",
    "B.1.1.1" = "Ischaemic coronary artery disorders",
    "B.2.1.2" = "Vascular hypertensive disorders NEC",
    "B.2.2.3" = "Vascular hypotensive disorders",
    "C.1.1.1" = "Lower respiratory tract and lung infections",
    "C.2.1.2" = "Allergic conditions NEC",
    "D.1.1.1" = "Central nervous system haemorrhages and cerebrovascular accidents",
    "D.1.1.4" = "Headaches NEC",
    "D.2.1.5" = "Asthenic conditions",
    # AEDECOD / AELLT / AETERM (depth 5)
    "A.1.1.1.1" = "Diarrhoea",
    "A.1.1.1.2" = "Nausea",
    "B.1.1.1.1" = "Myocardial infarction",
    "B.2.1.2.1" = "Hypertension",
    "B.2.2.3.1" = "Hypotension",
    "C.1.1.1.3" = "Pneumonia",
    "C.2.1.2.1" = "Hypersensitivity",
    "D.1.1.1.1" = "Cerebrovascular accident",
    "D.1.1.4.2" = "Headache",
    "D.2.1.5.3" = "Fatigue"
  )
}

#' @noRd
ae_code_pattern <- "^(cl|hlgt|hlt|llt|dcd|trm) ([A-Z](\\.[0-9]+)*)$"

# Whole-value codes, e.g. "dcd A.1.1.1.1" -> "Diarrhoea"
#' @noRd
decode_ae_values <- function(x, dict = ae_dictionary()) {
  code <- sub(ae_code_pattern, "\\2", x)
  code[!grepl(ae_code_pattern, x)] <- NA_character_
  out <- unname(dict[code])
  ifelse(is.na(out), x, out)
}

# Codes embedded in text, e.g. "C.1.1.1.3/B.2.2.3.1 AESI" -> "Pneumonia/Hypotension AESI"
#' @noRd
decode_ae_text <- function(x, dict = ae_dictionary()) {
  pt <- dict[lengths(strsplit(names(dict), ".", fixed = TRUE)) == 5]
  for (code in names(pt)) {
    x <- gsub(
      sprintf("(?<![A-Za-z0-9.])%s(?![0-9.])", gsub(".", "\\.", code, fixed = TRUE)),
      pt[[code]], x,
      perl = TRUE
    )
  }
  x
}

# Recode a character/factor column via its unique values, keeping attributes (labels)
#' @noRd
recode_column <- function(x, fun) {
  if (is.factor(x)) {
    levels(x) <- fun(levels(x))
  } else if (is.character(x)) {
    u <- unique(x)
    x[] <- unname(stats::setNames(fun(u), u)[x])
  }
  x
}

#' @rdname ae_dictionary
#' @export
decode_ae_terms <- function(adae) {
  term_cols <- intersect(c("AETERM", "AELLT", "AEDECOD", "AEHLT", "AEHLGT", "AEBODSYS", "AESOC"), names(adae))
  text_cols <- intersect(c("SMQ01NAM", "SMQ02NAM", "CQ01NAM"), names(adae))
  for (col in term_cols) adae[[col]] <- recode_column(adae[[col]], decode_ae_values)
  for (col in text_cols) adae[[col]] <- recode_column(adae[[col]], decode_ae_text)

  # Anything still coded is missing from ae_dictionary(): keep it, but say so
  leftover <- unique(unlist(lapply(term_cols, function(col) {
    v <- unique(as.character(adae[[col]]))
    v[!is.na(v) & grepl(ae_code_pattern, v)]
  })))
  if (length(leftover)) {
    warning("[BlockR] AE codes without a name in ae_dictionary() (left unchanged): ", toString(leftover), call. = FALSE)
  }
  adae
}

# ADCM: real medication names.
#
# {random.cdisc.data} uses placeholder codes for concomitant medications:
#   CMDECOD "medname B_1/4", CMTRT "B_1/4", CMCLAS / CMCAT "medcl B",
#   ATC1..ATC4 and ATC1CD..ATC4CD "ATCCLAS1 B" (a second or third ATC path of
#   the same medication: "ATCCLAS1 B p2", "... p3").
# The codes have a structure: class A holds 3 medications in one ATC group
# (one of them with a second ATC code), class B holds 4 (two with two more
# codes), class C holds 2 (both with a second code, one with a third). Real
# medications with that structure: NSAIDs (propionic acid derivatives), statins
# and penicillins, with their WHO ATC classes. Preferred names (CMDECOD)
# follow WHODrug; reported names (CMTRT) are what a site typically writes.
# NOTE: doses, routes, frequencies and indications stay as simulated by
# random.cdisc.data; the names are illustrative, not clinically matched.

#' Concomitant medication dictionary and decoding
#'
#' 'random.cdisc.data' uses placeholder codes for concomitant medications
#' (`CMDECOD "medname B_1/4"`, `CMTRT "B_1/4"`, `CMCLAS "medcl B"`,
#' `ATC2 "ATCCLAS2 B p2"`, ...). `cm_dictionary()` gives real names for them:
#' medications (NSAIDs, statins, penicillins), their reported names, their
#' classes and their WHO ATC classes and codes; `decode_cm_terms()` applies it
#' to ADCM.
#'
#' Medications keep distinct names, so counts by medication are unchanged. As
#' in the WHO ATC classification, different ATC paths can share their upper
#' levels (e.g. every statin path is in *CARDIOVASCULAR SYSTEM*), so tables by
#' ATC level 1 or 2 group them. Values that are not codes (e.g. real WHODrug
#' names) are left alone; codes missing from the dictionary are kept and
#' reported in a warning.
#'
#' @param adcm An ADCM data.frame.
#' @return `cm_dictionary()`: a list of named character vectors (code ->
#'   value): `medication`, `reported`, `class`, `atc` (one per level:
#'   `atc$ATC1`, ...) and `atc_code`. `decode_cm_terms()`: `adcm` with decoded
#'   values (same rows, labels kept).
#' @examples
#' cm_dictionary()$medication
#' decode_cm_terms(data.frame(CMDECOD = "medname B_1/4", ATC2 = "ATCCLAS2 B p2"))
#' @export
cm_dictionary <- function() {
  # an ATC path per code: level 1 to 4 names, and the level-4 code
  path <- function(l1, l2, l3, l4, code) list(names = c(l1, l2, l3, l4), code = code)
  paths <- list(
    "A" = path("MUSCULO-SKELETAL SYSTEM", "ANTIINFLAMMATORY AND ANTIRHEUMATIC PRODUCTS",
               "ANTIINFLAMMATORY AND ANTIRHEUMATIC PRODUCTS, NON-STEROIDS", "PROPIONIC ACID DERIVATIVES", "M01AE"),
    "A p2" = path("CARDIOVASCULAR SYSTEM", "CARDIAC THERAPY", "OTHER CARDIAC PREPARATIONS",
                  "OTHER CARDIAC PREPARATIONS", "C01EB"),
    "B" = path("CARDIOVASCULAR SYSTEM", "LIPID MODIFYING AGENTS", "LIPID MODIFYING AGENTS, PLAIN",
               "HMG COA REDUCTASE INHIBITORS", "C10AA"),
    "B p2" = path("CARDIOVASCULAR SYSTEM", "LIPID MODIFYING AGENTS", "LIPID MODIFYING AGENTS, COMBINATIONS",
                  "COMBINATIONS OF VARIOUS LIPID MODIFYING AGENTS", "C10BA"),
    "B p3" = path("CARDIOVASCULAR SYSTEM", "LIPID MODIFYING AGENTS", "LIPID MODIFYING AGENTS, COMBINATIONS",
                  "HMG COA REDUCTASE INHIBITORS, OTHER COMBINATIONS", "C10BX"),
    "C" = path("ANTIINFECTIVES FOR SYSTEMIC USE", "ANTIBACTERIALS FOR SYSTEMIC USE",
               "BETA-LACTAM ANTIBACTERIALS, PENICILLINS", "PENICILLINS WITH EXTENDED SPECTRUM", "J01CA"),
    "C p2" = path("ANTIINFECTIVES FOR SYSTEMIC USE", "ANTIBACTERIALS FOR SYSTEMIC USE",
                  "BETA-LACTAM ANTIBACTERIALS, PENICILLINS",
                  "COMBINATIONS OF PENICILLINS, INCL. BETA-LACTAMASE INHIBITORS", "J01CR"),
    "C p3" = path("SENSORY ORGANS", "OPHTHALMOLOGICALS", "ANTIINFECTIVES", "ANTIBIOTICS", "S01AA")
  )
  level <- function(k) stats::setNames(vapply(paths, function(p) p$names[[k]], character(1)), paste0("ATCCLAS", k, " ", names(paths)))
  code <- function(k) stats::setNames(vapply(paths, function(p) substr(p$code, 1, c(1, 3, 4, 5)[k]), character(1)), paste0("ATCCLAS", k, " ", names(paths)))
  list(
    medication = c(
      "medname A_1/3" = "NAPROXEN", "medname A_2/3" = "KETOPROFEN", "medname A_3/3" = "IBUPROFEN",
      "medname B_1/4" = "ATORVASTATIN", "medname B_2/4" = "ROSUVASTATIN",
      "medname B_3/4" = "SIMVASTATIN", "medname B_4/4" = "PRAVASTATIN",
      "medname C_1/2" = "AMOXICILLIN", "medname C_2/2" = "AMPICILLIN"
    ),
    reported = c(
      "A_1/3" = "NAPROXEN 500 MG", "A_2/3" = "KETOPROFEN", "A_3/3" = "ADVIL",
      "B_1/4" = "LIPITOR", "B_2/4" = "CRESTOR", "B_3/4" = "SIMVASTATIN 20MG", "B_4/4" = "PRAVACHOL",
      "C_1/2" = "AMOXIL", "C_2/2" = "AMPICILLIN 500MG"
    ),
    class = c(
      "medcl A" = "ANTIINFLAMMATORY AGENTS (NSAIDS)",
      "medcl B" = "LIPID MODIFYING AGENTS",
      "medcl C" = "ANTIBACTERIALS FOR SYSTEMIC USE"
    ),
    atc = list(ATC1 = level(1), ATC2 = level(2), ATC3 = level(3), ATC4 = level(4)),
    atc_code = list(ATC1CD = code(1), ATC2CD = code(2), ATC3CD = code(3), ATC4CD = code(4))
  )
}

#' @rdname cm_dictionary
#' @export
decode_cm_terms <- function(adcm) {
  dict <- cm_dictionary()
  maps <- c(
    list(CMDECOD = dict$medication, CMTRT = dict$reported, CMCLAS = dict$class, CMCAT = dict$class),
    dict$atc,
    # ATC1CD..ATC4CD hold the same placeholder as ATC1..ATC4
    stats::setNames(lapply(1:4, function(k) stats::setNames(dict$atc_code[[k]], names(dict$atc[[k]]))), names(dict$atc_code))
  )
  leftover <- character(0)
  for (col in intersect(names(maps), names(adcm))) {
    map <- maps[[col]]
    adcm[[col]] <- recode_column(adcm[[col]], function(v) {
      hit <- !is.na(v) & v %in% names(map)
      v[hit] <- unname(map[v[hit]])
      v
    })
    v <- unique(as.character(adcm[[col]]))
    leftover <- c(leftover, v[!is.na(v) & grepl("^(medname |medcl |ATCCLAS[1-4] )|^[A-Z]_[0-9]+/[0-9]+$", v)])
  }
  if (length(leftover)) {
    warning("[BlockR] Medication codes without a name in cm_dictionary() (left unchanged): ", toString(unique(leftover)), call. = FALSE)
  }
  adcm
}
